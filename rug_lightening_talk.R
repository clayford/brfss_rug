# BRFSS lightning talk
# UVa R Users' group - March 29, 2017

library(foreign)
library(survey)
library(ggplot2)

# https://www.cdc.gov/brfss/

# SAS Transport format downloaded from here:
# https://www.cdc.gov/brfss/annual_data/annual_2015.html

URL <- "https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015XPT.zip"
download.file(URL, destfile = basename(URL))
unzip(basename(URL))
# this takes a few seconds
brfss <- read.xport("LLCP2015.XPT")

# some data prep

# CHCCOPD1
# Ever told you have chronic obstructive pulmonary disease, emphysema or chronic bronchitis?
# 1 = Yes, 2 = No, 7 = Not Sure, 9 = Refused
prop.table(table(brfss$CHCCOPD1))
# recode 2 (no) as 0
brfss$COPD <- ifelse(brfss$CHCCOPD1==2, 0, brfss$CHCCOPD1)
brfss$COPD <- factor(ifelse(brfss$COPD %in% c(7,9), NA, brfss$COPD), 
                     labels = c("No","Yes"))
table(brfss$COPD, useNA = "ifany")

# X_RFSMOK3
# Adults who are current smokers (1 = no, 2 = yes, 9 = Don't know/Refused/Missing)
prop.table(table(brfss$X_RFSMOK3))
# set as factor
brfss$SMOKE <- ifelse(brfss$X_RFSMOK3 == 9, NA, brfss$X_RFSMOK3)
brfss$SMOKE <- factor(brfss$SMOKE, labels = c("No", "Yes"))
table(brfss$SMOKE, useNA = "ifany")

# X_RACE_G1
# Race groups used for internet prevalence tables
# 1 = White, 2 = Black, 3 = Hispanic, 4 = Other race only, Non-Hispanic,
# 5 = Multiracial, Non-Hispanic, BLANK = Don't know/Not sure/Refused 
table(brfss$X_RACE_G1, useNA = "ifany")
# create 4 groups
brfss$RACE <- ifelse(brfss$X_RACE_G1 %in% c(4,5), 4, brfss$X_RACE_G1)
brfss$RACE <- factor(brfss$RACE, labels = c("White","Black","Hispanic","Other"))
table(brfss$RACE, useNA = "ifany")

# X_AGE_G
# Imputed age in six groups
# 18-24, 25-34, 35-44, 45-54, 55-64, 65+
table(brfss$X_AGE_G)
brfss$AGEG4 <- ifelse(brfss$X_AGE_G %in% c(2,3), 2, brfss$X_AGE_G) 
brfss$AGEG4 <- ifelse(brfss$AGEG4 %in% c(4,5), 3, brfss$AGEG4) 
brfss$AGEG4 <- factor(brfss$AGEG4, 
                    labels = c("18-24", "25-44", "45-64", "65+"))

# create survey design object

# The svydesign object combines a data frame and all the survey design
# information needed to analyse it.

# This takes a minute or two
brfss.svy <- svydesign(ids = ~X_PSU,           # Primary Sampling Unit
                       strata = ~X_STSTR,      # Sample Design Stratification Variable 
                       weights = ~X_LLCPWT,    # Final weight assigned to each respondent
                       nest = TRUE,            # PSUs nested within strata
                       data = brfss)
brfss.svy


# analysis example 1 ------------------------------------------------------

# Replicate BRFSS Web Enabled Analysis Tool
# https://nccd.cdc.gov/s_broker/WEATSQL.exe/weat/index.hsql

# Model "Ever diagnosed with Chronic Obstructive Pulmonary Disease or COPD, 
# emphysema or chronic bronchitis" as a function of "Adults who are current
# smokers" for Virginia and Four level race/ethnicity category (computed)

# cond <- expression(X_STATE==51 & X_RFSMOK3 != "NA" & CHCCOPD1 %in% c(0,1))

sm1 <- svyglm(COPD ~ SMOKE + RACE + AGEG4, 
              design = brfss.svy, 
              subset= X_STATE == 51, 
              family=quasibinomial())
sm1.summary <- summary(sm1)
sm1.summary

# 95% CI for odds ratio 
round(exp(confint(sm1, method="Wald")),2)


# predicted probability of answering Yes given race and whether you smoke 
nd <- expand.grid(SMOKE = c("No","Yes"), 
                  RACE = c("White","Black","Hispanic","Other"),
                  AGEG4 = c("18-24", "25-44", "45-64", "65+"))
pred <- predict(sm1, newdata = nd, type = "response")
p.out <- as.data.frame(print(pred))
p.out <- cbind(p.out, nd)

# graph predicted probabilities and error bars
pd <- position_dodge(0.3)
p <- ggplot(p.out, aes(x = SMOKE, y = response, 
                       group = RACE, color = RACE)) + 
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = response - 2*SE, 
                    ymax = response + 2*SE), 
                width = 0.1, position = pd) +
  facet_wrap(~ AGEG4)
p

# y axis scaled to logit to show parallel slopes
p + scale_y_continuous(trans = scales::probability_trans("logis"))



# fit interactionS, something we cannot do with WEAT
sm2 <- svyglm(COPD ~ SMOKE + RACE + AGEG4 + SMOKE:AGEG4 + SMOKE:RACE, 
              design = brfss.svy, 
              subset= X_STATE==51, 
              family=quasibinomial())
sm2.summary <- summary(sm2)
sm2.summary
# are interactions significant?
sm2.anova <- anova(sm2, method = "Wald", test = "Chisq")
sm2.anova

pred2 <- predict(sm2, newdata = nd, type = "response")
p.out2 <- as.data.frame(print(pred2))
p.out2 <- cbind(p.out2, nd)

ggplot(p.out2, aes(x = SMOKE, y = response, group = RACE, color = RACE)) + 
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = response - 2*SE, 
                    ymax = response + 2*SE), 
                width = 0.1, position = pd) +
  facet_wrap(~AGEG4)

# rescaled y axis
ggplot(p.out2, aes(x = SMOKE, y = response, group = RACE, color = RACE)) + 
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = response - 2*SE, 
                    ymax = response + 2*SE), 
                width = 0.1, position = pd) +
  facet_wrap(~AGEG4) +
  scale_y_continuous(trans = scales::probability_trans("logis"))

# why the errors? Hispanics less likely to have COPD if they smoke?
xtabs(~ RACE + COPD + SMOKE, brfss,
      subset = X_STATE == 51)


tab <- svymean(~interaction(RACE, SMOKE, AGEG4, drop = TRUE),
               design = subset(brfss.svy, X_STATE==51 & COPD == "Yes"),
               na.rm = TRUE)

round(tab, 2)
ftab <- ftable(tab, rownames = list(SMOKE = c("No","Yes"),
                                    RACE = c("White","Black","Hispanic","Other"), 
                            AGEG4 = c("18-24", "25-44", "45-64", "65+")))
print(ftab, digits= 2)


# Does it matter whether or not we incorporate survey design?
# Fit model using raw data (without survey design)
m2 <- glm(COPD ~ SMOKE + RACE + AGEG4 + SMOKE:AGEG4 + SMOKE:RACE, 
              data = brfss, 
              subset= X_STATE==51, 
              family=binomial())
m2.summary <- summary(m2)

round(cbind(sm2.summary$coefficients[,1:2], 
            m2.summary$coefficients[,1:2]),
      3)

# Visualize with a coefficient plot
m2c <- data.frame(coef=rownames(m2.summary$coefficients),
                  m2.summary$coefficients[,1:2],
                  model = "glm",row.names = NULL)
sm2c <- data.frame(coef=rownames(sm2.summary$coefficients),
                  sm2.summary$coefficients[,1:2],
                  model = "svyglm",row.names = NULL)
cdat <- rbind(m2c, sm2c)

# First try
ggplot(cdat, aes(y=Estimate, x = coef, color=model)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin=Estimate - Std..Error, 
                    ymax=Estimate + Std..Error),
                width=0.1, position = pd) +
  coord_flip()

# remove obs with large SEs and intercept
ggplot(subset(cdat, !coef %in% c("SMOKEYes:RACEHispanic", "(Intercept)")), 
       aes(y=Estimate, x = coef, color=model)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin=Estimate - Std..Error, 
                    ymax=Estimate + Std..Error),
                width=0.1, position = pd) +
  coord_flip()


save.image(file = "brfss2015.Rdata")
load("brfss2015.Rdata")
