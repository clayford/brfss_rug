# BRFSS lightning talk
# UVa R Users' group - March 29, 2017

library(foreign)
library(survey)
library(dplyr)
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
# derive new var: COPD
# recode 2 (no) as 0 and set 7 and 9 as NA
brfss$COPD <- ifelse(brfss$CHCCOPD1==2, 0, brfss$CHCCOPD1)
brfss$COPD <- factor(ifelse(brfss$COPD %in% c(7,9), NA, brfss$COPD), 
                     labels = c("No","Yes"))
table(brfss$COPD, useNA = "ifany")

# X_RFSMOK3
# Adults who are current smokers (1 = no, 2 = yes, 9 = Don't know/Refused/Missing)
table(brfss$X_RFSMOK3)
# derive new var: SMOKE
# set 9 to NA and make factor
brfss$SMOKE <- ifelse(brfss$X_RFSMOK3 == 9, NA, brfss$X_RFSMOK3)
brfss$SMOKE <- factor(brfss$SMOKE, labels = c("No", "Yes"))
table(brfss$SMOKE, useNA = "ifany")

# X_RACE_G1
# Race groups used for internet prevalence tables
# 1 = White, 2 = Black, 3 = Hispanic, 4 = Other race only, Non-Hispanic,
# 5 = Multiracial, Non-Hispanic, BLANK = Don't know/Not sure/Refused 
table(brfss$X_RACE_G1, useNA = "ifany")
# derive new var: RACE
# create 4 groups
brfss$RACE <- ifelse(brfss$X_RACE_G1 %in% c(4,5), 4, brfss$X_RACE_G1)
brfss$RACE <- factor(brfss$RACE, labels = c("White","Black","Hispanic","Other"))
table(brfss$RACE, useNA = "ifany")

# X_AGE_G
# Imputed age in six groups
# 1 = 18-24, 2 = 25-34, 3 = 35-44, 4 = 45-54, 5 = 55-64, 6 = 65+
table(brfss$X_AGE_G)
# derive new var: AGEG4
# create 4 groups (collapse 2/3 and 4/5 into one group, respectively)
brfss$AGEG4 <- ifelse(brfss$X_AGE_G %in% c(2,3), 2, brfss$X_AGE_G) 
brfss$AGEG4 <- ifelse(brfss$AGEG4 %in% c(4,5), 3, brfss$AGEG4) 
brfss$AGEG4 <- factor(brfss$AGEG4, 
                    labels = c("18-24", "25-44", "45-64", "65+"))
table(brfss$AGEG4)

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

# Number of unique strata per State
brfss %>% group_by(X_STATE) %>% 
  summarise(strata = length(unique(X_STSTR))) %>% 
  as.data.frame()

# analysis example 1 ------------------------------------------------------

# Replicate BRFSS Web Enabled Analysis Tool
# https://nccd.cdc.gov/s_broker/WEATSQL.exe/weat/index.hsql

# Model "Ever diagnosed with Chronic Obstructive Pulmonary Disease or COPD, 
# emphysema or chronic bronchitis" as a function of "Adults who are current 
# smokers" for Virginia, Four level race/ethnicity category (computed) and
# Reported age (18-24,25-44,45-64,65+)

sm1 <- svyglm(COPD ~ SMOKE + RACE + AGEG4, 
              design = brfss.svy, 
              subset = X_STATE == 51, 
              family = quasibinomial)
sm1.summary <- summary(sm1)
sm1.summary

# 95% CI for odds ratio 
round(exp(confint(sm1, method="Wald")),2)


# expected proportion of answering Yes given other variables
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
              subset = X_STATE==51, 
              family = quasibinomial)
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
xtabs(~ AGEG4 + RACE + SMOKE + COPD, brfss,
      subset = X_STATE == 51)


# Does it matter whether or not we incorporate survey design?

# Fit model using raw data (without survey design)
glm1 <- glm(COPD ~ SMOKE + RACE + AGEG4, 
              data = brfss, 
              subset= X_STATE==51, 
              family=binomial)
glm1.summary <- summary(glm1)

# Fit model using raw data allowing a random effect at level of strata
library(lme4)
glmer1 <- glmer(COPD ~ SMOKE + RACE + AGEG4 + (1|X_STSTR),
                data = brfss,
                subset= X_STATE==51,
                family=binomial)
glmer1.summary <- summary(glmer1)


round(cbind(svyglm = sm1.summary$coefficients[,1], 
            glm = glm1.summary$coefficients[,1],
            glmer = glmer1.summary$coefficients[,1]),
      3)

# Visualize with a coefficient plot
glm1c <- data.frame(coef=rownames(glm1.summary$coefficients),
                    glm1.summary$coefficients[,1:2],
                    model = "glm",row.names = NULL)
glmer1c <- data.frame(coef=rownames(glmer1.summary$coefficients),
                      glmer1.summary$coefficients[,1:2],
                      model = "glmer",row.names = NULL)
sm1c <- data.frame(coef=rownames(sm1.summary$coefficients),
                   sm1.summary$coefficients[,1:2],
                   model = "svyglm",row.names = NULL)

cdat <- rbind(glm1c, glmer1c, sm1c)

# coefficient plot
# remove obs with large SEs and intercept
ggplot(subset(cdat, coef != "(Intercept)"), 
       aes(y=Estimate, x = coef, color=model)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin=Estimate - Std..Error, 
                    ymax=Estimate + Std..Error),
                width=0.1, position = pd) +
  coord_flip()




save.image(file = "brfss2015.Rdata")
load("brfss2015.Rdata")
