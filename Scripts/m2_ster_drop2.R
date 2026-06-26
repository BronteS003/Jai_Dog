################################################################################
##           MODEL 2: STERILIZATION STATUS (WITH DAY 2 DATA DROPPED)          ##
################################################################################
# Model 2 examining the effect of sterilization effort and other predictors    #
# on number of sterilization status. Using binomial GLMM. Day 2 data removed   #
# for comparison to model 2 with all data.                                     #
################################################################################
##Created by Bronte Slote, last edited June 26, 2026                          ##
################################################################################

##LOAD LIBRARIES##
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(stringr) #manipulating text
library(tidyr)
library(ggeffects)
library(lme4)
library(RVAideMemoire)
library(car)
library(DHARMa)
library(ggplot2)
library(patchwork)


################################################################################

##LOAD DATA##

#Create sightings data frame dropping all day 2
#import sightings .rds
sightings<- readRDS("Data/sightings.rds", refhook = NULL)
#remove all day 2 observations
sightings_no_re <- sightings %>%
  filter(day == 1)

#Remove puppies as they're not relevant to analysis (cannot be sterilized)
sightings_ster <- sightings_no_re %>% 
  filter(Puppy != 1)

#Remove adults with unknown sterilization status 
sightings_ster <- sightings_ster %>% 
  filter(Unknown != 1)

################################################################################

#Most complex model - since intervention
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
summary(m2_since)
#check vif
vif(m2_since) #all good
#drop 1 test
drop1(m2_since, test = "Chisq") #drop subdistrict

#Updated model 2 since
m2.1_since <- glmer(Neutered ~ since_intervention + owned + sex +
                      (1 | polygon),
                    family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m2.1_since, test = "Chisq") #drop owned

#Updated model 2 since
m2.2_since <- glmer(Neutered ~ since_intervention + sex +
                      (1 | polygon),
                    family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m2.2_since, test = "Chisq") #both significant

################################################################################

#Check fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_since)

#Check with DHARMa
simulationOutput_m2_since <- simulateResiduals(fittedModel = m2.2_since) #create simulated data

testDispersion(simulationOutput_m2_since)

testOutliers(simulationOutput_m2_since)

testZeroInflation(simulationOutput_m2_since)

testUniformity(simulationOutput_m2_since)

plot(simulationOutput_m2_since)

################################################################################
################################################################################

##Total effort model

#Most complex model
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#check VIF
vif(m2_total) #all good

#drop 1 test
drop1(m2_total, test = "Chisq") #drop subdistrict

#Updated model
m2.1_total <- glmer(Neutered ~ effort_humanpop + owned + sex +
                      (1 | polygon),
                    family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#drop 1 test
drop1(m2.1_total, test = "Chisq") #drop owned

#Updated model
m2.2_total <- glmer(Neutered ~ effort_humanpop + sex +
                      (1 | polygon),
                    family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m2.2_total, test = "Chisq") #both significant

################################################################################

#Check fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_total)

#Check with DHARMa
simulationOutput_m2_total <- simulateResiduals(fittedModel = m2.2_total) #create simulated data

testDispersion(simulationOutput_m2_total)

testOutliers(simulationOutput_m2_total)

testZeroInflation(simulationOutput_m2_total)

testUniformity(simulationOutput_m2_total)

plot(simulationOutput_m2_total)

################################################################################
################################################################################

#MODELS ANNUAL EFFORT

#Most complex model
m2_year <- glmer(Neutered ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#check VIF
vif(m2_year) #all good

#check drop 1
drop1(m2_year, test = "Chisq") #drop 4y ago

#updated model
m2.1_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                     (1 | polygon),
                   family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#check drop 1 
drop1(m2.1_year, test = "Chisq")#drop owned 

#updated model
m2.2_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + sex +
                     (1 | polygon),
                   family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#check drop 1
drop1(m2.2_year, test = "Chisq") #drop subdistrict

#updated model
m2.3_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex +
                     (1 | polygon),
                   family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#check drop 1
drop1(m2.3_year, test = "Chisq") #all significant

################################################################################

#Check fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.3_year)

#Check with DHARMa
simulationOutput_m2_year <- simulateResiduals(fittedModel = m2.3_year) #create simulated data

testDispersion(simulationOutput_m2_year)

testOutliers(simulationOutput_m2_year)

testZeroInflation(simulationOutput_m2_year)

testUniformity(simulationOutput_m2_year)

plot(simulationOutput_m2_year)

################################################################################
################################################################################

##COMPARE AIC##

AIC(m2.2_since, m2.2_total, m2.3_year)#year model has lowest AIC

################################################################################
################################################################################

##PLOT##

#Create predicted values for 3 years ago
preds_3y <- ggpredict(m2.3_year, c("effort_3y_humanpop[0,0.05,0.12]", "sex"),
                      condition = c(effort_2y_humanpop = 0,
                                    effort_1y_humanpop = 0))

p3 <- plot(preds_3y) +
  labs(
    title = NULL,
    x = "Total Sterilizations Conducted 3 Years Ago (Per Human Capita)",
    y = NULL
  ) +
  coord_cartesian(
    xlim = c(0,0.13),
    ylim = c(0,0.9)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12))

#Create predicted values for 2 years ago
preds_2y <- ggpredict(m2.3_year, c("effort_2y_humanpop[0,0.05,0.12]", "sex"),,
                      condition = c(effort_3y_humanpop = 0,
                                    effort_1y_humanpop = 0))

p2 <- plot(preds_2y) +
  labs(
    title = NULL,
    x = "Total Sterilizations 2 years ago (Per Human Capita)",
    y = "Probability of Being Neutered"
  ) +
  coord_cartesian(
    xlim = c(0,0.13),
    ylim = c(0,0.9)) +
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#Create predicted values for 1 years ago
preds_1y <- ggpredict(m2.3_year, c("effort_1y_humanpop[0,0.05,0.12]", "sex"),
                      condition = c(effort_2y_humanpop = 0,
                                    effort_3y_humanpop = 0))

p1 <- plot(preds_1y) +
  labs(
    title = NULL,
    x = "Total Sterilizations 1 year ago (Per Human Capita)",
    y = NULL
  ) +
  coord_cartesian(
    xlim = c(0,0.13),
    ylim = c(0,0.9)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12))

plot_year <-p1/p2/p3 +
  plot_annotation(tag_levels = "A")

################################################################################

#save plot
ggsave("Plots/m2_ster_drop2.png", plot = plot_year, width = 8, height = 6, dpi = 300)
