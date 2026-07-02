################################################################################
##           MODEL 2: STERILIZATION STATUS (WITH ALL DAYS OF DATA)            ##
################################################################################
# Model 2 examining the effect of sterilization effort and other predictors    #
# on number of sterilization status. Using binomial GLMM.                      #
################################################################################
##Created by Bronte Slote, last edited June 26, 2026                          ##
################################################################################

##Load Libraries
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(lme4) #creating mixed models with random effect
library(stringr) #manipulating text
library(RVAideMemoire) #checking for overdispersion
library(DHARMa) #checking overdispersion visually
library(ggeffects) #creating predicted values and visualizing them
library(lmtest) #conducting likelihood ratio tests
library(car) #check for multicollinearity
library(performance)
library(emmeans)
library(patchwork)
library(tidyr)
library(scales)

################################################################################

##LOAD DATA##

#load RDS file
sightings <- readRDS("Data/sightings.rds")

################################################################################

##CLEAN DATA##

#Remove puppies as they're not relevant to analysis (cannot be sterilized)
sightings <- sightings %>% 
  filter(Puppy != 1)

#Remove adults with unknown sterilization status 
sightings <- sightings %>% 
  filter(Unknown != 1)

################################################################################

##FIT MODEL - SINCE INTERVENTION##

#Most complex model
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_since)

#Check vif
vif(m2_since)

#Drop 1 test
drop1(m2_since, test = "Chisq") #drop subdistrict

#updated model 2 dropping subdistrict
m2.1_since <- glmer(Neutered ~ since_intervention + owned + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_since)

#Drop 1 test
drop1(m2.1_since, test = "Chisq") 

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.1_since) #good 1.037

#Check with DHARMa
simulationOutput_m2_since <- simulateResiduals(fittedModel = m2.1_since) #create simulated data

testDispersion(simulationOutput_m2_since)

testOutliers(simulationOutput_m2_since)

testZeroInflation(simulationOutput_m2_since)

testUniformity(simulationOutput_m2_since)

plot(simulationOutput_m2_since)

################################################################################

##FIT MODEL - TOTAL STERILIZATION EFFORT##

#Most complex model
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_total)

#check vif
vif(m2_total)

#Drop 1 test
drop1(m2_total, test = "Chisq") #drop subdistrict

#updated model 2 dropping subdistrict
m2.1_total <- glmer(Neutered ~ effort_humanpop + owned + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_total)

#Drop 1 test
drop1(m2.1_total, test = "Chisq") #drop owned

#updated model 2 dropping owned
m2.2_total <- glmer(Neutered ~ effort_humanpop + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2_total)

#Drop 1 test
drop1(m2.2_total, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_total) #good 0.989

#Check with DHARMa
simulationOutput_m2_total <- simulateResiduals(fittedModel = m2.2_total) #create simulated data

testDispersion(simulationOutput_m2_total)

testOutliers(simulationOutput_m2_total)

testZeroInflation(simulationOutput_m2_total)

testUniformity(simulationOutput_m2_total)

plot(simulationOutput_m2_total)

################################################################################

##FIT MODEL - YEARLY STERILIZATION EFFORT##

#Most complex model
m2_year <- glmer(Neutered ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_year)

#check vif
vif(m2_year)

#Drop 1 test
drop1(m2_year, test = "Chisq") #drop 4 years ago

#updated model 2 dropping 4 years ago
m2.1_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_year)

#Drop 1 test
drop1(m2.1_year, test = "Chisq") #drop subdistrict

#updated model 2 dropping owned
m2.2_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2_year)

#Drop 1 test
drop1(m2.2_year, test = "Chisq") #drop owned

#updated model 2 dropping owned
m2.3_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.3_year)

#Drop 1 test
drop1(m2.3_year, test = "Chisq") #all good

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.3_year) #good 0.968

#Check with DHARMa
simulationOutput_m2_year <- simulateResiduals(fittedModel = m2.3_year) #create simulated data

testDispersion(simulationOutput_m2_year)

testOutliers(simulationOutput_m2_year)

testZeroInflation(simulationOutput_m2_year)

testUniformity(simulationOutput_m2_year)

plot(simulationOutput_m2_year)

################################################################################

##MODEL COMPARISON##

#Compare models with AIC
AIC(m2.1_since, m2.2_total, m2.3_year) #year is the best fit with the lowest AIC (504.8550), then total (475.9618) and since (504.8525)

################################################################################

##PLOT MODEL##

#Create predicted values for 3 years ago
preds_3y <- ggpredict(m2.3_year, terms = c("effort_3y_humanpop [0,0.05,0.12]", "sex"),
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
    y = "Probability of Being Sterilized"
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
ggsave("Plots/m2_ster_all.jpg", plot = plot_year, width = 8, height = 6, dpi = 300)


