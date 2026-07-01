################################################################################
##          MODEL 1: DOG DENSITY (WITH ALL DAYS OF DATA)                      ##
################################################################################
# Model 1 examining the effect of sterilization effort and other predictors    #
# on number of dogs observed per transect. Using poisson GLMM.                 #
################################################################################
##Created by Bronte Slote, last edited June 26, 2026                          ##
################################################################################

##LOAD LIBRARIES##
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(lme4) #creating mixed models with random effect
library(stringr) #manipulating text
library(RVAideMemoire) #checking for overdispersion
library(DHARMa) #checking overdispersion visually
library(ggeffects) #creating predicted values and visualizing them
library(lmtest) # for likelihood ratio tests
library(car) # for variance inflation factors
library(corrplot) # to check for correlation between explanatory variables
library(patchwork) #combine graphs into 1 panel

################################################################################

##IMPORT DATA##

#Read rds for dog_density file
dog_density <- readRDS("Data/dog_density.rds", refhook = NULL)

################################################################################

##FIT MODEL - TIME SINCE INTERVENTION##

#Most complex model 
m1_since_intervention <- glmer(Sighting.Count ~ since_intervention + subdistrict + day + Mode.Transport +
                                 (1 | polygon/survey) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_since_intervention)
#check vif
vif(m1_since_intervention)
#check drop 1
drop1(m1_since_intervention, test = "Chisq")#drop day

#Updated model, dropping day
m1.1_since <- glmer(Sighting.Count ~ since_intervention + subdistrict + Mode.Transport +
                      (1 | polygon/survey) +
                      offset(log(Track.Length)), 
                    family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.1_since, test = "Chisq")#drop mode of transport

#Updated model, dropping mode of transport
m1.2_since <- glmer(Sighting.Count ~ since_intervention + subdistrict +
                      (1 | polygon/survey) +
                      offset(log(Track.Length)),
                    family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.2_since, test = "Chisq")#both significant

##CHECK MODEL FIT##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.2_since)

#Check with DHARMa
simulationOutput_m1_since <- simulateResiduals(fittedModel = m1.2_since) #create simulated data

testDispersion(simulationOutput_m1_since)

testOutliers(simulationOutput_m1_since)

testZeroInflation(simulationOutput_m1_since)

testUniformity(simulationOutput_m1_since)

plot(simulationOutput_m1_since) #is there a problem with quantile deviation?

################################################################################

##FIT MODEL - TOTAL STERILIZATION EFFORT##

#Create m1 using total sterilization effort by human population
m1_effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + subdistrict + day + Mode.Transport +
                              (1 | polygon/survey) +
                              offset(log(Track.Length)), 
                            family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_effort_humanpop)
#check vif
vif(m1_effort_humanpop)
#check drop 1
drop1(m1_effort_humanpop, test = "Chisq")#drop day

#Updated model dropping day
m1.1_effort <- glmer(Sighting.Count ~ effort_humanpop + subdistrict + Mode.Transport +
                       (1 | polygon/survey) +
                       offset(log(Track.Length)), 
                     family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.1_effort, test = "Chisq")#drop mode of transport

#Updated model dropping mode of transport
m1.2_effort <- glmer(Sighting.Count ~ effort_humanpop + subdistrict +
                       (1 | polygon/survey) +
                       offset(log(Track.Length)), 
                     family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.2_effort, test = "Chisq")#all significant

##CHECK MODEL FIT##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.2_effort)

#Check with DHARMa
simulationOutput_m1_effort <- simulateResiduals(fittedModel = m1.2_effort) #create simulated data

testDispersion(simulationOutput_m1_effort)

testOutliers(simulationOutput_m1_effort)

testZeroInflation(simulationOutput_m1_effort)

testUniformity(simulationOutput_m1_effort)

plot(simulationOutput_m1_effort)

################################################################################

##Sterilization by Year##

#Most complex m1 using years
m1_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + day + Mode.Transport +
                   (1 | polygon/survey) +
                   offset(log(Track.Length)), 
                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_year)

#check vif
vif(m1_year) #drop mode of transport
#updated model dropping mode of transport
m1.1_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + day +
                     (1 | polygon/survey) +
                     offset(log(Track.Length)), 
                   family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check vif
vif(m1.1_year)

#check drop 1
drop1(m1.1_year, test = "Chisq")#drop day

#Updated model dropping day
m1.2_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict +
                     (1 | polygon/survey) +
                     offset(log(Track.Length)), 
                   family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.2_year, test = "Chisq")#drop effort 2 years ago

#Updated model dropping effort 2y ago
m1.3_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + subdistrict +
                     (1 | polygon/survey) +
                     offset(log(Track.Length)), 
                   family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.3_year, test = "Chisq")#drop effort 4 years ago

#updated model dropping effort 4 years ago
m1.4_year <- glmer(Sighting.Count ~ effort_3y_humanpop + effort_1y_humanpop + subdistrict +
                     (1 | polygon/survey) +
                     offset(log(Track.Length)), 
                   family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.4_year, test = "Chisq")#drop 3y ago

#updated model dropping effort 3 years ago
m1.5_year <- glmer(Sighting.Count ~ effort_1y_humanpop + subdistrict +
                     (1 | polygon/survey) +
                     offset(log(Track.Length)), 
                   family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.5_year, test = "Chisq")#both significant

##CHECK MODEL FIT##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.5_year)

#Check with DHARMa
simulationOutput_m1_year <- simulateResiduals(fittedModel = m1.5_year) #create simulated data

testDispersion(simulationOutput_m1_year)

testOutliers(simulationOutput_m1_year)

testZeroInflation(simulationOutput_m1_year)

testUniformity(simulationOutput_m1_year)

plot(simulationOutput_m1_year)

################################################################################

##COMPARE MODELS##

AIC(m1.2_since,m1.2_effort,m1.5_year)#since and effort basically the same

################################################################################

##PLOTTING MODELS##

# Get predicted values over time since intervention
preds1_since <- ggpredict(m1.2_since,
                          terms = c("since_intervention", "subdistrict"),
                          condition = c(Track.Length = 1),
                          type = "random"
)

# Get predicted values over total effort
preds1_effort <- ggpredict(m1.2_effort,
                           terms = c("effort_humanpop", "subdistrict"),
                           condition = c(Track.Length = 1),
                           type = "random"
)

#Create raw data points for graphs - summarizing the average number of dogs sight per
# km per survey
raw_since <- dog_density %>%
  group_by(survey) %>%
  summarise(
    mean_density = mean(Dogs.per.km, na.rm = TRUE),
    since_intervention = first(since_intervention),   # keep variables for plotting
    subdistrict = first(subdistrict),
    .groups = "drop"
  )

#Plot since intervention
since <- plot(preds1_since) +
  geom_point(
    data = raw_since,
    aes(x = since_intervention, y = mean_density, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = NULL,
       x = "Time Since Intervention (year)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))

#Plot total effort
raw_effort <- dog_density %>%
  group_by(survey) %>%
  summarise(
    mean_density = mean(Dogs.per.km, na.rm = TRUE),
    effort_humanpop = first(effort_humanpop),   # keep variables for plotting
    subdistrict = first(subdistrict),
    .groups = "drop"
  )

effort <- plot(preds1_effort) +
  geom_point(
    data = raw_effort,
    aes(x = effort_humanpop, y = mean_density, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = NULL,
       x = "Total Sterilization Effort (Per Human Capita)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))

m1_plot <- since+effort +
  plot_annotation(tag_levels = "A")

#save plot
ggsave("Plots/m1_dd_all.png", plot = m1_plot, width = 12, height = 6, dpi = 300)
