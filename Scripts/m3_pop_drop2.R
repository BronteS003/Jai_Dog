################################################################################
##           MODEL 3: POPULATION STRUCTURE (WITH ALL DAY 2 DATA REMOVED)      ##
################################################################################
# Model 3.2 examining presence of puppies, using binomial GLM. Day 2 data      #
# removed for comparison to model 3.2 with all data included.                  #
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


################################################################################

#Most complex - since intervention
m3.2_since <- glm(Puppy ~ since_intervention + owned + subdistrict,
                  family = binomial, data = sightings_no_re)
vif(m3.2_since) #all fine
drop1(m3.2_since, test = "Chisq") #drop owned

#Updated model
m3.2.1_since <- glm(Puppy ~ since_intervention + subdistrict,
                    family = binomial, data = sightings_no_re)
drop1(m3.2.1_since, test = "Chisq") #both significant

summary(m3.2.1_since) #since intervention is not significant 

################################################################################

#most complex model - total effort
m3.2_effort <- glm(Puppy ~ effort_humanpop + owned + subdistrict,
                   family = binomial, data = sightings_no_re)
vif(m3.2_effort) #all fine
drop1(m3.2_effort, test = "Chisq") #drop owned

#updated model
m3.2.1_effort <- glm(Puppy ~ effort_humanpop + subdistrict,
                     family = binomial, data = sightings_no_re)
drop1(m3.2.1_effort, test = "Chisq") #drop effort

#updated model
m3.2.2_effort <- glm(Puppy ~ subdistrict,
                     family = binomial, data = sightings_no_re)
drop1(m3.2.2_effort, test = "Chisq") #no effort measures significant

################################################################################

#most complex model - annual effort
m3.2_year <- glm(Puppy ~ effort_4y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict,
                 family = binomial, data = sightings_no_re)

#check vif
vif(m3.2_year) #drop effort 1y
m3.2_year <- glm(Puppy ~ effort_4y_humanpop + effort_2y_humanpop + owned + subdistrict,
                 family = binomial, data = sightings_no_re)
#drop 1 test
drop1(m3.2_year, test = "Chisq")#4y

#updated model
m3.2.1_year <- glm(Puppy ~ effort_2y_humanpop + owned + subdistrict,
                   family = binomial, data = sightings_no_re)
#drop 1 test
drop1(m3.2.1_year, test = "Chisq")#drop owned

#updated model
m3.2.2_year <- glm(Puppy ~ effort_2y_humanpop + subdistrict,
                   family = binomial, data = sightings_no_re)

#drop 1 test
drop1(m3.2.2_year, test = "Chisq") #no effort measures significant


################################################################################

##COMPARE AIC##
AIC(m3.2.1_since, m3.2.2_effort, m3.2.2_year) 
#no significant measures of effort variables in puppy model, but since and year
#have lowest AICs

################################################################################

##PLOT##

# Get predicted values over time since intervention
preds1_since <- ggpredict(m3.2.1_since,
                          terms = c("since_intervention", "subdistrict")
)

#Plot
#set common y axis
common_y <- scale_y_continuous(
  limits = c(0, 0.25),
  breaks = seq(0, 0.25, by = 0.1),
  labels = scales::percent_format(accuracy = 1)
)

puppy_since <- plot(preds1_since) +
  labs(x = "Time Since Intervention (Years)", y = "Probability of being a puppy", color = "Subdistrict", title ="") +
  scale_y_continuous(limits = c(0, 0.30)) +
  theme(legend.position = "bottom",,
        legend.text = element_text(size = 9)) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


#Annual effort
preds_year <- ggpredict(m3.2.2_year, terms = c("effort_2y_humanpop", "subdistrict"))

puppy_annual <- plot(preds_year) +
  labs(x = "Sterilizations 2 Years Ago", y = "Probability of being a puppy", color = "Subdistrict", title ="") +
  theme(legend.position = "bottom",,
        legend.text = element_text(size = 9)) +
  scale_y_continuous(limits = c(0, 0.30)) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

day2_puppy <- puppy_since + puppy_annual +
  plot_annotation(tag_levels = "A")

################################################################################

#save plot
ggsave("Plots/m3_pup_drop2.jpg", plot = day2_puppy, width = 12, height = 6, dpi = 300)
