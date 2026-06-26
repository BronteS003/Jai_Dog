################################################################################
##                               SIGHTINGS RDS                                ##
################################################################################
# Script cleaning dog sightings transect data and saving as .RDS file for use  #
#in sterilization status (model 2), population structure (model 3.1;model 3.2) #
#and health status (model 4).                                                  #
################################################################################
## Created by Bronte Slote, last edited June 26, 2026                         ##
################################################################################

##LOAD LIBRARIES##
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(stringr) #manipulating text

################################################################################

##IMPORT DATA##
sightingsRAW <- read.csv("Data/FULL_sightings.csv")

KK_TC_Clinic <- readRDS("Data/clinic_data.rds")


################################################################################
##CLEANING DATA##
################################################################################

##REMOVE RESIGHTS##
#to avoid duplication of dogs

#Remove all rows that are resights
sightings <- sightingsRAW %>%
  filter(!str_to_lower(Notes) %in% c("resight", "unknown"))


################################################################################

##CLARIFY COLUMNS##

#Rename columns
sightings <- sightings %>% rename("polygon" = "Sandbox.Name",#rename so easier to remember
                                  "date"= "Timestamp")

################################################################################

##ORGANIZE POLYGONS AND SUBDISTRICTS##

#Recognize polygon as a categorical variable with 7 levels
sightings <- sightings %>%
  mutate(polygon = factor (polygon, levels = c("Khok Kruat 01","Khok Kruat 06","Khok Kruat 07", "Tha Chang 12", "Tha Chang 16","Tha Chang 20","Tha Chang 24")))

#Rename long form of polygon name to short form
sightings <- sightings %>%
  mutate(polygon = str_replace(polygon, "^Khok Kruat", "KK"),
         polygon = str_replace(polygon, "^Tha Chang", "TC"))

#Create subdstrict column
sightings$subdistrict <- str_extract(sightings$polygon, "^[A-Za-z]+") #extract the letters from the "polygon" column and make them a new column "subdistrict"

#Define subdistrict as categorical with 2 levels
sightings <- sightings %>%
  mutate(subdistrict = factor(subdistrict, levels = c("KK","TC"))) #make "subdistrict" a factor with the levels 'KK' and 'TC'

################################################################################

##CREATE SURVEY ID##

#Create survey identifier based on polygon and year of survey
sightings <- sightings %>%
  mutate(
    year = format(as.Date(date), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
sightings <- sightings %>%
  mutate(survey = as.factor(survey))

################################################################################

##ORGANIZE DATE##

#Convert date to a date variable
sightings <- sightings %>%
  mutate(date = substr(date, 1, 10))#just take date

#Recognize date as date
sightings <- sightings %>%
  mutate(date = as.Date(date))

#Find duplicate dates
dup_rows <- sightings %>%
  group_by(survey, date) %>%
  summarise(n = n(), .groups = "drop")

#Correct dates for TC 12_2024 (showing up as all on same day)
sightings <- sightings %>%
  mutate(date = if_else(
    survey == "TC 12_2024" & Notes %in% c("new_dog", "resight"), #any that are labelled as "new_dog" or "resight" have to be from a 2nd day of surveying
    date + days(2), #correct day for these dogs from day 2
    date
  ))

################################################################################

##CREATE DAY VARIABLE##

#Create variable "day" to show either day 1 or 2 survey
sightings <- sightings %>%
  group_by(survey) %>%
  mutate(day = dense_rank(date)) %>%
  ungroup()

#Make day a factor variable  
sightings <- sightings %>%
  mutate(day= as.factor(day))

################################################################################

##ORGANIZE PREDICTORS##

#Define column Neutered as numerical
sightings <- sightings %>%
  mutate(Neutered = as.numeric(Neutered)) #make "Neutered" numerical

#Make column age
sightings <- sightings %>%
  mutate(age = case_when(
    Adult.male == 1 ~ "Adult",
    Adult.NON.lactating.female == 1 | Adult.Lactating.female == 1 ~ "Adult",
    Adult.unknown.sex == 1 ~ "Adult",
    Puppy == 1 ~ "Puppy"))

#Make age a factor
sightings<-sightings %>%
  mutate(age = as.factor(age))

#Make column sex
sightings <- sightings %>%
  mutate(sex = case_when(
    Adult.male == 1 ~ "M",
    Adult.NON.lactating.female == 1 | Adult.Lactating.female == 1 ~ "F",
    Adult.unknown.sex == 1|Puppy ==1 ~ "Unknown"))

#Make sex a factor
sightings<-sightings %>%
  mutate(sex = as.factor(sex))

#Make column owned
sightings <- sightings %>%
  mutate(owned = case_when(
    Free.roaming.NO.collar == 1 ~ "No",
    Free.roaming.collared | Confined.in.yard | On.chain.or.lead == 1 ~ "Yes"))

#Make owned a factor
sightings<-sightings %>%
  mutate(owned = as.factor(owned))

#Remove NAs
sightings <- na.omit(sightings) #one dog missing owership info, causing errors in models


################################################################################
##DEFINE STERILIZATION EFFORT##
################################################################################

##CREATE SINCE INTERVENTION COLUMN##

#Create "since_intervention" column
sightings <- sightings %>%
  mutate(
    intervention_start = case_when( #create column "intervention_start"
      subdistrict == "KK" ~ as.Date("2022-02-11"), #where the subdistrict is "KK" make the intervention start date as 2022-02-11
      subdistrict == "TC" ~ as.Date("2022-11-17") #where the subdistrict is "TC" make the intervention start date as 2022-11-17
    ),
    since_intervention = as.numeric(date - intervention_start) #create a new numeric column "since_intervention" by subtracting intervention start date from date of survey resulting ina column showing number of days since intervention
  )

#Make days since intervention to years
sightings <- sightings %>%
  mutate(since_intervention = since_intervention / 365)

################################################################################

##CREATE TOTAL AND ANNUAL STERILIZATION EFFORT##

sightings <- sightings %>%
  rowwise() %>%
  mutate(
    effort_all_time = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_1y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        KK_TC_Clinic$date_admission >= date - years(1) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_3y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date - years(2) &
        KK_TC_Clinic$date_admission >= date - years(3) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_2y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date - years(1) &
        KK_TC_Clinic$date_admission >= date - years(2) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_4y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date - years(3) &
        KK_TC_Clinic$date_admission >= date - years(4) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    )
    
  ) %>%
  ungroup()

#Create total sterilization effort by human population
sightings <- sightings %>%
  mutate(effort_humanpop = case_when(
    subdistrict == "KK" ~ effort_all_time/3000,
    subdistrict == "TC" ~ effort_all_time/4938))

#Create total sterilization effort 1 year ago by human population
sightings <- sightings %>%
  mutate(effort_4y_humanpop = case_when(
    subdistrict == "KK" ~ effort_4y_ago/3000,
    subdistrict == "TC" ~ effort_4y_ago/4938))

#Create total sterilization effort 3 years ago by human population
sightings <- sightings %>%
  mutate(effort_3y_humanpop = case_when(
    subdistrict == "KK" ~ effort_3y_ago/3000,
    subdistrict == "TC" ~ effort_3y_ago/4938))

#Create total sterilization effort 2 years ago by human population
sightings <- sightings %>%
  mutate(effort_2y_humanpop = case_when(
    subdistrict == "KK" ~ effort_2y_ago/3000,
    subdistrict == "TC" ~ effort_2y_ago/4938))

#Create total sterilization effort 1 year ago by human population
sightings <- sightings %>%
  mutate(effort_1y_humanpop = case_when(
    subdistrict == "KK" ~ effort_1y_ago/3000,
    subdistrict == "TC" ~ effort_1y_ago/4938))


################################################################################
##SAVE AS RDS##
################################################################################

saveRDS(sightings, file = "Data/sightings.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)