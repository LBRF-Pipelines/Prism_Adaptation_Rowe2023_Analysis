###################################
### Two-way ANOVA import script ###
###################################

# Author: Juliet Rowe

### Import required packages ###

library(data.table)
library(dplyr)
library(tidytable)
library(afex)
library(emmeans)
library(car)
library(performance)

### Import trial data ###

df <- list.files(path = "./_Data/reach_and_point/",
                 pattern="*.csv",
                 all.files = TRUE,
                 full.names = TRUE) %>%
  map_df(~fread(., colClasses = "character"))

# Remove participant 34, experiment crashed

df <- subset(df, !(df$id == "P34"))

# Subsetting Baseline and Post-Test Trials
df_aftereffects <- subset(df, block %in% c("Baseline", "PostTest"))

# Converting mm to visual angle
df_aftereffects$distance_x <- as.numeric(df_aftereffects$distance_x)
df_aftereffects$visual_angle <- atan(df_aftereffects$distance_x/450)* (180/pi) #converting radians to degrees

# Subsetting only appropriate columns
df_aftereffects <- df_aftereffects[, !c("created",
                                        "age",
                                        "handedness",
                                        "response_time",
                                        "reaction_time",
                                        "points_x",
                                        "points_y",
                                        "location_x",
                                        "location_y",
                                        "distance_x",
                                        "distance_y",
                                        "run_time")]

# Averaging baseline and Post Test trials
df_aftereffects$id <- as.factor(df_aftereffects$id)
df_aftereffects$block <- as.factor(df_aftereffects$block)
df_aftereffects <- df_aftereffects %>%
  group_by(id, block) %>%
  mutate(visual_angle_avg = mean(visual_angle))

### Import demographic data ###

demo_dat <- read.csv("./_Data/Participant_info.csv")

# Remove participant 34, experiment crashed
demo_dat <- subset(demo_dat, !(demo_dat$P_ID == "P34"))


