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

# Subsetting exposure trials to look at movement times between MI and PP
df_movement_time <- subset(df, block == "Exposure")

# Subsetting only important columns
df_movement_time <- df_movement_time[, !c("created",
                                        "age",
                                        "sex",
                                        "handedness",
                                        "reaction_time",
                                        "points_x",
                                        "points_y",
                                        "location_x",
                                        "location_y",
                                        "distance_x",
                                        "distance_y"
                                        )]

# Remove participant 34
df_movement_time <- subset(df_movement_time, !(df_movement_time$id == "P34"))

# Making trial_num and response_time numerical values
df_movement_time$trial_num <- as.numeric(df_movement_time$trial_num)
df_movement_time$response_time <- as.numeric(df_movement_time$response_time)

avg_movement_time <- df_movement_time %>%
  group_by(id) %>%
  mutate(
    subblock = cumsum(!is.na(lag(trial_num)) & trial_num < lag(trial_num)) # Creating column of sub-blocks from 0 - 9 (10 blocks of 25 trials)
  ) %>%
  group_by(group, subblock) %>%
  summarize(
    mean_rt = mean(response_time),
    sd_rt = sd(response_time) # Averaging response times for each sub-block per group
  )

### Import demographic data ###

demo_dat <- read.csv("./_Data/Participant_info.csv")

# Subset complete data
demo_dat <- demo_dat[1:67,]

# Remove participant 34, experiment crashed
demo_dat <- subset(demo_dat, !(demo_dat$P_ID == "P34"))

### Movement Time Data ###

# Subsetting Exposure Trials
df_exposure <- subset(df, block == "Exposure")

# Subsetting only appropriate columns
df_exposure <- df_exposure[, !c("created",
                                "age",
                                "handedness",
                                "reaction_time",
                                "points_x",
                                "points_y",
                                "location_x",
                                "location_y",
                                "distance_x",
                                "distance_y",
                                "run_time")]

# 10 groups of 25 trials for each participant
df_grouped_trials <- df_exposure %>%
  group_by(id) %>%
  mutate(subblock = rep(1:10, each = 25)) %>%
  ungroup()

df_grouped_trials$subblock<- as.numeric(df_grouped_trials$subblock)
df_grouped_trials$group <- as.character(df_grouped_trials$group)
df_grouped_trials$response_time <- as.numeric(df_grouped_trials$response_time)

df_movement_time <- df_grouped_trials %>%
  group_by(group, subblock) %>%
  summarise(response_time_avg = mean(response_time),
            response_time_sd = sd(response_time))

### KVIQ and Aftereffects Data ###

# Subsetting Baseline and Post-Test Trials
df_MI <- subset(df, block %in% c("Baseline", "PostTest"))

# Subsetting MI group
df_MI <- subset(df_MI, group == "MI-CE")

# Converting mm to visual angle
df_MI$distance_x <- as.numeric(df_MI$distance_x)
df_MI$visual_angle <- atan(df_MI$distance_x/450)* (180/pi) #converting radians to degrees

# Subsetting only appropriate columns
df_MI <- df_MI[, !c("created",
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

# Reshaping data from long format to wide
df_wide <- reshape(df_MI, idvar = c("trial_num", "id", "sex", "group"), timevar = "block", direction = 'wide')
df_wide <- df_wide %>%
  rename(
    Baseline_VA = visual_angle.Baseline,
    PostTest_VA = visual_angle.PostTest
  )

# Subtracting Baseline trials from Post Test per participant
df_aftereffects_MI <- df_wide %>%
  group_by(id, trial_num) %>%
  mutate(aftereffects = PostTest_VA -Baseline_VA)

# Subset only MI-CE
demo_dat_MI <- subset(demo_dat, Group_Name == "MI_CE")

# Adding KVIQ data 
indices <- match(df_aftereffects_MI$id, demo_dat_MI$P_ID) # Helps to match columns of different lengths
df_aftereffects_MI$KVIQ_K <- demo_dat_MI$KVIQ_K[indices]

# Averaging Aftereffects Across Participants per 10 trials
df_aftereffects_MI$trial_num <- as.numeric(df_aftereffects_MI$trial_num)
df_aftereffects_MI <- df_aftereffects_MI %>%
  group_by(id) %>%
  summarise(aftereffects_avg = mean(aftereffects),
            aftereffects_sd = sd(aftereffects),
            KVIQ_K = mean(KVIQ_K))

