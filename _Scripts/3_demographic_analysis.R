# Descriptive statistics for each group: PP, MI, CTRL
library(Rmisc)
Data_sum <- summarySE(df_aftereffects,
                      measurevar = "visual_angle_avg",
                      groupvars = c("group", "block"))

# Average response times for groups 
library(Rmisc)
movement_time_sum <- summarySE(avg_movement_time,
                               measurevar = "mean_rt",
                               groupvars = "group")

# Mean and standard deviation of age
mean_age <- mean(demo_dat$Age) # 26.55
sd_age <- sd(demo_dat$Age) # 9.47

# Range of age
min(demo_dat$Age)
max(demo_dat$Age)

# Num of F and M in experiment
sum(demo_dat$Sex == 'M') # 31
sum(demo_dat$Sex == 'F') # 35

# Num of R and L in experiment
sum(demo_dat$Handedness == "L") # 3
sum(demo_dat$Handedness == "R") # 63

# Checking the Normality of KVIQ-V and KVIQ-K
by(demo_dat$KVIQ_V, demo_dat$Group_Name, shapiro.test) 
by(demo_dat$KVIQ_K, demo_dat$Group_Name, shapiro.test)
# Data is not normal. Right skew. ANOVA is robust. 

# Transforming data
demo_dat$KVIQ_V_New <- log10(26 - demo_dat$KVIQ_V)
hist(demo_dat$KVIQ_V_New)
demo_dat$KVIQ_K_New <- log10(26 - demo_dat$KVIQ_K)
hist(demo_dat$KVIQ_K_New)
by(demo_dat$KVIQ_V_New, demo_dat$Group_Name, shapiro.test) # Data is Normal
by(demo_dat$KVIQ_K_New, demo_dat$Group_Name, shapiro.test) # Data is non normal, but better and ANOVA is robust


# Checking homogeneity of variance
leveneTest(demo_dat$KVIQ_V, demo_dat$Group_Name, center = median)
leveneTest(demo_dat$KVIQ_K, demo_dat$Group_Name, center = median)
# Assumption met, p > 0.05

# ANOVA
mod_oneway <- aov_ez("P_ID", "KVIQ_V_New",
                     data = demo_dat,
                     between = c("Group_Name")
)
mod_oneway
# F (2,63) = 0.91, p > 0.05, n2 = 0.028; no sig diff between groups

mod_oneway_k <- aov_ez("P_ID", "KVIQ_K_New",
                       data = demo_dat,
                       between = c("Group_Name")
)
mod_oneway_k
# F(2,63) = 0.98, p > 0.05, n2 = 0.030; no sig diff between groups

# Summary data for age, KVIQ_V, and KVIQ_K
age <- summarySE(demo_dat,
                 measurevar = "Age",
                 groupvars = "Group_Name")

KVIQ_V <- summarySE(demo_dat,
                    measurevar = "KVIQ_V",
                    groupvars = "Group_Name")

KVIQ_K <- summarySE(demo_dat,
                    measurevar = "KVIQ_K",
                    groupvars = "Group_Name")

