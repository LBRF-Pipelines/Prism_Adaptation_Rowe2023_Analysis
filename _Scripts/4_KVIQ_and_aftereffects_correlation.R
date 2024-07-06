# Correlation between KVIQ & Aftereffects
shapiro.test(df_aftereffects_MI$aftereffects_avg) # Data is Normal
shapiro.test(df_aftereffects_MI$KVIQ_K) # Data is not Normal

cor.test(df_aftereffects_MI$aftereffects_avg, df_aftereffects_MI$KVIQ_K,
         alternative = "two.sided",
         method = "pearson")

# Non-significant Correlation
# t(20) = 1.5732, p > 0.05, r = 0.33

# Plotting Correlation
# Plot movement time across 10 blocks of 25 trials for all groups
library(ggplot2)
pd = position_dodge(0.2)
KVIQ_Aftereffects <- ggplot(df_aftereffects_MI,
                            aes(x = KVIQ_K, y = aftereffects_avg))+
  geom_point(size = 4,
             position = pd) +
  labs(x = "KVIQ Scores",
       y = "Mean Visual Angle (degrees)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = rel(1.5)), #adjusting font size
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.25)))

KVIQ_Aftereffects