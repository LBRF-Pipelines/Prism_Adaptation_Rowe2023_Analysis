# Checking the assumptions of ANOVA: Normality

# Baseline
library(ggplot2)
Baseline <- subset(df_aftereffects, block == "Baseline")
norm_plot_baseline <- ggplot(data = Baseline,
                             aes(x = visual_angle, fill = group)) +
  geom_histogram(bins = 30) +
  xlab('Visual Angle (degrees)') + 
  ylab('Frequency') +
  facet_grid(cols = vars(group)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
norm_plot_baseline

# Post Test
PostTest <- subset(df_aftereffects, block == "PostTest")
norm_plot_posttest <- ggplot(data = PostTest,
                             aes(x = visual_angle, fill = group)) +
  geom_histogram(bins = 30) +
  xlab('Visual Angle (degrees)') + 
  ylab('Frequency') +
  facet_grid(cols = vars(group)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
norm_plot_posttest

# Two-Way ANOVA

mod_twoway <- aov_4(
  visual_angle ~ block * group + (block | id), #block is nested in participant
  data = df_aftereffects
)

mod_twoway 
# Significant effect of group, block, and interaction of group and block

# Normality of the residuals
qqPlot(mod_twoway$lm$residuals, xlab = "Standard Normal Distribution Quantiles", ylab = "Residual Quantiles")
shapiro.test(mod_twoway$lm$residuals)
# Residuals are normal, W = 0.99, p > 0.05

# Check the assumption of homogeneity of variance
check_homogeneity(mod_twoway)
# Assumption met.

# Contrasts
emm1 <- emmeans(mod_twoway, specs = pairwise ~ block: group)
emm1$contrasts # look at p values
emm1$emmeans # look at means

# Plotting interaction plot with emmeans
pd = position_dodge(0.2)
ggplot(as_tibble(emm1$emmeans), aes(x = block, y = emmean,color = group, group = group)) +
  geom_errorbar(aes(ymin = lower.CL,
                    ymax = upper.CL), 
                width = 0.2,
                size = 0.7, 
                position = pd) + 
  geom_line(position = pd) +
  geom_point(aes(shape = group, color = group), #Adding in shapes 
             size = 4,
             position = pd) +
  labs(x = "Time Point",
       y = "Mean Visual Angle (degrees)") + 
  scale_x_discrete(labels = c('Baseline', 'Post Test')) +
  scale_shape_discrete(name = "Groups", labels = c('Control', 'Motor Imagery', "Physical Practice")) +
  scale_color_discrete(name = "Groups", labels = c('Control', 'Motor Imagery', "Physical Practice")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = rel(1.5)), #adjusting font size
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.25)))

