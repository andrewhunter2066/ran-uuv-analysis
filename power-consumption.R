# Analysis of raw power consumption provided by the Navy
# Date: 2025-08-26

# load libraries
# library(dplyr)
library(tidyverse)
library(rstatix)
library(ggridges)
library(effectsize)

# Clear project variables
rm(list = ls())

# load power data
pwData <- read.delim("data/power-consumption.csv",header=TRUE, sep=",")
pwData$speed <- factor(pwData$speed)
pwData$depth <- factor(pwData$depth)

# Quick summary stats
summary(pwData)

pwData %>%
  summarise(
    mean_var = mean(pw.consuption, na.rm = TRUE),
    sd_var = sd(pw.consuption, na.rm = TRUE),
    median_var = median(pw.consuption, na.rm = TRUE)
  )

# Grouped summary by depth
pwData %>%
  group_by(depth) %>%
  summarise(
    mean_var = mean(pw.consuption, na.rm = TRUE),
    sd_var = sd(pw.consuption, na.rm = TRUE),
    median_var = median(pw.consuption, na.rm = TRUE)
  )

# Grouped summary by speed
pwData %>%
  group_by(speed) %>%
  summarise(
    mean_var = mean(pw.consuption, na.rm = TRUE),
    sd_var = sd(pw.consuption, na.rm = TRUE),
    median_var = median(pw.consuption, na.rm = TRUE)
  )


# Box plot speed v AHr
pwData %>% ggplot(aes(x = reorder(speed, desc(pw.consuption), FUN = median),
                   y = pw.consuption,
                   fill = speed)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  xlab("Speed (Knots)") +
  ylab("AHr")

# Box plot depth v AHr
pwData %>% ggplot(aes(x = reorder(depth, desc(pw.consuption), FUN = median),
                      y = pw.consuption,
                      fill = depth)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  xlab("Depth (m)") +
  ylab("AHr")

# Grouped Density Plots - Speed
pwData %>%
  group_by(speed) %>%
  ggplot(aes(x = pw.consuption,
             y = reorder(depth, pw.consuption),
             fill = speed)) +
  ggridges::stat_density_ridges(bandwidth = 0.6,
                                quantile_lines = TRUE,   # adds median indicator
                                quantiles = (0.5)) +
  # Remove legend
  theme(legend.position = "none")

# Grouped Density Plots - depth
pwData %>%
  group_by(depth) %>%
  ggplot(aes(x = pw.consuption,
             y = reorder(speed, pw.consuption),
             fill = speed)) +
  ggridges::stat_density_ridges(bandwidth = 0.6,
                                quantile_lines = TRUE,   # adds median indicator
                                quantiles = (0.5)) +
  # Remove legend
  theme(legend.position = "none")

# ANOVA
## equal variances assumed - pw.consuption ~ speed
pwData %>%
  anova_test(pw.consuption ~ speed,
             detailed = TRUE)

# ANOVA
## equal variances assumed - pw.consuption ~ depth
pwData %>%
  anova_test(pw.consuption ~ depth,
             detailed = TRUE)

## Equal variances not assumed - pw.consuption ~ speed
(oneway_test <- oneway.test(pw.consuption ~ speed,
                            data = pwData,
                            var.equal = FALSE))
# After ANOVA with var.equal = FALSE
effectsize::eta_squared(oneway_test)

## Equal variances not assumed - pw.consuption ~ depth
(oneway_test <- oneway.test(pw.consuption ~ depth,
                            data = pwData,
                            var.equal = FALSE))
# After ANOVA with var.equal = FALSE
effectsize::eta_squared(oneway_test)

# Kruskall-Wallis test - pw.consuption ~ speed
pwData %>% kruskal_test(pw.consuption ~ speed)

# Kruskall-Wallis test - pw.consuption ~ depth
pwData %>% kruskal_test(pw.consuption ~ depth)

# POST-HOC TEST FOR PARAMETRIC DATA pw.consuption ~ speed
# Bonferroni
pairwise.t.test(pwData$pw.consuption,
                pwData$speed,
                p.adjust.method = "bonferroni")

# POST-HOC TEST FOR PARAMETRIC DATA pw.consuption ~ depth
# Bonferroni
pairwise.t.test(pwData$pw.consuption,
                pwData$depth,
                p.adjust.method = "bonferroni")

# Tukey - pw.consuption ~ speed
pwData %>% tukey_hsd(pw.consuption ~ speed)
# Tukey - pw.consuption ~ depth
pw_depth <- tukey_hsd(pwData, pw.consuption ~ depth)
print(pw_depth)

# POST-HOC TEST FOR NON-PARAMETRIC DATA  - pw.consuption ~ speed
print(n=45, pwData %>% dunn_test(pw.consuption ~ speed))

# POST-HOC TEST FOR NON-PARAMETRIC DATA  - pw.consuption ~ depth
print(n=15, pwData %>% dunn_test(pw.consuption ~ depth))

pwData$speed <- as.integer(pwData$speed)
# Create regression model
m0 <- lm(pw.consuption ~ speed + depth, data = pwData)
# Inspect the model specifications.
# round the p.value since 
broom::tidy(m0) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m0)

m1 <- lm(pw.consuption ~ speed, data = pwData)
broom::tidy(m1) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m1)
# Compare models
performance::compare_performance(m0, m1)


pw_1_8 <- filter(pwData, speed<= 8)

# Create regression model
m2 <- lm(pw.consuption ~ speed + depth, data = pw_1_8)
# Inspect the model specifications.
# round the p.value since 
broom::tidy(m2) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m2)

m3 <- lm(pw.consuption ~ speed, data = pw_1_8)
broom::tidy(m3) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m3)
# Compare models
performance::compare_performance(m2, m3)

pw_8_10 <- filter(pwData, speed>7)
# Create regression model
m4 <- lm(pw.consuption ~ speed + depth, data = pw_8_10)
# Inspect the model specifications.
# round the p.value since 
broom::tidy(m4) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m4)

m5 <- lm(pw.consuption ~ speed, data = pw_8_10)
broom::tidy(m5) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m5)
# Compare models
performance::compare_performance(m4, m5)

# Create regression model
m6 <- lm(pw.consuption ~ log(speed) + depth, data = pwData)
# Inspect the model specifications.
# round the p.value since 
broom::tidy(m6) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m6)

m7 <- lm(pw.consuption ~ log(speed), data = pwData)
broom::tidy(m7) %>%
  mutate(p.value = round(p.value, 3))
# Evaluate the quality of our model
performance::model_performance(m7)
# Compare models
performance::compare_performance(m6, m7)

performance::compare_performance(m0,m1,m2,m3,m4,m5,m6,m7)

## Model Summary: For speeds less than equal to 8 knots use a single model of 
AHr ~ x0 + (x1 * speed)
## where X0 = -0.0452 and X1 = 0.0359
## For speed greater than 8 knots depth appears to contribute.
## the model becomes AHr ~ X0 + (X1 * speed) + (X2 * depth10-20) 
##                            + (X3 * depth100-200) + (X4 * depth20-40) 
##                            + (X5 * depth40-60) + (X6 * depth60-100)
## where X0 = -5.57, X1 = 0.771, X2 = -0.343, X3 = -0.299, X4 = -0.0858, 
##       X5 = -0.275, and X6 = -0.196. Depth variables are factors, so 1 if the 
##       UUV is in that depth range, otherwise 0.
