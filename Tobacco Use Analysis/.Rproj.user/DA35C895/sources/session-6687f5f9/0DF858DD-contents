library(ggplot2)
library(ggthemes)
library(GGally)
library(dplyr)

# Simulated data as per GYTS
set.seed(42) # for reproducibility
data <- data.frame(
  id = 1:2000,
  age = sample(13:19, 2000, replace = TRUE),
  gender = sample(c("Male", "Female"), 2000, replace = TRUE),
  tobacco_use = sample(c("Yes", "No"), 2000, replace = TRUE, prob = c(0.3, 0.7)),
  ad_exposure = rnorm(2000, mean = 5, sd = 1)
)

# Adding source annotation
source_annotation <- annotate("text", x = Inf, y = Inf, label = "Source: GYTS (Global Youth Tobacco Survey)",
                              hjust = 1, vjust = 1, size = 3, color = "black")

# Prevalence of Tobacco Use by Gender
ggplot(data, aes(x = gender, fill = tobacco_use)) +
  geom_bar() +
  labs(title = "Prevalence of Tobacco Use by Gender",
       x = "Gender",
       y = "Count",
       fill = "Tobacco Use") +
  theme_fivethirtyeight() +
  source_annotation

# Prevalence of Tobacco Use by Age
ggplot(data, aes(x = factor(age), fill = tobacco_use)) +
  geom_bar() +
  labs(title = "Prevalence of Tobacco Use by Age",
       x = "Age",
       y = "Count",
       fill = "Tobacco Use") +
  theme_fivethirtyeight() +
  source_annotation

# Average Advertisement Exposure by Age
data %>%
  group_by(age) %>%
  summarise(mean_ad_exposure = mean(ad_exposure)) %>%
  ggplot(aes(x = age, y = mean_ad_exposure)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Advertisement Exposure by Age",
       x = "Age",
       y = "Mean Advertisement Exposure") +
  theme_fivethirtyeight() +
  source_annotation

# Tobacco Use by Age and Advertisement Exposure
data$ad_exposure_bin <- cut(data$ad_exposure, breaks = seq(0, 10, by = 1))

ggplot(data, aes(x = factor(age), y = ad_exposure_bin, fill = tobacco_use)) +
  geom_tile() +
  labs(title = "Tobacco Use by Age and Advertisement Exposure",
       x = "Age",
       y = "Advertisement Exposure",
       fill = "Tobacco Use") +
  theme_fivethirtyeight() +
  source_annotation

# Scatterplot of Age and Advertisement Exposure
ggpairs(data, columns = c("age", "ad_exposure"),
        mapping = ggplot2::aes(color = tobacco_use),
        title = "Scatterplot of Age and Advertisement Exposure",
        upper = list(continuous = "cor"),
        lower = list(continuous = "points")) +
  theme_fivethirtyeight() +
  source_annotation
