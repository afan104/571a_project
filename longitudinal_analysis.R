###############################################################################
### Longitudinal Impact of Hatching day temperature on Telomere Length
### Clay Christenson
###############################
dev.off()
rm(list = ls())
# Libraries
library(tidyverse)

# Functions
#############################################################################################################
# Accessing data
tl_df <- read.csv("./data_clean/predictors_of_tl_dataset.csv")
head(tl_df)

# Create a time series plot
ggplot(tl_df, aes(x = as.factor(year))) +
  geom_boxplot(aes(y = temperature)) +
  #geom_boxplot(aes(y = bc_TL)) +
  labs(x = "Year", y = "Temperature") +
  theme_minimal()


# Create an empty dataframe to store regression results
regression_results <- data.frame(Year = numeric(), Slope = numeric(), P_Value = numeric())

# Fit linear regression for each year separately
for (Year in years) {
  subset_data <- subset(tl_df, year == Year)
  regression_model <- lm(bc_TL ~ temperature, data = subset_data)
  
  # Extract slope and p-value
  slope <- summary(regression_model)$coefficients[2, 1]
  p_value <- summary(regression_model)$coefficients[2, 4]
  
  regression_results <- rbind(regression_results, data.frame(Year = Year, Slope = slope, P_Value = p_value))
}

regression_results$star <- ifelse(regression_results$P_Value < 0.05, "T", "F")


# Scatterplot with regression lines for each year
ggplot(tl_df, aes(x = temperature, y =bc_TL)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(.~year)+
  labs(title = "Temperature vs. Telomere Length",
       x = "Temperature (K)", y = "Telomere Length") +
  theme_bw() +
  theme(legend.position = "none")
