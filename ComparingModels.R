golf_weather_data$date <- as.Date(golf_weather_data$date)

# Splitting the dataset
new_golf <- golf_weather_data %>%
  filter(format(date, "%Y") %in% c("2021", "2022"))

old_golf <- golf_weather_data %>%
  filter(!format(date, "%Y") %in% c("2021", "2022"))

old_golf$course_round <- interaction(old_golf$course, old_golf$round)
new_golf$course_round <- interaction(new_golf$course, new_golf$round)

# Find the levels in 'old_golf'
old_levels <- unique(old_golf$course_round)

# Filter 'new_golf' to keep only rows where 'course_round' matches the levels in 'old_golf'
new_golf_filtered <- dplyr::filter(new_golf, course_round %in% old_levels)

lm_model_train <- lm(score ~ WindSpeed + DayTemp + Humidity + Pressure + PriorTemp + DayStatus + PriorStatus, data = old_golf)

predicted_scores_lm <- predict(lm_model_train, new_golf_filtered)

# Assuming new_golf$score contains the actual scores
actual_scores_lm <- new_golf_filtered$score

# Calculate RMSE
rmse_lm <- sqrt(mean((predicted_scores_lm - actual_scores_lm)^2))
print(paste("RMSE:", rmse_lm))

# Calculate R^2
ss_total_lm <- sum((actual_scores_lm - mean(actual_scores_lm))^2)
ss_residual_lm <- sum((actual_scores_lm - predicted_scores_lm)^2)
r_squared_lm <- 1 - (ss_residual_lm / ss_total_lm)
print(paste("R-squared:", r_squared_lm))

mixed_effects_train <- lmer(score ~ WindSpeed + DayTemp + Humidity + Pressure + PriorTemp + DayStatus + PriorStatus +
                              (1 | course: round),
                            data = old_golf)

# Create a new interaction term in both old_golf and new_golf
old_golf$course_round <- interaction(old_golf$course, old_golf$round)
new_golf$course_round <- interaction(new_golf$course, new_golf$round)

# Find the levels in 'old_golf'
old_levels <- unique(old_golf$course_round)

# Filter 'new_golf' to keep only rows where 'course_round' matches the levels in 'old_golf'
new_golf_filtered <- dplyr::filter(new_golf, course_round %in% old_levels)

# Now predict using the filtered 'new_golf'
predicted_scores_me <- predict(mixed_effects_train, new_golf_filtered)

# Note: This code snippet assumes 'course' and 'round' are the names of your columns.
# Adjust as necessary to match your actual dataset column names.


# Assuming new_golf$score contains the actual scores
actual_scores_me <- new_golf_filtered$score

# Calculate RMSE
rmse_me <- sqrt(mean((predicted_scores_me - actual_scores_me)^2))
print(paste("RMSE:", rmse_me))

# Calculate R^2
ss_total_me <- sum((actual_scores_me - mean(actual_scores_me))^2)
ss_residual_me <- sum((actual_scores_me - predicted_scores_me)^2)
r_squared_me <- 1 - (ss_residual_me / ss_total_me)
print(paste("R-squared:", r_squared_me))

metrics_data <- data.frame(
  Metric = rep(c("R^2", "RMSE"), each = 2),
  Value = c(r_squared_lm, r_squared_me, rmse_lm, rmse_me),
  Model = rep(c("Linear", "Mixed Effects"), 2)
)

# Convert Metric and Model to factor for ordered plotting
metrics_data$Metric <- factor(metrics_data$Metric, levels = c("R^2", "RMSE"))
metrics_data$Model <- factor(metrics_data$Model, levels = c("Linear", "Mixed Effects"))

ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  scale_fill_manual(values = c("Linear" = "red", "Mixed Effects" = "blue")) +
  geom_text(aes(label = sprintf("%.2f", round(Value, 2)), 
                y = Value + 0.02, group = Model),
            position = position_dodge(width = 0.7), vjust = -0.25) +
  labs(title = "Comparing Linear and Mixed Effect Model Predictions",
       y = "Value", x = "") +
  theme_minimal() +
  theme(legend.title = element_blank())