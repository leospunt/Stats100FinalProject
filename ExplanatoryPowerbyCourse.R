list_of_courses_data <- split(golf_weather_data, golf_weather_data$course)

models_summaries <- lapply(list_of_courses_data, function(course_data) {
  lm_model <- lm(score ~  WindSpeed + DayTemp + Humidity + Pressure + PriorTemp, data = course_data)
  summary(lm_model)
})

model_metrics <- lapply(models_summaries, function(model_summary) {
  data.frame(R_Squared = model_summary$r.squared,
             Adjusted_R_Squared = model_summary$adj.r.squared)
})
names(model_metrics) <- names(list_of_courses_data)

# Convert the list of model metrics to a data frame for easier comparison
model_metrics_df <- do.call(rbind, model_metrics)
# If your list elements (models_summaries) are named after courses, ensure these names are carried over as row names
row.names(model_metrics_df) <- names(model_metrics)

# View the consolidated metrics
print(model_metrics_df)

library(ggplot2)
ggplot(model_metrics_df, aes(x = row.names(model_metrics_df), y = R_Squared)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Course", y = "R-squared", title = "Comparative Explanatory Power of Weather Conditions by Course")