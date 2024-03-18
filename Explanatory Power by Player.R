more_than_25 <- golf_weather_data %>%
  group_by(Player_initial_last) %>%          # Group the data by PlayerName
  summarise(Appearances = n()) %>%  # Count appearances for each player
  filter(Appearances > 25)   

players_more_than_25 <- golf_weather_data %>%
  filter(Player_initial_last %in% more_than_10$Player_initial_last)

list_of_courses_data <- split(players_more_than_10, players_more_than_10$Player_initial_last)

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

model_metrics_ordered <- model_metrics_df %>%
  arrange(desc(R_Squared))

top_bottom_five <- bind_rows(
  head(model_metrics_ordered, 5), # Top 5 players with the highest R^2
  tail(model_metrics_ordered, 5)  # Bottom 5 players with the lowest R^2
)

ggplot(top_bottom_five, aes(x = row.names(top_bottom_five), y = R_Squared)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(R_Squared, scientific = TRUE, digits = 2), y = R_Squared + 0.02), # Add R^2 labels slightly above the bars
            position = position_dodge(width = 0.9), # Adjust position to match the bars if using dodged bars
            angle = 45, # Optional: angle the text if it fits better
            vjust = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Player", y = "R-squared", title = "Comparative Explanatory Power of Weather Conditions by Player")