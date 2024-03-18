lm_model <- lm(score ~ WindSpeed + DayTemp + Humidity + Pressure + PriorTemp + DayStatus + PriorStatus, data = golf_weather_data)

predicted_scores <- predict(lm_model, golf_weather_data)

plot(golf_weather_data$score, predicted_scores, 
     xlab = "Actual Average Score", ylab = "Predicted Average Score",
     main = "Actual vs. Predicted Average Score per Player",
     pch = 19) # pch = 19 makes the points solid circles

# Adding a line of perfect prediction for reference
fit <- lm(predicted_scores ~ golf_weather_data$score)

# Add the line of fit
abline(fit, col = "red")

rmse_value <- sqrt(mean((golf_weather_data$score - predicted_scores)^2, na.rm = TRUE))


# Calculate R-squared
r_squared <-summary(lm_model)$r.squared
adj_r_squared <-summary(lm_model)$adj.r.squared

legend_text <- paste("RMSE:", round(rmse_value, 2), "\nR^2:", round(r_squared, 2))
legend("topleft", legend = legend_text, bty = "n", cex = 0.8)