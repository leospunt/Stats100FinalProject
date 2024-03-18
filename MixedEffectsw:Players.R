library(ggplot2)

install.packages("lme4")

filtered_data <- subset(golf_weather_data, !is.na(PriorStatus))

mixed_model <- lmer(score ~ WindSpeed + DayTemp + Humidity + Pressure + PriorTemp + DayStatus + PriorStatus +
                      (1 | Player_initial_last),
                    data = filtered_data)

r.squared <- r.squaredGLMM(mixed_model)

r2_cond = r.squared[2]

Mixed_Effects_BIC = BIC(mixed_model)
Linear_Model_BIC = BIC(lm_model)

predicted_scores <- predict(mixed_model)

plot(filtered_data$score, predicted_scores, 
     xlab = "Actual Average Score", ylab = "Predicted Average Score",
     main = "Mixed Effects Model - Player Random Effect ",
     pch = 19) # pch = 19 makes the points solid circles

fit <- lm(predicted_scores ~ filtered_data$score)

# Add the line of fit
abline(fit, col = "red")

annotation_text <- sprintf("Conditional R^2: %.3f\nMixed Effects Model BIC: %.2f\nLinear Model BIC: %.2f", 
                           r2_cond, Mixed_Effects_BIC, Linear_Model_BIC)

# Add text annotations to the plot
# Choose x and y such that the text does not overlap with your data points
# Adjust `pos` and `cex` for better positioning and size of the text, respectively
x_pos <- min(filtered_data$score) + (max(filtered_data$score) - min(filtered_data$score)) * 0.02
y_pos <- max(predicted_scores) - (max(predicted_scores) - min(predicted_scores)) * 0.1

text(x = x_pos, y = y_pos, labels = annotation_text, pos = 4, cex = 0.8)
