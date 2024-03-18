lm_model_wind <- lm(score ~ WindSpeed, data = golf_weather_data)
lm_model_daytemp <- lm(score ~ DayTemp, data = golf_weather_data)
lm_model_pressure <- lm(score ~ Pressure, data = golf_weather_data)
lm_model_humidity <- lm(score ~ Humidity, data = golf_weather_data)
lm_model_priortemp <- lm(score ~ PriorTemp, data = golf_weather_data)
lm_model_daystatus <- lm(score ~ DayStatus, data = golf_weather_data)
lm_model_priorstatus <- lm(score ~ PriorStatus, data = golf_weather_data)

# Example R^2 values for each parameter
R2_wind <- summary(lm_model_wind)$r.squared
R2_daytemp <- summary(lm_model_daytemp)$r.squared
R2_pressure <- summary(lm_model_pressure)$r.squared
R2_humidity <- summary(lm_model_humidity)$r.squared
R2_priortemp <- summary(lm_model_priortemp)$r.squared
R2_daystatus <- summary(lm_model_daystatus)$r.squared
R2_priorstatus <- summary(lm_model_priorstatus)$r.squared

# Create a dataframe
r_by_condition <- data.frame(
  Parameter = c("Wind", "Day Temp", "Pressure", "Humidity", "Prior Temp", "Day Status", "Prior Status"),
  R2_Value = c(R2_wind,R2_daytemp,R2_pressure,R2_humidity,R2_priortemp,R2_daystatus,R2_priorstatus)
)

ggplot(r_by_condition, aes(x = Parameter, y = R2_Value, fill = Parameter)) +
  geom_col() + # Use geom_col() for bar charts
  labs(title = "R^2 Values by Parameter", x = "Parameter", y = "R^2 Value") +
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for clarity
