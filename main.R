library(caret)
library(ggplot2)
sleep_data <- read.csv("sleep.csv")
colSums(is.na(sleep_data))
summary(sleep_data)
str(sleep_data)
#Dummy Variables
sleep_data$Gender <- ifelse(sleep_data$Gender == "Male", 1, 0)
sleep_data$Occupation <- factor(sleep_data$Occupation) 
sleep_data <- cbind(sleep_data, model.matrix(~ Occupation - 1, data = sleep_data))

set.seed(123)

train_index <- createDataPartition(sleep_data$Quality.of.Sleep, p = 0.8, list = FALSE)
train_data <- sleep_data[train_index, ]
test_data <- sleep_data[-train_index, ]


model <- lm(Quality.of.Sleep ~ Sleep.Duration + Physical.Activity.Level+Occupation + Stress.Level + Daily.Steps , 
            data = train_data)
summary(model)


predictions <- predict(model, newdata = test_data)

rmse <- sqrt(mean((predictions - test_data$Quality.of.Sleep)^2))
print(paste("RMSE:", rmse))

comparison <- data.frame(
  Actual = test_data$Quality.of.Sleep,
  Predicted = predictions
)

ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +  # จุดสีฟ้า
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # เส้นอ้างอิง y = x
  labs(
    title = "Predicted vs Actual Quality of Sleep",
    x = "Actual Quality of Sleep",
    y = "Predicted Quality of Sleep"
  ) +
  theme_minimal()



