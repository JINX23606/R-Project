install.packages("ggplot2")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)
summary(train)

table(train$Survived)
prop.table(table(train$Survived))

table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived), margin = 1) 

table(train$Pclass, train$Survived)
prop.table(table(train$Pclass, train$Survived), margin = 1)

train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)
train$Embarked[is.na(train$Embarked)] <- "S"  

# แปลงตัวแปรให้เป็น Factor
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

# สร้างโมเดล Logistic Regression
model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, family = binomial())

# ดูสรุปผล
summary(model)

ggplot(train, aes(x = factor(Survived), fill = factor(Survived))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "Survival Count", x = "Survived", y = "Count") +
  theme_minimal()

ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +  # Normalized to show proportion
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "Survival by Gender", x = "Sex", y = "Proportion") +
  theme_minimal()

ggplot(train, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

dt_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                  data = train, method = "class")
rpart.plot(dt_model, type = 3, extra = 102, fallen.leaves = TRUE)
