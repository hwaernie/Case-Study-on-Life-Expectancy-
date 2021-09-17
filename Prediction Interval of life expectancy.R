#Clear
rm(list=ls())

library("ggplot2")
library("reshape")

library(readxl)
df <-read.csv("C:\Users\HWA\Desktop\1904184 Y3S1\BAMS3043 Mathematical and Statistical Software\Assignment 4\Life Expectancy.csv")

nrow(df)
View(df)


# Drop NaN and Inf
any(is.na(df))
is.na(df) <- sapply(df, is.infinite)
df <- na.omit(df)
nrow(df)


# -------------------------------------------------------------------------------------------------------
# Task 1
Y <- df$Life.expectancy
X1 <- df$Schooling
# Model 1
model1 <- lm(Y ~ X1)
model1
s <- summary(model1)
s


# -------------------------------------------------------------------------------------------------------
# Task 2
Y <- df$Life.expectancy
X1 <- df$Schooling
X2 <- df$Income.composition.of.resources
X3 <- df$Adult.Mortality
X4 <- df$HIV.AIDS
X5 <- df$BMI
# d2 <- data.frame(Y, X1, X2, X3, X4)
# d3 <- data.frame(Y, X1, X2, X3, X4, X5)

# Model 2
model2 <- lm(Y ~ X1 + X2 + X3 + X4)
model2
s2 <- summary(model2)
s2

# Model 3
model3 <- lm(Y ~ X1 + X2 + X3 + X4 + X5)
model3
s3 <- summary(model3)
s3


# -------------------------------------------------------------------------------------------------------
library(Metrics)
y_pred1 <- predict(model1)
y_pred2 <- predict(model2)
y_pred3 <- predict(model3)

print(paste0("RMSE for model 1: ", rmse(Y, y_pred1)))
print(paste0("RMSE for model 2: ", rmse(Y, y_pred2)))
print(paste0("RMSE for model 3: ", rmse(Y, y_pred3)))

# RMSE for model 1: 6.03253237610839
# RMSE for model 2: 3.84159722270347
# RMSE for model 3: 3.7888098789165


# -------------------------------------------------------------------------------------------------------
# Task 4
# Find confidence interval for y
test <- data.frame(X1 = 16, X2 = 0.5, X3 = 168, X4 = 0.1, X5 = 16.5)
predict.lm(model3,test)
predict.lm(model3, newdata = test, interval = "confidence", level = 0.95)

predict(model3, test)
