library(tidyverse)

data <- read.csv("data.csv", na.strings = "") %>%
    select(where(~!all(is.na(.)))) %>%
    select(Sex, Age, Ht, Wt, HR.rest, FF, Waist) %>%
    mutate(bmi = Wt / Ht^2,
           height_weight = Ht / Wt,
           weight_rest = Wt / HR.rest)



# Histogram related to FF
ggplot(data, aes(x = FF, color = Sex)) +
  geom_histogram() +
  labs(title = "Histogram of FF")

# Scatter plot of height related to FF
ggplot(data, aes(x = FF, y = Ht, color = Sex)) +
  geom_point() +
  labs(title = "Height vs FF")
# Scatter plot of weight related to FF
ggplot(data, aes(x = FF, y = Wt, color = Sex)) +
  geom_point() +
  labs(title = "Weight vs FF")
# Scatter plot of Resting Heart Rate related to FF
ggplot(data, aes(x = FF, y = HR.rest, color = Sex)) +
  geom_point() +
  labs(title = "Resting Heart Rate vs FF")


# b.
set.seed(1234)

training_data <- data %>%
  sample_frac(0.5)
testing_data <- data %>%
  anti_join(training_data)


# c. 

model1 <- lm(FF ~ Age + Wt + HR.rest + Ht + bmi, data = training_data)
model2 <- lm(FF ~ poly(Age, 2) + Wt + HR.rest + log(Ht) + bmi, data = training_data)
model3 <- lm(FF ~ Age + Wt + HR.rest + Ht + bmi + height_weight + weight_rest, data = training_data)
model4 <- lm(FF ~ Age + Wt + HR.rest + Ht + poly(bmi, 2) + height_weight + weight_rest, data = training_data)
model5 <- lm(FF ~ Age + Wt + HR.rest + Ht + bmi + height_weight + weight_rest, data = training_data)


# "R^2 of the models"
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared
summary(model5)$r.squared

# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}


"Model 1"
predict(model1, newdata = testing_data) %>%
  rmse(testing_data$FF)

"Model 2"
predict(model2, newdata = testing_data) %>%
  rmse(testing_data$FF)

"Model 3"
predict(model3, newdata = testing_data) %>%
  rmse(testing_data$FF)

"Model 4"
predict(model4, newdata = testing_data) %>%
  rmse(testing_data$FF)

"Model 5"
predict(model5, newdata = testing_data) %>%
  rmse(testing_data$FF)
