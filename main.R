library(tidyverse)

# Load the dataset, assuming the file is named 'fitness_data.csv'
data <- read.csv("data.csv", na.strings = "")

# Remove columns that contain only NA values
data_clean <- data %>%
  select(where(~!all(is.na(.))))


# i only want variables Sex, Age, Ht, Wt

data_clean <- data_clean %>%
  select(Sex, Age, Ht, Wt, DBP)


data_clean <- data_clean %>%
  mutate(BMI = Wt / (Ht/100)^2)

# split half data at random into two groups - training_data and testing_data

set.seed(123)
training_data <- data_clean %>%
  sample_frac(0.5)
testing_data <- data_clean %>%
  anti_join(training_data)

head(training_data)

head(testing_data)
