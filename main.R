library(tidyverse)

# Load the dataset, assuming the file is named 'fitness_data.csv'
data <- read.csv("data.csv", na.strings = "")

data_clean <- data %>%
  select(where(~!all(is.na(.)))) %>%
  select(Sex, Age, Ht, Wt, HR.rest)

# a.
data_clean <- data_clean %>%
    mutate(bmi = Wt / Ht^2,
           heightweight = Ht / Wt,
           rest_weight = Wt / HR.rest)

# b.
set.seed(1234)
training_data <- data_clean %>%
  sample_frac(0.5)
testing_data <- data_clean %>%
  anti_join(training_data)
