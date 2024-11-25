# Install necessary packages if they are not already installed
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyverse")

library(readxl)
library(dplyr)
library(tidyverse)
source("remove_outliers.R")
source("rename_columns.R")

data <- read_excel("data.xlsx", sheet = 1)

data_cleaned <- data %>%
  rename_with(~ as.character(data[1, ]), everything()) %>%
  slice(-1)

data_cleaned_no_outliers <- remove_outliers(data_cleaned)

data_cleaned_mean <- data_cleaned_no_outliers %>%
  mutate(group = cumsum(!is.na(`Student group`))) %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

current_colnames <- colnames(data_cleaned_mean)

new_colnames <- rename_columns(current_colnames)

colnames(data_cleaned_mean) <- new_colnames

data_mean_of_means <- data_cleaned_mean %>%
  summarise(across(starts_with("mean_"), ~ mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

head(data_mean_of_means)


library(ggplot2)

drug_concentrations <- gsub("mean_", "", current_colnames[grepl("mean_", current_colnames)])

drug_concentrations <- as.numeric(drug_concentrations)

control_value <- data_mean_of_means$overall_mean_0  # Extract mean_0 (control)
cell_viability <- data_mean_of_means %>%
  select(starts_with("overall_mean")) %>%
  mutate(across(everything(), ~ . / control_value * 100)) %>%
  pivot_longer(cols = everything(), names_to = "concentration", values_to = "cell_viability")

cell_viability <- data_mean_of_means %>%
  select(starts_with("overall_mean")) %>%
  mutate(across(everything(), ~ . / control_value * 100)) %>%
  pivot_longer(cols = everything(), names_to = "concentration", values_to = "cell_viability")

cell_viability$concentration <- as.numeric(gsub("overall_mean_", "", cell_viability$concentration))

cell_viability_sorted <- cell_viability %>%
  filter(concentration != 0) %>%
  arrange(desc(concentration))

cell_viability_sorted$concentration <- factor(cell_viability_sorted$concentration, 
                                              levels = rev(sort(unique(cell_viability_sorted$concentration))))

# Line chart
ggplot(cell_viability_sorted, aes(x = concentration, y = cell_viability)) +
  geom_line() +
  geom_point() +
  labs(x = "Drug Concentration (µM)", y = "Cell Viability (%)", title = "Cell Viability vs Drug Concentration") +
  theme_minimal()

# Bar chart
ggplot(cell_viability_sorted, aes(x = concentration, y = cell_viability)) +
  geom_col(fill = "skyblue") +  # Using geom_col to create a bar chart
  labs(x = "Drug Concentration (µM)", y = "Cell Viability (%)", title = "Cell Viability vs Drug Concentration") +
  theme_minimal()