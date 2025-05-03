# nolint start

# Set working directory
setwd("/Users/giomhern/struct-topic-model/data/")

# Libraries and setup
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)


# Exploratory data analysis

meta <- read.csv("final_anes_metadata.csv")
meta |> head()
dim(meta)

rand <- read.csv("tdm89_metadata.csv")
dim(rand)

gandarian <- read.csv("gadarian_metadata.csv")
dim(gandarian)

# Basic structure
str(meta)

# Clean invalid codes (-9, -4 etc.)
metadata_clean <- meta %>%
    filter(age > 0, highest.grade.completed > 0, pid_summary >= 0)

summary(metadata_clean[, c("age", "highest.grade.completed", "pid_summary", "female")])

ggplot(metadata_clean, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "#2b8cbe", color = "white") +
    theme_minimal() +
    labs(
        title = "Age Distribution of Respondents",
        x = "Age", y = "Count"
    )

ggplot(metadata_clean, aes(x = factor(pid_summary), y = highest.grade.completed)) +
    geom_boxplot(fill = "#a1dab4") +
    theme_minimal() +
    labs(
        title = "Education Level by Party ID",
        x = "Party ID (pid_summary)", y = "Years of Education"
    )

metadata_clean %>%
    mutate(female = ifelse(female == 2, "Female", "Male")) %>%
    group_by(pid_summary, female) %>%
    summarise(n = n()) %>%
    group_by(pid_summary) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = factor(pid_summary), y = pct, fill = female)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = percent) +
    labs(title = "Gender Breakdown by Party ID", x = "Party ID", y = "Proportion") +
    scale_fill_manual(values = c("Male" = "#4575b4", "Female" = "#d73027")) +
    theme_minimal()


# nolint end
