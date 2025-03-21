# Name: Josh Ray
# Date: 03-21-2025
# Exercise 7

library(tidyverse)

covid_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid_data <- read_csv(covid_url)

covid_data <- covid_data %>%
  mutate(date = as.Date(date))

latest_date <- max(covid_data$date, na.rm = TRUE)

# Question 1: Identify top 6 states with most cases
top_states <- covid_data %>%
  filter(date == latest_date) %>%
  arrange(desc(cases)) %>%
  head(6) %>%
  pull(state)

covid_top6 <- covid_data %>%
  filter(state %in% top_states)

covid_plot <- ggplot(covid_top6, aes(x = date, y = cases, color = state)) +
  geom_line() + 
  labs(title = "COVID-19 Cases Over Time in the 6 Most Affected States",
       x = "Date",
       y = "Total Cases",
       color = "State") +
  facet_wrap(~ state, scales = "free_y") +  # Facet by state
  theme_minimal()

print(covid_plot)

if (!dir.exists("img")) {
  dir.create("img")
}

ggsave("img/covid_top6_states.png", plot = covid_plot, width = 10, height = 6)

# Question 2: Total daily cases in the USA
usa_daily_cases <- covid_data %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

usa_cases_plot <- ggplot(usa_daily_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "steelblue") +
  labs(title = "Daily Total COVID-19 Cases in the USA",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

print(usa_cases_plot)

ggsave("img/usa_daily_cases.png", plot = usa_cases_plot, width = 10, height = 6)