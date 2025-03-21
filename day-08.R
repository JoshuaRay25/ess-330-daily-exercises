#joshray
#03-21-2025
#assignment8

library(tidyverse)

covid_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

covid_data <- read_csv(covid_url)

covid_data <- covid_data %>%
  mutate(date = as.Date(date))

state_regions <- data.frame(
  state = state.name,
  abb = state.abb,
  region = state.region
)

head(state_regions)

covid_region <- covid_data %>%
  left_join(state_regions, by = "state") %>%
  filter(!is.na(region))  # Remove non-matching territories (e.g., Puerto Rico)

covid_region_summary <- covid_region %>%
  group_by(date, region) %>%
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  ungroup()

covid_long <- covid_region_summary %>%
  pivot_longer(cols = c(total_cases, total_deaths), 
               names_to = "metric", 
               values_to = "count")

covid_region_plot <- ggplot(covid_long, aes(x = date, y = count, color = metric)) +
  geom_line() +
  labs(title = "Cumulative COVID-19 Cases & Deaths by USA Region",
       x = "Date",
       y = "Count",
       color = "Metric") +
  facet_wrap(~ region, scales = "free_y") +  # Create facets by region
  theme_minimal()

print(covid_region_plot)

if (!dir.exists("img")) {
  dir.create("img")
}

ggsave("img/covid_region_cases_deaths.png", plot = covid_region_plot, width = 12, height = 6)