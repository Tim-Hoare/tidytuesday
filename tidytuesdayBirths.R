library(tidyverse)
library(lubridate)

us_births <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-02/us_births_2000-2014.csv")

# How does the total babies born in each year change over time?

us_births %>%
  group_by(year) %>%
  summarise(total_births = sum(births)) %>%
  arrange(desc(total_births))

# What is the most common birth month?

us_births %>%
  group_by(month) %>%
  summarise(total_births = sum(births)) %>%
  arrange(desc(total_births))

# What is the most common birth date?

us_births %>%
  group_by(month, date_of_month) %>%
  summarise(total_births = sum(births)) %>%
  arrange(desc(total_births))

# What is the most common day of the week to be born on?

us_births %>%
  group_by(day_of_week) %>%
  summarise(total_births = sum(births)) %>%
  arrange(desc(total_births)) %>%
  mutate(day_of_week = fct_reorder(factor(day_of_week), total_births)) %>%
  ggplot(aes(x = day_of_week, y = total_births)) +
  geom_col(fill = "blue", colour = "black") +
  scale_y_continuous(name = "Total Births",
                     labels = scales::comma_format()) +
  scale_x_discrete(name = NULL,
                   breaks = c(2, 3, 4, 5, 1, 6, 7),
                   labels = c("Tuesday", "Wednesday", "Thursday", "Friday", "Monday", "Saturday", "Sunday")) +
  coord_flip()
  

# Graph births over time

us_births %>%
  mutate(date = dmy(paste(date_of_month, month, year, sep = "/"))) %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise(total_births = sum(births)) %>%
  filter(date > min(date), date < max(date)) %>%
  ggplot(aes(x = date, y = total_births)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(y = "Total Births",
       x = "Date")










