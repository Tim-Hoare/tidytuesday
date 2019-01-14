library(tidyverse)

launches <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv")

launches %>%
  count(agency, sort = TRUE)

launches %>%
  count(state_code, sort = TRUE)

launches %>%
  group_by(launch_year) %>%
  summarise(total_launches = n()) %>%
  ggplot(aes(launch_year, total_launches)) +
  geom_line()

launches %>%
  filter(agency %in% c("US", "RU", "CN", "AE", "ILSK", "J")) %>%
  group_by(launch_year, agency) %>%
  summarise(total_launches = n()) %>%
  ggplot(aes(launch_year, total_launches, colour = agency)) +
  geom_line(size = 1) +
  scale_colour_brewer(name = NULL,
                      labels = c("Arianespace", "China", "International Launch Services Khrunichev",
                                 "Japan", "Russia", "United States"),
                      palette = "Set1") +
  labs(x = "Year",
       y = "Total Launches") +
  theme(legend.position = c(0.85, 0.875),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size = 1))



launches %>%
  mutate(category = if_else(category == "O", 1, 0)) %>%
  group_by(agency) %>%
  summarise(launches = n(),
            success_rate = sum(category) / n()) %>%
  filter(launches > 50) %>%
  arrange(desc(success_rate)) %>%
  mutate(agency = fct_reorder(agency, success_rate)) %>%
  ggplot(aes(agency, success_rate, size = launches, colour = agency)) +
  geom_point(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(name = "Success Rate",
                     labels = scales::percent_format()) +
  scale_size_continuous(range = c(2, 12)) +
  scale_x_discrete(
    name = NULL,
    breaks = c(NA, "ULAL", "ULAB", "BLS", "MDSSC", "ILSK", "AE", "RU", "CN", "SPX",
               "OSC", "US", "IN", "J"),
    labels = c(NA, "United Launch Alliance/Lockheed", "United Launch Alliance/Boeing",
      "Boeing Launch Services", "McDonnell Douglas Space Systems Corp.",
      "International Launch Services Khrunichev", "Arianespace",
      "Russia", "China", "SpaceX", "Orbital Sciences Corp.",
      "United States", "India", "Japan")) +
  labs(title = "Top agencies launch success rate (minumum 50 launches)",
       subtitle = "larger point size reflects more launches")


launches %>%
  group_by(period = 10 * launch_year %/% 10) %>%
  mutate(category = if_else(category == "O", 1, 0)) %>%
  summarise(launches = n(),
            success_rate = sum(category) / n()) %>%
  ggplot(aes(period, success_rate)) +
  geom_col(colour = "black", fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010),
                     labels = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
  labs(x = "Year",
       y = "Success Rate",
       title = "Has launch success rate improved over time?")














