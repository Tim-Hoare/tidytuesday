library(tidyverse)
library(lubridate)
library(ggrepel)

theme_set(theme_light())

comic_characters <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-05-29/week9_comic_characters.csv")

comic_characters <- comic_characters %>%
  rename(character_id = X1)

# What publishers does this dataset include ?
 
comic_characters %>%
  count(publisher, sort = TRUE)

# What identities do we have in this dataset?

comic_characters %>%
  count(id, sort = TRUE)

# How do they align?

comic_characters %>%
  count(align, sort = TRUE)

# What is the gender distribution?

comic_characters %>%
  count(sex, sort = TRUE)

# What characters have had the most appearances?

comic_characters %>%
  select(name, appearances) %>%
  arrange(desc(appearances))

# How does appearances vary within categories?

comic_characters %>%
  filter(!is.na(appearances)) %>%
  ggplot(aes(appearances + 1, fill = align)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ align, scales = "free") +
  scale_x_log10()

comic_characters %>%
  filter(!is.na(appearances)) %>%
  ggplot(aes(appearances + 1, fill = sex)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ sex, scales = "free_y") +
  scale_x_log10()

# How has the quantity of of characters changed over time?

data <- comic_characters %>%
  filter(!is.na(date)) %>%
  mutate(date = ymd(date)) %>%
  arrange(date) %>%
  mutate(count = row_number())

labels <- data %>%
  arrange(desc(appearances)) %>%
  head(5) %>%
  mutate(name = str_extract(name, "[A-z\\s-]+"))

data %>%
  ggplot() +
  geom_line(aes(date, count)) +
  geom_point(data = labels, aes(date, count)) +
  geom_text_repel(data = labels, aes(date, count, label = name)) +
  labs(x = "Date",
       y = "Total comic characters",
       title = "Increase of number of Marvel and DC comic-book\n characters ove time")


data %>%
  group_by(year = floor_date(date, "year")) %>%
  summarise(new_chars = n()) %>%
  ggplot(aes(year, new_chars)) +
  geom_line() +
  labs(x = "Year",
       y = "Number of new characters",
       title = "Number of new DC and Marvel comic book characters per year")
