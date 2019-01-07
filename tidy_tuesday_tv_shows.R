library(tidyverse)

tv_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# What are the most common genres?

tv_ratings %>%
  count(genres, sort = T)

# What are the highest rated tv show seasons? (minumum 5% share)

tv_ratings %>%
  filter(share >= 5) %>%
  arrange(desc(av_rating))

# What are the longest running shows?

tv_ratings %>%
  group_by(titleId, title) %>%
  summarise(seasons = n(),
            length = max(date) - min(date)) %>%
  arrange(desc(length)) %>%
  select(-titleId)

# What are the most watched series?

tv_ratings %>%
  group_by(titleId, title) %>%
  summarise(avg_share = mean(share)) %>%
  arrange(desc(avg_share))

##------------------------------------ Looking at change of rating over time --------------------------------

shows <- tv_ratings %>%  # Filter for shows with at least 5 seasons & at least 5% avg share
  group_by(titleId, title) %>%
  summarise(seasons = n(),
            avg_share = mean(share)) %>%
  filter(seasons > 2, avg_share >= 1) %>%
  pull(titleId)

by_title <- tv_ratings %>%  # Nest a df for each show
  mutate(season1 = seasonNumber - 1) %>%
  filter(titleId %in% shows) %>%
  group_by(titleId, title) %>%
  nest()

mod <- function(df) {
  lm(av_rating ~ season1, data = df)
}

models <- by_title %>% # Fit a model for each show
  mutate(model = map(data, mod),
         glance = map(model, broom::glance),
         rsq = map_dbl(glance, "r.squared"),
         tidy = map(model, broom::tidy),
         augment = map(model, broom::augment))

models %>%  
  unnest(tidy) %>%
  select(title, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  ggplot(aes(`(Intercept)`, season1)) +
  geom_point(aes(size = rsq), alpha = 0.5) +
  geom_smooth(se = F) +
  labs(x = "Season 1 Rating",
       y = "Seasonly Improvement")


models %>%  # Which shows showed most improvement over time
  unnest(tidy) %>%
  select(title, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  arrange(desc(season1)) %>%
  head(15) %>%
  ggplot(aes(reorder(title, season1), season1, size = rsq)) +
  geom_point() +
  coord_flip() +
  labs(y = "Rating improvement per season",
       x = NULL)


models %>%  # Which showed most negative improvement over time
  unnest(tidy) %>%
  select(title, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  arrange(desc(season1)) %>%
  tail(15) %>%
  ggplot(aes(reorder(title, season1), season1, size = rsq)) +
  geom_point() +
  coord_flip() +
  labs(y = "Rating improvement per season",
       x = NULL)