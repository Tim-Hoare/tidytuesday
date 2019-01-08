library(tidyverse)
library(readxl)

mortality <- read_xlsx("Datasets/globalMortality/global_mortality.xlsx")

mortality_processed <- mortality %>%
  gather(CoD, Share, -c("country", "country_code", "year"))


mortality_processed %>%
  distinct(CoD)

#What are the most common causes of death across the globe?
mortality_processed %>%
  group_by(CoD) %>%
  summarise(Avg = mean(Share, na.rm = TRUE)) %>%
  arrange(desc(Avg))%>%
  mutate(CoD = fct_reorder(CoD, Avg)) %>%
  head(15) %>%
  ggplot(aes(CoD, Avg, fill = CoD)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")

#What are the least common causes of death across the globe?
mortality_processed %>%
  group_by(CoD) %>%
  summarise(Avg = mean(Share, na.rm = TRUE)) %>%
  arrange(desc(Avg))%>%
  mutate(CoD = fct_reorder(CoD, Avg)) %>%
  tail(15) %>%
  ggplot(aes(CoD, Avg, fill = CoD)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")

# What were the highest share of cause of death for a country when compared to the global average for that cod

mortality_processed %>%
  group_by(year, CoD) %>%
  mutate(avg_share = median(Share)) %>%
  ungroup() %>%
  mutate(dev_from_avg = Share - avg_share,
         unique_name = paste(country, year, CoD),
         unique_name = fct_reorder(unique_name, dev_from_avg)) %>%
  arrange(desc(dev_from_avg)) %>%
  head(20) %>%
  ggplot(aes(unique_name, dev_from_avg)) +
  geom_col(fill = "blue", col = "black") +
  coord_flip()
# Dominated by HIV/AIDS


#---------------------------------------- How has share changed over the years? ------------------------
  
by_cod <- mortality_processed %>%
  mutate(year1990 = year - 1990) %>%
  group_by(CoD) %>%
  nest()

mod <- function(df) {
  lm(Share ~ year1990, data = df)
}

models <- by_cod %>% 
  mutate(model = map(data, mod),
         glance = map(model, broom::glance),
         rsq = map_dbl(glance, "r.squared"),
         tidy = map(model, broom::tidy),
         augment = map(model, broom::augment))


models %>%
  unnest(tidy) %>%
  select(CoD, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  ggplot(aes(x = `(Intercept)`, y = year1990)) +
  geom_point(aes(size = rsq), alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(x = "1990 Share",
       y = "Yearly Change in Share")



# All of these models have terrible fit



