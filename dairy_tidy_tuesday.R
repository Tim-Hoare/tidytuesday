library(tidyverse)
library(gganimate)
library(extrafont)

fluid_milk_sales <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/fluid_milk_sales.csv")
state_milk_production <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv")
cheese <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv")


#--------------------------------------------------- MILK ------------------------------------------------

### How have milk sales changed over time?

fluid_milk_sales %>%
  filter(milk_type == "Total Production") %>%
  ggplot(aes(year, pounds / 1e+9)) +
  geom_line(size = 1) +
  theme_light() +
  labs(x = "Year",
       y = "Quantity of milk sold (Billions of pounds)") +
  theme(text = element_text(family = "Tw Cen MT Condensed", size = 18))

### How have milk sales changed over time between milk types?

fluid_milk_sales %>%
  filter(milk_type != "Total Production") %>%
  mutate(milk_type = fct_reorder(milk_type, pounds) %>% fct_rev()) %>%
  ggplot(aes(year, pounds / 1e+9, colour = milk_type)) +
  geom_line(size = 1) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_light() +
  labs(x = "Year",
       y = "Amount of milk sold (Billions of pounds)",
       colour = "Milk type") +
  theme(text = element_text(family = "Tw Cen MT Condensed", size = 18))

### ------------------------------------------------------------------ MILK CHLOROPLETH ----------------------------------------


### How does milk production vary between states?

milk_production_summary <- state_milk_production %>%
  group_by(state) %>%
  summarise(avg_milk_produced = mean(milk_produced)) %>%
  ungroup()


milk_production_summary %>%
  mutate(region = str_to_lower(state)) %>%
  left_join(map_data("state"), by = "region") %>%
  ggplot(aes(long, lat, group = group, fill = avg_milk_produced / 1e+9)) +
  geom_polygon() +
  ggthemes::theme_map() +
  coord_map() +
  scale_fill_viridis_c(name = "Average milk produced since 1970,\n in billions of pounds",
                       option = "C") +
  theme(legend.background = element_blank(),
        legend.position = "right",
        text = element_text(family = "Tw Cen MT Condensed",
                            size = 12)) 

#How has milk production changed over time between states?

state_milk_production %>%
  mutate(region = str_to_lower(state)) %>%
  left_join(map_data("state"), by = "region") %>%
  ggplot(aes(long, lat, group = group, fill = milk_produced / 1e+9)) +
  geom_polygon() +
  ggthemes::theme_map() +
  coord_map() +
  scale_fill_viridis_c(name = "Average Milk Produced Since 1970,\n in Billions of Pounds",
                       option = "C") +
  labs(title = "Year: {current_frame}",
       x = NULL,
       y = NULL) +
  transition_manual(year) +
  theme(legend.background = element_blank(),
        legend.position = "right",
        text = element_text(family = "Tw Cen MT Condensed",
                            size = 16)) 

#Which regions of the US have produced the largest quantities of milk since 1970?

state_milk_production %>%
  group_by(region) %>%
  summarise(total_milk_produced = sum(milk_produced)) %>%
  mutate(region = fct_reorder(region, total_milk_produced)) %>%
  ggplot(aes(region, total_milk_produced / 1e+9)) +
  geom_col(fill = "blue", colour = "black") +
  coord_flip() +
  theme_light() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "Region of the US",
       y = "Total Milk Produced since 1970 (Billions of Pounds)") +
  theme(text = element_text(family = "Tw Cen MT Condensed",
                            size = 14))

# Which states of the US have produced the largest quantities of milk since 1970

state_milk_production %>%
  group_by(state) %>%
  summarise(total_milk_produced = sum(milk_produced)) %>%
  arrange(desc(total_milk_produced)) %>%
  mutate(state = fct_reorder(state, total_milk_produced)) %>%
  head(10) %>%
  ggplot(aes(state, total_milk_produced / 1e+9)) +
  geom_col(fill = "blue", colour = "black") +
  coord_flip() +
  theme_light() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = NULL,
       y = "Total Milk Produced since 1970 (Billions of Pounds)") +
  theme(text = element_text(family = "Tw Cen MT Condensed",
                            size = 14))


# Which states have seen the largest increase and decrease in milk production since 1970?

state_milk_production %>%
  filter(year %in% c(1970, 2017)) %>%
  spread(year, milk_produced) %>%
  rename(milk_prod_1970 = `1970`,
         milk_prod_2017 = `2017`) %>%
  mutate(percent_change = (milk_prod_2017 - milk_prod_1970) / milk_prod_1970) %>%
  arrange(desc(percent_change)) %>%
  head(10) %>%
  mutate(state = fct_reorder(state, percent_change)) %>%
  ggplot(aes(state, percent_change)) +
  geom_col(fill = "blue", colour = "black") +
  coord_flip() +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Largest Increases in Milk Production Over a 47 year Period",
       x = NULL,
       y = "Increase in milk production between 1970 and 2017") +
  theme(text = element_text(family = "Tw Cen MT Condensed",
                            size = 14),
        plot.title = element_text(size = 18,
                                  hjust = 0.5))

state_milk_production %>%
  filter(year %in% c(1970, 2017)) %>%
  spread(year, milk_produced) %>%
  rename(milk_prod_1970 = `1970`,
         milk_prod_2017 = `2017`) %>%
  mutate(percent_change = (milk_prod_2017 - milk_prod_1970) / milk_prod_1970) %>%
  arrange(percent_change) %>%
  head(10) %>%
  mutate(state = fct_reorder(state, percent_change)) %>%
  ggplot(aes(state, percent_change)) +
  geom_col(fill = "blue", colour = "black") +
  coord_flip() +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Largest Decreases in Milk Production Over a 47 year Period",
       x = NULL,
       y = "Decrease in milk production between 1970 and 2017") +
  theme(text = element_text(family = "Tw Cen MT Condensed",
                            size = 14),
        plot.title = element_text(size = 18,
                                  hjust = 0.5))


###--------------------------------------------- CHEEEEEEEEEEEEESE --------------------------------------------------------------


cheese_processed <- cheese %>%
  gather(type, lbs_consumed_pp, -Year) %>%
  rename(year = Year)

# How has cheese consumption changed over time?

cheese_processed %>%
  filter(type %in% c("Total American Chese", "Total Italian Cheese", "Total Natural Cheese", "Total Processed Cheese Products")) %>%
  mutate(type = fct_reorder(type, lbs_consumed_pp) %>% fct_rev()) %>%
  ggplot(aes(year, lbs_consumed_pp, colour = type)) +
  geom_line(size = 1) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_light() +
  labs(x = "Year",
       y = "Amount of cheese consumed per person (lbs)",
       colour = "Cheese type") +
  theme(text = element_text(family = "Tw Cen MT Condensed", size = 14))

# Which types of cheeses have been consumed the most over the last 50 years?

cheese_processed %>%
  group_by(type) %>%
  filter(!type %in% c("Total American Chese", "Total Italian Cheese", "Total Natural Cheese", "Total Processed Cheese Products")) %>%
  summarise(mean_consumption = mean(lbs_consumed_pp, na.rm = TRUE)) %>%
  mutate(type = fct_reorder(type, mean_consumption)) %>%
  ggplot(aes(type, mean_consumption)) +
  geom_col(fill = "blue", colour = "black") +
  coord_flip() +
  theme_light() +
  labs(x = "Type of Cheese",
       y = "Mean Consumtion Per Person Since 1970 (lbs)") +
  theme(text = element_text(family = "Tw Cen MT Condensed", size = 16))


# Which types of cheese have shown the largest increase in consumption over the last 50 years?

cheese_processed %>%
  filter(year %in% c(1970, 2017),
         !type %in% c("Total American Chese", "Total Italian Cheese", "Total Natural Cheese", "Total Processed Cheese Products")) %>%
  spread(year, lbs_consumed_pp) %>%
  rename(cheese_consumed_1970 = `1970`,
         cheese_consumed_2017 = `2017`) %>%
  mutate(percent_change = (cheese_consumed_2017 - cheese_consumed_1970) / cheese_consumed_1970) %>%
  arrange(desc(percent_change)) %>%
  head(10) %>%
  mutate(type = fct_reorder(type, percent_change)) %>%
  ggplot(aes(type, percent_change)) +
  geom_col(fill = "blue", colour = "black") +
  coord_flip() +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Largest Increases in Cheese Consumption Over a 47 year Period",
       x = NULL,
       y = "Increase in milk production between 1970 and 2017") +
  theme(text = element_text(family = "Tw Cen MT Condensed",
                            size = 14),
        plot.title = element_text(size = 18,
                                  hjust = 0.5))