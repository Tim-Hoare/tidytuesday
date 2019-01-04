# Russian Troll Tweets

This is a small analysis on some [data](https://github.com/fivethirtyeight/russian-troll-tweets) provided by FiveThirtyEight.
You can read more about this data in their [article](https://fivethirtyeight.com/features/why-were-sharing-3-million-russian-troll-tweets/),
But essentially, it contains around 3 million tweets obtained by Clemson University from accounts associated with the Internet Research
Agency, a "troll factory" based in St Petersburg.

Getting started with the data
-----------------------------

The dataset is supplied by FiveThirtyEight as 13 separate csv files the data can be imported to R as so:
```r
library(tidyverse)

tweets <- map_df(list.files("russian-troll-tweets-master/",
           pattern = "IRAhandle_tweets", full.names = TRUE), read_csv)
           
# Save the object as a .RDS file so we have all the tweets in one file for the future          
saveRDS(tweets, "russian_troll_tweets.RDS")
```
Let's have a look at the data:
![](https://github.com/TimHoare/tidytuesday/blob/master/images/trolltweetsimg1.png)

Explainations for the variable names can be found in the data dictionary on FiveThirtyEight's github. 

Now we can have a look at the data with some basic queries such as:\
What were the most active accounts and their account type?

```r
tweets %>%
  count(author, account_type, sort = TRUE)
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/trolltweetsimg2.png)

What are the most common account types?
```r
tweets %>%
  count(account_type, sort = TRUE)
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/trolltweetsimg3.png)

And the most common account categories for those types?
```r
tweets %>%
  count(account_type, account_category, sort = TRUE)
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/trolltweetsimg4.png)

Time series analysis
------------------

Now we have a reasonable understanding of the data we can move on and try and find some more interesting insight, for example
how did the quantity of tweets change over time.

```r
library(lubridate)

tweets %>%
  mutate(publish_date = mdy_hm(publish_date)) %>%
  filter(publish_date > "2014-06-01") %>% # Remove pre summer 2014 tweets
  group_by(publish_date = floor_date(publish_date, "day")) %>%
  summarise(no_of_tweets = n()) %>%
  ggplot(aes(x = publish_date, y = no_of_tweets)) +
  geom_line()
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/trolltweetsimg6.png)

There is quite a lot of noise in this graph, but there are a few interesting areas, firstly the largest peak at around October
2016, which the [Washington Post](https://www.washingtonpost.com/technology/2018/07/20/russian-operatives-blasted-tweets-ahead-huge-news-day-during-presidential-campaign-did-they-know-what-was-coming/?noredirect=on&utm_term=.20ee1040f1d1)
attributed to WikiLeaks releasing emails from the Clinton campaign. Also a large peak in summer 2017 which is largely made up of 
tweets from "right troll" accounts.

To get a bit more insight, we can break the graph down by account category: 

```r
tweets %>%
  mutate(publish_date = mdy_hm(publish_date)) %>%
  filter(publish_date > "2015-01-01") %>%
  group_by(account_category, publish_date = floor_date(publish_date, "week")) %>%
  summarise(no_of_tweets = n()) %>%
  ungroup() %>%
  ggplot(aes(x = publish_date, y = no_of_tweets, colour = account_category)) +
  geom_line() +
  facet_wrap(~ account_category, ncol = 3, scales = "free_y")
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/facetedlinegraph.png)

Again, explainations for these catogorisations can be found in FiveThirtyEight's [article](https://fivethirtyeight.com/features/why-were-sharing-3-million-russian-troll-tweets/).
This graph reveals a distictive shape for each account category, for example, "left trolls" have a large spike at around the time
of the Clinton Email's leak and the time of the election, and the "right trolls" have the aforementioned spike in summer 2017.
An interesting account category in my opinion, although rare in the dataset is the fearmongerer. The accounts spread news of a fake crisis, 
for example the peak that can be seen around December 2015 relates to a food-posioning crisis of turkeys at Thanksgiving, invented
by the Internet Research Agency.

Text analysis
------------

The content variable contains the text of the tweet itself. We can analyse this using the tidytext package, by unnesting each tweet
into tidy form (one row per word).

```r
library(tidytext)

tweets_tidy <- tweets %>%
  unnest_tokens(word, content) 
# Returns a dataframe with roughly 42 million rows - This may consume a lot of memory.
# Consider filtering the dataset down beforehand if this is likely to be an issue.

topwords <- tweets_tidy %>%
  count(account_category, word, sort = TRUE) %>%
  anti_join(stop_words) # Remove uninteresting words such as the, a, etc.
  
# Need to add more twitter-specific stop-words  
morestopwords <- data_frame(word = c("t.co", "https", "http", "rt", "amp")) 

topwords <- anti_join(topwords, morestopwords)

# Find the top 50 words for each account category
top50 <- topwords %>%
  group_by(account_category) %>%
  top_n(50) %>%
  ungroup()

```
If we take a look at some of the most popular words across all categories:

![](https://github.com/TimHoare/tidytuesday/blob/master/images/top50%20words.png)

We can see it is dominated by non-english stop-words. For the moment it might be easiest to filter these out.
We can now plot a bar chart for each category of what are the most common words for each.

```r
top50 %>%
  filter(account_category != "NonEnglish") %>% # Remove non-english tweets
  group_by(account_category) %>%
  arrange(desc(n)) %>%
  top_n(10) %>% # Filter down again for top 10 in each category
  ungroup() %>%
  ggplot(aes(x = reorder_within(word, n, account_category), y = n, fill = account_category)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() + # This function reorders the bars within each facet
  facet_wrap(~ account_category, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "Occurences of word",
       x = NULL) +
  ggtitle("Most Common Words In Tweets By Account Category", subtitle = "Uninteresting words and non-english accounts filtered out")
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/MostCommonWords.png)

Each account category has a distinctive set of words (with a certain amount of crossover). As mentioned by FiveThirtyEight, left
trolls tend to 'adopt the personae of Black Lives Matter activists', and we can see that here. The fake foodpoisinging crisis, spread
by the fearmongers is also evident from their most common words.

We can also do tf-idf analysis, a measure of how important each word is to a corpus of documents, or in this case, a group of 
account categories. This technique adjusts for the fact that some words will occur more frequently in general, even thogh they 
aren't stop words.

```r
tf_idf_tweets <- topwords %>%
  bind_tf_idf(word, account_category, n)  

mystopwords <- data_frame(word = c("iaeqrdhqpo", "sqjlvubhbi", "3awz2kwj5k", "uvjmw3ybii", "astirkb3e2",
                                   "owp51ix4vs", "3fr1dxakjh", "tqqbk6myee", "wdvm8zhf9r", "iaigyd28p2", 
                                   "dj3dsj2igk", "camzagaxfo", "ghycc26ncs", "5jcz3zfzgr", "mmpoxm8w1e", 
                                   "sc3tcmomyv", "vbdx8uinel", "i1r1stib0c", "vej2jtgj0l", "sa4fo4pwc2", 
                                   "mh6zro6uwc", "aixbmvtlvj", "6vrx086xej", "sam1moore", "line:bizgod",
                                   "emx9jgtv3v", "8bmdm8ell5", "ichlzwqg0y", "1js42r66sy", "1kpxto2hfw", 
                                   "8h0lejtvcr", "md7ep27xyn", "krzbjh9cfm", "mfbjijyl05", "mj4n1lcxvf",
                                   "ectbjumquv", "it3iypb6sw", "kvuwmlhscr"))
# A large amount of links need to removed here

tf_idf_tweets <- anti_join(tf_idf_tweets, mystopwords, by = "word")

# Plot the graph
tf_idf_tweets %>%
  filter(account_category != "NonEnglish") %>%
  arrange(desc(tf_idf)) %>%
  group_by(account_category) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder_within(word, tf_idf, account_category), tf_idf, fill = account_category)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ account_category, ncol = 2, scales = "free") +
  ggtitle("Term Frequency - Inverse Document Frequency by Account Category",
          subtitle = "Non English Accounts Filteded Out")
  
```
![](https://github.com/TimHoare/tidytuesday/blob/master/images/tf_idf_plot.png)

We now see that words like 'Trump' for example, which was used very often by several account categories, does
not appear here because it was not a distinctive word for a specific category. 'kochfarms' however remains because
it related to the fake food-poisoning crisis which was spread only by the fearmongers.






















