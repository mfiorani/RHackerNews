## RHackerNews

RHackerNews is a basic R client to the Hacker News API, with minimal post-processing (dates, html entities to UTF-8, root URL extraction). The package collects basic functions to retrieve data from the main available endpoints as specified by the *v0* version of the API, as of April 2019.

More info about HackerNews project can be found at at https://news.ycombinator.com/

More info about HackerNews API is available at https://github.com/HackerNews/API

The author is not affiliated with HackerNews or YCombinator.

### Motivation
Who doesn't love HackerNews? :)

### Installation
To install RHackerNews from GitHub use the following:

`devtools::install_github("mfiorani/RHackerNews")`

### Basic usage
```R
library(RHackerNews)
top <- getHN(what = "top", n = 100)
job <- getHN(what = "job", n = 50)

news_by_user <- top$df %>% group_by(by) %>% summarise(count = n()) %>% arrange(desc(count))
top$df %>% filter(by %in% news_by_user$by[1:10]) %>% ggplot(aes(by)) +
  geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top news - Histogram for user")

news_by_site <- top$df %>% group_by(site) %>% filter(!is.na(site)) %>% summarise(count = n()) %>% arrange(desc(count))
top$df %>% filter(site %in% news_by_site$site[1:10]) %>% ggplot(aes(site)) +
  geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top news - Histogram for website")

jobs_by_site <- job$df %>% group_by(site) %>% filter(!is.na(site)) %>% summarise(count = n()) %>% arrange(desc(count))
job$df %>% filter(site %in% jobs_by_site$site[1:10]) %>% ggplot(aes(site)) +
  geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top jobs - Histogram for website")
```


![Topstories by user](https://github.com/mfiorani/RHackerNews/raw/master/img/topstories_by_user.png "Top stories by user")
![Top stories by site](https://github.com/mfiorani/RHackerNews/raw/master/img/topstories_by_site.png "Top stories by site")
![Top jobs by site](https://github.com/mfiorani/RHackerNews/raw/master/img/jobstories_by_site.png "Top jobs by site")



