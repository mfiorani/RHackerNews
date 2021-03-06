## RHackerNews

RHackerNews is a basic R client for the Hacker News API, with minimal post-processing (dates, html entities to UTF-8 encoding, root URL extraction). The package collects basic functions to retrieve data from the available endpoints as specified by the *v0* version of the API, as of April 2019 (also explained [here](https://www.dummyvars.xyz/post/RHackerNews/)).

More info about HackerNews project can be found at https://news.ycombinator.com/

More info about HackerNews API is available at https://github.com/HackerNews/API




The author is not affiliated with HackerNews or YCombinator.

### Motivation
Who doesn't love HackerNews? :)

### Installation
To install RHackerNews from GitHub use the following:

```R
devtools::install_github("mfiorani/RHackerNews")
```

### Basic usage
```R
library(RHackerNews)
library(dplyr)
library(ggplot2)

# retrieving topstories and jobstories
top <- getHN(what = "top", n = 500)
job <- getHN(what = "job", n = 200)

# plotting top 10 users for topstories 
news_by_user <- top$df %>% group_by(by) %>% summarise(count = n()) %>% arrange(desc(count))
top$df %>% filter(by %in% news_by_user$by[1:10]) %>% ggplot(aes(by)) +
  geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top news - Histogram for user")

# plotting top 10 sites for topstories
news_by_site <- top$df %>% group_by(site) %>% filter(!is.na(site)) %>% summarise(count = n()) %>% arrange(desc(count))
top$df %>% filter(site %in% news_by_site$site[1:10]) %>% ggplot(aes(site)) +
  geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top news - Histogram for website")

# plotting top 10 sites for job posting
jobs_by_site <- job$df %>% group_by(site) %>% filter(!is.na(site)) %>% summarise(count = n()) %>% arrange(desc(count))
job$df %>% filter(site %in% jobs_by_site$site[1:10]) %>% ggplot(aes(site)) +
  geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top jobs - Histogram for website")
```


![Topstories by user](https://github.com/mfiorani/RHackerNews/raw/master/pics/topstories_by_user.png "Top stories by user")
![Top stories by site](https://github.com/mfiorani/RHackerNews/raw/master/pics/topstories_by_site.png "Top stories by site")
![Top jobs by site](https://github.com/mfiorani/RHackerNews/raw/master/pics/jobstories_by_site.png "Top jobs by site")



