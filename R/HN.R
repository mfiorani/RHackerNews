#' RHackerNews
#'
#' RHackerNews is a package that provides a basic R client to the Hacker News API.
#'
#' More info about HackerNews project can be found at at https://news.ycombinator.com/
#'
#' More info about HackerNews API is available at https://github.com/HackerNews/API
#'
#' The package collects basic functions to collect data from the main available endpoints as specified
#' by the v0 version of the API, as of April 2019.
#'
#' The author is not affiliated with HackerNews or YCombinator.
#' @docType package
#' @name RHackerNews
NULL

baseurl <- "https://hacker-news.firebaseio.com/v0/"

unescape_html <- function(str){
  tmp <- xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
  enc2utf8(tmp)
}

#' getHN
#'
#' \code{getHN} is the main function to collect data from a specific endpoint.
#'
#' @param what string value to specify the desired endpoint. Admitted values are "top", "new", "best", "ask", "show", "job", "updates".
#'      The default value is \code{"top"}.
#' @param n integer value to specifiy the maximum desired numbero of items to be retrieved.
#'      \code{n} is automatically adjusted if it exceeds the maximum allowed size specified by the API for the specific endpoint.
#'      The default value is \code{500}.
#' @return It return a list of four objects:
#'
#' 1. a dataframe with tabular data (named "df")
#'
#' 2. a named list of retrieved kids objects (the ids of the item's comments, in ranked display order).
#'
#' 3. a named list of profiles (only populated by the "updates" API call)
#'
#' 4. a named list of poll-parts (related pollopts, in display order)
#'
#' 
#' The tabular dataframe contains:
#' 
#' 1. id:  The item's unique id.
#' 
#' 2. deleted: Boolean if the item is deleted.
#' 
#' 3. type:  The type of item. One of "job", "story", "comment", "poll", or "pollopt".
#' 
#' 4. by:  The username of the item's author.
#' 
#' 5. time:  Creation datetime of the item.
#' 
#' 6. text:  The comment, story or poll text. UTF-8.
#' 
#' 7. dead:  Boolean if the item is dead.
#' 
#' 8. parent:  The comment's parent: either another comment or the relevant story.
#' 
#' 9. poll:  The pollopt's associated poll.
#' 
#' 10. url:   The URL of the story.
#' 
#' 11. score:   The story's score, or the votes for a pollopt.
#' 
#' 12. title:   The title of the story, poll or job.
#' 
#' 13. descendants: In the case of stories or polls, the total comment count.
#' 
#' 14. nkids: The count of kid's id
#' 
#' 15. site: Base website URL of the story or job posting.
#'
#' @examples
#' # Retrieve data from endpoints
#' top <- getHN(what = "top", n = 10)
#' job <- getHN(what = "job", n = 10)
#' best <- getHN(what = "best", n = 10)
#' ask <- getHN(what = "ask", n = 10)
#' show <- getHN(what = "show", n = 10)
#' updates <- getHN(what = "updates", n = 10)
#'
#' # View data table from previous "topstories" API call
#' View(top$df)
#'
#' # List "kids" elements from previous "beststories" API call
#' for(id in best$df$id){
#'    pid <- as.character(id)
#'    print(best$kids[pid])
#' }
#'
#' # Example plots
#' library(dplyr)
#' library(ggplot2)
#' news_by_user <- top$df %>% group_by(by) %>% summarise(count = n()) %>% arrange(desc(count))
#' top$df %>% filter(by %in% news_by_user$by[1:10]) %>% ggplot(aes(by)) +
#'   geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#'   labs(title="Top news - Histogram for user")
#'
#' news_by_site <- top$df %>% group_by(site) %>% filter(!is.na(site)) %>% summarise(count = n()) %>% arrange(desc(count))
#' top$df %>% filter(site %in% news_by_site$site[1:10]) %>% ggplot(aes(site)) +
#'   geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#'   labs(title="Top news - Histogram for website")
#'
#' jobs_by_site <- job$df %>% group_by(site) %>% filter(!is.na(site)) %>% summarise(count = n()) %>% arrange(desc(count))
#' job$df %>% filter(site %in% jobs_by_site$site[1:10]) %>% ggplot(aes(site)) +
#'   geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#'   labs(title="Top jobs - Histogram for website")
#'
#' @export
getHN <- function(what = "top", n = 500){
  endpoints <- c("top", "new", "best", "ask", "show", "job", "updates")
  stopifnot(what %in% endpoints)

  if(what %in% c("ask", "show", "job") & n > 200) {
    n <- 200
    cat("Max allowed number of elements for'", what, "'enpoint is 200. Setting n = 200.")
  }
  if(what %in% c("top", "new", "best") & n > 500) {
    n <- 500
    cat("Max allowed number of elements for '", what, "'enpoint is 500. Setting n = 500.")
  }

   url <- switch(what,
      top = "topstories.json?print=pretty",
      new = "newstories.json?print=pretty",
      best = "beststories.json?print=pretty",
      ask = "askstories.json?print=pretty",
      show = "showstories.json?print=pretty",
      job = "jobstories.json?print=pretty",
      updates = "updates.json?print=pretty"
  )

  json <- jsonlite::fromJSON(paste0(RHackerNews:::baseurl, url))

  if(what == "updates"){
    ids <- json$items
    profiles <- json$profiles
  } else {
    ids <- json
    profiles <- NA
  }

  limit <- ifelse(length(ids) > n, n, length(ids))
  ids <- ids[1:limit]

  df <- data.frame(stringsAsFactors = F)
  kids <- list()
  parts <- list()
  for(id in ids){
    tmp <- getItem(id)
    df <- rbind(df, tmp$df, stringsAsFactors = F)
    kids[as.character(id)] <- list(tmp$kids)
    parts[as.character(id)] <- list(tmp$parts)
  }
  list(df = df, kids = kids, profiles = profiles, parts = parts)
}

#' getItem
#'
#'  \code{getItem} is a function to retrieve data for a specific item.
#'
#' @param id integer value corresponding to the item id.
#' @return It return a list of two objects:
#'
#'    1. a dataframe with tabular data (named "df")
#'
#'    2. a named list of retrieved kids objects
#'
#'    3. a named list of retrieved poll-parts objects
#'
#' @examples
#' # Get a single item
#' job1 <- getItem(19567427)
#' story1 <- getItem(19553941)
#'
#' # Look into the object elements
#' job1$df
#' story1$df
#' story1$kids
#' @export
getItem <- function(id){
  url <- paste0(RHackerNews:::baseurl, "item/", as.character(id), ".json?print=pretty")
  content <- jsonlite::fromJSON(url)
  ll <- length(content$kids)

  if(ll > 0) {
    kids <- content$kids
  } else { kids <- NA }
  if(length(content$parts)) {
    parts <- content$parts
  } else { parts <- NA }

  content$descendants <- ifelse(length(content$descendants) > 0, content$descendants, NA)
  content$nkids <- ifelse(ll > 0, ll, 0)
  content$text <- ifelse(length(content$text) > 0, RHackerNews:::unescape_html(content$text), NA)
  content$title <- ifelse(length(content$title) > 0, RHackerNews:::unescape_html(content$title), NA)
  content$time <- as.POSIXct(content$time, origin="1970-01-01", tz = "America/Los_Angeles")
  content$site <- ifelse(length(content$url) > 0, gsub("www.", "", unlist(strsplit(content$url, "/"))[3]), NA)
  content$url <- ifelse(length(content$url) > 0, content$url, NA)
  content$score <- ifelse(length(content$score) > 0, content$score, NA)
  content$parent <- ifelse(length(content$parent) > 0, content$parent, NA)
  content$deleted <- ifelse(length(content$deleted) > 0, content$deleted, FALSE)
  content$dead <- ifelse(length(content$dead) > 0, content$dead, FALSE)
  content$poll <- ifelse(length(content$poll) > 0, content$poll, NA)
  content$kids <- NULL
  content$parts <- NULL
  list(df = data.frame(content, stringsAsFactors = F), kids = kids, parts = parts)
}

#' getUsers
#'
#' \code{getUsers} is a function to retrieve data for a specific user or a vector of user IDs.
#'
#' @param users string or list of user id(s).
#' @return The function returns a list of two objects:
#'
#'    1. a dataframe with tabular data (named "df")
#'
#'    2. a named list of submitted items
#'
#' @examples
#' # Get a single user data
#' user1 <- getUsers("jl")
#' View(user1$df)
#' user1$submitted
#'
#' # Get last updated users profiles
#' updates <- getHN(what = "updates", n = 10)
#' users <- getUsers(updates$profiles)
#'
#' View(users$df)
#' for(id in users$df$id){
#'    print(users$submitted[id])
#' }
#' @export
getUsers <- function(users){
  df <- data.frame(stringsAsFactors = F)
  submitted <- list()
  for(user in users){
    url <- paste0(RHackerNews:::baseurl, "user/", as.character(user), ".json?print=pretty")
    content <- jsonlite::fromJSON(url)
    content$created <- as.POSIXct(content$created, origin="1970-01-01", tz = "America/Los_Angeles")
    content$about <- ifelse(length(content$about) > 0, RHackerNews:::unescape_html(content$about), NA)
    content$karma <- ifelse(length(content$karma) > 0, content$karma, 0)
    content$delay <- ifelse(length(content$delay) > 0, content$delay, NA)
    submitted[as.character(user)] <- list(content$submitted)
    content$submitted <- NULL
    df <- rbind(df, data.frame(content), stringsAsFactors = F)
  }
  list(df = df, submitted = submitted)
}
