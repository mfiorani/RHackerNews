

baseurl <- "https://hacker-news.firebaseio.com/v0/"

unescape_html <- function(str){
  tmp <- xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
  enc2utf8(tmp)
}

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
