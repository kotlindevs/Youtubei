library(httr)
library(jsonlite)

youtube_search <- "https://www.youtube.com/youtubei/v1/search"
response_okay <- 200

header <- add_headers(
  "Content-Type" = "application/json",
  "prettyPrint"  = "false"
)

next_token <- ""

videos <- data.frame(
  video_id = character(), video_title = character(),
  channel_id = character(), channel_title = character(),
  published_time = character(), video_length = character(),
  short_views = character()
)

contents <- function(result) {
  if ("contents" %in% names(result)) {
    contents <- result[["contents"]]
    two_col_search_result <- contents[["twoColumnSearchResultsRenderer"]]
    primary_con <- two_col_search_result[["primaryContents"]]
    section_lst_render <- primary_con[["sectionListRenderer"]]
    return(section_lst_render)
  }
  if ("continuationContents" %in% names(result)) {
    continuation_contents <- result[["continuationContents"]]
    sec_lst_continuation <- continuation_contents$sectionListContinuation
    sec_lst_continuation
  }
}

section_list <- function(result) {
  continuations <- result$continuations
  next_continuation <- continuations[["nextContinuationData"]]
  continuation <- next_continuation$continuation
  continuation
}

contents_result <- function(result) {
  contents <- result[["contents"]]
  item_sec_render <- contents[["itemSectionRenderer"]]
  contents <- item_sec_render$contents
  for (content in contents) {
    if ("videoRenderer" %in% names(content)) {
      video_renderer <- content$videoRenderer
      video_id <- video_renderer$videoId
      video_id <- unlist(lapply(video_id, function(video) {
        video <- na.omit(video)
        video
      }))
      channel_id <- video_renderer$longBylineText$runs
      channel_id <- unlist(lapply(channel_id, function(video) {
        video$navigationEndpoint$browseEndpoint$browseId
      }))
      channel_title <- video_renderer$longBylineText$runs
      channel_title <- unlist(lapply(channel_title, function(channel) {
        channel$text
      }))
      published_time <- video_renderer$publishedTimeText$simpleText
      published_time <- unlist(lapply(published_time, function(pub_time) {
        pub_time <- na.omit(pub_time)
        pub_time
      }))
      video_length <- video_renderer$lengthText$simpleText
      video_length <- unlist(lapply(video_length, function(video_len) {
        video_len <- na.omit(video_len)
        video_len
      }))
      short_views <- video_renderer$shortViewCountText$simpleText
      short_views <- unlist(lapply(short_views, function(views) {
        views <- na.omit(views)
        views
      }))
      video_title <- video_renderer$title$runs
      video_title <- unlist(lapply(video_title, function(vtitle) {
        vtitle <- vtitle$text
        vtitle
      }))
      df <- data.frame(
        video_id, video_title, channel_id,
        channel_title, published_time, video_length, short_views
      )
      if (nrow(df) > 0) {
        videos <- rbind(videos, df)
      }
    }
    if ("shelfRenderer" %in% names(content)) {
      self_renderer <- content$shelfRenderer
      content <- self_renderer$content
      vert_lst_renderer <- content$verticalListRenderer
      items <- vert_lst_renderer$items
      for (item in items) {
        video_renderer <- item$videoRenderer
        video_id <- video_renderer$videoId
        video_id <- unlist(lapply(video_id, function(video) {
          video <- na.omit(video)
          video
        }))
        channel_id <- video_renderer$longBylineText$runs
        channel_id <- unlist(lapply(channel_id, function(video) {
          video$navigationEndpoint$browseEndpoint$browseId
        }))
        channel_title <- video_renderer$longBylineText$runs
        channel_title <- unlist(lapply(channel_title, function(channel) {
          channel$text
        }))
        published_time <- video_renderer$publishedTimeText$simpleText
        published_time <- unlist(lapply(published_time, function(pub_time) {
          pub_time <- na.omit(pub_time)
          pub_time
        }))
        video_length <- video_renderer$lengthText$simpleText
        video_length <- unlist(lapply(video_length, function(video_len) {
          video_len <- na.omit(video_len)
          video_len
        }))
        short_views <- video_renderer$shortViewCountText$simpleText
        short_views <- unlist(lapply(short_views, function(views) {
          views <- na.omit(views)
          views
        }))
        video_title <- video_renderer$title$runs
        video_title <- unlist(lapply(video_title, function(vtitle) {
          vtitle <- vtitle$text
          vtitle
        }))
        df <- data.frame(
          video_id, video_title, channel_id,
          channel_title, published_time,
          video_length, short_views
        )
        if (nrow(df) > 0) {
          videos <- rbind(videos, df)
        }
      }
    }
  }
  print(videos)
}

body <- list(
  context = list(
    client = list(
      clientName = "WEB",
      clientVersion = "1.20241111.01.00"
    )
  ), query = "android", params = "EgWKAQIQAWoKEAkQChAFEAMQBA%3D%3D"
)

format_body <- toJSON(
  body,
  auto_unbox = TRUE,
  pretty = TRUE
)

youtube <- POST(
  url = youtube_search,
  config = header,
  body = format_body
)

temp_vec <- c()

if (status_code(youtube) == response_okay) {
  search_response <- content(youtube, as = "text", encoding = "UTF-8")
  result <- fromJSON(search_response)
  content <- contents(result = result)
  section_lst <- section_list(result = content)
  next_token <- section_lst
  contents_result(result = content)

  if (!is.null(next_token)) {
    while (TRUE) {
      body <- list(
        context = list(
          client = list(
            clientName = "WEB",
            clientVersion = "1.20241111.01.00"
          )
        ), query = "android", params = "EgWKAQIQAWoKEAkQChAFEAMQBA%3D%3D",
        continuation = next_token
      )

      format_body <- toJSON(
        body,
        auto_unbox = TRUE,
        pretty = TRUE
      )

      youtube <- POST(
        url = youtube_search,
        config = header,
        body = format_body
      )

      search_response <- content(youtube, as = "text", encoding = "UTF-8")
      result <- fromJSON(search_response)
      content <- contents(result = result)
      section_lst <- section_list(result = content)
      next_token <- section_lst
      contents_result(result = content)
      temp_vec <- c(temp_vec, section_lst)
      if (is.null(section_lst)) {
        break()
      }
    }
  }
}
