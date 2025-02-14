library(jsonlite)
library(httr)

youtube_search <- "https://www.youtube.com/youtubei/v1/search"

header <- add_headers(
  "Content-Type" = "application/json",
  "X-Goog-Api-Key" = "AIzaSyAO_FJ2SlqU8Q4STEHLGCilw_Y9_11qcW8",
  "prettyPrint" = "false"
)

user_io <- readline(prompt = "enter your keyword : ")

yt_search <- function(search_key) {
  body <- list(
    context = list(
      client = list(
        clientName = "WEB",
        clientVersion = "2.20220918"
      )
    ), query = search_key
  )

  fmt_body <- toJSON(body, auto_unbox = TRUE)
  youtube <- POST(
    url = youtube_search,
    config = header,
    body = fmt_body
  )

  if (status_code(youtube) == 200) {
    videos <- content(youtube, as = "text", encoding = "UTF-8")
    result <- fromJSON(videos)
    contents <- result$contents
    search_results <- contents$twoColumnSearchResultsRenderer
    primary_contents <- search_results$primaryContents
    section_list_renderer <- primary_contents$sectionListRenderer
    contents <- section_list_renderer$contents
    item_sec_list <- contents$itemSectionRenderer
    contents <- item_sec_list$contents
    for (content in contents) {
      video_renderer <- content[["videoRenderer"]]
      video_id <- video_renderer[["videoId"]]
      # thumbnail <- video_renderer[["thumbnail"]]
      # thumbnail_parse <- thumbnail$thumbnails
      # video_image <- unlist(lapply(thumbnail_parse, function(df) {
      #   df$url[df$width == 720]
      # }))
      # title <- video_renderer$title$runs
      # video_title <- unlist(lapply(title, function(df) {
      #   df$text
      # }))
      view_count <- video_renderer$viewCountText$simpleText
      video_view_count <- unlist(lapply(view_count, function(df) {
        df
      }))
      mydf <- data.frame(video_id, video_view_count)
      print(mydf)
    }
  }
}

yt_search(user_io)