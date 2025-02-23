library(httr)
library(jsonlite)

response_okay <- 200
header <- add_headers(
  "Content-Type" = "application/json",
  "prettyPrint"  = "false"
)

interval <- function(i) {
  cat(":::> Please wait...", i, "sec !\n")
}

youtube_token <- function(yt_url, yt_query, yt_params, yt_body) {
  tokens <- c()
  fetch_continuation <- function(response) {
    if ("contents" %in% names(response)) {
      return(response[["contents"]][["twoColumnSearchResultsRenderer"]]
             [["primaryContents"]][["sectionListRenderer"]][["contents"]]
             [["continuationItemRenderer"]][["continuationEndpoint"]]
             [["continuationCommand"]][["token"]])
    }

    if ("onResponseReceivedCommands" %in% names(response)) {
      return(response[["onResponseReceivedCommands"]] #nolint
             [["appendContinuationItemsAction"]]
             [["continuationItems"]][[1]][["continuationItemRenderer"]]
             [["continuationEndpoint"]][["continuationCommand"]][["token"]])
    }
  }

  youtube <- POST(
    url = yt_url,
    config = header,
    body = yt_body
  )

  result <- fromJSON(content(youtube, as = "text", encoding = "UTF-8"))
  token <- fetch_continuation(result)[2]

  if (!is.na(token)) {
    next_token <- token
    duplicated_token <- 1
    i <- 1
    while (TRUE) {
      body <- list(
        context = list(
          client = list(
            clientName = "WEB",
            clientVersion = "2.20250219.07.00"
          )
        ), query = yt_query,
        continuation = token,
        params = yt_params
      )
      format_body <- toJSON(
        body,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      yt <- POST(
        url = yt_url, config = header, body = format_body
      )

      if (status_code(yt) == response_okay) {
        res <- content(yt, as = "text", encoding = "UTF-8")
        to_lst <- fromJSON(res)
        token <- fetch_continuation(to_lst)[2]
        if (!is.null(token)) {
          if (next_token != token) {
            tokens <- c(tokens, token)
            interval(i)
            i <- i + 1
            next_token <- token
          } else {
            print(duplicated_token)
            duplicated_token <- duplicated_token + 1
          }
        } else {
          break()
        }
      } else {
        print("Response not okay !")
      }
    }
    return(tokens) #nolint
  } else {
    print("No token found !")
  }
}

videos <- data.frame(
  video_id = character(), video_title = character(),
  channel_id = character(), channel_title = character(),
  published_time = character(), video_length = character(),
  short_views = character()
)

video_renderer <- function(video_renderer) {
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

  min_df <- min(
    length(video_id),
    length(channel_title),
    length(video_title),
    length(channel_title),
    length(short_views),
    length(video_length),
    length(published_time)
  )

  video_id <- video_id[1:min_df]
  channel_id <- channel_id[1:min_df]
  channel_title <- channel_title[1:min_df]
  video_title <- video_title[1:min_df]
  video_length <- video_length[1:min_df]
  short_views <- short_views[1:min_df]
  published_time <- published_time[1:min_df]

  df <- data.frame(
    video_id, video_title, channel_id,
    channel_title, published_time,
    video_length, short_views
  )
  return(df) #nolint
}

contents <- function(result) {
  if ("contents" %in% names(result)) {
    contents <- result[["contents"]]
    two_col_search_res <- contents[["twoColumnSearchResultsRenderer"]]
    primary_con <- two_col_search_res[["primaryContents"]]
    section_lst_ren <- primary_con[["sectionListRenderer"]]
    contents <- section_lst_ren[["contents"]]
    item_sec_ren <- contents[["itemSectionRenderer"]]
    contents <- item_sec_ren[["contents"]]
    item_section_renderer(results = contents)
  }

  if ("onResponseReceivedCommands" %in% names(result)) {
    on_response_rec_cmd <- result[["onResponseReceivedCommands"]] 
    append_con_item <- on_response_rec_cmd[["appendContinuationItemsAction"]]
    continuation_items <- append_con_item[["continuationItems"]][[1]]
    item_section_renderer <- continuation_items[["itemSectionRenderer"]]
    contents <- item_section_renderer[["contents"]]
    item_section_renderer(results = contents)
  }
}

item_section_renderer <- function(results) {
  for (result in results) {
    if ("videoRenderer" %in% names(result)) {
      video_ren <- result$videoRenderer
      v <- video_renderer(video_renderer = video_ren)
      if (nrow(v) > 0) {
        videos <- rbind(videos, v)
      }
    }

    if ("shelfRenderer" %in% names(result)) {
      shelf_renderer <- result$shelfRenderer
      content <- shelf_renderer$content
      vertical_list_renderer <- content$verticalListRenderer
      items <- vertical_list_renderer$items
      for (item in items) {
        if ("videoRenderer" %in% names(item)) {
          video_ren <- item[["videoRenderer"]]
          v <- video_renderer(video = video_ren)
          if (nrow(v) > 0) {
            videos <- rbind(videos, v)
          }
        }
      }
    }
  }
  if (!is.null(videos)) {
    print(videos)
  }
}

print("::: Youtube :::")
print("===============")
print("<1> Search")
print("<2> Channel")
print("<3> Shorts")
print("<4> Playlist")
print("<5> Youtube music")

cat("\n")

input <- readline(prompt = "Select the option => ")
print(input)
if (input == 1) {
  print("::: Search :::")
  print("==============")
  cat("\n")
  user_key <- readline(prompt = "Search something....")

  youtube_search <- "https://www.youtube.com/youtubei/v1/search"
  s_quary <- user_key
  s_params <- "EgWKAQIQAWoKEAkQChAFEAMQBA%3D%3D"

  body <- list(
    context = list(
      client = list(
        clientName = "WEB",
        clientVersion = "2.20250219.07.00"
      )
    ), query = s_quary,
    continuation = "",
    params = s_params
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

  if (status_code(youtube) == response_okay) {
    content <- content(youtube, as = "text", encoding = "UTF-8")
    result <- fromJSON(content)
    content <- contents(result = result)
  }

  option <- readline(prompt = "Want to load more videos ? (Y/n)")

  print(option)
  if (option == "Y" || option == "y") {
    tokens <- youtube_token(yt_url = youtube_search, yt_query = s_quary,
                            yt_params = s_params, yt_body = format_body)

    if (!is.null(tokens)) {
      i <- 1
      while (i <= length(tokens)) {

        body <- list(
          context = list(
            client = list(
              clientName = "WEB",
              clientVersion = "2.20250219.07.00"
            )
          ), query = s_quary,
          continuation = tokens[i],
          params = s_params
        )

        cat("\n\n\n=====> Token [", i, "]", tokens[i], "\n\n\n")

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

        if (status_code(youtube) == response_okay) {
          content <- content(youtube, as = "text", encoding = "UTF-8")
          result <- fromJSON(content)
          content <- contents(result = result)
        }
        i <- i + 1
      }
    }
  } else if (option == "N" || option == "n") {
    print("Aborted !")
  } else {
    print("Invalid option !")
  }
} else if (input == 2) {
  print("::: Channel :::")
  print("==============")
  cat("\n")
}