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

channel_subscribers <- function(result) {
  return(result[["header"]][["pageHeaderRenderer"]] #nolint
         [["content"]][["pageHeaderViewModel"]][["metadata"]]
         [["contentMetadataViewModel"]][["metadataRows"]][["metadataParts"]]
         [[2]][["text"]][["content"]])
}

channel_uid <- function(result) {
  return(result[["responseContext"]][["serviceTrackingParams"]] #nolint
         [["params"]][[1]][["value"]][4])
}

browse_contents <- function(result) {
  tab_renderer <- (result[["contents"]][["twoColumnBrowseResultsRenderer"]]
                   [["tabs"]][["tabRenderer"]])
  tabs <- tab_renderer[["title"]]
  tabs_bid <- tab_renderer[["endpoint"]][["browseEndpoint"]][["browseId"]]
  tabs_params <- tab_renderer[["endpoint"]][["browseEndpoint"]][["params"]]
  cat("\n======::: Tabs :::======\n")
  i <- 1
  for (tab in tabs) {
    if (!is.na(tab)) {
      cat("\n ->> <", i, ">", tab)
    }
    i <- i + 1
  }
  cat("\n\n")

  input <- 1#readline(prompt = "\nSelect the tab : ") no lint
  if (input <= length(tabs)) {
    if (input == 1) {
      tabs_params(id = tabs_bid[1], param = tabs_params[1])
    }else if (input == 2) {
      tabs_params(id = tabs_bid[2], param = tabs_params[2])
    }else if (input == 3) {
      tabs_params(id = tabs_bid[3], param = tabs_params[3])
    }else if (input == 4) {
      tabs_params(id = tabs_bid[4], param = tabs_params[4])
    }else if (input == 5) {
      tabs_params(id = tabs_bid[5], param = tabs_params[5])
    }else if (input == 6) {
      tabs_params(id = tabs_bid[6], param = tabs_params[6])
    }else if (input == 7) {
      tabs_params(id = tabs_bid[7], param = tabs_params[7])
    }
  } else {
    cat("Error : Invalid number !")
  }
}

tabs_const <- function(result) {
  tab_renderer <- (result[["contents"]][["twoColumnBrowseResultsRenderer"]]
                   [["tabs"]][["tabRenderer"]])
  content <- tab_renderer[["content"]]
  contents <- content[["sectionListRenderer"]][["contents"]]
  for (cont in contents) {
    if ("itemSectionRenderer" %in% names(cont)) {
      item_sec_ren <- cont[["itemSectionRenderer"]]
      content <- item_sec_ren[["contents"]]
      for (c in content) {
        if ("shelfRenderer" %in% names(c)) {
          shelf_renderer <- c[["shelfRenderer"]]
          main_title <- shelf_renderer[["title"]][["runs"]][[1]][["text"]]
          cat("\n\n===========================")
          cat("\n # ", main_title, " # \n")
          cat("===========================\n\n")
          main_c <- (shelf_renderer[["content"]][["horizontalListRenderer"]]
                     [["items"]])
          for (it in main_c) {
            if ("gridVideoRenderer" %in% names(it)) {
              grid_vid_ren <- it[["gridVideoRenderer"]]
              video_id <- grid_vid_ren[["videoId"]]
              video_title <- grid_vid_ren[["title"]][["simpleText"]]
              published_time <- (grid_vid_ren[["publishedTimeText"]]
                                 [["simpleText"]])
              short_views <- (grid_vid_ren[["shortViewCountText"]]
                              [["simpleText"]])
              video_length <- (grid_vid_ren[["thumbnailOverlays"]])
              video_length <- unlist(lapply(video_length, function(len) {
                (len[["thumbnailOverlayTimeStatusRenderer"]]
                 [["text"]][["simpleText"]])
              }))
              channel_id <- (grid_vid_ren[["shortBylineText"]]
                             [["runs"]])
              channel_id <- unlist(lapply(channel_id, function(channel) {
                (channel[["navigationEndpoint"]]
                 [["browseEndpoint"]][["browseId"]])
              }))
              channel_title <- (grid_vid_ren[["shortBylineText"]]
                                [["runs"]])
              channel_title <- unlist(lapply(channel_title, function(channel) {
                channel[["text"]]
              }))
              sf_vid <- data.frame(video_id, channel_id,
                                   channel_title, video_title,
                                   video_length, short_views, published_time)
              print(sf_vid)
            }
          }
        }
      }
    }
  }
}

tabs_params <- function(id, param) {
  youtube_browse <- "https://www.youtube.com/youtubei/v1/browse"

  body <- list(
    context = list(
      client = list(
        clientName = "WEB",
        clientVersion = "2.20250219.07.00"
      )
    ), browseId = id, params = param
  )

  format_body <- toJSON(
    body,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  youtube <- POST(
    url = youtube_browse,
    config = header,
    body = format_body
  )

  if (status_code(youtube) == response_okay) {
    content <- content(youtube, as = "text", encoding = "UTF-8")
    result <- fromJSON(content)
    tabs_const(result = result)
  } else {
    cat("\nError : Response not okay !\n")
  }
}

channel_metadata <- function(result) {
  channel_id <- channel_uid(result = result)
  metadata <- result[["metadata"]]
  channel_metadata_renderer <- metadata[["channelMetadataRenderer"]]
  channel_title <- channel_metadata_renderer[["title"]]
  channel_desc <- channel_metadata_renderer[["description"]]
  external_id <- channel_metadata_renderer[["externalId"]]
  owner_url <- channel_metadata_renderer[["ownerUrls"]]
  channel_url <- channel_metadata_renderer[["channelUrl"]]
  channel_subs <- channel_subscribers(result = result)
  c_subscribers <- channel_subs[1]
  c_videos <- channel_subs[2]
  vanity_channel_url <- channel_metadata_renderer[["vanityChannelUrl"]]
  is_family_safe <- channel_metadata_renderer[["isFamilySafe"]]
  facebook_id <- channel_metadata_renderer[["facebookProfileId"]]
  country_codes <- channel_metadata_renderer[["availableCountryCodes"]]

  cat("\n=======================")
  cat("\n::: Channel Details :::\n")
  cat("=======================\n")
  cat(" ->>> Id             : ", channel_id, "\n")
  cat(" ->>> Title          : ", channel_title, "\n")
  cat(" ->>> Description    : ", channel_desc, "\n")
  cat(" ->>> Url            : ", channel_url, "\n")
  cat(" ->>> Subscribers    : ", c_subscribers, "\n")
  cat(" ->>> Videos         : ", c_videos, "\n")
  cat(" ->>> Family safe    : ", is_family_safe, "\n")
  cat(" ->>> External ID    : ", external_id, "\n")
  cat(" ->>> Owner url      : ", owner_url, "\n")
  cat(" ->>> Facebook       : ", facebook_id, "\n")
  cat(" ->>> Vanity url     : ", vanity_channel_url, "\n")
  cat(" ->>> Supported countries : \n")
  print(country_codes)
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

cat("\n===============")
cat("\n::: Youtube :::")
cat("\n===============")
cat("\n<1> Search")
cat("\n<2> Channel")
cat("\n<3> Shorts")
cat("\n<4> Playlist")
cat("\n<5> Youtube music")

cat("\n")

input <- readline(prompt = "\nSelect the option => ")
if (input == 1) {
  cat("\n==============")
  cat("::: Search :::")
  cat("==============\n")

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
  cat("\n==============")
  cat("::: Channel :::")
  cat("==============\n")

  cat("ChannelID Example = UCK8sQmJBp8GCxrOtXWBpyEA\n")
  input <- readline(prompt = "\nEnter the channel id : ")
  if (!is.null(input)) {
    youtube_browse <- "https://www.youtube.com/youtubei/v1/browse"

    body <- list(
      context = list(
        client = list(
          clientName = "WEB",
          clientVersion = "2.20250219.07.00"
        )
      ), browseId = input
    )

    format_body <- toJSON(
      body,
      auto_unbox = TRUE,
      pretty = TRUE
    )

    youtube <- POST(
      url = youtube_browse,
      config = header,
      body = format_body
    )

    if (status_code(youtube) == response_okay) {
      content <- content(youtube, as = "text", encoding = "UTF-8")
      result <- fromJSON(content)
      brow_con <- browse_contents(result = result)
      channel_metadata(result = result)
      tabs_const(result = result)
    } else {
      cat("\nError : Response not okay !\n")
    }
  } else {
    cat("Please insert valid channel ID :>")
  }
}