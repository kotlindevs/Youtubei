library(httr)
library(jsonlite)

youtube_search <- "https://www.youtube.com/youtubei/v1/search"
response_okay <- 200

header <- add_headers(
  "Content-Type" = "application/json",
  "prettyPrint"  = "false"
)

contents <- function(result) {
  contents <- result[["contents"]]
  two_col_search_result <- contents[["twoColumnSearchResultsRenderer"]]
  primary_con <- two_col_search_result[["primaryContents"]]
  section_lst_render <- primary_con[["sectionListRenderer"]]
  section_lst_render
}

section_list <- function(result) {
  continuations <- result$continuations
  next_continuation <- continuations[["nextContinuationData"]]
  continuation <- next_continuation$continuation
  continuation
}

body <- list(
  context = list(
    client = list(
      clientName = "WEB",
      clientVersion = "1.20241111.01.00"
    )
  ), query = "Bhagwan mahavir college surat",
  continuation = ""
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

  if (!is.null(section_lst) && next_token == section_lst) {
    while (TRUE) {
      body$continuation <- content

      youtube <- POST(
        url = youtube_search,
        config = header,
        body = format_body
      )

      if (status_code(youtube) == response_okay) {
        search_response <- content(youtube, as = "text", encoding = "UTF-8")
        result <- fromJSON(search_response)
        content <- contents(result = result)
        section_lst <- section_list(result = content)
        next_token <- section_lst
        temp_vec <- c(temp_vec, next_token)
        if (is.null(section_lst)) {
          break()
        }
      }

      if (is.null(body$continuation)) {
        break()
      }
    }
    print(temp_vec)
  }
}