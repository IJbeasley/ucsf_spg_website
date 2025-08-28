library(yaml)


output_file = here::here("news/news.yml")

{
  googlesheets4::gs4_auth(
    token = gargle::secret_read_rds(
      here::here(".secrets/gs4-drive-token.rds"),
      key = "GDRIVE_KEY"
    ),
    scopes = "drive.readonly"
  )
  
  news_sheet_url <- Sys.getenv("NEWS_GSHEET_URL")
  news_spreadsheet <- googlesheets4::read_sheet(news_sheet_url)
  
}

# Functions I custom built to process columns etc. are found in this
# Rscript
source(here::here("events/fns_clean_spreadsheet.R"))

names(news_spreadsheet) <- clean_names(
  names(news_spreadsheet)
)

# custom string handler: always double-quote
quote_string <- function(x) {
  # escape double quotes inside the string
  x <- gsub('"', '\\"', x, fixed = TRUE)
  paste0('"', x, '"')
}

quote_string <- function(x) {
  # remove leading/trailing quotes if user typed them in sheet
  x <- gsub("^(\"|')|(\"|')$", "", x)
  # escape any embedded double quotes
  x <- gsub('"', '\\"', x, fixed = TRUE)
  paste0('"', x, '"')
}

inline_list <- function(x) {
  # double-quote each element if it has spaces/special chars
  x <- ifelse(grepl("\\s", x), paste0('"', x, '"'), x)
  paste0("[", paste(x, collapse = ", "), "]")
}

block_list <- function(x) {
  # emit as a proper block list
  out <- paste0("  - ", x, collapse = "\n  - ")
  # add a newline at the beginning so YAML key is separate
  paste0("\n", out)
}


{
  col_title <- "title"
  col_link <- "link"
  col_file_name <- "file_name"
  col_description <- "description"
  col_date <- "date"
  col_tags <- "tags"
}

news <- vector("list", nrow(news_spreadsheet))

for (i in seq_len(nrow(news_spreadsheet))) {
  
  title <- stringr::str_trim(news_spreadsheet[[col_title]][i])
  title <-  gsub("'([^']+)'", '"\\1"', title, fixed = TRUE)
  
  date  <- stringr::str_trim(news_spreadsheet[[col_date]][i])
  
  description <- stringr::str_trim(news_spreadsheet[[col_description]][i])
  if (is.na(description)) description <- ""
  
  link <- stringr::str_trim(news_spreadsheet[[col_link]][i])
  
  # split tags into a character vector
  tags_raw <- stringr::str_trim(news_spreadsheet[[col_tags]][i])
  tags <- if (is.na(tags_raw) || tags_raw == "") character(0) else
    stringr::str_split(tags_raw, ",\\s*")[[1]]
  
  categories_list <- as.list(tags)
  
  # Add to YAML list
  news[[i]] <- list(
    title       = structure(title, class = "Title"),     # optionally use your quote_title() here
    date        = date,
    path        = link,
    description = description,
    categories = categories_list     # <- plain vector
  )
}

# Write YAML
writeLines(
  as.yaml(
    news,
    handlers = list(
      Title = function(x) rlang::expr(!!x)
    )
  ),
  output_file
)
