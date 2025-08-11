############### script to take excel spreadsheet of events #####################
############### and convert into quarto files #################################

##################### Get google sheets of event #########################
# googledrive::drive_auth(token = Sys.getenv("GARGLE_OAUTH_TOKEN"),
#                         email = Sys.getenv("GARGLE_OAUTH_EMAIL")
#                         )

googlesheets4::gs4_auth(token = gargle::secret_read_rds(
  ".secrets/gs4-token.rds",
  key = "GARGLE_OAUTH_KEY"
))

# path = gargle::secret_decrypt_json(
#   system.file("secret", 
#               "googledrive-testing.json", 
#               package = "googledrive"),
#   "GARGLE_OAUTH_KEY"
# )

# googlesheets4::gs4_auth(token = googledrive::drive_token())

events_sheet_url <- Sys.getenv("EVENTS_GSHEET_URL")
events_spreadsheet <- googlesheets4::read_sheet(events_sheet_url)

events_spreadsheet = events_spreadsheet |>
  tidyr::fill(Year, .direction = 'down')

events_spreadsheet = events_spreadsheet[9: nrow(events_spreadsheet),]


library(stringr)
library(here)

###################### Functions to process events spreadsheet ##############
# Function to help create filenames / folder
slugify <- function(text) {
  text <- tolower(text)
  text <- gsub("[^a-z0-9]+", "-", text)
  text <- gsub("-+", "-", text)
  text <- gsub("^-|-$", "", text)
  return(text)
}

extract_start_date <- function(date_str) {
  # Remove tildes or other non-breaking dashes if present
  date_str <- stringr::str_replace_all(date_str, "[–—~]", "-")
  
  # Extract month and day from the start
  start_part <- stringr::str_extract(date_str, "^[A-Za-z]+\\s\\d{1,2}")
  
  # Extract year (assumes 4-digit year at the end)
  year <- stringr::str_extract(date_str, "\\d{4}$")
  
  # Combine to form the full starting date
  paste(start_part, year)
}


##################### Process spreadsheet events files ###############
# Loop through rows (each event) to generate .qmd files
for (i in 1:nrow(events_spreadsheet)) {
  
  # Extract required information
  title <- str_trim(events_spreadsheet$Event[i])
  
  format <- str_trim(events_spreadsheet$Format[i])
  format <- str_replace(format,"Document", "Information")
  
  date <- as.character(events_spreadsheet$`Date(s)`[i])  # Ensure it's in character format
  date <- stringr::str_remove(date, "~")
  date <- stringr::str_remove(date, "- Fall\\?")
  date <- stringr::str_trim(date)
  date <- stringr::str_replace(date, "and", "-")
  
  # Extract starting portion if range
  if (grepl("-", date)) {
    date <- extract_start_date(date)
  }
  
  # If day is missing (e.g. just "April 2025"), append "01"
  if (grepl("^[A-Za-z]+\\s\\d{4}$", date)) {
    date <- paste0(sub(" ", " 01 ", date))
  }
  
  # date <- ifelse(grepl("-", date), extract_start_date(date), date)
  date <- as.Date(stringr::str_remove(date, ","),
                         format = "%B %d %Y")
  
  speakers <- str_trim(events_spreadsheet$Speakers[i])
  speakers <- ifelse(grepl("NA", speakers) | 
                    is.na(speakers) | 
                    grepl(".pdf", speakers), NA, speakers)
  
  location <- str_trim(events_spreadsheet$Location[i])
  location <- ifelse(is.na(location) | location == "NA", "", location)
  
  description <- str_trim(events_spreadsheet$`Website Description`[i])
  description <- stringr::str_remove(description, '‍')
  # Get first sentence only
  first_sentence_description <- stringr::str_extract(
    description,
    "(?s).*?(?<!\\b(?:Dr|Mr|Ms|Mrs|Prof|Sr|Jr))\\.(\\s|$)"
  )
  #first_sentence_description <- str_extract(description, ".*?[.!?](\\s|$)")
  
  
  # Where to save the information?
  output_dir <- here::here("events")
  qmd_dir = paste0(output_dir, "/",slugify(date), "_", slugify(title))
  filename <- "index.qmd" 
  filepath <- paste0(qmd_dir, "/", filename)
  
  # Remove existing folder (if any)
  if (dir.exists(qmd_dir)) {
    unlink(qmd_dir, recursive = TRUE)
  }
  
  dir.create(qmd_dir)
  
  # Conditionally build speaker section
  speaker_section <- ""
  if (!is.na(speakers)) {
    speaker_section <- paste0(
      "### Speakers  \n",
      gsub("\n", "  \n", speakers), "\n", "\n"
    )
  }
  
  text_section <- "" 
  if (!is.na(description)) {
    text_section <- paste0(
      description, "\n"
    )
  }
  
  description_section <- "" 
  if (!is.na(first_sentence_description)) {
    description_section <- paste0(
      "description: ", shQuote(first_sentence_description), "\n"
    )
  }
  
  title_block <- paste0("title: |\n  ", gsub("\n", "\n  ", title), "\n")
  
  
  content <- paste0(
    "---\n",
    title_block,
    #"title: |\n  ", title, "\n",
    # "title: ", shQuote(title), "\n",
    "subtitle: ", shQuote(location), "\n",
    description_section,
    "date: ", shQuote(date), "\n",
    "categories: [", format, "]", "\n",
    "date-format: medium \n",
    "---\n\n",
    speaker_section,
    text_section
  )
  
  writeLines(content, filepath)
}

cat(paste0("✅ Created ", 
           nrow(events_spreadsheet), 
           " Quarto files in '", 
           output_dir, 
           "'\n")
    )

