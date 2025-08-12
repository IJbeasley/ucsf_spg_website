############### script to take excel spreadsheet of events #####################
############### and convert into quarto files #################################
# Expected column names: 
# this is after clean_names is performed - 
# replaces space & / with _, all lowercase, ( & ) are removed, 
# you can test out how a given name will be converted with
# clean_names("My example column name")
{
col_year       <- "year"
col_event      <- "event"
col_date       <- "dates"
col_format     <- "format_tags"
col_location   <- "location"
col_speakers   <- "speakers"
col_description<- "full_description"
col_rsvp       <- "rsvp_link"
col_flyer_link <- "flyer_website_link"
col_flyer_notes <- "flyer_notes"
col_time       <- "time"
col_draft <- "website_ready"
}

{
  events_dir = here::here("events")
}

##################### Get google sheets of event #########################
{
googlesheets4::gs4_auth(token = gargle::secret_read_rds(
  ".secrets/gs4-token.rds",
  key = "GARGLE_OAUTH_KEY"
))

events_sheet_url <- Sys.getenv("EVENTS_GSHEET_URL")
events_spreadsheet <- googlesheets4::read_sheet(events_sheet_url)
}

# Functions I custom built to process columns etc. are found in this
# Rscript
source(here::here("events/fns_clean_spreadsheet.R"))

names(events_spreadsheet) <- clean_names(
                                         names(events_spreadsheet)
                                         )

events_spreadsheet = events_spreadsheet |>
  tidyr::fill(!!col_year, .direction = 'down')

# events_spreadsheet = events_spreadsheet |>
#   dplyr::slice_sample(n = 10)

library(stringr)
library(here)


##################### Process spreadsheet events files ###############
# Loop through rows (each event) to generate .qmd files
for (i in 1:nrow(events_spreadsheet)) {
  
###### Step 1.  #####
  # Extract & process sheet columns
  # required to make the quarto file and relevant event folder 
  
  # Get event title
  title <- stringr::str_trim(events_spreadsheet[[col_event]][i])

  # Get & format event date
  date <- get_event_date(events_spreadsheet[[col_date]][i]) 
  if(any_na_type(as.character(date))){
    
    date <- get_event_date(events_spreadsheet[[col_year]][i])
    
  }
  
  # Set up the file path for this event
  qmd_dir <- mk_qmd_dir(events_dir =  events_dir,
                        date = date,
                        title = title)
  
  filepath = set_up_qmd(qmd_dir = qmd_dir)
  
  
###### Step 2. ######
  #### Get and process the required components for the quarto file #####
  
  # Format Title # 
  title_block <- paste0("title: |\n  ", gsub("\n", "\n  ", title), "\n")
  
  # Subtitle  - is the location #
  # Get event location
  location <- stringr::str_trim(events_spreadsheet[[col_location]][i])
  
  if(!any_na_type(location)){
    
    subtitle_block <- paste0("subtitle: |\n  ", 
                             gsub("\n", "\n  ", paste0('Location: ', location)), 
                             "\n")
    
  } else {
    
  subtitle_block <- NULL
  
  }
  
  # Event category - is event format
  # Get event format (e.g. Q&A)
  format <- get_event_format(events_spreadsheet[[col_format]][i])
  if(any_na_type(format)){
    
    format_block <- NULL
  } else {
    
    format_block <- paste0("categories: [", format, "]", "\n")
    
  }
  
  # Event 'author' - is event time 
  time <- events_spreadsheet[[col_time]][i]
  if(!any_na_type(time)){
    
    time_block <- paste0("author: ", shQuote(time), "\n")
    
  } else {
    
    time_block <- NULL 
  }
  
  # Brief description # 
  description <- format_event_description(events_spreadsheet[[col_description]][i])
  # Use description to make a brief 1-2 line description of the event shown
  # in event summary / listing
  description_section <- make_desc_section(description)
  
  
  # Is this page a draft? (i.e. not ready to be published on the webpage) #
  # Make sure the page is listed as a draft
  draft_val = tolower(events_spreadsheet[[col_draft]][i])
  if(grepl("no|draft", draft_val)){
    
    draft_val <- "draft: true \n"
  } else {
    
    draft_val <- NULL
  }
  
  
  # Main text content: # 
  # Include RSVP Link (if exists) # 
  rsvp_link = stringr::str_trim(events_spreadsheet[[col_rsvp]][i])
  
  if(grepl("https|http|eventbrite|www|.com|url", rsvp_link) &&
     !any_na_type(rsvp_link)
  ){
    
    rsvp_link_button <- paste0("<a href=",
                               rsvp_link,
                               " class='btn btn-danger'>",
                               "RSVP Here</a>"
    ) 
  } else {
    
  rsvp_link_button <- NULL
  }
  
  
  # Add full event description to text section of listing in quarto file
  if (!any_na_type(description)) {
    text_section <- paste0(
      description, "\n"
    )
  } else {
    
    text_section <- NULL
  }
  
  # Get event speakers
  speakers <- stringr::str_trim(events_spreadsheet[[col_speakers]][i])
  # Make event speakers 'NA' if described in pdf, or includes NA in text
  speakers <- ifelse(any_na_type(speakers), NA, speakers)
  
  # Conditionally build speaker section (don't build if speakers are NA)
  if (!any_na_type(speakers) && !grepl(".pdf", speakers)) {
    speaker_section <- paste0(
      "### Speakers  \n",
      gsub("\n", "  \n", speakers), "\n", "\n"
    )
  } else {
    
    speaker_section <- NULL
    
  }
  
  # Embed flyer image - where applicable 
  flyer_photo_path <- get_event_flyer(flyer_link = events_spreadsheet[[col_flyer_link]][i],
                                      flyer_name = events_spreadsheet[[col_flyer_notes]][i],
                                      qmd_dir = qmd_dir)
  
  image_embed = embed_event_flyer(flyer_photo_path = flyer_photo_path,
                                  qmd_dir = qmd_dir,
                                  title = title,
                                  date = date
                                  )
  
  
  
  
  ##### Step 3. Build / write the quarto file #### 
  
  content <- paste0(
    "---\n",
    draft_val, 
    title_block,
    subtitle_block,
    time_block,
    description_section,
    "date: ", shQuote(date), "\n",
    format_block,
    image_embed$image_section,
    "date-format: medium \n",
    "---\n",
    rsvp_link_button, "\n", "\n", 
    speaker_section,
    text_section, "\n",
    image_embed$in_text_image
  )
  
  writeLines(content, filepath)
}

cat(paste0("✅ Created ", 
           nrow(events_spreadsheet), 
           " Quarto files in '", 
           events_dir, 
           "'\n")
    )

