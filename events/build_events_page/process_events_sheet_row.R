##### Process events sheet function ############

process_events_sheet_row <- function(event_row,
                                     rownum,
                                     events_dir,
                                     col_event,
                                     col_date,
                                     col_time,
                                     col_year,
                                     col_location,
                                     col_tags,
                                     col_description,
                                     col_draft,
                                     col_rsvp,
                                     col_speakers,
                                     col_flyer_link,
                                     col_flyer_notes) {
  
  
  ###### Step 1. Extract & process required sheet columns  #####
  # These are used to make the quarto file and relevant event folder 
  
  # Get event title
  title <- stringr::str_trim(event_row[[col_event]])
  
  if(any_na_type(title)){
    
    stop("Problem with event title: Is missing or NA")
    
  }
  
  message("\n \nProcessing Row ", rownum, ": ", title, "\n")
                    
  # Format Event Title # - for the quarto file
  title_block <- paste0("title: ", shQuote(title), "\n")
  # title_block <- paste0("title: |\n  ", gsub("\n", "\n  ", title), "\n")
  
  # Get & tags event date
  date <- get_event_date(event_row[[col_date]]) 
  
  if(any_na_type(as.character(date))){
    
    date <- get_event_date(event_row[[col_year]])
    
  }
  
  ############ Step 2. Create the folder for this event #########
  qmd_dir <- mk_qmd_dir(events_dir =  events_dir,
                        date = date,
                        title = title)
  
  filepath = set_up_qmd(qmd_dir = qmd_dir)
  
  ###### Step 3. Process any optional components to build the quarto event file ######
  
  # Subtitle  - is the location #
  # Get event location #
  location <- stringr::str_trim(event_row[[col_location]])
  
  # if location is missing, raise a warning note
  # and leave blank 
  if(!any_na_type(location)){
    
    subtitle_block <- paste0("subtitle: |\n  ", 
                             gsub("\n", "\n  ", paste0('Location: ', location)), 
                             "\n")
    
  } else {
    
    log_missing("location", rownum)  
    subtitle_block <- NULL
    
  }
  
  
  # Event 'author' - is event time 
  time <- event_row[[col_time]]
  if(!any_na_type(time)){
    
    time_block <- paste0("author: ", shQuote(time), "\n")
    
  } else {
    
    time_block <- NULL 
    
  }
  
  # Event category - is event tags
  # Get event tags (e.g. Q&A)
  tags <- stringr::str_trim(event_row[[col_tags]])
  
  if(!any_na_type(tags)){
    
    tags_block <- paste0("categories: [", tags, "]", "\n")
    
    
  } else {
    
    log_missing("tags", rownum)  
    tags_block <- NULL
    
  }
  
  
  # Main text content: # 
  # Include RSVP Link (if exists) # 
  rsvp_link = stringr::str_trim(event_row[[col_rsvp]])
  
  # Check that this contains a string indicating it is a url
  url_strings = "https|http|eventbrite|www|.com|url|.org"
  
  if(grepl(url_strings, rsvp_link) && !any_na_type(rsvp_link)){
  
  # If it is a url, but it is missing https or http, add https
  rsvp_link <- ifelse(!grepl("^https?://", rsvp_link) && !grepl("^http?://", rsvp_link),
                      paste0("https://", rsvp_link),
                      rsvp_link
                      )

  # Create the rsvp link button     
  # rsvp_link_button <- paste0("<a href=",
  #                             rsvp_link,
  #                             " class='btn btn-danger'>",
  #                             "RSVP Here</a>"
  #                            )
  
  # if the rsvp link doesn't seem to be a url
  # then don't make a rsvp link button 
  } else {
    
  rsvp_link <- NULL
    
  }
  
  # Add full event description to text section of listing in quarto file # 
  # Brief description # 
  description <- stringr::str_trim(event_row[[col_description]])
  description <- stringr::str_remove(description, '‍')
  
  
  if (!any_na_type(description)) {
    text_section <- paste0(
      description, "\n"
    )
  } else {
    
    log_missing("description", rownum)
    text_section <- NULL
      
  }
  
  # Get event speakers #
  speakers <- stringr::str_trim(event_row[[col_speakers]])
  # Make event speakers 'NA' if described in pdf, or includes NA in text
  speakers <- ifelse(any_na_type(speakers), NA, speakers)
  
  # Conditionally build speaker section (don't build if speakers are NA)
  if (!any_na_type(speakers) && !grepl(".pdf", speakers)) {
    speaker_section <- paste0(
      "<b>Speakers</b>  \n",
      gsub("\n", "  \n", speakers), "\n", "\n"
    )
  } else {
    
    speaker_section <- NULL
    
  }
  
  # Embed flyer image - if available
  flyer_photo_path <- get_event_flyer(flyer_link = event_row[[col_flyer_link]],
                                      flyer_name = event_row[[col_flyer_notes]],
                                      qmd_dir = qmd_dir)
  
  image_embed = embed_event_flyer(flyer_photo_path = flyer_photo_path,
                                  qmd_dir = qmd_dir,
                                  title = title,
                                  date = date)
  
  # Is this page a draft? (i.e. not ready to be published on the webpage) #
  # Make sure the page is listed as a draft
  draft_val = tolower(event_row[[col_draft]])
  
  if(grepl("^published$", draft_val)){
    
    draft_val <- "draft: false \n"
    
  } else if(grepl("draft", draft_val)) {
    
    draft_val <- "draft: true \n"
    
  }  else {
    
    log_missing_draft(rownum)
    draft_val <- "draft: true \n"
    
  }
  
  
  ##### Step 4. Build / write the quarto file #### 
  
  content <- paste0(
    "---\n",
    draft_val, 
    title_block,
    subtitle_block,
    time_block,
    "date: ", shQuote(date), "\n",
    tags_block,
    "rsvp_link: ", rsvp_link, "\n",
    # image_embed$image_section,
    "---\n",
     "\n", 
    text_section, "\n",
    speaker_section,
    image_embed$in_text_image
  )
  
  writeLines(content, filepath)  
  
}