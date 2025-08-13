{
  col_name      <- "name"
  col_headshot_link <- "headshot_link"
  col_headshot_name <- "headshot_name"
  col_ucsf_position <- "ucsf_position"
  col_bio <- "bio"
  col_status <- "current_or_alumni"
  
}


{
  googlesheets4::gs4_auth(token = gargle::secret_read_rds(
    ".secrets/gs4-token.rds",
    key = "GARGLE_OAUTH_KEY"
  ))
  
  leadership_sheet_url <- Sys.getenv("LEADERSHIP_GSHEET_URL")
  leadership_spreadsheet <- googlesheets4::read_sheet(leadership_sheet_url)
}

source(here::here("events/fns_clean_spreadsheet.R"))

names(leadership_spreadsheet) <- clean_names(
  names(leadership_spreadsheet)
)

##################### Process spreadsheet leadership files ###############
# Loop through rows (each person) to generate .qmd files
for (i in 1:nrow(leadership_spreadsheet)) {
  
  # Get person name
  team_name <- stringr::str_trim(leadership_spreadsheet[[col_name]][i])
  title_block <- paste0("title: |\n  ", gsub("\n", "\n  ", team_name), "\n")
  
  ucsf_position <- stringr::str_trim(leadership_spreadsheet[[col_ucsf_position]][i])
  if(any_na_type(ucsf_position)){
    
    subtitle_block <- NULL
  } else {
  
  subtitle_block <- paste0("subtitle: |\n  ", gsub("\n", "\n  ", ucsf_position), "\n")
  
  }
  
  status <- stringr::str_trim(leadership_spreadsheet[[col_status]][i])
  
  qmd_dir <- mk_qmd_dir(here::here(paste0("leadership", "/", tolower(status))),
                        slugify(team_name),
                        NULL
                        )
  filepath = set_up_qmd(qmd_dir = qmd_dir)
  
  headshot_link <- stringr::str_trim(leadership_spreadsheet[[col_headshot_link]][i])
  headshot_name <- stringr::str_trim(leadership_spreadsheet[[col_headshot_name]][i])
  
  googledrive::drive_download(headshot_link,
                              path = paste0(qmd_dir, "/", headshot_name)
  )
  
  headshot_block <- paste0("image: ", shQuote(headshot_name), "\n",
                           "about: \n",
                           "  ", "template: jolla \n",
                           "  ", "image: ", shQuote(headshot_name), "\n"
                           )
  
  image_alt <- paste0("  ", "image-alt: ", 
                      shQuote(paste0("Headshot of ", team_name)), 
                      "\n")
  
  bio <- stringr::str_trim(leadership_spreadsheet[[col_bio]][i])
  if (!any_na_type(bio)) {
    bio_block <- paste0(
      bio, "\n"
    )
  } else {
    
    bio_block  <- NULL
  }
    
  
  
  ##### Step 3. Build / write the quarto file #### 
  
  content <- paste0(
    "---\n",
    title_block,
    subtitle_block,
    headshot_block,
    image_alt, 
    "---\n",
    bio_block
  )
  
  writeLines(content, filepath)
  
}
