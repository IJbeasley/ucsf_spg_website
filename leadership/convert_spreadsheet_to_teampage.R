# R Script to build Quarto Pages from the Team page Google sheet

# Google sheet column names 
# get the functions to clean column names
source(here::here("events/fns_clean_spreadsheet.R"))

{
  
  col_name      <- clean_names("Name") # Persons name
  
  col_headshot_link <- clean_names("Headshot link") # Google drive link to headshot
  col_headshot_name <- clean_names("Headshot name") # Name of headshot file
  
  col_ucsf_position <- clean_names("UCSF position") # UCSF Position (e.g. PhD student)
  col_program <- clean_names("Program") # UCSF Program name (e.g. PSPG, BMI etc.)
  
  col_bio <- clean_names("Bio") # Biography / Policy Interests
  
  col_social <- clean_names("Social Media or LinkedIn Link") # Link to Person Professional Social Media 
  col_social_icon <- clean_names("Social Media Icon") # Name of Social Media Icon
  
  col_status <- clean_names("Current or Alumni?") # Is this person a current or alumni member?
  col_active_years <- clean_names("Years") # What years have they been an active member?
  
}

# Tell R what google account to log into to see the google sheet
# And where the google sheet is
{
  
  googlesheets4::gs4_auth(
    token = gargle::secret_read_rds(
      here::here(".secrets/gs4-drive-token.rds"),
      key = "GDRIVE_KEY"
    ),
    scopes = "drive.readonly"
  )

  leadership_sheet_url <- Sys.getenv("LEADERSHIP_GSHEET_URL")
  leadership_spreadsheet <- googlesheets4::read_sheet(leadership_sheet_url)
  
}

# Remove all old quarto files - needed because of changes from
# current to alumni 
leadership_folders <- c(
                        here::here("leadership/current"),
                        here::here("leadership/alumni")
                        )

# delete all files & subfolders inside, but keep the folder itself
unlink(list.files(leadership_folders, full.names = TRUE), 
       recursive = TRUE, 
       force = TRUE)


guess_icon <- function(href) {
  # mapping of common domains to icons
  patterns <- list(
    bluesky = "bsky.app",
    twitter = "twitter.com",
    "twitter-x" = "x.com",
    linkedin = "linkedin.com",

    github = "github.com",
    gitlab = "gitlab.com",

    "threads-fill" = "threads.com",
    facebook = "facebook.com",
    instagram = "instagram.com",
    youtube = "youtube.com",
    mastodon = "mastodon.social"
    
#? Maybe add orcid &/or google scholar - required academicons 
# using this extension: https://github.com/schochastics/academicons?tab=readme-ov-file
    
  )
  
  # check each pattern
  for (icon in names(patterns)) {
    if (grepl(patterns[[icon]], href, ignore.case = TRUE)) {
      return(icon)
    }
  }
  
  # fallback if no match
  return("globe2")
}





##################### Process spreadsheet leadership files ###############


# process the column names of the google sheet
names(leadership_spreadsheet) <- clean_names(
  names(leadership_spreadsheet)
)


# Loop through rows (each person) to generate .qmd files
for (i in 1:nrow(leadership_spreadsheet)) {
  
  
  # Get membership status? - Are they a current or alumni member?
  status <- stringr::str_trim(leadership_spreadsheet[[col_status]][i])
  
  # if status is not available, skip:
  if(any_na_type(status)){
    
    message("Row Number ", i, ": ", "Invalid or missing information provided",
            "for whether this team member is current or alumni.",
            "\n Skipping ... ")
    next
    
  }
  
  # Get person name
  team_name <- stringr::str_trim(leadership_spreadsheet[[col_name]][i])
  
  if(any_na_type(team_name)){
    
    stop("Problem with person name: Is missing or NA")
    
  }
  
  # Title is persons name
  title_block <- paste0("title: ", shQuote(team_name))
  
  # Get person position 
  ucsf_position <- stringr::str_trim(leadership_spreadsheet[[col_ucsf_position]][i])
  if(any_na_type(ucsf_position)){
    ucsf_position <- NULL
  }
  
  
  # Get person program or department
  ucsf_program <- stringr::str_trim(leadership_spreadsheet[[col_program]][i])
  if(any_na_type(ucsf_program)){
    ucsf_program <- NULL
  }
  
  # Subtitle is UCSF Position + Program / Department
  subtitle_block <- paste0("subtitle: ", 
                           shQuote(paste0(ucsf_position, 
                                          "<br>", 
                                          ucsf_program)
                                   )
                           )
  
  # Get years active 
  years_active <- stringr::str_trim(leadership_spreadsheet[[col_active_years]][i])
  if(!any_na_type(years_active)){
    
    description_block <- paste0("description: ",
                                shQuote(years_active))
      
  } else {
    
    description_block <- NULL
    
  }
  
  # Get headshot information 
  # this is where the quarto file will be saved
  # and also the headshot, where relevant
  qmd_dir <- mk_qmd_dir(here::here(paste0("leadership", "/", tolower(status))),
                        slugify(team_name),
                        NULL
                        )
  filepath = set_up_qmd(qmd_dir = qmd_dir)
  
  headshot_link <- stringr::str_trim(leadership_spreadsheet[[col_headshot_link]][i])
  headshot_name <- stringr::str_trim(leadership_spreadsheet[[col_headshot_name]][i])
  
  # Default to NULL
  headshot_block <- NULL
  image_alt <- NULL
  
  # Only download and link in headshot (and alt text) if headshot link is non-empty
  if (!any_na_type(headshot_link)) {
  
  googledrive::drive_auth(
    token = gargle::secret_read_rds(
      here::here(".secrets/gs4-drive-token.rds"),
      key = "GDRIVE_KEY"
    ),
    scopes = "drive.readonly"
  )
  
  suppressMessages(  
  googledrive::drive_download(headshot_link,
                              path = paste0(qmd_dir, 
                                            "/", 
                                            headshot_name)
                              )
                    )
  
  # Add in headshot image
  headshot_block <- paste0("image: ", 
                           shQuote(headshot_name), 
                           "\n")
  
  # Alternative text for headshot
  image_alt <- paste0("image-alt: ", 
                      shQuote(paste0("Headshot of ", team_name)), 
                      "\n")
  
  }
  
  # Professional social media link for team members
  social_link <- stringr::str_trim(leadership_spreadsheet[[col_social]][i])
  
  # Name of social media icon - 
  # name from https://icons.getbootstrap.com/
  social_icon <-stringr::str_trim(leadership_spreadsheet[[col_social_icon]][i])
  social_icon <- tolower(social_icon)
  
  # if no icon is listed, but a social link is provided
  # guess the name of the icon
  if(any_na_type(social_icon) & !any_na_type(social_link)){
    
    social_icon <- guess_icon(tolower(social_link))
    
  }
  
  # If a social link is provided, include it in the text 
  if(!any_na_type(social_link)){
    
    link_block <- paste0("  ", "links: \n",
                         "    ", "- icon: ", social_icon, "\n",
                         "      ", "text: ", "Social Media Link", "\n",
                         "      ", "href: ", social_link, "\n",
                         "      ", "aria-label: ", "Link to ", social_icon, " account of", team_name, "\n"
                         )
    
    
    # use metadata for about section from leadership/_metadata.yml - 
    # if available
    metadata_file <- here::here('leadership/_metadata.yml')
    
    if(file.exists(metadata_file)){
    
    yaml_metadata <- suppressWarnings(yaml::read_yaml(metadata_file))
    meta_about <- yaml_metadata$about 
    
    about_defaults <- purrr::imap_chr(meta_about, 
                                     function(meta_value, 
                                              meta_name) { 
                                                            paste0("  ", 
                                                                   meta_name, ": ", 
                                                                   meta_value) 
                                                           }
                                     )
    } else {
      
    about_defaults <- character()  
    
    }
    
    # if template not included in metadata (or no metadata)
    if(!"template" %in% names(meta_about)){
      
      about_defaults["template"] <- paste0("  ", "template: solana \n")
      
    }

    # Creates about block structure   
    about_defaults <- unname(about_defaults)
    about_defaults <- paste(about_defaults, collapse = "\n")
    

    about_block <- paste0("about: \n",
                          about_defaults, "\n",
                          link_block)
    
    } else {
    
    link_block <- NULL
    about_block <- NULL
    
    }
  

  
  # Add biography, where available to about page
  bio <- stringr::str_trim(leadership_spreadsheet[[col_bio]][i])
  
  if (!any_na_type(bio)) {
    
    bio_block <- paste0(bio, "\n")
  } else {
    
    bio_block  <- NULL
    
  }
    

  
  
  ##### Step 3. Build / write the quarto file ####
  
  content <- paste0(
    "---\n",
    title_block, "\n",
    subtitle_block, "\n",
    description_block, "\n", 
    headshot_block,
    image_alt, 
    about_block,
    "---\n",
    bio_block
  )
  
  writeLines(content, filepath)
  
}
