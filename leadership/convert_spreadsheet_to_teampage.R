# R Script to build Quarto Pages from the Team page Google sheet

# Google sheet column names 
# get the functions to clean column names
source(here::here("events/fns_clean_spreadsheet.R"))

{
  col_name      <- clean_names("Name")
  
  col_headshot_link <- clean_names("Headshot link")
  col_headshot_name <- clean_names("Headshot name")
  
  col_ucsf_position <- clean_names("UCSF position")
  col_program <- clean_names("Program")
  
  col_bio <- clean_names("Bio")
  col_social <- clean_names("Social Media or LinkedIn Link")
  
  col_status <- clean_names("Current or Alumni?")
  col_active_years <- clean_names("Years")
  
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
       recursive = TRUE, force = TRUE)


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
#? Maybe add orcid &/or google scholar
    
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
    
    message("Row Number", i, ": ", "Invalid or missing information provided",
            "for whether this team member is current or alumni.",
            "\n Skipping ... ")
    next
    
  }
  
  # Get person name
  team_name <- stringr::str_trim(leadership_spreadsheet[[col_name]][i])
  
  if(any_na_type(title)){
    
    stop("Problem with person name: Is missing or NA")
    
  }
  
  title_block <- paste0("title: ", shQuote(team_name))
  
  # Get person position 
  ucsf_position <- stringr::str_trim(leadership_spreadsheet[[col_ucsf_position]][i])
  if(any_na_type(ucsf_position)){
    ucsf_position <- NULL
  }
  
  
  # Get person program or deparment
  ucsf_program <- stringr::str_trim(leadership_spreadsheet[[col_program]][i])
  if(any_na_type(ucsf_program)){
    ucsf_program <- NULL
  }
  
  
  subtitle_block <- paste0("subtitle: ", 
                           shQuote(paste0(ucsf_position, "<br>", ucsf_program))
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
  
  # Only download and build blocks if link is non-missing and non-empty
  if (!any_na_type(headshot_link) && nzchar(headshot_link)) {
  
  googledrive::drive_auth(
    token = gargle::secret_read_rds(
      here::here(".secrets/gs4-drive-token.rds"),
      key = "GDRIVE_KEY"
    ),
    scopes = "drive.readonly"
  )
  
  suppressMessages(  
  googledrive::drive_download(headshot_link,
                              path = paste0(qmd_dir, "/", headshot_name))
                    )
  
  headshot_block <- paste0("image: ", shQuote(headshot_name), "\n")
  
  image_alt <- paste0("image-alt: ", 
                      shQuote(paste0("Headshot of ", team_name)), 
                      "\n")
  
  }
  
  social_link <- stringr::str_trim(leadership_spreadsheet[[col_social]][i])
  
  if(!any_na_type(social_link)){
    
    link_block <- paste0("  ", "links: \n",
                         "    ", "- icon: ", guess_icon(social_link), "\n",
                         "      ", "text: ", "Social Media Link", "\n",
                         "      ", "href: ", social_link, "\n"
    )
  } else {
    
    link_block <- NULL
    
  }
  
  about_block <- paste0("about: \n",
                        "  ", "template: solana \n",
                        link_block
                        )
  
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
