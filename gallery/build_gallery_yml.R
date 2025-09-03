library(yaml)
# RScript to build gallery.yml from Gallery Google Sheets spreadsheet

# Where to save the output YAML file
output_file = here::here("gallery/gallery.yml")
img_dir <- "images"

############# Define google sheet column names ##############
# get the functions to clean column names

# Functions I custom built to process columns etc. 
# are found in this Rscript
source(here::here("events/fns_clean_spreadsheet.R"))

{
  
  col_file_link <- clean_names("File link") # Google drive link to image file
  col_file_name <- clean_names("File name") # Name of image file
  
  col_title <- clean_names("Title") # Title of image
  col_description <- clean_names("Description") # Description of image
  col_alt_text <- clean_names("Alt text") # Alt text for image
  
  col_date <- clean_names("Date") # Date or quarter when photo was taken
  
}



################ Download google sheet of SPG gallery images #########################
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
  
  gallery_sheet_url <- Sys.getenv("GALLERY_GSHEET_URL")
  gallery_spreadsheet <- googlesheets4::read_sheet(gallery_sheet_url)
  
}

# Check sheet not empty
if (nrow(gallery_spreadsheet) == 0) {
  stop("⚠️ The gallery spreadsheet is empty. \n
        It's likely that the URL of the google sheet has changed. \n
        Please update the GALLERY_GSHEET_URL in the github repository secrets. \n
        Otherwise, if the URL is correct, and the sheet is indeed empty, \n
        then please add some rows to the sheet before running this script.")
}


# process the column names of the google sheet
names(gallery_spreadsheet) <- clean_names(
  names(gallery_spreadsheet)
)


##################### Check required columns exist #####################
req_cols <- c(
  col_file_link = col_file_link,
  col_file_name = col_file_name
)

# Check all required columns exist
# & get their expected names
missing_req_cols <- setdiff(req_cols, names(gallery_spreadsheet))
names(missing_req_cols) <- names(req_cols[req_cols %in% missing_req_cols])

if(length(missing_req_cols) > 0){
  
  missing_req_cols <- paste(names(missing_req_cols), collapse = ", ")
  
  stop(
  paste0(
         "❌ The following required columns are missing from the gallery spreadsheet: ",
         missing_req_cols
         )
       )

}

##################### Check optional columns exist #####################
opt_cols <- c(
  col_title = col_title,
  col_description = col_description,
  col_alt_text = col_alt_text,
  col_date = col_date
)

# Check all optional columns exist
# & get their expected names
missing_opt_cols <- setdiff(opt_cols, names(gallery_spreadsheet))
names(missing_opt_cols) <- names(opt_cols[opt_cols %in% missing_opt_cols])

if(length(missing_opt_cols) > 0){
  
  missing_opt_cols <- paste(names(missing_opt_cols), collapse = ", ")
  
  warning(
    paste0(
      "⚠️ The following optional columns are missing from the gallery spreadsheet: ",
      paste0("'", missing_opt_cols, "'", collapse = ", "),
      "\n These fields will be left blank in the gallery.yml file.",
      "\n It is likely that the names of these column/s have changed in the sheet.", 
      "\n please update the expected column names at the top of this script to reflect this change."
      )
    )
  
}

# Add missing optional columns as empty to prevent errors later
for (col in opt_cols) {
  if (!col %in% names(gallery_spreadsheet)) {
    gallery_spreadsheet[[col]] <- NA_character_
  }
}

##################### Create images folder if it doesn't exist #####################

if(!dir.exists(here::here("images"))){
  dir.create(here::here("images"))
}

##################### Process spreadsheet gallery files ###############

# Loop through rows (each event) to generate .qmd files
# Build gallery list from spreadsheet - 
# in turn build gallery.yml from this list

gallery <- vector("list", nrow(gallery_spreadsheet))

for (i in 1:nrow(gallery_spreadsheet)) {
  
  ######## Step 1 Extract relevant fields from the spreadsheet #########
  
  # Get image title
  title <- stringr::str_trim(gallery_spreadsheet[[col_title]][i])
  if(any_na_type(title)){
    title <- ""
  }
  
  # Get image date or quarter
  quarter <- stringr::str_trim(gallery_spreadsheet[[col_date]][i])
  if(any_na_type(quarter)){
    quarter <- ""
  }
  
  # Get image description
  description <- stringr::str_trim(gallery_spreadsheet[[col_description]][i])
  if(any_na_type(description)){
    description <- ""
  }
  
  # Get image alt text
  alt_text <- stringr::str_trim(gallery_spreadsheet[[col_alt_text]][i])
  if(any_na_type(alt_text)){
    alt_text <- ""
  }
  
  # Get image file link & name
  photo_link <- stringr::str_trim(gallery_spreadsheet[[col_file_link]][i])
  if(any_na_type(photo_link)){
    message(paste0("Row Number ", i, ": ", "Invalid or missing information provided",
                "for the google drive link to the image file.",
                "\n Skipping this image"))
    
    next
  }
  
  photo_name <- stringr::str_trim(gallery_spreadsheet[[col_file_name]][i])
  if(any_na_type(photo_name)){
    message(paste0("Row Number ", i, ": ", "Invalid or missing information provided",
                   "for the name of the image file.",
                   "\n Skipping this image"))
    
    next
  }
  
  ######## Step 2 Download image file from google drive #########
  photo_path <- paste0("images/", photo_name)
  
  # If file already exists, delete it & re-download
  if(file.exists(here::here(photo_path))){
    
    file.remove(here::here(photo_path))
    
  }

  suppressMessages(
      googledrive::drive_download(photo_link,
                                  path = here::here(photo_path
                                                    )
                                  )
  )
    
  
  photo_path <- paste0("../", photo_path) # adjust path for yaml file
  
  ########## Step 3. Add gallery details to YAML list ##########
  gallery[[i]] <- list(
    title       = title,
    quarter     = quarter,
    description = description,
    alt_text    = alt_text,
    path        = photo_path
  )
}
  
# Write YAML
writeLines(yaml::as.yaml(gallery), output_file)  

if (file.exists(output_file)) {
  message("✅ Gallery YAML file successfully written: ", output_file)
} else {
  stop("❌ Something went wrong: gallery.yml was not created.")
}
