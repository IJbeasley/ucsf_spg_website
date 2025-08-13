library(yaml)


output_file = here::here("gallery/gallery.yml")

{
  googlesheets4::gs4_auth(token = gargle::secret_read_rds(
    ".secrets/gs4-token.rds",
    key = "GARGLE_OAUTH_KEY"
  ))
  
  gallery_sheet_url <- Sys.getenv("GALLERY_GSHEET_URL")
  gallery_spreadsheet <- googlesheets4::read_sheet(gallery_sheet_url)
  
}


# Functions I custom built to process columns etc. are found in this
# Rscript
source(here::here("events/fns_clean_spreadsheet.R"))

names(gallery_spreadsheet) <- clean_names(
  names(gallery_spreadsheet)
)

{
  col_title <- "title"
  col_file_link <- "file_link"
  col_file_name <- "file_name"
  col_description <- "description"
  col_alt_text <- "alt_text"
  col_date <- "date"
}


##################### Process spreadsheet gallery files ###############
# Loop through rows (each event) to generate .qmd files
# Build gallery list from spreadsheet
gallery <- vector("list", nrow(gallery_spreadsheet))

for (i in 1:nrow(gallery_spreadsheet)) {
  
  title <- stringr::str_trim(gallery_spreadsheet[[col_title]][i])
  
  quarter <- stringr::str_trim(gallery_spreadsheet[[col_date]][i])
  
  description <- stringr::str_trim(gallery_spreadsheet[[col_description]][i])
  
  alt_text <- stringr::str_trim(gallery_spreadsheet[[col_alt_text]][i])
  
  photo_link <- stringr::str_trim(gallery_spreadsheet[[col_file_link]][i])
  photo_name <- stringr::str_trim(gallery_spreadsheet[[col_file_name]][i])
  
  if(!file.exists(paste0("images/", photo_name))){

    googledrive::drive_download(photo_link,
                                path = paste0("images/", photo_name))
    
  }
  
  photo_path <- paste0("../images/", photo_name)
  
  # Add to YAML list
  gallery[[i]] <- list(
    title       = title,
    quarter     = quarter,
    description = description,
    alt_text    = alt_text,
    path        = photo_path
  )
}
  
# Write YAML
writeLines(as.yaml(gallery), output_file)  
