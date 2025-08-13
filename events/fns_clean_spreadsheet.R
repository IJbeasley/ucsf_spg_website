# Functions to help converting sheets data to quarto documents 



####################### Clean column names ############

# Normalize column names: trim spaces, convert to lowercase, replace spaces and punctuation with underscores
clean_names <- function(colname) {
  
  clean_colname = 
  stringr::str_trim(colname) |>
  stringr::str_replace_all("/", "_") |> 
  stringr::str_replace_all("\\(", "") |> 
  stringr::str_replace_all("\\)", "") |> 
  stringr::str_to_lower() |>
  stringr::str_replace_all("[^a-z0-9]+", "_") |>
  stringr::str_replace_all("_$", "") 
  
  return(clean_colname)
}

###################### Make event folder ##############
#### In format: YYYY-MM-DD_event_name ########

slugify <- function(text) {
  text <- tolower(text)
  text <- gsub("[^a-z0-9]+", "-", text)
  text <- gsub("-+", "-", text)
  text <- gsub("^-|-$", "", text)
  return(text)
}

mk_qmd_dir <- function(events_dir, 
               date,
               title){
  
  ###################### Slugify: ##############
  ######### Converts title to lower case, and removes spaces etc. (used in make event folder, below) #######
  if(!is.null(title)){

    qmd_dir  = paste0(events_dir, 
                      "/",slugify(date), "_", slugify(title)
    )        
    
  } else {
    
    qmd_dir  = paste0(events_dir, 
                      "/",slugify(date))
    
  }
               
  
  return(qmd_dir)

}

############## any_na_type ##########
# Check  - for a table element, should this value be considered NA? 
# Dealing for conversion from spreadsheet to code - 
# Sometimes NAs are listed as empty "", NULL, or as strings / characters 

any_na_type <- function(string) {
  any(
    is.na(string) |
      string == "NA" |
      stringr::str_starts(string, "NA") |
      !nzchar(string) |
      is.null(string)
  )
}

############# Set up folder and quarto file ##########

set_up_qmd <- function(qmd_dir,
                       filename = "index.qmd"
){
  
  # Remove event folder - if it already exists
  if (dir.exists(qmd_dir)) {
    unlink(qmd_dir, recursive = TRUE)
  }
  
  # Make event folder
  dir.create(qmd_dir)
  
  # Set up event quarto file 
  filepath <- paste0(qmd_dir, "/", filename)
  
  return(filepath)
  
}


############# Format date: ##############

get_event_date <- function(date){

date <- as.character(date)  # Ensure it's in character format
date <- stringr::str_remove(date, "~")
date <- stringr::str_remove(date, "- Fall\\?")
date <- stringr::str_remove(date, "tentative")
date <- stringr::str_trim(date)
date <- stringr::str_replace(date, "and", "-")



# Extract starting portion if range
if (grepl("-", date)) {
  
  extract_start_date <- function(date_str) {
    # Remove tildes or other non-breaking dashes if present
    date_str <- stringr::str_replace_all(date_str, "[–—~]", "-")
    
    # Extract month and day from the start
    start_part <- stringr::str_extract(date_str, "^[A-Za-z]+\\s\\d{1,2}")
    
    if(is.na(start_part) & grepl("-", date_str)){
      
      year = strsplit(date_str, "-")[[1]][1]
      # Default start month: December
      start_part = "December"
      
    } else {
    
    # Extract year (assumes 4-digit year at the end)
    year <- stringr::str_extract(date_str, "\\d{4}$")
    
    }
    
    # Combine to form the full starting date
    paste(start_part, year)
  }
  
  date <- extract_start_date(date)
}


# If day is missing (e.g. just "April 2025"), append "01"
if (grepl("^[A-Za-z]+\\s\\d{4}$", date)) {
  date <- paste0(sub(" ", " 01 ", date))
}

# date <- ifelse(grepl("-", date), extract_start_date(date), date)
date <- as.Date(stringr::str_remove(date, ","),
                format = "%B %d %Y")


return(date)

} 


########## Format event description #############

format_event_description <- function(description){

description <- stringr::str_trim(description)
description <- stringr::str_remove(description, '‍')

return(description)

}


########## Get event format ########
# Remove white space around text, and where event format is document,
# Change it to information 

get_event_format <- function(format){
 
  format <- stringr::str_trim(format) 
  format <- stringr::str_replace(format,"Document", "Information")
  
  return(format)
}



############ Format flyer / image #########

get_event_flyer <- function(flyer_link,
                            flyer_name,
                            qmd_dir){

  # if not a drive link, then don't include flyer in page:
    flyer_link <- ifelse(
                               grepl("drive.google.com",flyer_link),
                              flyer_link,
                               NA
  )
  
  # if drive link for flyer exists -   download the flyer in the
  # event folder
  if(!is.na(flyer_link) && 
     nzchar(flyer_link) &&
     nzchar(flyer_name)
     ){
      
    flyer_type = tolower(tools::file_ext(flyer_name))
    # if flyer type not shown - asume pdf
    if(flyer_type == ""){
      flyer_type <- "pdf"
    }
    
    flyer_photo_path = paste0(qmd_dir, "/featured.", flyer_type)
    
   #drive_email <- Sys.getenv('GDRIVE_EMAIL')
    
   googledrive::drive_auth(
      #email = drive_email,
      token = gargle::secret_read_rds(
        here::here(".secrets/gs4-drive-token.rds"),
        key = "GDRIVE_KEY"
      ),
      scopes = "drive.readonly"
    )
    
    googledrive::drive_download(flyer_link,
                                path = flyer_photo_path)
    
    # if pdf - convert to png
    if(flyer_type == "pdf" && 
       file.exists(flyer_photo_path) && 
       file.info(flyer_photo_path)$size > 0
    ){
      
      flyer_photo = magick::image_read(flyer_photo_path)
      flyer_photo <<- magick::image_convert(image = flyer_photo,
                                          format = "png")
      
      # Generate paths for each page png file
      if(length(flyer_photo)==1){
        
        png_paths = file.path(
          qmd_dir,
          "featured.png"
        )
          
      } else {
      
      png_paths <<- file.path(
        qmd_dir,
        paste0("featured", 
               c("", paste0("-",seq_along(flyer_photo)[-1])), 
                 ".png"
               )
      )
      }
      
      if(length(png_paths) == 1){
        
        magick::image_write(flyer_photo,
                            path = png_paths,
                            format = "png")
        
      } else {
      
      # Write each page separately
      for(page in 1:length(png_paths)){
        
        magick::image_write(flyer_photo[page], 
                            path = png_paths[page],
                            format = "png")
        
      }
      }
      # Remove original PDF
      file.remove(flyer_photo_path)
      flyer_photo_path = png_paths
    }
    
  } else {
      
    flyer_photo_path <- NA
    
    }
    
    return(flyer_photo_path)
  
}

########## Embed event flyer ########

embed_event_flyer <- function(flyer_photo_path,
                              qmd_dir,
                              date,
                              title){

# Conditionally build image section - and 
# include image in text - if flyer photo is not downloaded
# then don't do it 
if(length(flyer_photo_path)==1){
  
if(is.na(flyer_photo_path)){
  
  image_section <- NULL
  in_text_image <- NULL  

} else if (!file.exists(flyer_photo_path) | 
          file.info(flyer_photo_path)$size == 0
          ) {
  
  image_section <- NULL
  in_text_image <- NULL  
  
} else {
  
  # get image path relative to quarto event page
  image <- stringr::str_remove(flyer_photo_path, 
                               paste0(qmd_dir, "/"))
  
  # label image section in yaml header
  image_section <- paste0( "image: ", 
                           shQuote(image),
                           "\n"
                           )
  
  # get code for embedding image inline
  in_text_image <- paste0(
    "![", "Flyer for ", title, " (", date, ")","]",
    "(", image, ")",
    "{.lightbox}"
  )
  
  }
} 

if (length(flyer_photo_path)>1){
  
  images <- stringr::str_remove(flyer_photo_path, 
                                paste0(qmd_dir, "/"))
  
  # make first page image the featured image (in the .yml header)
  image_section <- paste0( "image: ", 
                           shQuote(images[1]),
                           "\n"
                           )
  
  # get code for embedding images inline
  in_text_images = list()
  
  for(page in 1:length(flyer_photo_path)){
    
    in_text_images[page] <- paste0(
      "![", "Flyer for ", title, " (", date, ")", " - Page: ", page, "]",
      "(", images[page], ")", 
      "{.lightbox group=", sQuote(slugify(title)), "}"
    )
  }
  
  
  in_text_image <- paste(in_text_images, collapse = "\n\n")
  
} 
  
image_embed = list("image_section" = image_section,
                   "in_text_image" = in_text_image) 
  
  return(image_embed)
}


######### Make description section ###########


make_desc_section <- function(description){
  
  ### Make description shown in listing 
  # Do this by getting the first sentence only 
  first_sentence_description <- stringr::str_extract(
    description,
    "(?s).*?(?<!\\b(?:Dr|Mr|Ms|Mrs|Prof|Sr|Jr))\\.(\\s|$)"
  )
  first_sentence_description <- stringr::str_trim(first_sentence_description)
  
  if (!any_na_type(first_sentence_description)) {
    description_section <- paste0(
      "description: ", shQuote(first_sentence_description), "\n"
    )
  } else if(any_na_type(first_sentence_description) && 
            !any_na_type(description) &&
            nzchar(description)) {
    
    description_section <- paste0(
      "description: ", shQuote(description), "\n"
    )
    
  } else {
    
    description_section <- NULL 
  }
  
  return(description_section)
}

