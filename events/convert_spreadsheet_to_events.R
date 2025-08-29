############### script to take excel spreadsheet of events #####################
############### and convert into quarto files #################################
# Expected column names: 
# this is after clean_names is performed - 
# replaces space & / with _, all lowercase, ( & ) are removed, 
# you can test out how a given name will be converted with
# clean_names("My example column name")

# Functions I custom built to process columns etc. are found in this
# Rscript
source(here::here("events/fns_clean_spreadsheet.R"))

{
col_year       <- clean_names("Year") # Year of event
col_event      <- clean_names("Event") # Event name
col_date       <- clean_names("Date(s)") # Event date
col_time       <- clean_names("Time") # Event time

col_tags     <- clean_names("Tags") # Event categories / tags 
col_location   <- clean_names("Location") # Event location
col_speakers   <- clean_names("Speakers") # Event Speakers
col_description<- clean_names("Full Description") # Description

col_rsvp       <- clean_names("RSVP Link") # Link to where you can RSVP to the event?

col_flyer_link <- clean_names("Flyer (Website link)") # Google Drive link to where the Flyer is
col_flyer_notes <- clean_names("Flyer/Notes") # Name of the flyer

col_draft <- clean_names("Website ready?") # Should the event be hosted on the website yet?
}




######## Remove all old quarto event files before rebuilding site #########

events_dir = here::here("events")

# subfolders to delete:
subdirs <- list.dirs(events_dir, full.names = TRUE, recursive = FALSE)

# only delete folders that match the expected naming convention (YYYY-MM-DD_title)
subdirs <- grep("^\\d{4}-\\d{2}-\\d{2}_.+", subdirs, value = TRUE)
           
unlink(subdirs, recursive = TRUE, force = TRUE)



################ Download google sheet of SPG events #########################
{
  
googlesheets4::gs4_auth(
  token = gargle::secret_read_rds(
    here::here(".secrets/gs4-drive-token.rds"),
    key = "GDRIVE_KEY"
  ),
  scopes = "drive.readonly"
)

events_sheet_url <- Sys.getenv("EVENTS_GSHEET_URL")
events_spreadsheet <- googlesheets4::read_sheet(events_sheet_url)
}





######## Clean up the spreadsheet names for analysis ########

names(events_spreadsheet) <- clean_names(names(events_spreadsheet))

events_spreadsheet <- events_spreadsheet |>
                     tidyr::fill(!!col_year, .direction = 'down')



######## Check necessary columns included ##########

required_cols <- c(col_year = col_year, 
                   col_event = col_event, 
                   col_date = col_date)

missing_required <- setdiff(required_cols, names(events_spreadsheet))

names(missing_required) <- names(required_cols[required_cols %in% missing_required])

if (length(missing_required) > 0) {
  message(paste0("❌ Can't find shese required column/s in the Google sheet: ",
                 paste(names(missing_required), collapse = ", ")),
          paste("\n It is likely that the names of these column/s have changed in the sheet."),
          paste("\n please update the expected column names at the top of this script to reflect this change.")
  )
}



########### Check optional columns ###############

optional_cols <- c(col_time, 
                   col_description, col_location, col_speakers, col_tags, 
                   col_rsvp, col_flyer_link, col_flyer_notes, 
                   col_draft)

missing_optional <- setdiff(optional_cols, names(events_spreadsheet))

if (length(missing_optional) > 0) {
  stop(paste0("⚠️ Warning: these column/s couldn't be found in the Google sheet: ",
              paste(names(missing_required), collapse = ", ")),
       paste("\n It is likely that the names of these column/s have changed in the sheet."),
       paste("\n please update the expected column names at the top of this script to reflect this change."),
       paste("\n Otherwise these details will remain blank on website for all events.")
  )
}

# Add missing optional columns as empty to prevent errors later
for (col in optional_cols) {
  if (!col %in% names(events_spreadsheet)) {
    events_spreadsheet[[col]] <- NA_character_
  }
}

############# Required packages ##################

library(stringr)
library(here)


##################### Process spreadsheet events files ###############
# Loop through rows (each event) to generate .qmd files

# This is where the function to process each event row is hosted
source(here::here("events/build_events_page/process_events_sheet_row.R"))

for (i in 1:nrow(events_spreadsheet)) {

  # if this fails for one row, print a warning message, rather than
  # stop the rest of the script from running - 
  # you can see what the warning message looks like at the end of this script
  event_row = events_spreadsheet[i,]
  
  tryCatch({
    
    withCallingHandlers({
    process_events_sheet_row(event_row = event_row,
                             rownum = i,
                             events_dir = events_dir,
                             qmd_dir = qmd_dir, 
                             col_event = col_event,
                             col_date = col_date,
                             col_time = col_time,
                             col_year = col_year,
                             col_location = col_location,
                             col_tags = col_tags,
                             col_description = col_description,
                             col_draft = col_draft,
                             col_rsvp = col_rsvp,
                             col_speakers = col_speakers,
                             col_flyer_link = col_flyer_link,
                             col_flyer_notes = col_flyer_notes 
                             )  
},
 error = function(e) {
  message(paste0("⚠️ Skipping row ", i, 
                 " due to error: ", conditionMessage(e)))
  message("This error ")
   traceback()
})
    
}, error = function(e) NULL)
  
}

cat(paste0("✅ Created ", 
           nrow(events_spreadsheet), 
           " Quarto files in '", 
           events_dir, 
           "'\n")
    )

