library(yaml)

# Set folder and output
image_folder <- here::here("images")
output_file <- here::here("gallery/gallery.yml")


# List image files
image_files <- list.files(image_folder, 
                          pattern = "\\.(jpg|jpeg|png|webp|svg)$", 
                          ignore.case = TRUE
                          )
# Where to find images, relative to .yml output
image_folder <- fs::path_rel(path = image_folder,
                             start = here::here("gallery")
                             )

# Create YAML-friendly list
gallery <- lapply(seq_along(image_files), function(i) {
  file <- image_files[i]
  list(
    title = paste("SPG Test Photo #", i),
    quarter = "Fall 2021",
    description = "Test Description",
    alt_text = "Test Alt Text",
    path = paste0(image_folder, "/", file)
  )
})

# Write to YAML
writeLines(as.yaml(gallery), output_file)
