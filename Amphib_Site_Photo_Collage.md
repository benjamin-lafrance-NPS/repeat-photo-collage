---
title: "GRYN Repeat Photo Point Time Series"
subtitle: "Generate a grided PDF of historic wetland site photos"
author: "Ben LaFrance"
date: "2023-12-13"
output: 
  html_document:
    keep_md: true 
---

Ben LaFrance made this script to generate PDFs that tile historic wetland site photos automatically. 
Last edited on 2023-12-13.
Images must all be labeled with the following convention: Catchment-Site_pp_X_bearing_yyyymmdd.jpg
An example file would be: Y4170-1_pp_a_080_20060627.jpg

I would still like to do the following:
  - color coordinate the perimeter framing by annual SWE (wet vs dry years)
  - leave gaps in the grid to show missing data for a specific year
  - The base directory is hard-coded, and will need to be edited



## R Markdown


```r
# Load the necessary libraries
library(magick)
library(dplyr)
library(grid)
library(gridExtra)
```


```r
# Set the base directory
# Manually specify catchment number and name (should match folder layout)
catchment_number <- "Y4339"
catchment_name <- "Thorofare"

# Set the base directory from dynamic variables specified above
# Make sure all directories are set up like this before running this script
base_directory <- paste0(catchment_number, "_", catchment_name, "_Photos")

output_directory <- paste0(base_directory, "/TimeSeriesPDFs")
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

# Define the relevant repeat photo points folders (outreach photos not included)
sub_dirs <- list.dirs(base_directory, full.names = TRUE, recursive = FALSE )
relevant_subdirs <- sub_dirs[startsWith(basename(sub_dirs), catchment_number)]
```



```r
# Define all possible photo points for all years
all_photos <- list.files(relevant_subdirs, pattern = "\\.jpg$", full.names = TRUE)

# Remove any duplicate photo points (anything that has been repeatedly visited)
all_photo_visits <- sub("^(.*?_.*?_.*?)_.*\\.jpg", "\\1", basename(all_photos))
unique_photos <- unique(all_photo_visits)
```



```r
# Create an empty list to store matching files for each photo point
matching_files_list <- list()

# Look for each unique photo point in all the relevant directories
for (photo_point in unique_photos) {
  matching_filenames <- character(0)
  
  # Search within each relevant sub-directory for the matching jpegs
  for (subdir in relevant_subdirs) {
    sub_filenames <- list.files(subdir, pattern = ".jpg", full.names = TRUE)
    matching_files <- sub_filenames[grep(photo_point, sub_filenames, fixed = TRUE)]
    matching_filenames <- c(matching_filenames, matching_files)
    
    # Extract and sort files by date taken (all files named *_yyyymmdd.jpg)
    matching_filenames <- matching_filenames[order(as.Date(sub(".*_(\\d{8})\\.jpg$", "\\1", matching_filenames), format = "%Y%m%d"), decreasing = FALSE)]
  }
  
  # Create a blank PDF to act as the canvas for the time series tiling
  pdf_file <- paste0(output_directory,"/", photo_point, "_collage.pdf")
  pdf_width <- 11
  pdf_height <- 8.5
  n_rows <- 4
  n_cols <- 5
  cell_width <- unit(2, "inches")
  cell_height <- unit(1.7, "inches")
  
  # Open a PDF for the current photo point target
  pdf(pdf_file, width = pdf_width, height = pdf_height)
  
  # Calculate the number of grids
  num_grids <- n_rows * n_cols
  
  # Create an empty grid
  grid.newpage()
  
  # Loop through rows and columns to draw the boxes and add images
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      x <- unit((j - 1) * 2 + 1.5, "inches")
      y <- unit(8 - (i * 1.7) + 0.5, "inches") # Invert y to start from the top
      rect <- rectGrob(x = x, y = y, width = cell_width, height = cell_height,
                       gp = gpar(col = "black", fill = NA, lwd = 2))
      grid.draw(rect)
       
      # Add images if available in the directory
      image_index <- (i - 1) * n_cols + j
      if (image_index <= length(matching_filenames)) {
        img <- image_read(matching_filenames[image_index])
        img <- image_scale(img, "400x300") # This mintains a 4:3 aspect ratio. For speedy testing, set this to "4x3"
        grid.raster(as.raster(img), x = x, y = y + unit(-0.07, "inches"), width = cell_width / 1.05, height = cell_height / 1.15)

        # Extract the title (file name without ".jpg" extension)
        title <- sub(".jpg$", "", basename(matching_filenames[image_index]))
        title_x <- x # Centered x-position for the title
        title_y <- y + unit(+0.75, "inches")   # Adjust the y-position for the title
        grid.text(label = title, x = title_x, y = title_y, gp = gpar(fontsize = 7.5))
      }
    }
  }
  # close the PDF before looping on to the next photo target
  dev.off()
}
```




END of current script

