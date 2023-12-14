GRYN Wetland Photo Time Series
================
Ben LaFrance
2023-12-13

Ben LaFrance made this script to generate PDFs that tile historic
wetland site photos automatically. Last edited on 2023-12-13. Images
must all be labeled with the following convention:
Catchment-Site_pp_X_bearing_yyyymmdd.jpg An example file would be:
Y4170-1_pp_a_080_20060627.jpg

I would still like to do the following: 
     - color coordinate the perimeter framing by SWE for each year 
     - leave gaps in the grid to show missing data
     -The base directory is hard-coded, and will need to be edited

## R Markdown

``` r
# Load the necessary libraries
library(magick)
```

    ## Linking to ImageMagick 6.9.12.96
    ## Enabled features: cairo, freetype, fftw, ghostscript, heic, lcms, pango, raw, rsvg, webp
    ## Disabled features: fontconfig, x11

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(grid)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
# Set the base directory
# Manually specify catchment number and name (should match folder layout)
catchment_number <- "Y4339"
catchment_name <- "Thorofare"

# Set the base directory
base_directory <- paste0(catchment_number, "_", catchment_name, "_Photos")

output_directory <- paste0(base_directory, "/TimeSeriesPDFs")
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

# List all subdirectories, then only the relevant repeat photo points folders
sub_dirs <- list.dirs(base_directory, full.names = TRUE, recursive = FALSE )
relevant_subdirs <- sub_dirs[startsWith(basename(sub_dirs), catchment_number)]
relevant_subdirs 
```

    ##  [1] "Y4339_Thorofare_Photos/Y4339-2007"        
    ##  [2] "Y4339_Thorofare_Photos/Y4339-2008"        
    ##  [3] "Y4339_Thorofare_Photos/Y4339-2009-NoVisit"
    ##  [4] "Y4339_Thorofare_Photos/Y4339-2010"        
    ##  [5] "Y4339_Thorofare_Photos/Y4339-2011-NoVisit"
    ##  [6] "Y4339_Thorofare_Photos/Y4339-2012"        
    ##  [7] "Y4339_Thorofare_Photos/Y4339-2013"        
    ##  [8] "Y4339_Thorofare_Photos/Y4339-2014"        
    ##  [9] "Y4339_Thorofare_Photos/Y4339-2015"        
    ## [10] "Y4339_Thorofare_Photos/Y4339-2016"        
    ## [11] "Y4339_Thorofare_Photos/Y4339-2017"        
    ## [12] "Y4339_Thorofare_Photos/Y4339-2018"        
    ## [13] "Y4339_Thorofare_Photos/Y4339-2019"        
    ## [14] "Y4339_Thorofare_Photos/Y4339-2020-NoVisit"
    ## [15] "Y4339_Thorofare_Photos/Y4339-2021"        
    ## [16] "Y4339_Thorofare_Photos/Y4339-2022-NoVisit"

``` r
# Go through all the subdirectories (years) and output all jpeg files within them

all_photos <- list.files(relevant_subdirs, pattern = "\\.jpg$", full.names = TRUE)

all_photo_visits <- sub("^(.*?_.*?_.*?)_.*\\.jpg", "\\1", basename(all_photos))
unique_photos <- unique(all_photo_visits)

unique_photos
```

    ##  [1] "Y4339-14_pp_a"  "Y4339-15_pp_a"  "Y4339-16_pp_c"  "Y4339-17_pp_a" 
    ##  [5] "Y4339-18_pp_a"  "Y4339-19_pp_a"  "Y4339-20_pp_a"  "Y4339-21_pp_a" 
    ##  [9] "Y4339-22_pp_a"  "Y4339-23_pp_a"  "Y4339-24_pp_a"  "Y4339-25_pp_b" 
    ## [13] "Y4339-25_pp_a"  "Y4339-81_pp_b"  "Y4339-82_pp_a"  "Y4339-83_pp_a" 
    ## [17] "Y4339-83_pp_b"  "Y4339-84_pp_a"  "Y4339-85_pp_a"  "Y4339-86_pp_a" 
    ## [21] "Y4339-87_pp_a"  "Y4339-88_pp_a"  "Y4339-16_pp_b"  "Y4339-22_pp_b" 
    ## [25] "Y4339-81_pp_a"  "Y4339-131_pp_a" "Y4339-131_pp_b" "Y4339-18_pp_b" 
    ## [29] "Y4339-132_pp_a" "Y4339-132_pp_b" "Y4339-18_pp_c"  "Y4339-22_pp_c" 
    ## [33] "Y4339-16_pp_a"

``` r
# Create an empty list to store matching filenames for each search string
matching_files_list <- list()

# Look for each target photo site individually, and create an empty list of matching file names
for (photo_point in unique_photos) {
  matching_filenames <- character(0)
  
  # Search within each relevant sub-directory for the matching file names
  for (subdir in relevant_subdirs) {
    sub_filenames <- list.files(subdir, pattern = ".jpg", full.names = TRUE)
    matching_files <- sub_filenames[grep(photo_point, sub_filenames, fixed = TRUE)]
    matching_filenames <- c(matching_filenames, matching_files)
    
    # Extract and sort files by date taken (yyyymmdd.jpg, should be how all the files end)
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
