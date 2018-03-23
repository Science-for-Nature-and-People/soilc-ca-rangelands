###################################
# Plot papers with RMZ zones      #
# Stephen Wood                    #
# Last updated: March 23, 2018    #
###################################


# LOAD PACKAGES
library(googlesheets)
library(tidyverse)

# READ DATA
all_data <- gs_title("data_collection.xlsx")
context_data <- gs_read(ss=all_data, ws = "Contextual Information")
context_data <- as_tibble(context_data)

# OVERALL DATA MANIPULATION
context_data <- select(context_data,'reference','precip (mm)','N','W','N2','W2')

# GENERATE POLYGON FOR MULTI-POINT STUDIES
## Separate out the rows with multiple points
box_data <- filter(context_data,is.na(N2)==F) %>%
              unique()

# Generate polygon for paper
box_polygon <- rbind(
  c(box_data$W2,box_data$N2), 
  c(box_data$W,box_data$N2), 
  c(box_data$W,box_data$N), 
  c(box_data$W2,box_data$N), 
  c(box_data$W2,box_data$N2)
  ) %>%
    list() %>%
    st_polygon()

# GENERATE SPATIAL POINTS FOR SINGLE-POINT STUDIES
## Subset valid points
## 
point_data <- filter(context_data,is.na(N2)==T) %>% # Filter points that have one value
  filter(is.na(N)==F) %>%                           # Filter points that are valid
  unique() %>%                                      # Remove duplicates
  select('reference','precip (mm)', 'N', 'W') %>%   # Select relevant variables
  st_as_sf(coords=c("N","W"))                       # Convert NW values to sf point geometry

