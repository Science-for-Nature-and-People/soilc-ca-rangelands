# install.packages("googlesheets")

library(googlesheets)
library(tidyverse)

all_data <- gs_title("data_collection.xlsx")
raw_data <- gs_read(ss=all_data, ws = "Raw Evidence")
raw_data <- as_tibble(raw_data)

# Generate nodes, properties, and trophic.links

## Generate nodes sheet
node <- c(raw_data$response_lumped,raw_data$treatment_category_1) 
functional.group <- c(raw_data$response_type,rep("management",nrow(raw_data)))
  
nodes <- cbind(node,functional.group) %>% 
  as_tibble() %>%
  distinct() %>%
  na.omit()
  
rm(node)
rm(functional.group)

## Generate properties sheet
### Will wait to build out this functionality until we need it
get_properties <- function(data,vars) {
  ## This data frame is supposed to identify properties of each network
  ## So far we've only been using one network
}

## Generate trophic links sheet
resource <- raw_data$response_lumped
consumer <- raw_data$treatment_category_1
link.strength <- raw_data$`absolute Hedges G`
direction <- raw_data$`Hedge's G`

trophic_links <- cbind(resource,consumer,link.strength,direction) %>% 
  as_tibble() %>%
  filter(link.strength != "#DIV/0!") %>%
  filter(link.strength != "#VALUE!") %>%
  na.omit

trophic_links$direction <- as.numeric(trophic_links$direction)
trophic_links$link.strength <- as.numeric(trophic_links$link.strength)
trophic_links$direction <- ifelse(trophic_links$direction<0, 'Negative','Positive')

## Return all data as a list
data <- list(nodes,properties=NULL,trophic_links)


