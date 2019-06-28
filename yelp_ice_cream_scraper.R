# Martin
# June 28, 2019
# Final Hackathon!!!!!

library(dplyr)
library(tidyverse)
library(rvest)
library(ggmap)

# Scrape MTL ice cream adresses from Yelp

# 30 ice cream stores in each link, except for url6
url1 <- "http://www.yelp.ca/search?find_desc=Ice%20Cream&find_loc=Montreal%2C%20QC&ns=1&cflt=icecream&sortby=rating"
url2 <- "http://www.yelp.ca/search?find_desc=Ice%20Cream&find_loc=Montreal%2C%20QC&ns=1&cflt=icecream&sortby=rating&start=30"
url3 <- "http://www.yelp.ca/search?find_desc=Ice%20Cream&find_loc=Montreal%2C%20QC&ns=1&cflt=icecream&sortby=rating&start=60"
url4 <- "http://www.yelp.ca/search?find_desc=Ice%20Cream&find_loc=Montreal%2C%20QC&ns=1&cflt=icecream&sortby=rating&start=90"
url5 <- "http://www.yelp.ca/search?find_desc=Ice%20Cream&find_loc=Montreal%2C%20QC&ns=1&cflt=icecream&sortby=rating&start=120"
url6 <- "http://www.yelp.ca/search?find_desc=Ice%20Cream&find_loc=Montreal%2C%20QC&ns=1&cflt=icecream&sortby=rating&start=150" # only 11 stores in this link
url <- c(url1, url2, url3, url4, url5, url6)

n <- 1
N <- 30 # 30 ice cream stores per url
total <- 161 # 161 ice cream stores in total

iceCream_data <- data.frame(name = character(total), address1 = character(total), address2 = character(total), stars = numeric(total), stringsAsFactors = FALSE) # characters in dataframe are taken as factors by default

html <- read_html(url1)

name_path_1 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
name_path_2 <- "]/div/div/div/div[2]/div[1]/div[1]/div[1]/div[1]/h3/a"

address_path_1 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
address_path_2 <- "]/div/div/div/div[2]/div[1]/div[2]/div/address/div/p/span"
address_path_alt_1 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
address_path_alt_2 <- "]/div/div/div/div[2]/div[1]/div[2]/div/div[2]/p/span/a"
address_paths <- c(address_path_1, address_path_2, address_path_alt_1, address_path_alt_2)

borough_path_1 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
borough_path_2 <- "]/div/div/div/div[2]/div[1]/div[2]/div/div[2]/div/p"
borough_path_alt_1 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
borough_path_alt_2 <- "]/div/div/div/div[2]/div[1]/div[2]/div/div[3]/div/p"
borough_path_alt_3 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
borough_path_alt_4 <- "]/div/div/div/div[2]/div[1]/div[2]/div/div/div/p"
borough_paths <- c(borough_path_1, borough_path_2, borough_path_alt_1, borough_path_alt_2, borough_path_alt_3, borough_path_alt_4)

# star ratings not available for each ice cream store
stars_path_1 <- "/html/body/div[2]/div[3]/div[2]/div[2]/div/div[1]/div[1]/div/ul/li["
stars_path_2 <- "]/div/div/div/div[2]/div[1]/div[1]/div[1]/div[2]/span/div"

# extract names, addresses, and boroughs (and maybe star ratings) of ice-cream stores from Yelp
# will iterate over each url
for(i in 1:6){
  html <- read_html(url[i])
  index <- ifelse(i == 1, 0, ((i-1)*30)) # this makes sure the index counter will not restart after each inner loop iteration
  ifelse(i == 6, N <- 11, N <- N) # last url only has 11 observations, so this will prevent error messages from popping up
  for(j in n:N) {
    index <- index + 1
    name_path <- paste(name_path_1, name_path_2, sep=as.character(j))
    try(iceCream_data$name[index] <- html %>% html_node(xpath = name_path) %>% html_attr("name")) # attribute of that node is the name
    address_path <- paste(address_paths[1], address_paths[2], sep = as.character(j))
    address_path_alt <- paste(address_paths[3], address_paths[4], sep = as.character(j))
    try(iceCream_data$address1[index] <- ifelse(!is.na(html %>% html_node(xpath = address_path) %>% html_text()), html %>% html_node(xpath = address_path) %>% html_text(), 
                                                html %>% html_node(xpath = address_path_alt) %>% html_text())) # use alternative path if path returns NA
    borough_path <- paste(borough_paths[1], borough_paths[2], sep = as.character(j))
    borough_path_alt <- paste(borough_paths[3], borough_paths[4], sep = as.character(j)) # alternative xpath1
    borough_path_alt_alt <- paste(borough_paths[5], borough_paths[6], sep = as.character(j))# alternative xpath2
    # if xpath returns an NA, then try the alternatives. Remaining NA value mean there is no borough on that actual page
    try(iceCream_data$address2[index] <- ifelse(!is.na(html %>% html_node(xpath = borough_path) %>% html_text()), html %>% html_node(xpath = borough_path) %>% html_text(),
                                                ifelse(!is.na(html %>% html_node(xpath = borough_path_alt) %>% html_text()), html %>% html_node(xpath = borough_path_alt) %>% html_text(), # use first alternative path is path returns NA
                                                       html %>% html_node(xpath = borough_path_alt_alt) %>% html_text()))) # use second alternative path if first alternative returns NA as well
    #  stars_path <- paste(stars_path_1, stars_path_2, as.character(j))
    #  try(iceCream_data$stars[index] <- html %>% html_node(xpath = stars_path) %>% html_attr("aria-label"))
  }
}

# activating my google key for the geocoding API, charges $$ per request and my free trial has a couple thousand free request so plz don't run too many times lol
gkey <- ""
register_google(key = gkey)

# Combining addresses and ommitting stores with incomplete addresses
# geocode addresses using Google geocode function
iceCream_data <- iceCream_data %>% 
  filter(complete.cases(address1),
         complete.cases(address2)) %>% 
  mutate(address = paste(address1, address2, sep = ", ")) %>% 
  mutate(n = row_number())

# Geocoding addresses
lat_lon_creme_glace <- geocode(iceCream_data$address, output = "latlona", source = "google")
lat_lon_creme_glace <- lat_lon_creme_glace %>% 
  mutate(n = row_number())

# merging datasets and keeping important variables
iceCream_with_coord <- merge(iceCream_data, lat_lon_creme_glace, by = "n")
iceCream_with_coord <- iceCream_with_coord %>% 
  select(-address1, -address2, -address.x)

write.csv(iceCream_with_coord, file = "ice_cream_coords.csv")
