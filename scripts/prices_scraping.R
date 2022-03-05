library(rvest)
library(tidyverse)

##### Scrape property trend prices #####

boroughs <- c("manhattan", "brooklyn", "bronx", "queens", "staten island")

urls <- vector("character", length(boroughs))

#### Residential properties #####

# Links to residential properties price estimates for each borough
for(i in 1:length(boroughs)){
  urls[i] <- paste0("https://www.propertyshark.com/mason/market-trends/residential/nyc/", boroughs[i])
}

pages <- tibble(page = map(urls, read_html))

out <- list()
for(i in seq_len(length(pages$page))) {
  out[[i]] <- list(c(pages[[1]][[i]] %>% 
                       html_nodes(".table_types_of_properties") %>% 
                       html_table()))
  
}

# Clean scraping output
for(i in 1:length(out)) {
  names(out[[i]][[1]][[1]]) <- out[[i]][[1]][[1]][1, ]
  
  out[[i]][[1]][[1]] <- out[[i]][[1]][[1]][-1, ]
  
  out[[i]][[1]][[1]][, "borough"] <- boroughs[i]
}


resid_prices <- bind_rows(out[[1]][[1]][[1]], out[[2]][[1]][[1]],
          out[[3]][[1]][[1]], out[[4]][[1]][[1]],
          out[[5]][[1]][[1]])

# Scrape residential prices
# Done separately because of unique target HTML tag
resid_prices <- resid_prices %>% 
  select(borough, `Property Type`, `Median sale price/sqft`) %>% 
  rename(
    bldgclass = `Property Type`,
    est_sale_pr_sqft = `Median sale price/sqft`
  ) %>% 
  mutate(
    bldgclass = tolower(bldgclass),
    est_sale_pr_sqft = str_remove(est_sale_pr_sqft, '\\$')) %>% 
  mutate(est_sale_pr_sqft = str_replace(est_sale_pr_sqft, 'K', '000')) %>% 
  mutate(est_sale_pr_sqft = as.numeric(est_sale_pr_sqft)) %>% 
  mutate(
    borough = fct_recode(borough,
                         "BK" = "brooklyn",
                         "MN" = "manhattan",
                         "BX" = "bronx",
                         "QN" = "queens",
                         "SI" = "staten island"))

#### Other property categories ####

# Scrape price estimate from specific HTML element for each url
scrape_info <- function(urls, boroughs) {
  pages <- tibble(page = map(urls, read_html))
  
  out <- list()
  for(i in seq_len(length(pages$page))) {
    # Create list with scraping output and borough name (for reference)
    out[[i]] <- list(boroughs[i], c(pages[[1]][[i]] %>% 
                                      html_nodes(".comm_middle_hexagon_table , .top_hexagon_table") %>% 
                                      html_text()))
    
  }
  out
}

bldgclass <- c("commercial", "multifamily", "retail", 
               "office","industrial")

# Create link for each building categories
urls_comm       <- vector("character", length(boroughs))
urls_multifam   <- vector("character", length(boroughs))
urls_retail     <- vector("character", length(boroughs))
urls_office     <- vector("character", length(boroughs))
urls_industrial <- vector("character", length(boroughs))

for(i in 1:length(boroughs)){
  urls_comm[i]        <- paste0("https://www.propertyshark.com/mason/market-trends/commercial/nyc/", boroughs[i])
  urls_multifam[i]    <- paste0("https://www.propertyshark.com/mason/market-trends/multifamily/nyc/", boroughs[i])
  urls_retail[i]      <- paste0("https://www.propertyshark.com/mason/market-trends/retail/nyc/", boroughs[i])
  urls_office[i]      <- paste0("https://www.propertyshark.com/mason/market-trends/office/nyc/", boroughs[i])
  urls_industrial[i]  <- paste0("https://www.propertyshark.com/mason/market-trends/industrial/nyc/", boroughs[i])
}

urls <- list(urls_comm, urls_multifam, urls_retail, 
             urls_office, urls_industrial)

# Scrape HTML element
out_comm        <- scrape_info(urls[[1]], boroughs)
out_multifam    <- scrape_info(urls[[2]], boroughs)
out_retail      <- scrape_info(urls[[3]], boroughs)
out_office      <- scrape_info(urls[[4]], boroughs)
out_industrial  <- scrape_info(urls[[5]], boroughs)

# Collect scraped elements into a dataframe
my_bind <- function(out) {
  c(out[[1]][[1]], out[[1]][[2]], 
    out[[2]][[1]], out[[2]][[2]],
    out[[3]][[1]], out[[3]][[2]], 
    out[[4]][[1]], out[[4]][[2]],
    out[[5]][[1]], out[[5]][[2]])
}

# Create vectors with scraping output and respective building class
out_comm        <- c(bldgclass[1], my_bind(out_comm))
out_multifam    <- c(bldgclass[2], my_bind(out_multifam))
out_retail      <- c(bldgclass[3], my_bind(out_retail))
out_office      <- c(bldgclass[4], my_bind(out_office))
out_industrial  <- c(bldgclass[5], my_bind(out_industrial))

out <- c(out_comm, out_multifam, out_retail, out_office, out_industrial)

# Create prices variable
prices <- out[which(out == "Average price/sqft") + 1]

# Repeat building class strings to match dataframe rows
bclass_rep <- list()
for(i in 1:length(bldgclass)) {
  bclass_rep[[i]] <- rep(bldgclass[i], 5)
}

bclass_rep <- unlist(bclass_rep)

price_by_class <- data.frame(
  borough = boroughs,
  bldgclass = bclass_rep,
  est_sale_pr_sqft = prices
) %>% 
  mutate(est_sale_pr_sqft = str_remove(est_sale_pr_sqft, '\\$')) %>% 
  mutate(est_sale_pr_sqft = str_replace(est_sale_pr_sqft, 'K', '000')) %>% 
  mutate(est_sale_pr_sqft = as.numeric(est_sale_pr_sqft)) %>% 
  mutate(
    borough = fct_recode(borough,
                         "BK" = "brooklyn",
                         "MN" = "manhattan",
                         "BX" = "bronx",
                         "QN" = "queens",
                         "SI" = "staten island"))

price_by_class <- bind_rows(resid_prices, price_by_class)

##################################################################

# Get price estimates for available building categories from official sales data
source("rolling_sales.R")

summary_rolling_sales_ny <- rolling_sales_ny %>% 
  group_by(borough, bldgclass) %>% 
  summarize(
    est_sale_pr_sqft = mean(saleprice / bldgarea)
  ) %>% 
  ungroup()

# Replace scraped estimates with sales data estimates (if available for given building category)
df <- full_join(summary_rolling_sales_ny, price_by_class) %>% 
  na.omit() %>% 
  group_by(borough, bldgclass) %>% 
  summarize(
    count = n(),
    est_sale_pr_sqft) %>% 
  ungroup()

df <- df[which(!(df$count > 1 & df$est_sale_pr_sqft %% 1 == 0)), ]

price_by_class <- df %>% select(-count)

rm(list=ls()[which(!(ls() %in% c("price_by_class", "ny_housing", 
                                 "ny_housing_sub", "rolling_sales_ny")))])
