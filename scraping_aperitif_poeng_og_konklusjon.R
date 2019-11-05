library(xml2)
library(rvest)
library(robotstxt)
library(timeR)
library(tidyverse)



# robotstxt ---------------------------------------------------------------


paths_allowed(
  paths = c("https://www.aperitif.no/pollisten/vin?minPrice=&maxPrice=300&q=&sortBy=editorial_rating&offset=0")
)


## Scraper for å lage en url-liste for å scrape på hvert enkelt produkt. ----------------------------------------

# Setter opp url ---------------------------------------------------------------------------


url_loop_aperitif <- paste0("https://www.aperitif.no/pollisten/vin?minPrice=&maxPrice=300&q=&sortBy=editorial_rating&offset=",30 * 0:600 )


# Scraping fra aperitif ---------------------------------------------------------------------


url_aperitif <- lapply(url_loop_aperitif,
              function(url){
                url %>% read_html() %>% 
                html_nodes(".item-list a") %>% 
                html_attr('href')
              }) %>% 
  unlist()



# Rensker dataene ---------------------------------------------------------------------------


url_aperitif <- url_aperitif %>% 
  str_trim() %>% 
  str_remove("\\#") %>% 
  .[. != ""]

url_aperitif <- paste("https://www.aperitif.no", url_aperitif, sep = "")



## Scraper hver enkelt side fra utvalg --------------------------------------------------------------------


scraping <- function(info){
  lapply(url_aperitif,
         function(url){
           url %>% read_html() %>% 
             html_node(info) %>% 
             html_text() %>%
             gsub('[\r\n\t]', '', .) 
         }) %>% 
    unlist()
}



# Scraping fra vinmonopolets sider

# Disse funker ikke ennå. Fortsett her.

varenummer   <- scraping(".act span span")

navn         <- scraping("h1")

poeng        <- scraping(".rating-points")

konklusjon   <- scraping(".conclusion")

date         <- scraping(".date") %>% 
  str_trim()


# Creating tibble and writing csv -----------------------------------------


df <- tibble(
  varenummer,
  navn,
  poeng,
  konklusjon,
  date,
  url_aperitif
)


current_date <- Sys.Date() %>% 
  str_remove_all("-")


write_excel_csv2(df, paste("data/", current_date,"_aperitif_poeng_og_konklusjon.csv", sep="")) 

                  