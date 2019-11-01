library(xml2)
library(rvest)
library(robotstxt)
library(timeR)
library(tidyverse)




# robotstxt ---------------------------------------------------------------------------------

paths_allowed(
  paths = c("https://www.aperitif.no/pollisten/vin?minPrice=&maxPrice=300&q=&sortBy=editorial_rating&offset=0")
)


## Scraper for å lage en url-liste for å scrape på hvert enkelt produkt. ----------------------------------------

# Setter opp url ---------------------------------------------------------------------------


url_loop_aperitif <- paste0("https://www.aperitif.no/pollisten/vin?minPrice=&maxPrice=300&q=&sortBy=editorial_rating&offset=",30 * 0:1 )


# Scraping fra aperitif ---------------------------------------------------------------------


#varenummer   <- scraping(".product-details")

varenummer   <- lapply(url_loop_aperitif,
                       function(url){
                         url %>% read_html() %>% 
                           html_nodes(".product-details") %>% 
                           html_text() %>%
                           gsub('[\r\n\t]', '', .) 
                       }) %>% 
  unlist()



url_aperitif <- lapply(url_loop_aperitif,
              function(url){
                url %>% read_html() %>% 
                html_nodes(".item-list a") %>% 
                html_attr('href')
              }) %>% 
  unlist()



# Rensker dataene ---------------------------------------------------------------------------

           
varenummer <- varenummer %>% 
  str_trim() %>% 
  str_extract_all("\\([^()]+\\)") %>% 
  str_remove("\\(") %>% 
  str_remove("\\)")

url_aperitif <- url_aperitif %>% 
  str_trim() %>% 
  str_remove("\\#") %>% 
  .[. != ""]

url_aperitif <- paste("https://www.aperitif.no", url_aperitif, sep = "")


# lager tibble -----------------------------------------------------------------------------

df <- tibble(
  varenummer,
  url_aperitif
)




## Scraper hver enkelt side fra utvalg --------------------------------------------------------------------

url_enkel <- df %>% 
  select(url_aperitif) %>% 
  top_n(2) %>% 
  unlist()

url_enkel

scraping <- function(info){
  lapply(url_enkel,
         function(url){
           url %>% read_html() %>% 
             html_nodes(info) %>% 
             html_text() %>%
             gsub('[\r\n\t]', '', .) 
         }) 
}



# Scraping fra vinmonopolets sider

# Disse funker ikke ennå. Fortsett her.

konklusjon   <- scraping(".conclusion") %>% 
                  .[[1]] %>% 
  unlist()

poeng        <- scraping(".product-top clearfix") %>% 
  str_subset(., ".rating-points")




# write_excel_csv2(df, "aperitif_pollisten_300kr.csv")
                  