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

# Oppretter timer ---------------------------------------------------------------------------

timer <- createTimer()


# Setter opp url ---------------------------------------------------------------------------

timer$start("url_loop_aperitif")

url_loop_aperitif <- paste0("https://www.aperitif.no/pollisten/vin?minPrice=&maxPrice=400&q=&sortBy=editorial_rating&offset=",30 * 0:200 )

timer$stop("url_loop_aperitif")

# Scraping fra aperitif ---------------------------------------------------------------------

timer$start("scraping_url_aperitif")

url_aperitif <- lapply(url_loop_aperitif,
              function(url){
                url %>% read_html() %>% 
                html_nodes(".item-list a") %>% 
                html_attr('href')
              }) %>% 
  unlist()

timer$stop("scraping_url_aperitif")

# Rensker dataene ---------------------------------------------------------------------------

timer$start("url_aperitif")

url_aperitif <- url_aperitif %>% 
  str_trim() %>% 
  str_remove("\\#") %>% 
  .[. != ""]

url_aperitif <- paste("https://www.aperitif.no", url_aperitif, sep = "")

timer$stop("url_aperitif")

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

timer$start("varenummer")

# Scraping fra vinmonopolets sider

# Disse funker ikke ennå. Fortsett her.

varenummer   <- scraping(".act span span")

timer$stop("varenummer")

timer$start("poeng")

poeng        <- scraping(".rating-points")

timer$stop("poeng")

timer$start("konklusjon")

konklusjon   <- scraping(".conclusion")

timer$stop("konklusjon")

timer$start("date")

date         <- scraping(".date") %>% 
  str_trim()

timer$stop("date")

# Creating tibble and writing csv -----------------------------------------

timer$start("df")

df <- tibble(
  varenummer,
  poeng,
  konklusjon,
  date,
  url_aperitif
)


df <- df %>% 
  separate(date, c("argang_apreritif", "smakt"), sep = "Smakt")

df <- df %>% 
  mutate(argang_apreritif = as.numeric(str_sub(argang_apreritif, 1, 4)))


current_date <- Sys.Date() %>% 
  str_remove_all("-")


write_excel_csv2(df, paste("data/", current_date,"_aperitif_poeng_og_konklusjon.csv", sep="")) 

timer$stop("df")  

# Lager timer tibble -----------------------------------------------------------------------------------------


table1 <- getTimer(timer)
timer$toggleVerbose() # set verbose to FALSE as default is TRUE

table1 # print all records in a tibble(data frame)