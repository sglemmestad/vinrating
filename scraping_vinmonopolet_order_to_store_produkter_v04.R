library(xml2)
library(rvest)
library(robotstxt)
library(timeR)
library(tidyverse)




# robotstxt ---------------------------------------------------------------------------------

paths_allowed(
  paths = c("https://www.vinmonopolet.no/p/723301")
)


# Oppretter timer ---------------------------------------------------------------------------

timer <- createTimer()


# Henter produktfil fra Vinmonopolet --------------------------------------------------------

timer$start("produktfil")

produkter <-  read_csv2("https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv", locale = locale(encoding = "latin1")) %>% 
  rename_all(. %>% tolower())

timer$stop("produktfil")

# Definerer urler ---------------------------------------------------------------------------


varenummer <- produkter %>% 
  filter(grepl("vin", varetype)) %>% 
  pull(varenummer)


url_loop <- paste0("https://www.vinmonopolet.no/p/", varenummer)


# Scraper info -----------------------------------------------------------------------------


timer$start("scraper_order_to_store")

order_to_store <- lapply(url_loop,
              function(url){
                url %>% read_html() %>% 
                  html_nodes(".product-ordertostore") %>% 
                  html_text() %>%
                  gsub('[\r\n\t]', '', .) 
              }) %>% 
  unlist()

timer$stop("scraper_order_to_store")

# Lager tibble og lagrer csv ----------------------------------------------------------------------------

timer$start("tibble_csv")

df <- tibble(
  varenummer,
  order_to_store
)

current_date <- Sys.Date() %>% 
  str_remove_all("-")


write_excel_csv2(df, paste("data/", current_date,"_order_to_store_vin.csv", sep="")) 


timer$stop("tibble_csv")


# Lager timer tibble -----------------------------------------------------------------------------------------


table1 <- getTimer(timer)
timer$toggleVerbose() # set verbose to FALSE as default is TRUE

table1 # print all records in a tibble(data frame)


