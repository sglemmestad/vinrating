library("xml2")
library("rvest")
library("tidyverse")



# URLs and scraping function ----------------------------------------------


url_loop_aperitif <- paste0("https://www.aperitif.no/pollisten/vin?minPrice=&maxPrice=300&q=&sortBy=editorial_rating&offset=",30 * 0:2 )


scraping <- function(info){
  lapply(url_loop_aperitif,
         function(url){
           url %>% read_html() %>% 
             html_nodes(info) %>% 
             html_text() %>%
             gsub('[\r\n\t]', '', .) 
         }) %>% 
    unlist()
}



# Scraping from Vinmonopolet ----------------------------------------------


varenummer   <- scraping(".product-details")
poeng        <- scraping(".item-rating-percentage")
navn         <- scraping("h4")



# Cleaning the data -------------------------------------------------------


navn    <- navn %>% 
           str_remove("Pris") %>% 
           str_remove("Finn drikke til maten") %>% 
           str_remove("\\([^()]+\\)") %>% 
           str_trim() %>% 
           .[. != ""]

poeng   <- poeng %>% 
           str_remove("Poeng") %>% 
           str_trim()
           
varenummer <- varenummer %>% 
  str_trim() %>% 
  str_extract_all("\\([^()]+\\)") %>% 
  str_remove("\\(") %>% 
  str_remove("\\)")



# Creating tibble and writing csv -----------------------------------------


df <- tibble(
  varenummer,
  navn,
  poeng
)


current_date <- Sys.Date() %>% 
  str_remove_all("-")


#write_excel_csv2(df, "data/aperitif_poeng.csv")
write_excel_csv2(df, paste("data/", current_date,"_aperitif_poeng.csv", sep="")) 

