library("tidyverse")
# library("plotly")


# Load data --------------------------------------------------------------------


aperitif <-  read_csv2("data/aperitif_pollisten.csv", locale = locale(encoding = "latin1")) %>% 
  rename_all(. %>% tolower())

produkter <-  read_csv2("https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv", locale = locale(encoding = "latin1")) %>% 
  rename_all(. %>% tolower())

order_to_store <- read_csv2("data/order_to_store_vin.csv",  locale = locale(encoding = "UTF-8"))

df <- order_to_store %>% 
  left_join(aperitif, by = NULL) %>% 
  left_join(produkter, by = c("varenummer" = "varenummer")) %>% 
  distinct()




# Filter ---------------------------------------------------------------



df %>% 
  select(varenummer, varenavn, pris, volum, smak, friskhet, fylde, sodme, land, distrikt, rastoff, varetype, poeng, lagringsgrad, produktutvalg, order_to_store) %>% 
  filter(pris < 200, grepl("fat", smak), varetype == "Hvitvin", grepl("Chardonnay", rastoff)) %>% 
  arrange(pris) %>%
  distinct() %>% 
  view()

df %>% 
  select(varenummer, varenavn, pris, poeng, order_to_store, volum, land, rastoff, varetype, smak) %>% 
  filter(pris < 200, grepl("fat", smak), varetype == "Hvitvin", grepl("Chardonnay", rastoff)) %>% 
  arrange(pris) %>%
  distinct() %>% 
  view()



df %>% 
  select(varenummer, varenavn, argang, land, distrikt, pris, poeng, varetype, sodme, lagringsgrad) %>% 
  filter(lagringsgrad == "Kan drikkes nå, blir bedre ved lagring", pris < 300, varetype == "Hvitvin") %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()



df %>% 
  select(varenummer, varenavn, argang, rastoff, land, distrikt, pris, poeng, varetype, lagringsgrad, order_to_store) %>% 
  filter(pris < 170, varetype == "Rødvin") %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()


df %>% 
  count(varetype, sort = TRUE) %>% 
  top_n(10)

df %>% 
  group_by(varetype) %>% 
  summarise(antall = n()) %>% 
  arrange(desc(antall)) %>% 
  top_n(5)

df %>% 
  filter(varetype == "Rødvin", land == "Frankrike") %>% 
  group_by(distrikt) %>% 
  summarise(antall = n()) %>% 
  arrange(desc(antall)) %>% 
  top_n(5)

df %>% 
  filter(poeng > 88, 
         varetype == "Hvitvin") %>% 
  ggplot(aes(pris)) +
  geom_histogram(stat = "bin", binwidth = 25)
  


