library("tidyverse")
# library("plotly")


# Load data --------------------------------------------------------------------


aperitif <-  read_csv2("data/20191201_aperitif_poeng_og_konklusjon.csv") %>% 
  rename_all(. %>% tolower())

produkter <-  read_csv2("https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv", locale = locale(encoding = "latin1")) %>% 
  rename_all(. %>% tolower())

order_to_store <- read_csv2("data/20191201_order_to_store_vin.csv",  locale = locale(encoding = "UTF-8"))

df <- order_to_store %>% 
  left_join(aperitif, by = NULL) %>% 
  left_join(produkter, by = c("varenummer" = "varenummer")) %>% 
  distinct() %>% 
  mutate(argang = as.numeric(argang))




# Filter ---------------------------------------------------------------



df %>% 
  select(varenummer, poeng, konklusjon, argang, argang_apreritif, order_to_store, varenavn, pris, volum, varetype, smak, rastoff) %>% 
  filter(pris < 200, varetype == "Rødvin", volum == 0.75, order_to_store == "Kan bestilles til alle butikker", near(argang, argang_apreritif)) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()

df %>% 
  select(varenummer, poeng, konklusjon, argang, argang_apreritif, order_to_store, varenavn, pris, volum, varetype, smak, rastoff) %>% 
  filter(pris < 200, str_detect(smak, "fat"), varetype == "Hvitvin", str_detect(rastoff, "Chardonnay"), volum == 0.75, order_to_store == "Kan bestilles til alle butikker", near(argang, argang_apreritif)) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()

df %>% 
  select(varenummer, poeng, konklusjon, argang, argang_apreritif, order_to_store, varenavn, pris, volum, varetype, smak, rastoff) %>% 
  filter(pris < 500, varetype == "Rødvin", volum == 0.75, order_to_store == "Kan bestilles til alle butikker", near(argang, argang_apreritif), str_detect(varenavn, "Brunello")) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()

df %>% 
  select(varenummer, poeng, konklusjon, argang, argang_apreritif, order_to_store, varenavn, pris, volum, varetype, smak, rastoff) %>% 
  filter(pris < 500, varetype == "Portvin", order_to_store == "Kan bestilles til alle butikker", near(argang, argang_apreritif)) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()

df %>% 
  select(varenummer, poeng, konklusjon, argang, argang_apreritif, order_to_store, varenavn, pris, volum, varetype, smak, rastoff) %>% 
  filter(pris < 300, order_to_store == "Kan bestilles til alle butikker", near(argang, argang_apreritif), str_detect(varenavn, "Sider")) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()

df %>% 
  select(varenummer, poeng, konklusjon, argang, order_to_store, varenavn, pris, land, varetype, sodme, lagringsgrad, volum) %>% 
  filter(lagringsgrad == "Kan drikkes nå, blir bedre ved lagring", pris < 300, varetype == "Hvitvin", volum == 0.75) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()


df %>% 
  select(varenummer, poeng, konklusjon, argang, order_to_store, varenavn, pris, land, varetype, sodme, lagringsgrad, volum) %>% 
  filter(lagringsgrad == "Kan drikkes nå, blir bedre ved lagring", pris < 300, varetype == "Rødvin", volum == 0.75) %>% 
  arrange(desc(poeng)) %>%
  distinct() %>% 
  view()



df %>% 
  count(varetype, sort = TRUE) %>% 
  top_n(20)

df %>% 
  filter(varetype == "Rødvin", land == "Frankrike") %>% 
  count(distrikt, sort = TRUE) %>% 
  top_n(10)


df %>% 
  filter(varetype == "Rødvin", volum == 0.75) %>% 
  ggplot(aes(pris)) +
  geom_histogram(stat = "bin", binwidth = 50) +
  coord_cartesian(xlim = c(0, 1000))  


