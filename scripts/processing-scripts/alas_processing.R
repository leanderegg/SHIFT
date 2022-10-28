####Al:As processing####

#Date: 218 WP + LWC
wpwc218 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="218-221 WP + LWC", skip=5, na = "NA") %>% clean_names() %>% 
  mutate(date = mdy("02-18-2022")) 