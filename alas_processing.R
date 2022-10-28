####Al:As processing####

#Enter data
alas_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="ALAS DATA", skip=0, na = "NA") %>% 
  clean_names() 

alas_df <- alas_raw %>% 
  mutate(lma = )