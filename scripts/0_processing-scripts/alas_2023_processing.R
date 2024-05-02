datver <- "20231128" #date downloaded
dataversion <- paste0("Data_", datver)

morph_23_df <- read_csv(here(paste0("Data_",datver), "alas_2023.csv"),show_col_types = FALSE) %>% 
  mutate(area_cm2 = as.numeric(area_from_scan),
         leaf_dry_mass_g = case_when(
           leaf_dry_mass_g > 7.5 ~ NA, #these are weird dry mass values, remove
           leaf_dry_mass_g > 2 & area_cm2 < 50 ~ NA, 
           leaf_dry_mass_g > .38 & area_cm2 < 5 ~ NA, 
           TRUE ~ as.numeric(leaf_dry_mass_g)))  %>% 
  mutate(date = case_when(
    date %in% c("2023-07-10") ~ "2023-05-12", 
    date %in% c("2023-10-07") ~ "2023-09-29", 
    TRUE ~ as.character(date)
  ))%>% 
  mutate(diameter_mm = case_when(
    diameter_mm > 7.8 ~ NA, 
    diameter_mm < 0.3 ~ diameter_mm * 10, #weird value for this Al:As
    TRUE ~ as.numeric(diameter_mm))) %>% 
  mutate(diameter_cm = diameter_mm/100,
         area_mm2 = area_cm2*100,
         lma_g_cm2 = leaf_dry_mass_g/area_cm2,
         radius_mm = diameter_mm/2, #turn diameter (mm) into radius
         radius_cm = diameter_cm/2, #do same, but with cm.
         area_stem_mm2 = (pi*((radius_mm)^2)), #calculate area of stem in mm2.
         sla_cm_g = area_cm2/leaf_dry_mass_g,
         alas_cm2_per_mm2 = (area_cm2/area_stem_mm2), 
         tree = tree_id
  ) %>% 
  select(tree, date, year, branch_number, length_mm, leaf_number, lma_g_cm2, 
         sla_cm_g, alas_cm2_per_mm2) 
    
  
morph_23_df %>% 
  ggplot(aes(y = lma_g_cm2, 
             x = tree)) +
  geom_point()

morph_23_df %>% 
  ggplot(aes(y = lma_g_cm2, 
             x = alas_cm2_per_mm2)) +
  geom_point()


#add in tree info:
trees_sites_df <- read_csv(here("processed-data", "trees_sites.csv"),show_col_types = FALSE)


morph_23_trees_df <- merge(trees_sites_df,morph_23_df, by = c("tree"))

write_csv(morph_23_trees_df, here::here("processed-data", "2023_alas.csv"))
