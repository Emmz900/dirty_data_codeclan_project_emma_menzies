library(readxl)
library(tidyverse)

seabirds_ships <- read_xls("raw_data/seabirds.xls")
seabirds_birds <- read_xls("raw_data/seabirds.xls", sheet = 2)
seabirds_ship_key <- read_xls("raw_data/seabirds.xls", sheet = 3, skip = 1)
seabirds_bird_key <- read_xls("raw_data/seabirds.xls", sheet = 4, skip = 1)

seabirds_ships <- janitor::clean_names(seabirds_ships)
seabirds_birds <- janitor::clean_names(seabirds_birds)

# Ship data refinement
names(seabirds_ships)

seabirds_ship_key %>% 
  filter(!is.na(Label)) %>% 
  print(n=27)

seabirds_ships <- seabirds_ships %>% 
  select(record, record_id, date, lat, long)

# bird data refinement
names(seabirds_birds)

# check what each column means
seabirds_bird_key %>% 
  filter(!is.na(Label)) %>% 
  print(n=26)

# select and rename relevant data
seabirds_birds <- seabirds_birds %>% 
  select(record, record_id, species_common_name_taxon_age_sex_plumage_phase,
         species_scientific_name_taxon_age_sex_plumage_phase, species_abbreviation, 
         count) %>% 
  rename(species_common_name = 
           species_common_name_taxon_age_sex_plumage_phase,
          species_scientific_name = 
           species_scientific_name_taxon_age_sex_plumage_phase)

# simplify scientific name
seabirds_birds <- seabirds_birds %>% 
  mutate(species_scientific_name = str_extract(seabirds_birds$species_scientific_name, "[:alpha:]+ [:alpha:]*"))


# Join data
seabirds_joined <- full_join(seabirds_ships, seabirds_birds, by = "record_id") %>% 
  rename(ship_record = record.x, bird_record = record.y)

seabirds_joined %>% 
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x))))

# There is one record missing the common name and all other species information,
# this record is therefore not useful to us and can be removed.
# There are also records which state no birds were recorded, these will be removed.
  
seabirds_joined <- seabirds_joined %>% 
  # Remove missing species or no birds records
  filter(!is.na(species_scientific_name),
         species_common_name != "[NO BIRDS RECORDED]") %>% 
  
  # recode common name "Shy / white-capped / Salvin's....." to "White capped albatross" 
  mutate(species_common_name =
           case_when(str_detect(species_common_name, "Shy /")
                     ~ "White capped albatross",
                     .default = species_common_name)) %>% 
  # tidy names
  mutate(
    # remove the capital letters at the end of the common name for age
    species_common_name = 
      str_remove(species_common_name, "[:upper:]*[0-9]*$"),
    
    # remove the capital letters at the end of the abbreviation for age
    species_abbreviation = 
      str_extract(species_abbreviation, "^[:upper:]+"),
    
    # new column for just family
    family =
      str_extract(species_scientific_name, "^[:alpha:]+"),
    
    # new column for just genus
    genus = 
      str_extract(species_scientific_name, "[:alpha:]+$"),
    .after = species_scientific_name) %>% 
  
  relocate(family, .after = species_scientific_name)

  
# Write csv
write_csv(seabirds_joined, "clean_data/seabirds_clean.csv")
