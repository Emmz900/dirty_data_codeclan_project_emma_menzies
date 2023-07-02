library(readxl)
library(tidyverse)

seabirds_ships <- read_xls("raw_data/seabirds.xls")
seabirds_birds <- read_xls("raw_data/seabirds.xls", sheet = 2)
seabirds_ship_key <- read_xls("raw_data/seabirds.xls", sheet = 3, skip = 1)
seabirds_bird_key <- read_xls("raw_data/seabirds.xls", sheet = 4, skip = 1)

seabirds_ships <- janitor::clean_names(seabirds_ships)
seabirds_birds <- janitor::clean_names(seabirds_birds)

# Ship data refinement ------------
names(seabirds_ships)

  # Check what each column means
seabirds_ship_key %>% 
  filter(!is.na(Label)) %>% 
  print(n=27)
  
  ## Drop irrelevant data -------------
seabirds_ships_reduced <- seabirds_ships %>% 
  select(record_id, date, lat, long) %>% 
  mutate(year = format(date, "%Y"), .after = date)

# Bird data refinement ------------------
names(seabirds_birds)

  # Check what each column means
seabirds_bird_key %>% 
  filter(!is.na(Label)) %>% 
  print(n=26)

  ## Select and rename relevant data -------------------
seabirds_birds_renamed <- seabirds_birds %>% 
  select(record_id, species_common_name_taxon_age_sex_plumage_phase,
         species_scientific_name_taxon_age_sex_plumage_phase, species_abbreviation, 
         count) %>% 
  rename(species_common_name = 
           species_common_name_taxon_age_sex_plumage_phase,
          species_scientific_name = 
           species_scientific_name_taxon_age_sex_plumage_phase)

  ## Simplify names -------------
seabirds_birds_clean <- seabirds_birds_renamed %>% 
  # recode common name "Shy / white-capped / Salvin's....." to "White capped albatross" 
  # [https://en.wikipedia.org/wiki/White-capped_albatross]
  mutate(species_common_name =
           case_when(str_detect(species_common_name, "Shy /")
                     ~ "White capped albatross",
                     .default = species_common_name)) %>% 
  mutate(
    # remove the capital letters at the end of the name columns
    species_scientific_name =
      str_remove(species_scientific_name, "[:upper:]+$"),
    species_abbreviation = 
           str_extract(species_abbreviation, "^[:upper:]+"),
  species_common_name =
           str_remove(species_common_name, "[:upper:]*$"), 
    
    # new column for just family
    family =
      str_extract(species_scientific_name, "^[:alpha:]+"),
    # new column for just genus
    genus = 
      str_extract(species_scientific_name, "[:alpha:]+$"),
    .after = species_scientific_name) %>% 
  # remove "sensu lato" from common names
  mutate(species_common_name = 
           str_remove(species_common_name, "[0-9]*$")) %>% 
  mutate(species_common_name =
           str_remove(species_common_name, "sensu lato")) %>% # This just means "in a general sense". 
            # [https://www.merriam-webster.com/dictionary/sensu%20lato]
  relocate(family, .after = species_scientific_name)
  
  
# Join data ------------------------
seabirds_joined <-
  full_join(seabirds_ships_reduced, seabirds_birds_clean, by = "record_id")

seabirds_joined %>% 
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x))))

# There is one record missing the common name and all other species information,
# this record is therefore not useful to us and can be removed.
# There are also records which state no birds were recorded, these will be removed.

# Clean joined data   ---------------------- 
seabirds_joined_clean <- seabirds_joined %>% 
  # Remove missing species or no birds records
  filter(
    #!is.na(species_scientific_name),
    species_common_name != "[NO BIRDS RECORDED]")

# Write csv --------------
write_csv(seabirds_joined_clean, "clean_data/seabirds_clean.csv")
