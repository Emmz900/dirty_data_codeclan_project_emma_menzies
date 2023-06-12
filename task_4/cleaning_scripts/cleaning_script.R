library(tidyverse)
library(readxl)
library(janitor)

candy_2015 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

candy_2015 <- clean_names(candy_2015)
candy_2016 <- clean_names(candy_2016)
candy_2017 <- clean_names(candy_2017)

# TO DO
  #change timestamp to year


# Clean 2015
names(candy_2015)

candy_2015_clean <- candy_2015 %>% 
  select(timestamp:york_peppermint_patties, sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year, necco_wafers) %>% 
  rename("age" = how_old_are_you,
         "trick_or_treating" = are_you_going_actually_going_trick_or_treating_yourself) %>%
  mutate("gender" = NA, "country" = NA, "state_province" = NA) %>% 
  pivot_longer(butterfinger:necco_wafers, names_to = "candy_type", values_to = "rating") %>% 
  filter(!is.na(rating))

# Clean 2016
names(candy_2016)

candy_2016_clean <- candy_2016 %>% 
  select(timestamp:york_peppermint_patties) %>% 
  rename("age" = how_old_are_you,
         "trick_or_treating" = are_you_going_actually_going_trick_or_treating_yourself,
         "gender" = your_gender,
         "country" = which_country_do_you_live_in,
         "state_province" = which_state_province_county_do_you_live_in) %>% 
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy_type", values_to = "rating") %>% 
  filter(!is.na(rating))

# Clean 2017  

names(candy_2017)
head(candy_2017)

candy_2017_clean <- candy_2017 %>% 
  select(q1_going_out:q6_york_peppermint_patties) %>% 
  rename("age" = q3_age,
         "trick_or_treating" = q1_going_out,
         "gender" = q2_gender,
         "country" = q4_country,
         "state_province" = q5_state_province_county_etc) %>% 
  pivot_longer(q6_100_grand_bar:q6_york_peppermint_patties, names_to = "candy_type", values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  mutate(candy_type = str_remove(candy_type, "q[0-9]_"))

# Combine them
candy_full_data <- candy_2015_clean %>% 
  bind_rows(candy_2016_clean, candy_2017_clean)

candy_full_data %>% 
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x))))
