library(tidyverse)
library(readxl)
library(janitor)
library(assertr)

candy_2015 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

candy_2015 <- clean_names(candy_2015)
candy_2016 <- clean_names(candy_2016)
candy_2017 <- clean_names(candy_2017)

# Step 1 - Clean to join -------------------------------------------------------
# Clean each dataset to look like:
# year | gender | country | state_province | age | trick_or_treating | candy_type | rating

# Clean 2015
names(candy_2015)

candy_2015_clean <- candy_2015 %>% 
  select(how_old_are_you:york_peppermint_patties, necco_wafers) %>% 
  mutate("year" = 2015, "gender" = NA, "country" = NA, "state_province" = NA) %>% 
  rename("age" = how_old_are_you,
         "trick_or_treating" = are_you_going_actually_going_trick_or_treating_yourself) %>%
  relocate(age, .after = state_province) %>% 
  relocate(trick_or_treating, .after = age) %>% 
  pivot_longer(butterfinger:necco_wafers,
               names_to = "candy_type",
               values_to = "rating") %>% 
  filter(!is.na(rating))

# Clean 2016
names(candy_2016)

candy_2016_clean <- candy_2016 %>% 
  select(are_you_going_actually_going_trick_or_treating_yourself:york_peppermint_patties) %>% 
  mutate("year" = 2016) %>% 
  rename("gender" = your_gender,
         "country" = which_country_do_you_live_in,
         "state_province" = which_state_province_county_do_you_live_in,
         "age" = how_old_are_you,
         "trick_or_treating" = are_you_going_actually_going_trick_or_treating_yourself
         ) %>% 
  pivot_longer(x100_grand_bar:york_peppermint_patties,
               names_to = "candy_type",
               values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  # ensure columns are in right order
  select("year", "gender", "country", "state_province",
         "age", "trick_or_treating", "candy_type", "rating")

# Clean 2017  

names(candy_2017)
head(candy_2017)

candy_2017_clean <- candy_2017 %>% 
  select(q1_going_out:q6_york_peppermint_patties) %>% 
  mutate("year" = 2017) %>% 
  rename("gender" = q2_gender,
         "country" = q4_country,
         "state_province" = q5_state_province_county_etc,
         "age" = q3_age,
         "trick_or_treating" = q1_going_out
         ) %>% 
  pivot_longer(q6_100_grand_bar:q6_york_peppermint_patties,
               names_to = "candy_type",
               values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  # candy types contain question numbers at the start, these are removed
  mutate(candy_type = str_remove(candy_type, "q[0-9]_")) %>% 
  # ensure columns are in right order
  select("year", "gender", "country", "state_province",
         "age", "trick_or_treating", "candy_type", "rating")

# Step 2 - Join ------------------------------------

# Check the column headings match
expected_names <- c("year", "gender", "country", "state_province", "age", "trick_or_treating", "candy_type", "rating")

candy_2015_clean %>% 
verify(names(candy_2015_clean) == expected_names)
candy_2016_clean %>% 
  verify(names(candy_2016_clean) == expected_names)
candy_2017_clean %>% 
  verify(names(candy_2017_clean) == expected_names)

#check contents and types
glimpse(candy_2015_clean)
glimpse(candy_2016_clean)
glimpse(candy_2017_clean)

# join the rows together from all datasets
candy_full_data <- candy_2015_clean %>% 
  bind_rows(candy_2016_clean, candy_2017_clean)

# Step 3 - Check ----------------------------------------
# check the join has worked
candy_full_data %>% 
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x))))
glimpse(candy_full_data)
tail(candy_full_data)

# check year
candy_full_data %>% 
  group_by(year) %>% 
  summarise(total = n())

# check gender
candy_full_data %>% 
  group_by(gender) %>% 
  summarise(total = n())

# check trick or treating
candy_full_data_clean %>% 
  group_by(trick_or_treating) %>% 
  summarise(total = n())

# Step 4 - Clean Country -----------------------------------------------
candy_full_data %>% 
  group_by(country) %>% 
  summarise(total = n()) %>% 
  print(n = 20)

  # numeric entries:
  #(6 are numbers between 30-51, these seem likes ages in the wrong column,
  #one is asking for subscriptions, this should be removed)
candy_full_data_clean <- candy_full_data %>% 
  mutate(
    age = if_else(
      # an age has been put in country, and no age given, move it back to age
      str_detect(country, "[0-9]") & is.na(age) & country != "subscribe to dm4uz3 on youtube", 
      country,
      age
    ),
    country = if_else(
      str_detect(country, "[0-9]"),
      NA, # this should run when country is numeric
      country
    ))

  # Clean 
candy_full_data_clean <- candy_full_data_clean %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = str_remove_all(country, "[:punct:]"))
  
candy_full_data_clean <- candy_full_data_clean %>% 
  mutate(country = 
           case_when(
    # Find all USA type names
    str_detect(country,
               "^us|erica|urica|states|amerca|yoo ess|united s|u s|murrika")
    ~ "america",
    str_detect(country,
               "pitts|carolina|california|jersey|trump|york")
    ~ "america",
    
    # canada
    str_detect(country, "canada") ~ "canada",
    
    # Find all UK names
    str_detect(country, "united kin|england|scotland|endland") ~ "uk",
    
    #Remove nonsensical names
    str_detect(country,
               "one|where|never|gods|^eua|tropical|above|not|know|fear") 
    ~ NA,
    str_detect(country,
               "denial|earth|insanity|atlantis|narnia") 
    ~ NA,
    country %in% c("a", "can", "canae") ~ NA,
    
    # spain
    str_detect(country, "espa") ~ "spain",
    # cascadia 
    #(I am accepting this as a valid option although it is not technically a country at the moment)
    str_detect(country, "cascadia") ~ "cascadia",
    # netherlands
    str_detect(country, "netherlands") ~ "netherlands",
    
    .default = country
  )) 

# Step 5 - clean age ---------------------------------------------
candy_full_data_clean %>% 
  group_by(age) %>% 
  summarise(total = n()) %>% 
  print(n = 20)
# remove non-numeric (eg. "MY NAME JEFF")

candy_full_data_clean <- candy_full_data_clean %>% 
  mutate(age = str_extract(age, "[0-9]+")) %>% 
  mutate(age = as.numeric(age)) %>% # this step removes character strings as NA
  # oldest living person is 116. People under 3 cannot complete a survey or rate sweets.
  mutate(age = case_when(
    age > 116 ~ NA,
    age <= 3 ~ NA,
    .default = age
  ))

# Step 6 - Clean candy_type ---------------------------------------------
candy_full_data_clean %>% 
  group_by(candy_type) %>% 
  summarise(total = n()) %>% 
  print(n = 20)

candy_full_data_clean <- candy_full_data_clean %>%
  mutate(candy_type = case_when(
    str_detect(candy_type, "anonymous_brown_globs") ~ "mary_janes",
    str_detect(candy_type,
              "abstain|game|comics|dental|hugs|broken|vials|cash|glow_stick")
    ~ NA,
    str_detect(candy_type,
               "chalk|bread|wheat|acetaminophen")
    ~ NA,
    .default = candy_type
  )) %>% 
  filter(!is.na(candy_type))
  # distinct(candy_type) %>% 
  # print(n = 30)

# clean rating
candy_full_data_clean %>% 
  group_by(rating) %>% 
  summarise(total = n())

candy_full_data_clean <- candy_full_data_clean %>% 
  mutate(rating = str_to_lower(rating),
         rating_score = case_when(
           rating == "despair" ~ -1,
           rating == "meh" ~ 0,
           rating == "joy" ~ 1
         ))

# Final step - write clean data --------------------------------------------
write_csv(candy_full_data_clean, "clean_data/candy_data_clean.csv")
