# Cleaning Script for Halloween Candy Data

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

# Step 1 - Clean to Join -------------------------------------------------------
# Clean each dataset to look like:
# id | year | gender | country | state_province | age | trick_or_treating | candy_type | rating

  ## Clean 2015 -------------------
names(candy_2015)

candy_2015_clean <- candy_2015 %>% 
  select(how_old_are_you:york_peppermint_patties, necco_wafers) %>% 
  mutate("id" = paste0("2015_", row_number()), "year" = 2015, "gender" = "Not gathered",
         "country" = NA) %>% 
  rename("age" = how_old_are_you,
         "trick_or_treating" = 
           are_you_going_actually_going_trick_or_treating_yourself) %>%
  pivot_longer(butterfinger:necco_wafers,
               names_to = "candy_type",
               values_to = "rating") %>% 
  filter(!is.na(rating)) %>% # if there is no rating for a particular candy then that row is not of interest
  # ensure columns are in right order
  select("id", "year", "gender", "country",
         "age", "trick_or_treating", "candy_type", "rating")

  ## Clean 2016 -----------------------
names(candy_2016)

candy_2016_clean <- candy_2016 %>% 
  select(are_you_going_actually_going_trick_or_treating_yourself:york_peppermint_patties, -which_state_province_county_do_you_live_in) %>% 
  mutate("id" = paste0("2016_", row_number()),"year" = 2016) %>% 
  rename("gender" = your_gender,
         "country" = which_country_do_you_live_in,
         "age" = how_old_are_you,
         "trick_or_treating" =
           are_you_going_actually_going_trick_or_treating_yourself
         ) %>% 
  pivot_longer(x100_grand_bar:york_peppermint_patties,
               names_to = "candy_type",
               values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  # ensure columns are in right order
  select("id", "year", "gender", "country",
         "age", "trick_or_treating", "candy_type", "rating")

  ## Clean 2017 --------------------------

names(candy_2017)
head(candy_2017)

candy_2017_clean <- candy_2017 %>% 
  select(q1_going_out:q6_york_peppermint_patties, -q5_state_province_county_etc) %>% 
  mutate("id" = paste0("2017_", row_number()),"year" = 2017) %>% 
  rename("gender" = q2_gender,
         "country" = q4_country,
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
  select("id", "year", "gender", "country",
         "age", "trick_or_treating", "candy_type", "rating")

# Step 2 - Join ------------------------------------

  ## Check the column headings match -------------------
expected_names <- c("id", "year", "gender", "country", "age", "trick_or_treating", "candy_type", "rating")

candy_2015_clean %>% 
verify(names(candy_2015_clean) == expected_names)
candy_2016_clean %>% 
  verify(names(candy_2016_clean) == expected_names)
candy_2017_clean %>% 
  verify(names(candy_2017_clean) == expected_names)

  ## Join the rows together from all datasets -------------------
candy_full_data <- bind_rows(candy_2015_clean,
                             candy_2016_clean,
                             candy_2017_clean)

# Step 3 - Clean `gender` ----------------------------------------
  # There are 6 categories:
  # Female, I'd rather not say, Male, Not gathered, Other, and NA
  # Change NAs to "Not gathered"
 candy_full_data_gender <- candy_full_data %>% 
   mutate(gender = coalesce(gender, "Not gathered"))

# Step 4 - Clean `country` -----------------------------------------------
# Key issues the section will fix:
  # Numeric entries
  # Varied cases and punctuation
  # Many variations of country names
  # Non-existent countries
  # Countries which we are not interested in for the analysis

  ## Numeric Entries ---------
  #(6 are numbers between 30-51, these seem likes ages in the wrong column,
  #one is asking for subscriptions, this should be removed)
candy_full_data_country_numeric <- candy_full_data_gender %>% 
  mutate(
    age = if_else(
      # if an age has been put in country, 
      # and no age given, move it back to age.
      str_detect(country, "[0-9]") &
        is.na(age) &
        country != "subscribe to dm4uz3 on youtube", 
      country,
      age
    ),
    country = if_else(
      str_detect(country, "[0-9]"),
      NA, # this should run when country has numeric values
      country
    ))

  ## Clean general format -----------
candy_full_data_country_format <- candy_full_data_country_numeric %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = str_remove_all(country, "[:punct:]"))

  ## Fix variations in countries ------------
america_pattern <- "^us|u[a-z]* s|usa|[eu]+r[i]*ca|murrika|yoo ess|pitts|carolina|california|jersey|trump|york"

uk_pattern <- "^uk|^u[a-z]* k[a-z]*|en[gd]land|scotland"

nonsense_pattern <- "one|where|never|gods|tropical|above|not|know|fear|denial|earth|insanity|atlantis|narnia|^a$|^can[ae]*$|^eua$" 
  # These people could be from US/UK/Canada so will go in NA rather than "other"
  # Cascadia covers US and Canada, although it is not a recognised country it is an established region hence will go into "other" rather than NA/America/Canada

  # The analysis is interested in Canada, America, UK, and other. 
  # Therefore any other "real" country should be "other"
candy_full_data_country_clean <- candy_full_data_country_format %>% 
  mutate(country = case_when(
    
    # Find all USA type names
    str_detect(country, america_pattern) ~ "america",
    
    # canada
    str_detect(country, "canada") ~ "canada",
    
    # Find all UK names
    str_detect(country, uk_pattern) ~ "uk",
    
    # Remove nonsensical names
    str_detect(country, nonsense_pattern) ~ NA,
    
    # All other countries in other category
    .default = "other"
  ))

# Step 5 - Clean `age` ---------------------------------------------

  ## remove non-numeric 
candy_full_data_age_clean <- candy_full_data_country_clean %>% 
  mutate(age = str_extract(age, "[0-9]+")) %>% 
  mutate(age = as.numeric(age)) %>% # this step removes character strings as NA
  # oldest living person is 116. People under 3 cannot complete a survey or rate sweets.
  mutate(age = case_when(
    age > 116 ~ NA,
    age <= 3 ~ NA,
    .default = age
  ))

# Step 6 - Clean `candy_type` ---------------------------------------------

  ## Remove all non-candy rows ------------

non_candy_pattern <-
  "abstain|game|comics|dental|hugs|vial|cash|glow_stick|chalk|bread|wheat|season|acetaminophen|vicodin|chardonnay|lapel"

candy_full_data_candy_clean <- candy_full_data_age_clean %>% 
  mutate(candy_type = case_when(
    str_detect(candy_type, non_candy_pattern) ~ NA,
    .default = candy_type)) %>% 
  filter(!is.na(candy_type))

  ## Recoding -------------------

  # Each pattern will be searched for by str_detect
  # and matching values will be replaced with the corresponding new_value 

recode_values <- list(
  pattern = list("anonymous_brown_globs", "100_grand_bar", "raisin",
                 "chick_o_sticks", "sourpatch_kids", "sweetums", "restaurant",
                 "gummy_bear", "fruit", "tolberone", "boo_berry"),
  new_value = list("mary_janes","100_grand_bar","raisins","chick_o_sticks",
                   "sourpatch_kids","sweetums", "restaurant_candy",
                   "gummy_bears", "fruit", "toblerone", "boo_berry_cereal")
)

candy_full_data_candy_tidy <- candy_full_data_candy_clean %>% 
  mutate(
    for (i in 1:length(recode_values[[1]])){
      candy_type =
        case_when(
          str_detect(candy_type, recode_values[[1]][[i]]) 
          ~ recode_values[[2]][[i]],
          .default = candy_type
        )
    }    
  )

  # check for pattern in `pattern` 
  # replace with the value at the corresponding index in `new_value`
for (i in 1:length(recode_values[[1]])){
  candy_full_data_candy_tidy <- #this seems like an expensive step, is there another way?
    mutate(candy_full_data_candy_clean,
           candy_type = case_when(
             str_detect(candy_type, recode_values[[1]][[i]]) 
             ~ recode_values[[2]][[i]],
             .default = candy_type
           ))
}


# Step 7 - Clean `rating` and add `rating_score` column ----------------
candy_full_data_clean <- candy_full_data_candy_clean %>% 
  mutate(rating = str_to_lower(rating),
         rating_score = case_when(
           rating == "despair" ~ -1,
           rating == "meh" ~ 0,
           rating == "joy" ~ 1
         ))

# Final step - Write Clean Data --------------------------------------------
write_csv(candy_full_data_clean, "clean_data/candy_data_clean.csv")
