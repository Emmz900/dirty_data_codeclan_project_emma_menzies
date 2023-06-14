library(tidyverse)

dog_data <- read_csv("raw_data/dog_survey.csv")

names(dog_data)

dog_data %>% 
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x))))

# Remove Duplicates
dog_data_unique <- dog_data %>% 
  select(-...10, -...11) %>% 
  filter(!duplicated(dog_data))


#dog_data_individuals <- 
  dog_data_unique %>% 
# Remove amount spent on dog food as this will not be applicable to the dogs on their own  
  mutate(amount_spent_on_dog_food = case_when(
    id == 174 ~ NA,
    .default = amount_spent_on_dog_food)) %>% 
# Seperate id 174 into 3 rows.   
  separate_longer_delim(c("dog_size", "dog_gender", "dog_age"), ",") %>% 
  
  mutate(dog_size = str_to_upper(dog_size),
         dog_gender = str_to_upper(dog_gender)) %>% 
  # Clean dog_size
  mutate(dog_size = case_when(
    str_detect(dog_size, "^S") ~ "S",
    str_detect(dog_size, "^M") ~ "M",
    str_detect(dog_size, "^L") ~ "L",
    dog_size %in% c("NO", "N/A", "-") ~ NA,
    .default = dog_size
  ))

  dog_data_unique %>% 
    distinct(dog_gender)

  