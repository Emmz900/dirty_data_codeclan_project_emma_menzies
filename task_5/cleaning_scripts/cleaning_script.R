library(tidyverse)
library(assertr)

rwa_data_raw <- read_csv("raw_data/rwa.csv")

glimpse(rwa_data_raw)
# STEPS NEEDED
  # Overall RWA score is the mean of Q3-22 with 4, 6, 8, 9, 11, 13, 15, 18, 20, 21 reverse scored. 0-9
  # E1-22 is the average time for each question, this can likely be ignored and testelapse used for the average time
  # TIPI data is personality type and can be ignored
  # VCL is a validity check. Only keep data that have 0 for 6, 9, and 12
  # Keep: education, gender, age, hand, familysize


reverse_score <- function(dataframe){
  
  }


reverse_score(rwa_data_raw, Q4)


rwa_data_verified <- rwa_data_raw %>% 
  filter(VCL6 == 0 & VCL9 == 0 & VCL12 == 0)

rwa_data_long <- rwa_data_verified %>% 
  select(Q3:Q22, education,
         gender, age, hand, familysize, testelapse) %>%
  mutate(id = row_number(), .before = Q3) %>% 
  pivot_longer(Q3:Q22, names_to = "question", values_to = "answer") %>% 
  mutate(question = str_remove(question, "^Q")) %>% 
  mutate(answer = case_when(
    question %in% c(4, 6, 8, 9, 11, 13, 15, 18, 20, 21) ~ 9-answer,
    .default = answer
  ))

rwa_data_long %>% 
  group_by(id) %>% 
  mutate(rwa_score = mean(answer)) %>% 
  select(-question, -answer)

