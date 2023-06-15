library(tidyverse)

rwa_data_raw <- read_csv("raw_data/rwa.csv")

glimpse(rwa_data_raw)
# STEPS NEEDED
  # Overall RWA score is the mean of Q3-22 with 4, 6, 8, 9, 11, 13, 15, 18, 20, 21 reverse scored. 0-9
  # E1-22 is the average time for each question, this can likely be ignored and testelapse used for the average time
  # TIPI data is personality type and can be ignored
  # VCL is a validity check. Only keep data that have 0 for 6, 9, and 12
  # Keep: education, gender, age, hand, familysize


reverse_score <- function(dataframe, column){
  dataframe %>% 
    mutate({{column}} = 9 - {{column}})
}

rwa_data_raw %>% 
  select(Q3:Q22, testelapse, VCL6:VCL12, education,
         gender, age, hand, familysize) %>% 
  mutate(id = row_number(), .before = Q1,
         raw_score = )
