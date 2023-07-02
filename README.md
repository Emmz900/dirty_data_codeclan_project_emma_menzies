# Dirty Data Project - Codeclan
Emma Menzies

This project was completed in week 4 of my Professional Data Analysis course at Codeclan.
It consists of several different tasks requiring data cleaning and wrangling. 

## Tasks

* [Task 3](#3) - Sea bird observation Data
* [Task 4](#4) - **Halloween Candy Data**
* [Task 5](#5) - Right Wing Authoritarianism
* [Task 6](#6) - Dog Owners survey

## Skills Showcased

* Data Cleaning
* Data Wrangling with DPLYR
* Joins
* Functions and `For` Loops
* Basic Visualisations

## Folder Structure
Each task has 4 folders:

* raw_data
* cleaning_scripts
* clean_data
* analysis_scripts

There are further details of each project in the notebook/html within the analysis scripts folder.

### Task 3 - Sea bird observation data 
**Data**  

This project cleans, joins, and investigates data from ships in the southern hemisphere on seabird sightings.

The cleaning script:

* Selects the relevant columns and renames them where appropriate
* Joins the data from the ships to the data on the birds
* Creates columns for year, family, and genus
* Removes unnecessary abbreviations from the bird names
* Removes entries where no birds were recorded

**Data summary:**   

![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_3/analysis_and_documentation/total_sighting.png)

![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_3/analysis_and_documentation/most_common_species.png)

### Task 4 - Halloween Candy data 

**Data**  

This project joins survey data regarding Halloween Candy from three different years and cleans it.  
The data is then analysed. A thorough description of the cleaning process is given within the analysis.  

The project also creates a function to more easily answer questions about the top rated Candy Types per group:

```
most_popular_by_category <- function(dataset, category){
  dataset %>% 
        group_by({{category}}, candy_type) %>% 
        filter(candy_type != "any_full_sized_candy_bar") %>% 
        summarise(average_rating = sum(rating_score), number_of_ratings = n()) %>%
        ungroup() %>% 
        group_by({{category}}) %>% 
        slice_max(average_rating)
}
```

**Data Summary**   

Survey Results:
*Note: Country data was not collected in 2015.*
![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_4/analysis_and_documentation/candy_ratings_by_country_plot.png)


Illustration of results:
![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_4/analysis_and_documentation/survey_results_received.png)

### (Task 5 - Right Wing Authoritarianism)
*Not Completed*

### Task 6 - Dog Owners Survey 

This task cleans and analyses survey responses from dog owners regarding the size, gender, and age of their dog, as well as the average amount spent on dog food monthly.

Responses:
![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_6/analysis_script/responses_recieved.png)

Analysis:
![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_6/analysis_script/dog_ages.png) 

![](https://github.com/Emmz900/dirty_data_codeclan_project_emma_menzies/blob/main/task_6/analysis_script/spend_vs_size.png)
 
