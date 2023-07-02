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


## Folder Structure
Each task has 4 folders:

* raw_data
* cleaning_scripts
* clean_data
* analysis_scripts

There are further details of each project in the notebook/html within the analysis scripts folder.

### Task 3 - Sea bird observation data {#3}
**Data**

This project cleans, joins, and investigates data from ships in the southern hemisphere on seabird sightings.

The cleaning script:

* Selects the relevant columns and renames them where appropriate
* Joins the data from the ships to the data on the birds
* Creates columns for year, family, and genus
* Removes unnecessary abbreviations from the bird names
* Removes entries where no birds were recorded

**Data summary:**

![](task_3\analysis_and_documentation\total_sighting.png)

![](task_3\analysis_and_documentation\most_common_species.png)


### Task 4 - Halloween Candy data {#4}

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
![](task_4\analysis_and_documentation\survey_results_received.png)
*Note: Country data was not collected in 2015.*

Illustration of results:
![](task_4\analysis_and_documentation\candy_ratings_by_country_plot.png) 

### Task 5 - Right Wing Authoritarianism {#5}


### Task 6 - Dog Owners Survey {#6}

![](task_6\analysis_script\dog_ages.png) 
 
 
