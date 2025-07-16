#View data
# install.packages('here')
# install.packages('tidyverse')
rm(list=ls())


# Setup -------------------------------------------------------------------

pacman::p_load(here, 
               dplyr, 
               tidyverse, 
               ggplot2)



#create a new folder in your files depending on the current date for output
#update depending the date depending on the current date
results_folder<-here('PROMs', 'outputs','19_05_25','chaq')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder, recursive = TRUE), print('Outputs directory already exists'))


# Data load ---------------------------------------------------------------

#data location
proms_folder<-paste0('PROMS_Data_','2024-11-05')

flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

demog<-read.csv(here(proms_folder, 'demographics.csv'))

smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

# Data processing -------------------------------------------------------

#Step 1: Identify CHAQ 
chaq_flowsheet<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "chaq"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

length(unique(chaq_flowsheet$project_id))

chaq_smart_data<-smart_data_elements %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "chaq"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

length(unique(chaq_smart_data$project_id))

chaq<-chaq_smart_data

rm(chaq_smart_data)
rm(chaq_flowsheet)

#Number of unique patients 
length(unique(chaq$project_id))

#Step 2: Identify type of reporting, questionnaire, metric and order of questionnaire 
chaq2<-chaq %>%
  #Type of metric 
  mutate(ConceptName=tolower(ConceptName)) %>% 
  mutate(subscale=case_when(str_detect(ConceptName, "dressing")~ "dressing_and_personal_care", 
                            str_detect(ConceptName, "getting up")~ "getting_up", 
                            str_detect(ConceptName, "eating")~ "eating", 
                            str_detect(ConceptName, "walking")~ "walking",
                            str_detect(ConceptName, "aids&devices1")~ "aids&devices1",
                            str_detect(ConceptName, "aids&devices2")~ "aids&devices2",
                            str_detect(ConceptName, "aids&devices other")~ "aids&devices_other",
                            str_detect(ConceptName, "help1")~ "help_another_person1", 
                            str_detect(ConceptName, "help2")~ "help_another_person2",
                            str_detect(ConceptName, "hygiene")~ "hygiene",
                            str_detect(ConceptName, "reach")~ "reach",
                            str_detect(ConceptName, "grip")~ "grip",
                            str_detect(ConceptName, "activities")~ "activities",
                            str_detect(ConceptName, "hygiene")~ "hygiene")) %>% 
  mutate(type=ifelse(str_detect(ConceptName, "recorded|recored|reported"),"metadata",ifelse(is.na(subscale),"item", "score"))) %>% 
  mutate(subscale=case_when(!is.na(subscale)~subscale,
                            str_detect(ConceptName,"d&p")~"dressing_and_personal_care", 
                            str_detect(ConceptName,"stand|bed")~"getting_up",
                            str_detect(ConceptName,"cut|lift|chaq open")~"eating",
                            str_detect(ConceptName,"walk|climb")~"walking", 
                            str_detect(ConceptName,"wash|bath|toilet|teeth|comb")~"hygiene",
                            str_detect(ConceptName,"reach overhead|bend|pull|turn")~"reach", #Need to change the reach overhead to be item 
                            str_detect(ConceptName,"write|jar|taps|turn|push|chaqs open")~"grip", 
                            str_detect(ConceptName,"errands| get in|ride|chores|play")~"activities",
                            str_detect(ConceptName, "chaq score simple")~"score_simple",
                            ConceptName=="chaqs"~"overall_score",
                            str_detect(ConceptName, "recorded|recored|reported")~"metadata")) %>% 
        mutate(type=ifelse(str_detect(ConceptName,"reach overhead"),"item",ifelse(str_detect(subscale,"score"),"score",type)))

#Step 2: Identify and validate the metrics 

metrics<-chaq2 %>% 
  select(ConceptName,subscale,type) %>% 
  distinct() 

metrics_and_scores<-chaq2 %>% 
  select(type,subscale,ConceptName,StringValue) %>% 
  distinct() %>% 
  arrange(type)

#Number of the same metrics for pts on the same date 
num_pats_and_metrics <-chaq2 %>%  
  group_by(project_id,start_datetime, type,subscale,ConceptName) %>% 
  summarise(num_items=n()) %>% 
  filter(num_items>1)

#Number of pts with scores - pts with scores have completed questionnaires 
chaq_scores<-chaq2 %>% 
  filter(type=="score")

length(unique(chaq_scores$project_id))==length(unique(chaq$project_id)) #Not all pts have an overall score 


#Step 2 - Identify type of reporting 
check_type_reporting<-chaq2 %>% 
  filter(str_detect(ConceptName,"reported")) 

length(unique(check_type_reporting$project_id)) 


denom<-chaq2 %>% 
  select(project_id) %>% 
  distinct() %>% 
  summarise(count=n())

#Not enough information to impute type of reporting so cannot work out questionnaire IDs and process the data futher 
  
