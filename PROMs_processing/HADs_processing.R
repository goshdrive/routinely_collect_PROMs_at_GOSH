rm(list=ls())


# Setup -------------------------------------------------------------------

pacman::p_load(here, 
               dplyr, 
               tidyverse, 
               ggplot2,
               janitor)



#create a new folder in your files depending on the current date for output
#update depending the date depending on the current date
results_folder<-here('PROMs', 'outputs','19_05_25','hads')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder, recursive = TRUE), print('Outputs directory already exists'))


# Data load ---------------------------------------------------------------

proms_folder<-paste0('PROMS_Data_','2024-11-05')

flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

demog<-read.csv(here(proms_folder, 'demographics.csv'))

smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

# Data processing -------------------------------------------------------

#Step 1: Identify HADs 
hads_flowsheet<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "hads"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

hads_smartdata<-smart_data_elements %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "hads"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

hads<-hads_flowsheet

rm(list=c("hads_flowsheet", "hads_smartdata"))

length(unique(hads$project_id))

#Step 2: Identify type of metrics (HADs doesn't have different type of reporting or versions)
hads<-hads %>% 
  mutate(subscale=case_when(
    DisplayName %in% c("I feel tense or wound up", 
                       "I get a sort of frightened feeling as if something awful is going to happen", 
                       "Worrying thought goes through my mind", 
                       "I can sit at ease and feel relaxed", 
                      "I get sudden feelings of panic", 
                      "I feel restless, as if I had to be on the move", 
                      "I get a sort of frightened feeling like butterflies in the stomach", 
                      "Anxiety Score")~ "anxiety", 
                      TRUE~"depression")) %>% 
  mutate(score=case_when(str_detect(DisplayName, "Score")~ "score", 
                         TRUE~"item"))

#Step 2: Identify order of questionnaire and Step 3: create and assign questionnaire ID 
hads<-hads %>% 
  mutate(date=substr(start_datetime,1,10)) %>% 
  arrange(project_id, date) %>% 
  group_by(project_id) %>% 
  mutate(order_of_questionnaires=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires))

#Step 2: Identify type of metrics and Step 3: Validate the metrics  
subscale<-hads %>% 
  select(DisplayName, score, subscale) %>% 
  distinct() %>% 
  pivot_wider(id_cols=DisplayName, names_from=subscale, values_from=score)
  
 
num_items<-hads %>% 
  select(project_id, questionnaire_id, start_datetime, end_datetime, subscale) %>% 
  group_by(project_id, questionnaire_id, start_datetime, end_datetime) %>% 
  summarise(count=n()) %>% 
  mutate(flag=ifelse(count>16|count<16,1,0)) # some participants have less than or more than 16 items but technically all questionnaires shoud have all the items

#HADs should have 16 items in total 

pats_no_16_items<-num_items %>% 
  ungroup() %>% 
  filter(flag==1) %>% 
  select(project_id, questionnaire_id) %>% 
  distinct()

length(unique(pats_no_16_items$questionnaire_id))

check_items_not_16<-hads %>% 
  filter(questionnaire_id %in% pats_no_16_items$questionnaire_id) %>% 
  select(project_id, questionnaire_id, date, DisplayName, subscale,Value) %>% 
  group_by(project_id, questionnaire_id, date) %>% 
  mutate(num_items=n()) %>% 
  pivot_wider(id_cols=c(project_id, questionnaire_id, date, num_items), names_from=DisplayName, values_from=Value)

hads<-hads %>% 
   group_by(project_id, questionnaire_id, start_datetime, end_datetime) %>% 
   mutate(num_items=n())

#Calculate the number of items of each questionnaire 
hads %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  tabyl(num_items) 

#Check whether the value of those duplicated metrics within the same questionnaire are the same, if so, then just keep one, if not then retain both
hads<-hads %>% 
  group_by(project_id, questionnaire_id, start_datetime, end_datetime, DisplayName, Value) %>% 
  mutate(num_items_and_scores=n(),
         order=row_number()) %>% 
  filter(order==1)

#Check number of items 
hads2<-hads %>% 
  group_by(project_id, questionnaire_id, start_datetime, end_datetime) %>% 
  mutate(num_items=n())


#Step 2: Identify order of questionnaire 

order_reporting<-hads2 %>% 
  ungroup() %>% 
  select(project_id, order_of_questionnaires) %>% 
  distinct() %>% 
  mutate(order_label=ifelse(order_of_questionnaires==1,"initial","followup")) %>% 
  group_by(order_label) %>% 
  summarise(count=n())

write.csv(order_reporting, here(results_folder,'order_reporting.csv'))


#Step 3: Create and assign questionnaire ID  
hads2<-hads2 %>% 
  arrange(project_id, date) %>% 
  group_by(project_id) %>% 
  mutate(order_of_questionnaires=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires)) %>% 
  select(project_id, questionnaire_id, start_datetime, end_datetime, DisplayName, subscale, order_of_questionnaires, score, num_items, Value)

#Step 3: validate the questionnaires 

check_quest_less_16_items<-hads2 %>% 
  filter(num_items<16)

#Step 2: Identify the type of metrics including whether the values are valid 
unique_values<-hads2 %>% 
  ungroup() %>% 
  select(Value) %>% 
  distinct() #all numeric

#Step 3 - Assign questionnaire ID and check validity of the questionnaire
hads3<-hads2 %>% 
   left_join(demog %>% #This adds demographics data 
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name) %>% 
              mutate(project_id=project_id)) %>% 
  ungroup() %>% 
  mutate(date=substr(start_datetime,1,10)) %>% 
  filter(num_items==16) #Only keep questionnaires with 16 items 


# Denominators after processing -------------------------------------------

uniq_pat<-hads3 %>% 
  ungroup() %>% 
  select(project_id) %>% 
  summarise(unique_patients=n_distinct(project_id))

#Number of questionnaires 
uniq_quest<-hads3 %>% 
  ungroup() %>% 
  select(questionnaire_id) %>% 
  summarise(unique_questionnaire=n_distinct(questionnaire_id))

denom<-cbind(uniq_pat, uniq_quest)

write.csv(denom, here(results_folder,'denom_after_processing.csv'))


# #Save data set 
# saveRDS(hads3, here('PROMs', 'clean_data','hads.rds'))
