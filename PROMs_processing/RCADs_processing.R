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
results_folder<-here('PROMs', 'outputs', '23_05_25')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder, recursive = TRUE), print('Outputs directory already exists'))


# Data load ---------------------------------------------------------------
proms_folder<-paste0('PROMS_Data_','2024-11-05')

flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

demog<-read.csv(here(proms_folder, 'demographics.csv'))

smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

# data processing  -------------------------------------------------------

#Step 1: Identify the PROMs 
rcads<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "rcads"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

length(unique(rcads$project_id))

#Step 2: Identify the type of metrics and type of reporting (no different type of questionnaires)
rcads1<-rcads %>% 
  mutate(subscale=case_when(
    DisplayName %in% c("I feel sad or empty","I have trouble sleeping","I have problems with my appetite", 
                       "I have no energy for things","I am tired a lot", 
                       "I cannot think clearly", "I feel worthless", 
                       "I feel like I don't want to move","I feel restless", "Nothing is much fun anymore", 
                       "My child feels sad or empty", "Nothing is much fun for my child anymore", 
                       "My child has trouble sleeping", "My child has problems with his/her appetite", 
                       "My child has no energy for things","My child is tired a lot","My child cannot think clearly",
                       "My child feels worthless","My child feels like he/she doesn't want to move","My child feels restless")~ "depression", 
    DisplayName %in% c("I worry when I think I have done poorly at something", 
                       "I feel scared when I have to take a test", 
                       "I feel worried when I think someone is angry with me", 
                       "I worry that I will do badly at my school work", 
                       "I worry I might look foolish", 
                       "I worry about making mistakes", 
                       "I worry what other people think of me", 
                       "I feel afraid if I have to talk in front of my class", 
                       "I feel afraid that I will make a fool myself in front of people", 
                       "My child worries when he/she thinks he/she has done poorly at something",
                       "My child feels scared when taking a test", 
                       "My child worries when he/she thinks someone is angry with him/her",
                       "My child worries about doing badly at school work", 
                       "My child worries about looking foolish", 
                       "My child worries about making mistakes", 
                       "My child worries what other people think of him/her", 
                       "My child feels afraid if he/she have to talk in front of the class", 
                       "My child feels afraid that he/she will make a fool him/herself in front of people")~ "soc_phobia", 
    DisplayName %in% c("When I have a problem, I get a funny feeling in my stomach", 
                       "I suddenly feel as if I can't breathe when there is no reason for this", 
                       "When I have a problem, my heart beats really fast",
                       "I suddenly start to tremble or shake when there is no reason for this", 
                       "When I have a problem, I feel shaky", 
                       "I suddenly become dizzy or faint when there is no reason for this",
                       "All of a sudden I feel really scared for no reason at all", 
                       "My heart suddenly starts to beat too quickly for no reason", 
                       "I worry that I will suddenly get a scared feeling when there is nothing to be afraid of", 
                       "When my child has a problem, he/she gets a funny feeling in his/her stomach", 
                       "My child suddenly feels as if he/she can't breathe when there is no reason for this", 
                       "When my child has a problem, he/she feels shaky", 
                       "All of a sudden my child will feel really scared for no reason at all", 
                       "My child suddenly becomes dizzy or faint when there is no reason for this", 
                       "My child's heart suddenly starts to beat too quickly for no reason", 
                       "My child worries that he/she will suddenly get a scared feeling when there is nothing to be afraid of", 
                       "My child suddenly starts to tremble or shake when there is no reason for this")~ "panic_disorder", 
    DisplayName %in% c("I would feel afraid of being on my own at home", 
                       "I worry about being away from my parent", 
                       "I feel scared if I have to sleep on my own", 
                       "I have trouble going to school in the mornings because I feel nervous or afraid", 
                       "I am afraid of being in crowded places (like shopping centers, the movies, buses, busy playgrounds)", 
                       "I worry when I go to bed at night", 
                       "I would feel scared if I had to stay away from home overnight", 
                       "My child feels afraid of being alone at home",
                       "My child worries about being away from me", 
                       "My child feels scared to sleep on his/her own", 
                       "My child has trouble going to school in the mornings because of feeling nervous or afraid", 
                       "My child is afraid of being in crowded places (like shopping centers, the movies, buses, busy playgrounds)",
                       "My child worries when in bed at night", 
                       "My child would feel scared if he/she had to stay away from home overnight")~ "sep_anxiety",  
    DisplayName %in% c("I worry about things","I worry that something awful will happen to someone in my family", 
                       "I worry that bad things will happen to me","I worry that something bad will happen to me","I worry about what is going to happen", 
                       "I think about death", 
                       "My child worries about things", 
                       "My child worries that something awful will happen to someone in the family", 
                       "My child worries that something bad will happen to him/her", 
                       "My child worries that bad things will happen to him/her", 
                       "My child thinks about death", 
                       "My child worries about what is going to happen", 
                       "When my child has a problem, his/her heart beats really fast")~ "gad",
    DisplayName %in% c("I get bothered by bad or silly thoughts or pictures in my mind", 
                       "I have to keep checking that I have done things right (like the switch is off, or the door is locked)",
                       "I can't seem to get bad or silly thoughts out of my head", 
                       "I have to think of special thoughts (like numbers or words) to stop bad things from happening", 
                       "I have to do some things over and over again (like washing my hands, cleaning or putting things in a certain order)", 
                       "I have to do some things in just the right way to stop bad things from happening", 
                       "My child is bothered by bad or silly thoughts or pictures in his/her mind",
                       "My child has to keep checking that he/she has done things right (like the switch is off, or the door is locked)", 
                       "My child can't seem to get bad or silly thoughts out of his/her head", 
                       "My child has to think of special thoughts (like numbers or words) to stop bad things from happening", 
                       "My child has to do some things over and over again (like washing hands, cleaning or putting things in a certain order)", 
                       "My child has to do some things in just the right way to stop bad things from happening")~ "ocd"), 
    type_report=case_when(str_detect(type, "RCADS-P")~ "parent",
                          str_detect(type, "RCADS 8-18")~ "child", 
                           TRUE~NA_character_),
    type_metric=case_when(str_detect(DisplayName, "Score")~ "score", 
                         !is.na(subscale)~"item", 
                         str_detect(DisplayName, "Scoring")~"score",
                         str_detect(DisplayName, "Gender")~"metadata",
                         str_detect(DisplayName, "School Year")~"metadata",
                         TRUE ~ NA_character_)) %>% 
  mutate(type_score=case_when(str_detect(DisplayName,"Raw Score")~ "raw", 
                              str_detect(DisplayName,"Scoring Row")~ "scoring_hide",
                              str_detect(DisplayName,"T Score")~ "t",
                              TRUE~type_metric)) %>% 
  mutate(subscale=case_when(!is.na(subscale)~subscale,
                              str_detect(DisplayName,"Obsessive|Obsession")~ "ocd", 
                              str_detect(DisplayName,"Panic Disorder|Panic")~ "panic_disorder",
                              str_detect(DisplayName,"Social Phobia")~ "soc_phobia",
                              str_detect(DisplayName,"Separation Anxiety")~ "sep_anxiety", 
                              str_detect(DisplayName,"Major Depression")~ "depression",
                              str_detect(DisplayName,"Generalised Anxiety")~ "gad",
                              str_detect(DisplayName,"Total Anxiety")~ "total_anxiety",
                              str_detect(DisplayName,"Anxiety & Depression")~ "total_internalizing",
                              str_detect(DisplayName, "Anxiety")~ "total_anxiety", 
                              str_detect(DisplayName, "Depression")~ "total_depression",
                              TRUE~"metadata")) 
  
length(unique(rcads1$project_id))

denom<-rcads1 %>% 
  select(project_id) %>% 
  distinct() %>% 
  summarise(count=n())

write.csv(denom, here(results_folder,'denom.csv'))


#Step 2: Identify type of reporting and Step 3: Create and assign questionnaire ID 
questionnaire_type_each_pat<-rcads1 %>% 
  mutate(date=substr(TakenInstant,1,10)) %>% 
  select(project_id,date, type_report) %>% 
  distinct() %>% 
  arrange(project_id, date) %>% 
  group_by(project_id,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report)) %>% 
  group_by(project_id) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% 
  pivot_wider(id_cols=c(project_id,date, order_of_questionnaires, number_questionnaires), names_from=type_report, 
              values_from=questionnaire_id)

length(unique(questionnaire_type_each_pat$project_id))
 
#Check observations with no type of reporting 

miss_questionnaire_type<-questionnaire_type_each_pat %>%
  filter(!is.na(`NA`))

miss_questionnaire_type_pat<-unique(miss_questionnaire_type$project_id)

rcads1<-rcads1 %>%
  mutate(has_miss_type=case_when(project_id %in% miss_questionnaire_type_pat~1,
                                 TRUE~0)) #add this back into the main dataset

length(unique(rcads1$project_id))

#Check the format of those with missing type of reporting 
summary_template<-rcads1 %>% 
  select(TemplateName, type_report, has_miss_type) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  mutate(count=row_number()) %>% 
  pivot_wider(id_cols=count, names_from=type_report, values_from=TemplateName) %>% 
  select(-count, miss_type=`NA`)

write.csv(summary_template, here(results_folder,'summary_template.csv'))

summary_type<-rcads1 %>% 
  select(type, type_report, has_miss_type) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  mutate(count=row_number()) %>% 
  pivot_wider(id_cols=count, names_from=type_report, values_from=type) %>% 
  select(-count, miss_type=`NA`)

write.csv(summary_type, here(results_folder,'summary_type.csv'))

summary_displayname<-rcads1 %>% 
  select(DisplayName, type_report, has_miss_type) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  mutate(count=row_number()) %>% 
  pivot_wider(id_cols=count, names_from=type_report, values_from=DisplayName) %>% 
  select(-count, miss_type=`NA`)

write.csv(summary_displayname, here(results_folder,'summary_displayname.csv'))

type_reporting<-rcads1 %>% 
  select(project_id, type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(count=n())

write.csv(type_reporting, here(results_folder,'type_reporting.csv'))


#Step 2: Identify order of questionnaire and Step 3: Create and assing questionnaire ID 
order_questionnaire <-rcads1 %>% 
  mutate(date=substr(TakenInstant,1,10)) %>% 
  select(project_id,date, type_report) %>% 
  distinct() %>% 
  arrange(project_id, date) %>% 
  group_by(project_id,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(date)) %>% 
  mutate(order_label=ifelse(order_of_questionnaires==1,"initial","followup")) %>% 
  group_by(type_report, order_label) %>% 
  summarise(count=n())


write.csv(order_questionnaire, here(results_folder,'order_of_reporting.csv'))


#Step 3: Check the validity of the questionnaires 
# Join the demographics------------------------------------

rcads1_demographics<-rcads1 %>% 
  mutate(date=substr(TakenInstant,1,10)) %>% 
  select(project_id,date,TakenInstant, type_report) %>% 
  distinct() %>%  #31 
  mutate(project_id=tolower(project_id)) %>% 
  arrange(project_id, TakenInstant) %>% 
  group_by(project_id,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(TakenInstant)) %>% #works out which questionnaires are follow ups for specific pats 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report)) %>% 
  group_by(project_id) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% #some pats have more than one type of questionnaires and have follow ups  #This basically recreates the dt_type_questionnaire- so spot check that this is right 
  left_join(demog %>% #This adds demographics data 
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name) %>% 
              mutate(project_id=tolower(project_id))) %>% 
  mutate(age_questionnaire=as.numeric(str_sub(date,1,4))-as.numeric(str_sub(birth_date,1,4))) %>% 
  mutate(flag_incorrect_age=ifelse(age_questionnaire<8|age_questionnaire>18,1,0))


length(unique(rcads1_demographics$project_id))


# Work out what scales they have ------------------------------------------

rcads1<-rcads1 %>% 
  group_by(project_id) %>% 
  mutate(has_scale=ifelse(any(type_metric=="score"),1,0)) #working out if patients have scores in at least one of their questionnaires 

length(unique(rcads1$project_id))

miss_questionnaire_scale<-rcads1 %>% 
filter(has_scale==0)

miss_questionnaire_scale_pats<-unique(miss_questionnaire_scale$project_id) 

# Step 3: Create and assing questionnaire ID and check validity  ----------------------------------

rcads2<-rcads1 %>% 
  mutate(date=substr(TakenInstant,1,10)) %>% 
  mutate(project_id=tolower(project_id)) %>% 
  arrange(project_id, TakenInstant) %>% 
  group_by(project_id,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(TakenInstant)) %>% #works out which questionnaires are follow ups for specific pats 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report)) %>% 
  group_by(project_id) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% #some pats have more than one type of questionnaires and have follow ups  #This basically recreates the dt_type_questionnaire- so spot check that this is right 
  left_join(demog %>% #This adds demographics data 
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name) %>% 
              mutate(project_id=tolower(project_id))) %>% 
  mutate(age_questionnaire=as.numeric(str_sub(date,1,4))-as.numeric(str_sub(birth_date,1,4))) %>% 
  mutate(flag_incorrect_age=ifelse(age_questionnaire<8|age_questionnaire>18,1,0)) %>% 
  filter(!is.na(type_report)) #Remove those with missing type of reporting 

length(unique(rcads2$project_id))==length(unique(rcads$project_id))
length(unique(rcads2$questionnaire_id))==length(unique(rcads1_demographics$questionnaire_id))

#Save processed data 
saveRDS(rcads2, here('PROMs', 'clean_data','rcads.rds'))

# Denominators after processing ----------------------------------------

#Number of patients with at least one score in at least one of their questionnaires 
uniq_pat<-rcads2 %>% 
  ungroup() %>% 
  filter(has_scale==1)%>% 
  select(project_id) %>% 
  summarise(unique_patients=n_distinct(project_id))

#Number of questionnaires 
uniq_quest<-rcads2 %>% 
  ungroup() %>% 
  select(questionnaire_id) %>% 
  summarise(unique_questionnaire=n_distinct(questionnaire_id))

denom<-cbind(uniq_pat, uniq_quest)

uniq_quest_by_type<-rcads2 %>% 
  ungroup() %>% 
  select(questionnaire_id,type_of_questionnaire=type_report) %>% 
  distinct() %>% 
  group_by(type_of_questionnaire) %>% 
  summarise(unique_questionnaire=n()) 

denom<-denom %>% 
  mutate(type_of_questionnaire="Overall") %>% 
  full_join(uniq_quest_by_type) %>% 
  select(type_of_questionnaire, unique_questionnaire, unique_patients)

write.csv(denom, here(results_folder,'rcads_after_processing.csv'))

