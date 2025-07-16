rm(list=ls())


# Setup -------------------------------------------------------------------

library(here)
library(tidyverse)
library(gtsummary)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggplot2)
library(ggbump)
library(gridExtra)

#create a new folder in your files depending on the current date for output
#update depending the date depending on the current date
results_folder<-here('PROMs', 'outputs', '23_05_25')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder), print('Outputs directory already exists'))


# Data load ---------------------------------------------------------------

proms_folder<-paste0('PROMS_Data_','2024-11-05')

#flowsheets
flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

#smartdata 
smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

#demographics 
demog<-read.csv(here(proms_folder, 'demographics.csv'))

#Step 1: Identify PROMs 
flowsheet <- flowsheet_rows %>%
  mutate(across(where(is.character), tolower)) %>%
  filter(if_any(where(is.character), ~ str_detect(., "sdq")))

#Number of unique patients 
denom<-flowsheet %>% 
  ungroup() %>% 
  select(project_id) %>% 
  summarise(unique_patients=n_distinct(project_id))  %>% 
  mutate(Metric="Overall number of patients") 

#Step 2: Identify type of questionnaire 
flowsheet<-flowsheet %>% 
  mutate(metric_age_template_name=case_when(str_detect(TemplateName,"4-17")~"SDQ 4-17", 
                                            str_detect(TemplateName,"2-4")~"SDQ 2-4", 
                                            str_detect(TemplateName,"11-17")~"SDQ 11-17")) %>% 
  mutate(metric_age_type=case_when(str_detect(type,"4-17")~"SDQ 4-17", 
                                   str_detect(type,"2-4")~"SDQ 2-4", 
                                   str_detect(type,"11-17")~"SDQ 11-17")) %>% 
  mutate(metric_age_display_name=case_when(str_detect(DisplayName,"4-17")~"SDQ 4-17", 
                                           str_detect(DisplayName,"2-4")~"SDQ 2-4", 
                                           str_detect(DisplayName,"11-17")~"SDQ 11-17")) %>% 
  mutate(metric_age=ifelse(!is.na(metric_age_template_name), metric_age_template_name, ifelse(!is.na(metric_age_type), metric_age_type,metric_age_display_name))) %>% 
  mutate(metric_age=str_trim(metric_age))


#Step 2: Identify the type of metric, type of questionnaire and type of reporting 
dt_questionnaire<-flowsheet %>% 
  #select(project_id, TakenInstant,TemplateName, DisplayName, ValueType, type, Value, comment, metric_age) %>% 
  mutate(type_scale=case_when(str_detect(DisplayName,"peer")~ "peer", 
                              str_detect(DisplayName,"emotional")~ "emotional",
                              str_detect(DisplayName,"hyperactivity")~ "hyperactivity", 
                              str_detect(DisplayName,"conduct")~ "conduct", 
                              str_detect(DisplayName,"prosocial")~ "prosocial",
                              str_detect(DisplayName,"total")~ "total",
                              str_detect(DisplayName,"impact")~ "impact", 
                              TRUE~NA)) %>% 
  mutate(type_score=case_when(DisplayName=="completed by"~ "metadata", 
                              DisplayName=="do you have any other questions or concerns?"~ "free text comments",
                              DisplayName=="start/end of treatment"~"metadata",
                              DisplayName=="has coming to clinic been helpful in other ways e.g. providing information or making the problems more bearable?"~"other",
                              DisplayName=="since coming to clinic are your child's problems"~"other",
                              DisplayName=="follow up patient: has coming to clinic been helpful in other ways e.g. providing information or making the problems more bearable?"~"other",
                              DisplayName=="follow up patient:since coming to clinic are your problems:"~"other",
                              is.na(type_scale)~ "score", 
                              TRUE~ "scale")) %>% 
  #recode the items
  mutate(pconsid=ifelse(DisplayName=="considerate of other people's feelings",Value,NA), 
         prestles=ifelse(DisplayName=="restless, overactive, cannot stay still for long",Value,NA), 
         psomatic=ifelse(DisplayName=="often complains of headache, stomach-aches or sickness",Value,NA), 
         pshares=ifelse(DisplayName=="shares readily with other children (treats, toys, pencils etc)",Value,NA), 
         ptantrum=ifelse(DisplayName=="often has temper tantrums or hot tempers",Value,NA), 
         ploner=ifelse(DisplayName=="rather solitary, tends to play alone",Value,NA),
         pobeys=ifelse(DisplayName=="generally obedient, usually does what adults request ",Value,NA),
         pworries=ifelse(DisplayName=="many worries, often seems worried",Value,NA), 
         pargues=ifelse(DisplayName=="often argumentative with adults",Value,NA),
         pcaring=ifelse(DisplayName=="helpful if someone is hurt, upset or feeling ill",Value,NA), 
         pfidgety=ifelse(DisplayName=="constantly fidgeting or squirming",Value,NA), 
         pfriend=ifelse(DisplayName=="has at least one good friend",Value,NA), 
         pfights=ifelse(DisplayName=="often fights with other children or bullies them",Value,NA), 
         punhappy=ifelse(DisplayName=="often unhappy, down-hearted or tearful",Value,NA),
         ppopular=ifelse(DisplayName=="generally liked by other children",Value,NA),
         pdistrac=ifelse(DisplayName=="easily distracted, concentration wanders",Value,NA),
         pclingy=ifelse(DisplayName=="nervous or clingy in new situations, easily loses confidence",Value,NA),
         pkind=ifelse(DisplayName=="kind to younger children",Value,NA), 
         plies=ifelse(DisplayName=="often lies or cheats",Value,NA),
         pbullied=ifelse(DisplayName=="picked on or bullied by other children",Value,NA),
         phelpout=ifelse(DisplayName=="often volunteers to help others (parents, teachers, other children)",Value,NA),
         preflect=ifelse(str_detect(DisplayName,"things out before acting"),Value,NA),
         psteals=ifelse(DisplayName=="steals from home, school or elsewhere",Value,NA),
         poldbest=ifelse(DisplayName=="gets on better with adults than with other children",Value,NA),
         pafraid=ifelse(DisplayName=="many fears, easily scared",Value,NA),
         pattends=ifelse(DisplayName=="sees tasks through to the end, good attention span",Value,NA), 
         pebddiff=ifelse(str_detect(DisplayName, "difficulties in one or more of the following areas: emotions, concentration, behaviour"),Value,NA),
         pdistres=ifelse(DisplayName=="do the difficulties upset or distress your child?",Value,NA),
         pimphome=ifelse(DisplayName=="do the difficulties interfere with your child's everyday life regarding: home life",Value,NA),
         pimpfrie=ifelse(DisplayName=="do the difficulties interfere with your child's everyday life regarding: friendships",Value,NA),
         pimpclas=ifelse(DisplayName %in% c("do the difficulties interfere with your child's everyday life regarding: learning", "do the difficulties interfere with your child's everyday life regarding: classroom learning"),Value,NA),
         pimpleis=ifelse(DisplayName=="do the difficulties interfere with your child's everyday life regarding: leisure activities",Value,NA),
         pimfamily=ifelse(DisplayName=="do the difficulties put a burden on you or the family as a whole?",Value,NA),
         pdislong=ifelse(DisplayName=="if you have answered `yes`, how long have these difficulties been present?",Value,NA), 
         pspite=ifelse(DisplayName=="can be spiteful to others",Value,NA),
         sconsid=ifelse(DisplayName=="i try to be nice to other people. i care about their feelings",Value,NA), 
         srestles=ifelse(DisplayName=="i am restless, i cannot stay still for long",Value,NA), 
         ssomatic=ifelse(DisplayName=="i get a lot of headache, stomach-aches or sickness",Value,NA), 
         sshares=ifelse(DisplayName=="i usually share with others (food, games, pens etc)",Value,NA), 
         stantrum=ifelse(str_detect(DisplayName,"i get very angry and often"),Value,NA), 
         sloner=ifelse(DisplayName=="i am usually on my own. i generally play alone or keep to myself",Value,NA),
         sobeys=ifelse(DisplayName=="i usually do as i am told",Value,NA),
         sworries=ifelse(DisplayName=="i worry a lot",Value,NA), 
         scaring=ifelse(DisplayName=="i am helpful if someone is hurt, upset or feeling ill",Value,NA), 
         sfidgety=ifelse(DisplayName=="i am constantly fidgeting or squirming",Value,NA), 
         sfriend=ifelse(DisplayName=="i have one good friend or more",Value,NA), 
         sfights=ifelse(DisplayName=="i fight a lot. i can make other people do what i want",Value,NA), 
         sunhappy=ifelse(DisplayName=="i am often unhappy, down-hearted or tearful",Value,NA),
         spopular=ifelse(DisplayName=="other people my age generally like me",Value,NA),
         sdistrac=ifelse(DisplayName=="i am easily distracted. i find it difficult to concentrate",Value,NA),
         sclingy=ifelse(DisplayName=="i am nervous in new situations. i easily lose confidence",Value,NA),
         skind=ifelse(DisplayName=="i am kind to younger children",Value,NA), 
         slies=ifelse(DisplayName=="i am often accused of lying or cheating",Value,NA),
         sbullied=ifelse(DisplayName=="other children or young people pick on me or bully me",Value,NA),
         shelpout=ifelse(DisplayName=="i often volunteer to help others (parents, teachers, children)",Value,NA),
         sreflect=ifelse(DisplayName=="i think before i do things",Value,NA),
         ssteals=ifelse(DisplayName=="i take things that are not mine from home, school or elsewhere.",Value,NA),
         soldbest=ifelse(DisplayName=="i get on better with adults than with people my own age",Value,NA),
         safraid=ifelse(DisplayName=="i have many fears, i am easily scared",Value,NA),
         sattends=ifelse(DisplayName=="i finish the work i am doing. my attention is good",Value,NA), 
         sebddiff=ifelse(str_detect(DisplayName, "difficulties in one or more of the following areas: emotions, concentration, behaviour"),Value,NA),
         sdistres=ifelse(DisplayName=="do the difficulties upset or distress you?",Value,NA),
         simphome=ifelse(DisplayName %in% c("do the difficulties interfere with your everyday life regarding: home life", 
                                            "do the difficulties interfere with your child's everyday life regarding: home life"),Value,NA),
         simpfrie=ifelse(DisplayName %in% c("do the difficulties interfere with your everyday life regarding: friendships", 
                                            "do the difficulties interfere with your child's everyday life regarding: friendships"),Value,NA),
         simpclas=ifelse(DisplayName %in% c("do the difficulties interfere with your everyday life regarding: classroom learning", 
                                            "do the difficulties interfere with your child's everyday life regarding: classroom learning"),Value,NA),
         simpleis=ifelse(DisplayName %in% c("do the difficulties interfere with your everyday life regarding: leisure activities", 
                                            "do the difficulties interfere with your child's everyday life regarding: leisure activities"),Value,NA),
         simfamily=ifelse(DisplayName=="do the difficulties make it harder for those around you (family, friends, teachers, etc)",Value,NA),
         sdislong=ifelse(DisplayName=="if you have answered `yes`, how long have these difficulties been present?",Value,NA)) %>% 
  mutate(type_scale=case_when(!is.na(type_scale)~ type_scale, 
                              !is.na(psomatic)~"emotional", 
                              !is.na(pworries)~"emotional",
                              !is.na(punhappy)~"emotional",  
                              !is.na(pclingy)~"emotional",
                              !is.na(pafraid)~"emotional", 
                              !is.na(ptantrum)~"conduct", 
                              !is.na(pobeys)~"conduct", 
                              !is.na(pfights)~"conduct", 
                              !is.na(plies)~"conduct", 
                              !is.na(pargues)~"conduct", 
                              !is.na(psteals)~"conduct", 
                              !is.na(pspite)~"conduct", 
                              !is.na(prestles)~"hyperactivity", 
                              !is.na(pfidgety)~ "hyperactivity", 
                              !is.na(pdistrac)~ "hyperactivity", 
                              !is.na(preflect)~ "hyperactivity", 
                              !is.na(pattends)~"hyperactivity",
                              !is.na(ploner)~"peer", 
                              !is.na(pfriend)~"peer", 
                              !is.na(ppopular)~"peer", 
                              !is.na(pbullied)~"peer", 
                              !is.na(poldbest)~"peer", 
                              !is.na(pconsid)~"prosocial", 
                              !is.na(pshares)~"prosocial", 
                              !is.na(pcaring)~"prosocial", 
                              !is.na(pkind)~"prosocial", 
                              !is.na(phelpout)~"prosocial", 
                              !is.na(pdistres)~"impact", 
                              !is.na(pimphome)~"impact", 
                              !is.na(pimpfrie)~"impact", 
                              !is.na(pimpclas)~"impact", 
                              !is.na(pimpleis)~"impact", 
                              !is.na(pebddiff)~"other", 
                              !is.na(pdislong)~"other", 
                              !is.na(pimfamily)~"other", 
                              DisplayName=="do you have any other questions or concerns?"~"other", 
                              DisplayName=="completed by"~"other", 
                              str_detect(DisplayName, "if you have answered")~"other", 
                              DisplayName=="start/end of treatment"~"other",
                              DisplayName=="has coming to clinic been helpful in other ways e.g. providing information or making the problems more bearable?"~ "other", 
                              DisplayName=="since coming to clinic are your child's problems"~"other", 
                              !is.na(ssomatic)~"emotional", 
                              !is.na(sworries)~"emotional",
                              !is.na(sunhappy)~"emotional",  
                              !is.na(sclingy)~"emotional",
                              !is.na(safraid)~"emotional", 
                              !is.na(stantrum)~"conduct", 
                              !is.na(sobeys)~"conduct", 
                              !is.na(sfights)~"conduct", 
                              !is.na(slies)~"conduct", 
                              !is.na(ssteals)~"conduct", 
                              !is.na(srestles)~"hyperactivity", 
                              !is.na(sfidgety)~ "hyperactivity", 
                              !is.na(sdistrac)~ "hyperactivity", 
                              !is.na(sreflect)~ "hyperactivity", 
                              !is.na(sattends)~"hyperactivity",
                              !is.na(sloner)~"peer", 
                              !is.na(sfriend)~"peer", 
                              !is.na(spopular)~"peer", 
                              !is.na(sbullied)~"peer", 
                              !is.na(soldbest)~"peer", 
                              !is.na(sconsid)~"prosocial", 
                              !is.na(sshares)~"prosocial", 
                              !is.na(scaring)~"prosocial", 
                              !is.na(skind)~"prosocial", 
                              !is.na(shelpout)~"prosocial", 
                              !is.na(sdistres)~"impact", 
                              !is.na(simphome)~"impact", 
                              !is.na(simpfrie)~"impact", 
                              !is.na(simpclas)~"impact", 
                              !is.na(simpleis)~"impact", 
                              !is.na(sebddiff)~"other", 
                              !is.na(sdislong)~"other", 
                              !is.na(simfamily)~"other", 
                              DisplayName=="do you have any other questions or concerns?"~"other", 
                              DisplayName=="completed by"~"other", 
                              str_detect(DisplayName, "if you have answered")~"other", 
                              DisplayName=="start/end of treatment"~"other",
                              DisplayName=="follow up patient: has coming to clinic been helpful in other ways e.g. providing information or making the problems more bearable?"~ "other", 
                              DisplayName=="follow up patient:since coming to clinic are your problems: "~"other")) %>% 
  group_by(project_id, TakenInstant) %>% 
  mutate(metric_age=case_when(!is.na(metric_age)~metric_age, 
                              #If a patient has missing type of questionnaire but all their metrics 
                              #have 11-17 as the type, then make that missing 11-17 
                              all(metric_age %in% c("SDQ 11-17", NA), na.rm = TRUE) ~ "SDQ 11-17", 
                               TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  #Type of reporting 
  mutate(type_report=case_when(str_detect(DisplayName,"t4")~"teacher", 
                             str_detect(DisplayName,"t2")~"teacher", 
                             TemplateName=="t gosh sdq teacher"~"teacher", 
                             str_detect(DisplayName,"p4")~"parent", 
                             str_detect(DisplayName,"p2")~"parent",
                             str_detect(TemplateName,"p4")~"parent", 
                             str_detect(TemplateName,"p2")~"parent", 
                             metric_age=="SDQ 11-17"~"child")) %>% 
  group_by(project_id, TakenInstant) %>% 
  #If patients have missing type of reporting but all their other metrics only had parent/teacher 
  #then make that missing parent/teacher 
  mutate(type_report=case_when(!is.na(type_report)~type_report, 
                              all(type_report %in% c("parent", NA), na.rm = TRUE) ~ "parent", 
                              all(type_report %in% c("teacher", NA), na.rm = TRUE) ~ "teacher", 
                              TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  mutate(date=substr(TakenInstant,1,10))


#Clean up data set and remove some of the variables we've added 
dt_clean<-dt_questionnaire %>% 
  select(project_id, TakenInstant, type_score, DisplayName, type_scale, Value, metric_age, type_report) %>% 
  pivot_wider(id_cols= c(project_id, TakenInstant, type_score, DisplayName,metric_age, type_report), names_from=type_scale, values_from=Value) %>% 
  arrange(project_id, TakenInstant,metric_age) %>% 
  group_by(project_id,metric_age,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(TakenInstant)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report,"_",metric_age)) %>% 
  group_by(project_id) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id))

#Check the number of unique patients to make sure none have been lost 
unique_pat_id<-dt_clean %>% 
  select(project_id) %>% 
  distinct() 

#Calculate type of reporting 
type_report<-dt_clean %>% 
  select(project_id,metric_age, type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(count=n())

write.csv(type_report, here(results_folder,'type_report.csv'))

#Calculate type of questionnaire 
type_questionnaire<-dt_clean %>% 
  select(project_id,type_report,metric_age) %>% 
  distinct() %>% 
  group_by(type_report,metric_age) %>% 
  summarise(count=n())

write.csv(type_questionnaire, here(results_folder,'type_questionnaire.csv'))


#Step 2: Identify order of questionnaire 
dt_clean<-dt_clean %>% 
  #filter(!is.na(metric_age)) %>% 
  group_by(project_id,metric_age,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(TakenInstant)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report,"_",metric_age)) %>% 
  group_by(project_id) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% 
  ungroup()

#Calculate order of questionnaire 
order_of_questionnaire<-order_of_questionnaire %>% 
  mutate(order_label=ifelse(order_of_questionnaires==1,"initial","followup")) %>% 
  group_by(type_report,metric_age,order_label) %>% 
  summarise(count=n())

write.csv(order_of_questionnaire, here(results_folder,'order_of_questionnaire.csv'))

#Check type of questionnaire and type of reporting  
questionnaire_type_each_pat<-dt_clean %>% 
  select(project_id, order_of_questionnaires, metric_age, type_report, questionnaire_id) %>% 
  distinct() %>% 
  pivot_wider(id_cols=c(project_id, order_of_questionnaires,type_report), names_from=metric_age, 
              values_from=questionnaire_id) 


miss_type<-questionnaire_type_each_pat %>% 
  filter(!is.na(`NA`)) #Each NA has another type of questionnaire available 
#Some patient have both 4-17 and 2-4 at the same time so it is difficult to impute them 

#Check the metrics if scale scores available 
questionnaire_type_each_scale_each_pat<-dt_questionnaire %>% 
  filter(type_score=="scale") %>%  
  select(project_id, TakenInstant, type_scale, Value, metric_age, type_report) %>% 
  pivot_wider(id_cols= c(project_id, TakenInstant,type_report, type_scale), names_from=metric_age, values_from=Value) 


length(unique(questionnaire_type_each_scale_each_pat$project_id))==nrow(unique_pat_id)

#Step 3: Identify and assign questionnaire ID 
dt_clean<-dt_clean %>% 
  #filter(!is.na(metric_age)) %>% 
  group_by(project_id,metric_age,type_report) %>% 
  mutate(order_of_questionnaires=dense_rank(TakenInstant)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report,"_",metric_age)) %>% 
  group_by(project_id) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% 
  ungroup()

#Check questionnaire IDs 
questionnaire_type_each_pat<-dt_clean %>% 
  mutate(date=substr(TakenInstant,1,10)) %>% 
  select(project_id,date, metric_age, type_report) %>% 
  distinct() %>% 
  arrange(project_id, date) %>% 
  group_by(project_id) %>% 
  mutate(order_of_questionnaires=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report,"_",metric_age)) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% 
  pivot_wider(id_cols=c(project_id,date, order_of_questionnaires, number_questionnaires, type_report), names_from=metric_age, 
              values_from=questionnaire_id)

#Step 3: Validate the questionnaires 
#Link questionnaire with demographics 

dt_full<-dt_clean %>% 
  mutate(date=substr(TakenInstant,1,10)) %>% 
  select(project_id,date,TakenInstant, metric_age,type_report) %>% 
  distinct() %>%  
  arrange(project_id, TakenInstant) %>% 
  group_by(project_id) %>% 
  mutate(order_of_questionnaires=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_report,"_",metric_age)) %>% 
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>% 
            select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name) %>% 
              mutate(project_id=tolower(project_id)) %>% 
  mutate(age_questionnaire=as.numeric(str_sub(date,1,4))-as.numeric(str_sub(birth_date,1,4))) %>% 
  mutate(age_check=case_when(age_questionnaire>1&age_questionnaire<5&metric_age=="SDQ 2-4"~ "2-4", 
                             age_questionnaire>3&age_questionnaire<18& metric_age=="SDQ 4-17"~ "4-17", 
                             age_questionnaire>10&age_questionnaire<18&metric_age=="SDQ 11-17"~"11-17",
                             TRUE~"wrong type")) %>% #some questionnaires have the wrong type 
  left_join(dt_questionnaire %>% #add  scale scores for each of the questionnaires 
              filter(type_score=="scale") %>%  
              select(project_id, TakenInstant, type_score, , type_scale, Value, metric_age, type_report,date) %>% 
              pivot_wider(id_cols= c(project_id, TakenInstant,date,metric_age, type_report), names_from=type_scale, values_from=Value)) %>% 
  group_by(project_id,date) %>% 
  mutate(number_questionnaires_with_same_date=n()) 

#Filter those with wrong type 
dt_full<-dt_full %>% 
  filter(age_check!="wrong type")

dt_full <- dt_full %>%
  group_by(project_id, date) %>%
  mutate(
    has_4_17_parent = any(metric_age == "SDQ 4-17" & type_report == "parent", na.rm = TRUE),
    has_4_17_teacher = any(metric_age == "SDQ 4-17" & type_report == "teacher", na.rm = TRUE)
  ) %>%
  mutate(
    type_report = if_else(
      is.na(type_report) & metric_age == "SDQ 4-17" &
        has_4_17_parent & !has_4_17_teacher,
      "parent",
      type_report
    )
  ) %>%
  ungroup()

#Save final data set 
saveRDS(dt_full, here('PROMs', 'clean_data','sdq.rds'))


# Denominators after processing -------------------------------------------

#Number of patients 
uniq_pat<-dt_full %>% 
  ungroup() %>% 
  select(project_id) %>% 
  summarise(unique_patients=n_distinct(project_id))

#Number of questionnaires 
uniq_quest<-dt_full %>% 
  ungroup() %>% 
  select(questionnaire_id) %>% 
  summarise(unique_questionnaire=n_distinct(questionnaire_id))

denom<-cbind(uniq_pat, uniq_quest)

uniq_quest_by_age<-dt_full %>% 
  select(questionnaire_id,type_of_questionnaire=metric_age) %>% 
  distinct() %>% 
  group_by(type_of_questionnaire) %>% 
  summarise(unique_questionnaire=n()) 

denom<-denom %>% 
  mutate(type_of_questionnaire="Overall") %>% 
  full_join(uniq_quest_by_age) %>% 
  select(type_of_questionnaire, unique_patients, unique_questionnaire)

write.csv(denom, here(results_folder,'sdq_after_processing.csv'))

uniq_quest_by_age_reporting<-dt_full %>% 
  select(questionnaire_id,type_of_questionnaire=metric_age,type_report) %>% 
  distinct() %>% 
  group_by(type_of_questionnaire,type_report) %>% 
  summarise(unique_questionnaire=n()) 

write.csv(uniq_quest_by_age_reporting, here(results_folder,'sdq_type_after_processing.csv'))

