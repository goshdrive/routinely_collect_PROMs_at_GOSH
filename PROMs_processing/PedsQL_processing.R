#Processing PedsQL data 
rm(list=ls())


# Setup -------------------------------------------------------------------

library(here)
library(tidyverse)
library(gtsummary)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(gridExtra)

#Create a new folder in your files depending on the current date for outputs
results_folder<-here('PROMs', 'outputs', '19_05_25','pedsql')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder, recursive = TRUE), print('Outputs directory already exists'))

# Data load ---------------------------------------------------------------
#Folder location 
proms_folder<-paste0('PROMS_Data_','2024-11-05')

#Data 
flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))
smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))
demog<-read.csv(here(proms_folder, 'demographics.csv'))

#Step 1 - Identify PROMs 
pedsql_flowsheets<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "pedsql"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

length(unique(pedsql_flowsheets$project_id))

pedsql_smart_data<-smart_data_elements %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "pedsql"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

length(unique(pedsql_smart_data$project_id))

#PedsQL exist both in smart data and flowsheets but discussions with Epic team suggest that they only built the smart data ones so will use those 
pedsql<-pedsql_smart_data

rm(pedsql_smart_data)
rm(pedsql_flowsheets)

#All the different names of the metrics - in preparation for step 2 
metrics<-pedsql %>% 
  select(ConceptName,SmartDataElementEpicId) %>% 
  distinct()

write.csv(metrics, here(results_folder,'metrics_pedsql.csv'))

#Number of unique patients 
denom<-pedsql %>% 
  ungroup() %>% 
  select(project_id) %>% 
  summarise(unique_patients=n_distinct(project_id))  %>% 
   mutate(Metric="Overall number of patients") 

write.csv(denom, here(results_folder,'denom.csv'))

#Step 2: Identify type of reporting, type of questionnaire, order of questionnaire, type of metric 

pedsql<-pedsql %>% 
  #Identifying type of questionnaire 
  mutate(metric_age=case_when(str_detect(ConceptName,"5-7")~"PedsQL 5-7", 
                              str_detect(ConceptName,"2-4")~"PedsQL 2-4", 
                              str_detect(ConceptName,"8-12")~"PedsQL 8-12",
                              str_detect(ConceptName,"13-18")~"PedsQL 13-18")) %>% 
  #Identifying type of reporting
  mutate(metric_type_desc=str_sub(ConceptName, str_length(paste0("GOSH ", metric_age))+1)) %>% 
  #Identifying type of metric 
  mutate(type_score=case_when( str_detect(ConceptName, "PSY")~"psychosocial_summary",
                               str_detect(ConceptName,"SCF|SCH")~"school_functioning", 
                               str_detect(ConceptName, "PF|PHYS")~"physical_functioning",
                               str_detect(ConceptName, "EF|EMO")~ "emotional_functioning", 
                               str_detect(ConceptName, "SF|SOC")~"social_functioning",
                               str_detect(ConceptName, "TOTAL|total")~"total_summary", 
                               str_detect(ConceptName, "PHYS |PHYS")~ "physical_summary", 
                               str_detect(ConceptName, "NF|NURSERY")~"nursery",
                               TRUE~NA_character_)) %>% 
  mutate(type_metric=case_when(str_detect(type_score, "summary")~"summary_scores", 
                               str_detect(ConceptName,"COUNTER")~"counter",
                               str_detect(ConceptName, "RECORDED|RECORED|RECORDED ")~"metadata", 
                               str_detect(ConceptName, "mean|MEAN")~"scales_scores", 
                               str_detect(type_score, "functioning")~"items",
                               TRUE~"unclear")) %>% 
  #Identifying type of reporting 
  mutate(type_reporting=case_when(type_score=="total_summary"~NA_character_, 
                                  str_detect(ConceptName, "CSCF|CPF|CEF|CSF|C SCORE")~"child", 
                                  str_detect(ConceptName,"NF")~"proxy", 
                                  !is.na(type_score)~"proxy", 
                                  str_detect(ConceptName,paste0(gsub("PedsQL","",metric_age),"C"))~"child",
                                  TRUE~NA_character_)) %>% 
  #Identifying type of metric 
  mutate(type_score=case_when(!is.na(type_score)~ type_score,
                              type_metric=="metadata"~ "metadata", 
                              TRUE~ "unclear")) %>% 
  #Identifying date of questionnaire for the order 
  mutate(takeninstant=ifelse(str_detect(ConceptDataType,"Instant"),StringValue,NA)) %>% 
  group_by(project_id, start_datetime) %>%
  fill(takeninstant, .direction = "updown") %>% 
  mutate(takeninstant=ifelse(is.na(takeninstant),start_datetime,takeninstant))


#Check type of metrics 
type_metrics_missing<-pedsql %>%
  ungroup() %>%
  select(ConceptName,metric_type_desc:type_reporting) %>%
  distinct() %>%
 #filter(type_metric=="unclear"|type_reporting=="unclear"|type_score=="unclear")
 filter(is.na(type_metric)|is.na(type_reporting)|is.na(type_score))

#Different type of scores available, some are unclear/unknown 
table(pedsql$type_score)

#Identifying type of reporting 
pedsql<-pedsql %>% 
  group_by(project_id, takeninstant) %>%
  #Imputing type of reporting and type of questionnaire 
  mutate(type_reporting = case_when(
    # If pt has NA and only 'proxy' exists in type of reporting, replace NA with 'proxy'
    all(type_reporting == 'proxy' | is.na(type_reporting)) ~ 'proxy',
    # If pt has NA and  only 'child' exists in type of reporting, replace NA with 'child'
    all(type_reporting == 'child' | is.na(type_reporting)) ~ 'child',
    # If pt has NA and the metric age is only 2-4, then replace NA with 'proxy'
    all(metric_age== "PedsQL 2-4" | is.na(type_reporting))~ "proxy",
    # If both 'parent' and 'child' exist, leave NA as is
    TRUE ~ type_reporting
  )) %>% 
  mutate(questionnaire_type=paste0(type_reporting,"_",str_trim(metric_age)))


#Check the type of scores available for each metric age and type of reporting 
check_type_scores<-pedsql %>% 
  ungroup() %>% 
  select(metric_age, ConceptName, type_score, type_metric,type_reporting) %>% 
  distinct() %>% 
  pivot_wider(id_cols=c(type_metric,ConceptName,type_reporting),names_from=metric_age, values_from=type_score) %>% 
  arrange(type_metric, type_reporting)

type_reporting<-pedsql %>% 
  ungroup() %>% 
  select(project_id, type_reporting) %>% 
  distinct() %>% 
  group_by(type_reporting) %>%
  #filter(any(is.na(type_reporting))) %>% 
  summarise(count=n_distinct(project_id))

write.csv(type_reporting, here(results_folder,'reporting_type.csv'))

#Unique patients by type of questionnaire and type of reporting 
type_reporting_questionnaire<-pedsql %>% 
  ungroup() %>% 
  select(project_id, metric_age, type_reporting) %>% 
  distinct() %>% 
  group_by(metric_age, type_reporting) %>% 
  summarise(count=n_distinct(project_id))

write.csv(type_reporting_questionnaire, here(results_folder,'metric_age_type_reporting.csv'))

plot<-type_reporting_questionnaire %>% 
  ggplot(aes(x=metric_age, y=count, fill=type_reporting))+
  geom_col(position="dodge")+
  facet_grid(cols=vars(type_reporting), scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y="number of unique patients")

plot

#Order of questionnaire by type of reporting and type of questionnaire 
order_of_questionnaires<-pedsql %>% 
  ungroup() %>% 
  select(project_id, start_datetime, takeninstant, questionnaire_type) %>% 
  distinct() %>% 
  mutate(date1=str_sub(start_datetime,1,10),
         date2=str_sub(takeninstant,1,10),
         check_date=date1==date2)%>% 
  filter(check_date==TRUE) %>% 
  group_by(project_id,questionnaire_type) %>% 
  mutate(order_of_questionnaires=dense_rank(date1)) %>% 
  mutate(order_label=ifelse(order_of_questionnaires==1,"initial","followup")) %>% 
  group_by(order_label,questionnaire_type) %>% 
  summarise(count=n())

write.csv(order_of_questionnaires, here(results_folder,'order_questionnaires.csv'))


type_scales<-pedsql %>%
  ungroup() %>%
  filter(type_metric=="scales_scores") %>%
  select(project_id, type_score, type_reporting, metric_age) %>%
  distinct() %>%
  group_by(type_score, type_reporting,metric_age) %>%
  summarise(count=n_distinct(project_id))

write.csv(type_scales, here(results_folder,'type_scales.csv'))



dt_clean<-pedsql %>%
  #Remove those without type of questionnaire 
  filter(!is.na(metric_age)) %>% 
  #Remove those without type of reporting 
  filter(!is.na(type_reporting)) %>% 
  #Try and work out the true date of the questionnaire 
   mutate(date1=str_sub(start_datetime,1,10),
         date2=str_sub(takeninstant,1,10),
         check_date=date1==date2)%>% 
  filter(check_date==TRUE) %>%
  group_by(project_id,questionnaire_type) %>%
  #Try and work out the order of questionnaire 
  mutate(order_of_questionnaires=dense_rank(date1)) %>% 
  #Create and assign questionnaire ID
  mutate(questionnaire_id=paste0(project_id,"_",order_of_questionnaires,"_",type_reporting,"_",metric_age)) %>% 
  group_by(project_id) %>%
  mutate(number_questionnaires=n_distinct(questionnaire_id)) %>%
  ungroup()


#Cannot attribute which metrics belong to which questionnaires, so cannot check the validty of the questionnaire 
