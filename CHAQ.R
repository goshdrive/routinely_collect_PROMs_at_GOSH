rm(list=ls())

options(scipen=999)
# Setup -------------------------------------------------------------------

pacman::p_load(here, 
               dplyr, 
               tidyverse, 
               janitor, 
               lubridate,
               patchwork)

#create a new folder in your files depending on the current date for output
#update depending the date depending on the current date
results_folder<-here('PROMs', 'outputs','chapter5')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder, recursive = TRUE), print('Outputs directory already exists'))


thesis_colours<-c("#56B4E9","#D55E00","#5D3A9B","#CC79A7","#117733","#DDCC77","#44AA99","#882255")

type_report_colour<-c("Unknown"= "#56B4E9", 
                      "CYP"= "#D55E00", 
                      "Proxy"="#5D3A9B", 
                      "Parent"="#CC79A7", 
                      "Teacher"="#117733")


# Data load ---------------------------------------------------------------

#data location
proms_folder<-paste0('PROMS_Data_','2024-11-05')

flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

demog<-read.csv(here(proms_folder, 'demographics.csv'))

smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

# Processing -------------------------------------------------------


# Step 1: Identify PROMs --------------------------------------------------

chaq_flowsheet<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "chaq"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

length(unique(chaq_flowsheet$project_id))
#all the lengths should be less than this 


chaq_smart_data<-smart_data_elements %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "chaq"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

#CHAQ only in smart data 
chaq<-chaq_smart_data

rm(chaq_smart_data)
rm(chaq_flowsheet)

length(unique(chaq$project_id))
#all the lengths should be less than this 

metrics<-chaq %>% 
  select(ConceptName) %>% 
  distinct()

#write.csv(metrics, here(results_folder,'metrics_chaq.csv'))

denom_start<-chaq %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  summarise(n_uniq_patients=n_distinct(project_id), 
            n_uniq_obs=n_distinct(obs_id), 
            n_records=n())

denom_start

write.csv(denom_start, here(results_folder,'chaq_step_1.csv'))


# Step 2 ------------------------------------------------------------------

# Order of questionnaire --------------------------------------------------
chaq2a<-chaq %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(time=str_trim(substr(start_datetime,11,nchar(start_datetime)))) %>% 
  mutate(date_time=lubridate::ymd_hm(start_datetime)) %>% 
  group_by(project_id) %>%
  mutate(order=dense_rank(date))


order_of_observations<- chaq2a %>% 
  select(project_id, order) %>% 
  distinct() %>% 
  mutate(order_of_obs=ifelse(order==1,"initial","follow-up")) %>% 
  group_by(order_of_obs) %>% 
  summarise(count=n())

order_of_observations

write.csv(order_of_observations, here(results_folder,'chaq_step2_order_report.csv'))

chaq_order<-order_of_observations %>% 
  mutate(order_label=factor(order_of_obs,levels=c("initial","follow-up"),labels=c("Initial","Follow-up"))) %>% 
  #mutate(type="CYP/Proxy") %>% 
  ggplot(aes(x = order_label,y=count, group=order_label, fill=order_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size=3) +
  scale_y_continuous(limits=c(0,10000))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  labs(title  = "CHAQ", y = "Number of observations",x="")+
  theme_minimal()+
  theme_minimal()+
  theme(legend.position = "none",legend.title = element_blank(), 
        #axis.text.x=element_text(angle=30, hjust=1), 
        axis.title.y = element_text(size=8))


chaq_order

#Flag patients with multiple times on the same date  
chaq2b<-chaq2a %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  group_by(obs_id) %>% 
  mutate(flag_multiple_times=ifelse(n_distinct(time)>1,1,0)) %>% 
  group_by(project_id,date) %>% 
  mutate(order_times=dense_rank(date_time))

table(chaq2b$flag_multiple_times)

table(chaq2b$order_times)

chaq_duplicate_times<-chaq2b %>%
  ungroup() %>%
  mutate(multiple_times=ifelse(flag_multiple_times==1,"Multiple time stamps on the same date","Only one time stamp on the same date")) %>%
  select(project_id, date, multiple_times) %>%
  distinct() %>%
  group_by(multiple_times) %>%
  summarise(count=n())

write.csv(chaq_duplicate_times, here(results_folder,'chaq_step_1_dup_times.csv'))


# Step 2 - Type of metric, reporting and version ----------------------------------------------------------

#Flag any duplicate scores by observations 

chaq2b<-chaq2b %>% 
  group_by(obs_id,ConceptName,StringValue) %>% 
  mutate(num_items_and_scores=n(),
         order_values=row_number()) %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  


table(chaq2b$order_values)

table(chaq2b$duplicates)

chaq_duplicate_scores<-chaq2b %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values")) %>% 
  group_by(duplicates) %>% 
  summarise(count=n())


write.csv(chaq_duplicate_scores, here(results_folder,'chaq_step_2_dup_scores.csv'))


chaq_dup_scores<-chaq2b %>% 
  ungroup() %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  %>% 
  group_by(duplicates) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = duplicates, y=count, fill=duplicates)) +
  geom_col() +
  scale_fill_manual(values=thesis_colours)+
  geom_text(aes(label=count),vjust=-0.5)+
  scale_y_continuous(limits=c(0,300000))+
  labs(title = "CHAQ", y = "Number of records", x="") +
  theme_minimal()+
  theme(axis.title.y=(element_text(size=8)), 
        legend.position = "none")


chaq_dup_scores

#Remove duplicate scores 
chaq2c<-chaq2b %>% 
  ungroup() %>% 
  filter(order_values==1)

#Metric, type of reporting and version 

chaq2<-chaq2c %>%
  ungroup() %>% 
  #Identify metrics 
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
  mutate(metric=ifelse(str_detect(ConceptName, "recorded|recored|reported"),"metadata",ifelse(is.na(subscale),"item", "score"))) %>% 
  mutate(subscale=case_when(!is.na(subscale)~subscale,
                            str_detect(ConceptName,"d&p")~"dressing_and_personal_care", 
                            str_detect(ConceptName,"stand|bed")~"getting_up",
                            str_detect(ConceptName,"cut|lift|chaq open")~"eating",
                            str_detect(ConceptName,"walk|climb")~"walking", 
                            str_detect(ConceptName,"wash|bath|toilet|teeth|comb")~"hygiene",
                            str_detect(ConceptName,"reach overhead|bend|pull|turn")~"reach", #Need to change the reach overhead to be item 
                            str_detect(ConceptName,"write|jar|taps|turn|push|chaqs open")~"grip", 
                            str_detect(ConceptName,"errands| get in|ride|chores|play")~"activities",
                            str_detect(ConceptName, "chaq score simple")~"simple_score",
                            ConceptName=="chaqs"~"overall_score",
                            str_detect(ConceptName, "instant")~"metadata_date", 
                            str_detect(ConceptName, "patient or parent")~"metadata_reported_by",
                            str_detect(ConceptName, "ser name")~"metadata_recorded_by")) %>%                                                     
  mutate(metric=ifelse(str_detect(ConceptName,"reach overhead"),"item",ifelse(subscale=="overall_score","score",metric)))

#Create type of report metric for those that have it 
chaq2<-chaq2 %>% 
  mutate(type_report=case_when(subscale=="metadata_reported_by"&str_detect(tolower(StringValue),"and")~"both",
                               subscale=="metadata_reported_by"&str_detect(tolower(StringValue),c("mum|mother|sister|father"))~"proxy",
                               subscale=="metadata_reported_by"~"self", 
                               TRUE~NA_character_)) %>% 
  mutate(StringValue=ifelse(subscale=="metadata_reported_by",type_report,StringValue)) %>% 
  #mutate(questionnaire_id=paste0(project_id,start_datetime)) %>% 
  group_by(obs_id) %>% 
  #For any questionnaires that is not missing, fill out all the other metrics with that type of report, otherwise keep it blank 
  mutate(type_report = if (any(!is.na(type_report))) first(na.omit(type_report)) else NA) %>% 
  ungroup() %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","self","both"), labels=c("Unknown", "Proxy","CYP","Both")))  



data_dictionary<-chaq2 %>%
  ungroup() %>%
  select(subscale,metric, type_label, ConceptName) %>%
  distinct()

write.csv(data_dictionary, here(results_folder,'chaq_metrics_recode.csv'))

type_report<-chaq2 %>% 
  select(obs_id, type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(num_observations=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","self","both"), labels=c("Unknown", "Proxy","CYP","Both")))  

type_report

write.csv(type_report, here(results_folder,'chaq_step2_type_report.csv'))


chaq_type_report<-type_report %>% 
  ggplot(aes(y = num_observations, x=type_label, fill=type_label)) +
  geom_col() +
  scale_fill_manual(values = type_report_colour)+
  geom_text(aes(label=num_observations), hjust=-0.5, size=4)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.9))  # add headroom for text
  , breaks=c(0,10000,20000)) +
  labs(title = "CHAQ*", y = "", x="") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")

chaq_type_report


# Step 3 - Create and Assign questionnaire ID -----------------------------
chaq3<-chaq2 %>% 
  group_by(project_id) %>%
  mutate(order=dense_rank(date)) %>% 
  #Create questionnaire id
  mutate(questionnaire_id=paste0(project_id,"_",order,"_",type_label)) 

# Attribute questionnaire ID and validate metrics/questionnaire -------

chaq3<-chaq3 %>% 
  left_join(demog %>% #Add demographics
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name)) %>% 
  ungroup() %>% 
  mutate(date=substr(start_datetime,1,10)) %>% 
  mutate(start=lubridate::ymd(birth_date), 
         end=lubridate::ymd(date), 
         age=round(lubridate::time_length(difftime(end, start), "years"))) 

#Number of patients 

denom<-chaq3 %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

write.csv(denom, here(results_folder,'chaq_after_processing.csv'))

denom


#Number of questionnaires by type of reporting

chaq_quests_type<-chaq3 %>%
  ungroup() %>% 
  select(questionnaire_id, type_label) %>% 
  distinct() %>%
  group_by(type_label) %>% 
  summarise(unique_questionnaire=n()) %>% 
  ggplot(aes(x = type_label,y=unique_questionnaire, group=type_label, fill=type_label)) +
  geom_col(position="dodge") +
  scale_y_continuous(limits=c(0,15000))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  geom_text(aes(label=unique_questionnaire), vjust=-0.5, size=3)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(title  = paste0("CHAQ (n = ",denom$unique_patients,")") , y = "Number of questionnaires",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        legend.position="none")

chaq_quests_type

# Data quality checks ----------------------------------------------------

#Flag duplicate metrics 
chaq3 <-chaq3 %>%
  group_by(questionnaire_id,ConceptName) %>%
  mutate(num_items=n()) %>%
  mutate(should_be_duplicates=ifelse((tolower(ConceptName) %in% c("chaq aids&devices1","chaq aids&devices2","chaq aids&devices other",
                                                                  "chaq help1","chaq help2")),1,0)) %>%
  mutate(flag_keep=case_when(num_items==1~1,
                             should_be_duplicates==1~1,
                             TRUE~0)) %>%  #This is for those with differing values for the same metrics
  group_by(questionnaire_id) %>%
  #If any of the metrics in that questionnaire are flagged to be a duplicate that we need to remove then flag all the metrics for that questionnaire
  #So that we can remove that questionnaire
  mutate(flag_dup_metrics=case_when(any(flag_keep==0) ~ 1,
                                    TRUE ~ 0))

length(unique(chaq3$questionnaire_id[chaq3$flag_dup_metrics==1]))

quest_with_dup_metrics<-chaq3 %>% 
  select(questionnaire_id, flag_dup_metrics) %>% 
  distinct() %>% 
  group_by(flag_dup_metrics) %>% 
  summarise(count=n()) %>% 
  mutate(flag_dup_metrics=ifelse(flag_dup_metrics==1,"With duplicate metrics", "No duplicate metrics")) %>% 
  full_join(chaq3 %>% 
              select(questionnaire_id) %>% 
              distinct() %>% 
              ungroup() %>% 
              summarise(count=n()) %>% 
              mutate(flag_dup_metrics="Overall"))


quest_with_dup_metrics

write.csv(quest_with_dup_metrics, here(results_folder,'chaq_step3_multiple_items.csv'))

#Check how many items in each questionnaire 
chaq3<-chaq3 %>% 
  group_by(questionnaire_id) %>% 
  mutate(num_items=n()) %>% 
  group_by(project_id, questionnaire_id) %>% 
  mutate(num_questionnaire_id=n_distinct(questionnaire_id))

chaq3 %>% 
  select(questionnaire_id,num_items) %>%
  distinct() %>% 
  tabyl(num_items)

chaq3 %>% 
  select(questionnaire_id,num_items,flag_dup_metrics) %>%
  distinct() %>% 
  tabyl(num_items,flag_dup_metrics)

num_quest_items<-chaq3 %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

write.csv(num_quest_items, here(results_folder,'chaq_step3_num_items.csv'))

num_quest_items

chaq_num_items<-chaq3 %>%
  ungroup() %>%
  select(questionnaire_id, num_items, flag_dup_metrics) %>%
  distinct() %>% 
  group_by(num_items, flag_dup_metrics) %>% 
  #summarise(count=n()) %>% 
  #mutate(num_items=as.factor(num_items)) %>% 
  mutate(dups=ifelse(flag_dup_metrics==0, "No duplicate metrics","With duplicate metrics")) %>% 
  full_join(chaq3 %>%
              ungroup() %>%
              select(questionnaire_id, num_items) %>%
              distinct() %>% 
              group_by(num_items) %>% 
              mutate(dups="Overall")) %>% 
  left_join(quest_with_dup_metrics, by=join_by("dups"=="flag_dup_metrics")) %>% 
  mutate(dups=factor(dups, levels=c("No duplicate metrics", "With duplicate metrics", 
                                          "Overall"))) %>% 
  mutate(group=1) %>% 
  mutate(dups_label=paste0(dups, " (n = ",count,")")) 

label_map <- setNames(chaq_num_items$dups_label, chaq_num_items$dups)


num_quest_items<-chaq_num_items %>% 
  select(questionnaire_id,dups, num_items) %>% 
  distinct() %>% 
  group_by(dups) %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

num_quest_items

write.csv(num_quest_items, here(results_folder,'chaq_step3_num_items.csv'))


chaq_num_items_plot<-chaq_num_items%>% 
  ggplot(aes(y=num_items, x=dups, fill=dups)) +
  geom_boxplot(outlier.shape = 4)+
  scale_x_discrete(labels = label_map)+
  scale_colour_manual(values=thesis_colours)+
  labs(title = "CHAQ",
       x = "",
       y = "") +
  #guides(colour = "none")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.text = element_text(size=8),
        #strip.text = element_text(size=6), 
        axis.text.x=element_text(size=8, angle=90), 
        axis.title.x=element_blank())


chaq_num_items_plot

chaq_type_scores<-chaq3 %>%
  ungroup() %>%
  select(StringValue) %>%
  mutate(Value=case_when(!is.na(ymd_hms(StringValue))~"Date", 
                         StringValue=="" ~ "Blank",
                         !str_detect(StringValue, "\\d")~"Text",
                         str_length(StringValue) >= 10 ~ "Text",
                         TRUE~"Numeric")) %>% 
  group_by(Value) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = Value, y=count,fill=Value)) +
  #geom_boxplot(fill = thesis_colours[1]) +
  geom_col()+
  #facet_grid(row=vars(flag_dup_metrics), scales="free_y") +
  scale_fill_manual(values=thesis_colours)+
  labs(title = "CHAQ",
       y = "", 
       x="") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size=8),     # Remove x-axis title
    axis.text.x=element_text(size=8),   # Remove x-axis text
    axis.ticks.x = element_blank(), 
    legend.position = "none"
  )


chaq_type_scores


# Save final data set -----------------------------------------------------

saveRDS(chaq3, here('PROMs', 'clean_data','chaq.rds'))

