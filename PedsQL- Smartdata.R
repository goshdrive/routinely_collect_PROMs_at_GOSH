rm(list=ls())

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

proms_folder<-paste0('PROMS_Data_','2024-11-05')

flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

demog<-read.csv(here(proms_folder, 'demographics.csv'))

smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

# Data processing ---------------------------------------------------------

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

pedsql_sd<-pedsql_smart_data

rm(pedsql_smart_data)
rm(pedsql_flowsheets)

metrics<-pedsql_sd %>% 
  select(ConceptName) %>% 
  distinct()

write.csv(metrics, here(results_folder,'metrics_smart_data_pedsql.csv'))

denom_start<-pedsql_sd %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  summarise(n_uniq_patients=n_distinct(project_id), 
            n_uniq_obs=n_distinct(obs_id), 
            n_records=n())

denom_start

write.csv(denom_start, here(results_folder,'pedsql_sd_step_1.csv'))

# Order of questionnaire --------------------------------------------------
pedsql_sd2a<-pedsql_sd %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(time=str_trim(substr(start_datetime,11,nchar(start_datetime)))) %>% 
  mutate(date_time=lubridate::ymd_hm(start_datetime)) %>% 
  group_by(project_id) %>%
  mutate(order=dense_rank(date))

order_of_observations<- pedsql_sd2a %>% 
  select(project_id, order) %>% 
  distinct() %>% 
  mutate(order_of_obs=ifelse(order==1,"initial","follow-up")) %>% 
  group_by(order_of_obs) %>% 
  summarise(count=n())

order_of_observations

write.csv(order_of_observations, here(results_folder,'pedsql_sd_step2_order_report.csv'))

pedsql_sd_order<-order_of_observations %>% 
  mutate(order_label=factor(order_of_obs,levels=c("initial","follow-up"),labels=c("Initial","Follow-up"))) %>% 
  #mutate(type="CYP/Proxy") %>% 
  ggplot(aes(x = order_label,y=count, group=order_label, fill=order_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size=3) +
  scale_y_continuous(limits=c(0,1700))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  labs(title  = "PedsQL - Smartdata", y = "Number of observations",x="")+
  theme_minimal()+
  theme_minimal()+
  theme(legend.position = "none",legend.title = element_blank(), 
        #axis.text.x=element_text(angle=30, hjust=1), 
        axis.title.y = element_text(size=8))


pedsql_sd_order

#Flag patients with multiple times on the same date  
pedsql_sd2b<-pedsql_sd2a %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  group_by(obs_id) %>% 
  mutate(flag_multiple_times=ifelse(n_distinct(time)>1,1,0)) %>% 
  group_by(project_id,date) %>% 
  mutate(order_times=dense_rank(date_time))

table(pedsql_sd2b$flag_multiple_times)

table(pedsql_sd2b$order_times)

pedsql_sd_duplicate_times<-pedsql_sd2b %>%
  ungroup() %>%
  mutate(multiple_times=ifelse(flag_multiple_times==1,"Multiple time stamps on the same date","Only one time stamp on the same date")) %>%
  select(project_id, date, multiple_times) %>%
  distinct() %>%
  group_by(multiple_times) %>%
  summarise(count=n())

write.csv(pedsql_sd_duplicate_times, here(results_folder,'pedsql_sd_step_1_dup_times.csv'))

#Flag any duplicate scores by observations 

pedsql_sd2b<-pedsql_sd2b %>% 
  group_by(obs_id,ConceptName,StringValue) %>% 
  mutate(num_items_and_scores=n(),
         order_values=row_number()) %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  


table(pedsql_sd2b$order_values)

table(pedsql_sd2b$duplicates)

pedsql_sd_duplicate_scores<-pedsql_sd2b %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values")) %>% 
  group_by(duplicates) %>% 
  summarise(count=n())


write.csv(pedsql_sd_duplicate_scores, here(results_folder,'pedsql_sd_step_2_dup_scores.csv'))


pedsql_sd_dup_scores<-pedsql_sd2b %>% 
  ungroup() %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  %>% 
  group_by(duplicates) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = duplicates, y=count, fill=duplicates)) +
  geom_col(fill=thesis_colours[1:2]) +
  geom_text(aes(label=count),vjust=-0.5)+
  scale_y_continuous(limits=c(0,120000))+
  labs(title = "PedsQL - Smartdata", y = "Number of records", x="") +
  theme_minimal()+
  theme(axis.title.y=(element_text(size=8)))


pedsql_sd_dup_scores

#Remove duplicate scores 
pedsql_sd2c<-pedsql_sd2b %>% 
  ungroup() %>% 
  filter(order_values==1)

#Metric, type of reporting and version
pedsql_sd2<-pedsql_sd2c %>% 
  #Identifying type of questionnaire 
  mutate(type_quest=case_when(str_detect(ConceptName,"5-7")~"PedsQL 5-7", 
                              str_detect(ConceptName,"2-4")~"PedsQL 2-4", 
                              str_detect(ConceptName,"8-12")~"PedsQL 8-12",
                              str_detect(ConceptName,"13-18")~"PedsQL 13-18")) %>% 
  #Identifying type of reporting
  mutate(metric_type_desc=str_sub(ConceptName, str_length(paste0("GOSH ", type_quest))+1)) %>% 
  #Identifying type of metric 
  mutate(subscale=case_when( str_detect(ConceptName, "PSY")~"psychosocial_summary",
                             str_detect(ConceptName,"SCF|SCH")~"school_functioning", 
                             str_detect(ConceptName, "PF|PHYS")~"physical_functioning",
                             str_detect(ConceptName, "EF|EMO")~ "emotional_functioning", 
                             str_detect(ConceptName, "SF|SOC")~"social_functioning",
                             str_detect(ConceptName, "TOTAL|total")~"total_summary", 
                             str_detect(ConceptName, "PHYS |PHYS")~ "physical_summary", 
                             str_detect(ConceptName, "NF|NURSERY")~"nursery",
                             TRUE~NA_character_)) %>% 
  mutate(metric=case_when(str_detect(subscale, "summary")~"summary_scores", 
                          str_detect(ConceptName,"COUNTER")~"counter",
                          str_detect(ConceptName, "RECORDED|RECORED|RECORDED ")~"metadata", 
                          str_detect(ConceptName, "mean|MEAN")~"scales_scores", 
                          str_detect(subscale, "functioning")~"items",
                          TRUE~"unclear")) %>% 
  #Identifying type of reporting 
  mutate(type_report=case_when(subscale=="total_summary"~NA_character_, 
                               str_detect(ConceptName, "CSCF|CPF|CEF|CSF|C SCORE")~"child", 
                               str_detect(ConceptName,"NF")~"proxy", 
                               !is.na(subscale)~"proxy", 
                               str_detect(ConceptName,paste0(gsub("PedsQL","",type_quest),"C"))~"child",
                               TRUE~NA_character_)) %>% 
  #Identifying type of metric 
  mutate(subscale=case_when(!is.na(subscale)~ subscale,
                            metric=="metadata"~ "metadata", 
                            TRUE~ "unclear")) 

#Identifying type of reporting 
pedsql_sd2<-pedsql_sd2 %>% 
  group_by(obs_id) %>%
  #Imputing type of reporting and type of questionnaire 
  mutate(type_report = case_when(
    # If pt has NA and only 'proxy' exists in type of reporting, replace NA with 'proxy'
    all(type_report == 'proxy' | is.na(type_report)) ~ 'proxy',
    # If pt has NA and  only 'child' exists in type of reporting, replace NA with 'child'
    all(type_report == 'child' | is.na(type_report)) ~ 'child',
    # If pt has NA and the metric age is only 2-4, then replace NA with 'proxy'
    all(type_quest== "PedsQL 2-4" | is.na(type_report))~ "proxy",
    # If both 'parent' and 'child' exist, leave NA as is
    TRUE ~ type_report
  )) %>% 
  mutate(questionnaire_type=paste0(type_report,"_",str_trim(type_quest)))


#Check the type of scores available for each type of questionnaire and reporting 
pedsql_data_dictionary<-pedsql_sd2 %>% 
  ungroup() %>% 
  select(type_quest, ConceptName, subscale, metric,type_report) %>% 
  distinct() 

write.csv(pedsql_data_dictionary, here(results_folder,'pedsql_sd_metrics_recode.csv'))

#Flag patients with missing type of reporting
pedsql_sd2<-pedsql_sd2 %>%    
  group_by(obs_id) %>% 
  mutate(flag_miss_type=ifelse(any(is.na(type_report)),1,0)) %>% 
  mutate(flag_miss_quest_type=ifelse(any(is.na(type_quest)),1,0))

length(unique(pedsql_sd2$obs_id[pedsql_sd2$flag_miss_type==1]))

length(unique(pedsql_sd2$obs_id[pedsql_sd2$flag_miss_quest_type==1]))


#Report type of reporting and questionnaire 
type_report<-pedsql_sd2 %>% 
  select(obs_id,type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(num_observations=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","child"), labels=c("Unknown", "Proxy","CYP"))) 


type_report




pedsql_sd_type_report<-type_report %>% 
  ggplot(aes(y = num_observations, x=type_label, fill=type_label)) +
  geom_col() +
  scale_fill_manual(values = type_report_colour)+
  geom_text(aes(label=num_observations), hjust=-0.5, size=4)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.3))  # add headroom for text
  ) +
  labs(title = "PedsQL - Smartdata", y = "", x="") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")

pedsql_sd_type_report


#Number of questionnaires by type of reporting

#Type of questionnaire 
pedsql_sd_type_report_quest<-pedsql_sd2 %>% 
  select(obs_id,type_report, type_quest) %>% 
  distinct() %>% 
  group_by(type_report, type_quest) %>% 
  summarise(count=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","child"), labels=c("Unknown", "Proxy","CYP"))) %>% 
  mutate(type_quest=str_sub(type_quest,start=7)) %>% 
  mutate(group=ifelse(is.na(type_quest)|type_report=="NA","Unknown",paste0(type_label,"",type_quest))) %>% 
  group_by(group) %>% 
  summarise(count=sum(count))

pedsql_sd_type_report_quest

type_report_colour_pedsql_sd<-c("Unknown"= "#56B4E9", 
                          "CYP 13-18"= "#D55E00", 
                          "CYP 5-7"="#5D3A9B", 
                          "CYP 8-12"="#CC79A7", 
                          "Proxy 13-18"="#117733", 
                          "Proxy 2-4"="#DDCC77", 
                          "Proxy 5-7"= "#44AA99", 
                          "Proxy 8-12"= "#882255")

pedsql_sd_type_report_quest_plot<-pedsql_sd_type_report_quest %>% 
  ggplot(aes(x = group,y=count, group=group, fill=group)) +
  geom_col() +
  geom_text(aes(label=count), hjust=-0.5, size=4)+
  scale_fill_manual(values = type_report_colour_pedsql_sd) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.5))  # add headroom for text
  ) +
  coord_flip()+
  labs(title  = "PedsQL - Smartdata", y = "",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        legend.position = "none")


pedsql_sd_type_report_quest_plot

# Create and Assign questionnaire ID -----------------------------
pedsql_sd3<-pedsql_sd2 %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","child"), labels=c("Unknown", "Proxy","CYP"))) %>% 
  mutate(type_quest2=str_sub(type_quest,start=7)) %>% 
  mutate(group=paste0(type_label,"",type_quest2)) %>% 
  group_by(project_id) %>%
  mutate(order=dense_rank(date)) %>% 
  #Create questionnaire id
  mutate(questionnaire_id=paste0(project_id,"_",order,"_",type_label,"_",type_quest)) 


pedsql_sd3<-pedsql_sd3 %>% 
  mutate(missing_report_quest=ifelse(type_report=="NA"|is.na(type_quest),1,0)) %>% #Remove any missing type of reporting or type of questionnaire
  filter(missing_report_quest==0) %>% 
  left_join(demog %>% #This adds demographics data 
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name)) %>% 
  mutate(start=lubridate::ymd(birth_date), 
         end=lubridate::ymd(date), 
         age_questionnaire=round(lubridate::time_length(difftime(end, start), "years"))) %>% 
  mutate(age_check=case_when(age_questionnaire>1&age_questionnaire<5&type_quest=="PedsQL 2-4"~ "2-4", 
                             age_questionnaire>4&age_questionnaire<8& type_quest=="PedsQL 5-7"~ "5-7", 
                             age_questionnaire>7&age_questionnaire<13& type_quest=="PedsQL 8-12"~ "8-12", 
                             age_questionnaire>12&age_questionnaire<=18&type_quest=="PedsQL 13-18"~"13-18",
                             TRUE~"wrong type")) 


table(pedsql_sd3$age_check)


#Number of patients 

denom<-pedsql_sd3 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom


pedsql_sd_type_quests_report<-pedsql_sd3 %>% 
  ungroup() %>% 
  select(questionnaire_id,age_check) %>% 
  distinct() %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check) %>% 
  summarise(count=n())

pedsql_sd_type_quests_report

write.csv(pedsql_sd_type_quests_report, here(results_folder,'pedsql_sd_step3_type_quest_report.csv'))

pedsql_sd_type_quest_all<-pedsql_sd3 %>% 
  ungroup() %>% 
  select(questionnaire_id, group, type_report, type_quest, age_check) %>% 
  distinct()

pedsql_sd_type_quest_all


pedsql_sd_quests_type<-pedsql_sd_type_quest_all %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check,group) %>% 
  mutate(count=n_distinct(questionnaire_id)) %>% 
  select(age_check, type_report, type_quest,count) %>% 
  distinct() %>% 
  ggplot(aes(x =group ,y=count, group=group, fill=group)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  facet_grid(cols=vars(age_check), scales="free_x") + 
  scale_fill_manual(values = type_report_colour_pedsql_sd) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.5))  # add headroom for text
  ) +
  labs(title  = paste0("PedsQL - Smartdata (n = ",denom$unique_patients,")"), 
       y = "",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1, size=8),
        legend.position="none",
        axis.title.y=element_text(size=10))

pedsql_sd_quests_type

#Filter those with wrong type 
pedsql_sd4<-pedsql_sd3 %>% 
  filter(age_check!="wrong type") 


#Number of patients 

denom<-pedsql_sd4 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom


write.csv(denom, here(results_folder,'pedsql_sd_after_processing.csv'))

# Data quality checks ----------------------------------------------------

#Flag duplicate metrics 
pedsql_sd4 <-pedsql_sd4 %>%
  group_by(questionnaire_id,ConceptName) %>%
  mutate(num_items=n()) %>%
  mutate(flag_keep=case_when(num_items==1~1,
                             TRUE~0)) %>%   #This is for those with differing values for the same metric
  group_by(questionnaire_id) %>%
  #If any of the metrics in that questionnaire are flagged to be a duplicate that we need to remove then flag all the metrics for that questionnaire
  #So that we can remove that questionnaire
  mutate(flag_dup_metrics=case_when(any(flag_keep==0) ~ 1,
                                    TRUE ~ 0))

length(unique(pedsql_sd4$questionnaire_id[pedsql_sd4$flag_dup_metrics==1]))

quest_with_dup_metrics<-pedsql_sd4 %>% 
  select(questionnaire_id, flag_dup_metrics) %>% 
  distinct() %>% 
  group_by(flag_dup_metrics) %>% 
  summarise(count=n()) %>% 
  mutate(flag_dup_metrics=ifelse(flag_dup_metrics==1,"With duplicate metrics", "No duplicate metrics")) %>% 
  full_join(pedsql_sd4 %>% 
              select(questionnaire_id) %>% 
              distinct() %>% 
              ungroup() %>% 
              summarise(count=n()) %>% 
              mutate(flag_dup_metrics="Overall"))


quest_with_dup_metrics

write.csv(quest_with_dup_metrics, here(results_folder,'pedsql_sd4_step3_multiple_items.csv'))

#Check how many items in each questionnaire 
pedsql_sd4<-pedsql_sd4 %>% 
  group_by(questionnaire_id) %>% 
  mutate(num_items=n()) %>% 
  group_by(project_id, questionnaire_id) %>% 
  mutate(num_questionnaire_id=n_distinct(questionnaire_id))

pedsql_sd4 %>% 
  select(questionnaire_id,num_items) %>%
  distinct() %>% 
  tabyl(num_items)

pedsql_sd4 %>% 
  select(questionnaire_id,num_items,flag_dup_metrics) %>%
  distinct() %>% 
  tabyl(num_items,flag_dup_metrics)

num_quest_items<-pedsql_sd4 %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

write.csv(num_quest_items, here(results_folder,'pedsql_sd_step3_num_items.csv'))

num_quest_items

pedsql_sd_num_items<-pedsql_sd4 %>%
  ungroup() %>%
  select(questionnaire_id, num_items, flag_dup_metrics) %>%
  distinct() %>% 
  group_by(num_items, flag_dup_metrics) %>% 
  #summarise(count=n()) %>% 
  #mutate(num_items=as.factor(num_items)) %>% 
  mutate(dups=ifelse(flag_dup_metrics==0, "No duplicate metrics","With duplicate metrics")) %>% 
  full_join(pedsql_sd4 %>%
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

label_map <- setNames(pedsql_sd_num_items$dups_label, pedsql_sd_num_items$dups)

num_quest_items<-pedsql_sd_num_items %>% 
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

write.csv(num_quest_items, here(results_folder,'pedsql_sd_step3_num_items.csv'))


pedsql_sd_num_items_plot<-pedsql_sd_num_items%>% 
  ggplot(aes(y=num_items, x=dups, fill=dups)) +
  geom_boxplot(outlier.shape = 4)+
  scale_x_discrete(labels = label_map)+
  scale_colour_manual(values=thesis_colours)+
  labs(title = "PedsQL - Smartdata",
       x = "",
       y = "") +
  #guides(colour = "none")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.text = element_text(size=8),
        #strip.text = element_text(size=6), 
        axis.text.x=element_text(size=8, angle=90), 
        axis.title.x=element_blank())


pedsql_sd_num_items_plot

pedsql_sd_type_scores<-pedsql_sd %>%
  ungroup() %>%
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
  scale_fill_manual(values=thesis_colours)+
  labs(title = "PedsQL - Smartdata",
       y = "", 
       x="") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size=8),     # Remove x-axis title
    axis.text.x=element_text(size=8),   # Remove x-axis text
    axis.ticks.x = element_blank(), 
    legend.position = "none"
  )

pedsql_sd_type_scores


# Save final data set -----------------------------------------------------

saveRDS(pedsql_sd4, here('PROMs', 'clean_data','pedsql_sd.rds'))





