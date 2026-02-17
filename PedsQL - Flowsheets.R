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

pedsql_fl<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "pedsql"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)


metrics_flowsheets<-pedsql_fl %>% 
  select(TemplateName,DisplayName,type) %>% 
  distinct()


write.csv(metrics_flowsheets, here(results_folder,'metrics_pedsql_fl.csv'))

denom_start<-pedsql_fl %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  summarise(n_uniq_patients=n_distinct(project_id), 
            n_uniq_obs=n_distinct(obs_id), 
            n_records=n())

denom_start

write.csv(denom_start, here(results_folder,'pedsql_fl_step_1.csv'))

# Step 2 ------------------------------------------------------------------

#Order of questionnaires 
pedsql_fl2a<-pedsql_fl %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(time=str_trim(substr(start_datetime,11,nchar(start_datetime)))) %>% 
  mutate(date_time=ymd_hms(start_datetime)) %>% 
  group_by(project_id) %>% 
  mutate(order=dense_rank(date))

order_of_observations<- pedsql_fl2a %>% 
  select(project_id, order) %>% 
  distinct() %>% 
  mutate(order_of_obs=ifelse(order==1,"initial","follow-up")) %>% 
  group_by(order_of_obs) %>% 
  summarise(count=n())

order_of_observations

write.csv(order_of_observations, here(results_folder,'pedsql_fl_step2_order_report.csv'))

pedsql_fl_order<-order_of_observations %>% 
  mutate(order_label=factor(order_of_obs,levels=c("initial","follow-up"),labels=c("Initial","Follow-up"))) %>% 
  #mutate(type="CYP/Proxy") %>% 
  ggplot(aes(x = order_label,y=count, group=order_label, fill=order_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size=3) +
  scale_y_continuous(limits=c(0,200))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  labs(title  = "PedsQL- Flowsheets", y = "Number of observations",x="")+
  theme_minimal()+
  theme_minimal()+
  theme(legend.position = "none",legend.title = element_blank(), 
        #axis.text.x=element_text(angle=30, hjust=1), 
        axis.title.y = element_text(size=8))


pedsql_fl_order

#Flag patients with multiple times on the same date  
pedsql_fl2b<-pedsql_fl2a %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  group_by(obs_id) %>% 
  mutate(flag_multiple_times=ifelse(n_distinct(time)>1,1,0)) %>% 
  group_by(project_id,date) %>% 
  mutate(order_times=dense_rank(date_time))

table(pedsql_fl2b$flag_multiple_times)

table(pedsql_fl2b$order_times)

pedsql_fl_duplicate_times<-pedsql_fl2b %>%
  ungroup() %>%
  mutate(multiple_times=ifelse(flag_multiple_times==1,"Multiple time stamps on the same date","Only one time stamp on the same date")) %>%
  select(project_id, date, multiple_times) %>%
  distinct() %>%
  group_by(multiple_times) %>%
  summarise(count=n())

write.csv(pedsql_fl_duplicate_times, here(results_folder,'pedsql_fl_step_1_dup_times.csv'))

#For observation ID, use datetime not just date

pedsql_fl2b<-pedsql_fl2b %>% 
  mutate(obs_id=paste0(project_id,"_",date_time)) %>% 
  group_by(obs_id,type,Value) %>% 
  mutate(num_items_and_scores=n(),
         order_values=row_number()) %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  


table(pedsql_fl2b$order_values)

table(pedsql_fl2b$duplicates)

pedsql_fl_duplicate_scores<-pedsql_fl2b %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values")) %>% 
  group_by(duplicates) %>% 
  summarise(count=n())

pedsql_fl_duplicate_scores

write.csv(pedsql_fl_duplicate_scores, here(results_folder,'pedsql_fl_step_2_dup_scores.csv'))

pedsql_fl_dup_scores<-pedsql_fl2b %>% 
  ungroup() %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  %>% 
  group_by(duplicates) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = duplicates, y=count, fill=duplicates)) +
  geom_col() +
  scale_fill_manual(values=thesis_colours)+
  geom_text(aes(label=count),vjust=-0.5)+
  scale_y_continuous(limits=c(0,650))+
  labs(title = "PedsQL - Flowsheets", y = "Number of records", x="") +
  theme_minimal()+
  theme(axis.title.y=(element_text(size=8)), 
        legend.position="none")


pedsql_fl_dup_scores

#Remove duplicate scores 
pedsql_fl2c<-pedsql_fl2b %>% 
  ungroup() %>% 
  filter(order_values==1)

pedsql_fl2<-pedsql_fl2c %>% 
  #Identifying type of metric 
  mutate(subscale=case_when( str_detect(type, "PSY")~"psychosocial_summary",
                             str_detect(type,"SCF|SCH")~"school_functioning", 
                             str_detect(type, "PF|PHYS")~"physical_functioning",
                             str_detect(type, "EF|EMO")~ "emotional_functioning", 
                             str_detect(type, "SF|SOC")~"social_functioning",
                             str_detect(type, "TOTAL|total")~"total_summary", 
                             str_detect(type, "PHYS |PHYS")~ "physical_summary", 
                             str_detect(type, "NF|NURSERY")~"nursery",
                             TRUE~NA_character_)) %>% 
  mutate(metric=case_when(str_detect(type, "SCALE")~"scale_score", 
                          TRUE~"unclear"))
#type of reporting
pedsql_fl2<-pedsql_fl2 %>% 
  group_by(obs_id) %>%
  arrange() %>% 
  mutate(comment = na_if(comment, "")) %>% 
  tidyr::fill(comment, .direction = "updown") %>% 
  mutate(type_report=case_when(
    str_detect(tolower(comment), "parent|proxy|mother|father")~"proxy", 
    str_detect(tolower(comment),"self|child")~"CYP", 
    TRUE~NA_character_)) %>% 
  ungroup() 


#Age of reporting - type of questionnaire

pedsql_fl2<-pedsql_fl2 %>% 
  group_by(project_id, date_time) %>%
  arrange() %>% 
  mutate(type_quest=case_when(
    str_detect(type, "2-18")~"2-18", 
    str_detect(tolower(type),"infant")~"infant", 
    TRUE~NA_character_)) %>% 
  ungroup() 

table(pedsql_fl2$type_quest)

#Check the type of scores available for each type of questionnaire and reporting 
pedsql_fl2_data_dictionary<-pedsql_fl2 %>% 
  ungroup() %>% 
  select(type, comment, subscale, metric,type_report,type_quest) %>% 
  distinct() 

write.csv(pedsql_fl2_data_dictionary, here(results_folder,'pedsql_fl2_data_dictionary.csv'))

#Flag patients with missing type of reporting
pedsql_fl2<-pedsql_fl2 %>%    
  group_by(obs_id) %>% 
  mutate(flag_miss_type=ifelse(any(is.na(type_report)),1,0)) %>% 
  mutate(flag_miss_quest_type=ifelse(any(is.na(type_quest)),1,0))

length(unique(pedsql_fl2$obs_id[pedsql_fl2$flag_miss_type==1]))

length(unique(pedsql_fl2$obs_id[pedsql_fl2$flag_miss_quest_type==1]))

#Report type of reporting and questionnaire 
type_report<-pedsql_fl2 %>% 
  select(obs_id,type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(num_observations=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","CYP"), labels=c("Unknown", "Proxy","CYP"))) 


type_report 

pedsql_fl_type_report<-type_report %>% 
  ggplot(aes(y = num_observations, x=type_label, fill=type_label)) +
  geom_col() +
  scale_fill_manual(values = type_report_colour)+
  geom_text(aes(label=num_observations), hjust=-0.5, size=4)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(limits=c(0,200))+
  labs(title = "PedsQL - Smartdata", y = "Number of observations", x="") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")

pedsql_fl_type_report


#Type of questionnaire 
pedsql_fl_type_report_quest<-pedsql_fl2 %>% 
  select(obs_id,type_report, type_quest) %>% 
  distinct() %>% 
  group_by(type_report, type_quest) %>% 
  summarise(count=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","CYP"), labels=c("Unknown", "Proxy","CYP"))) %>% 
  #mutate(type_quest=str_sub(type_quest,start=7)) %>% 
  #mutate(group=str_trim(ifelse(type_report=="NA",paste0(type_label," ",type_quest,"*"),paste0(type_label," ",type_quest)))) %>% 
  mutate(group=str_trim(paste0(type_label," ",type_quest))) %>% 
  group_by(group) %>% 
  summarise(count=sum(count))

pedsql_fl_type_report_quest

type_report_colour_pedsql_fl<-c("Unknown 2-18"= "#56B4E9", 
                                "Unknown infant" = "#56B4E9",
                                "CYP 2-18"= "#D55E00", 
                                "Proxy 2-18"="#5D3A9B")

pedsql_fl_type_report_quest_plot<-pedsql_fl_type_report_quest %>% 
  ggplot(aes(x = group,y=count, group=group, fill=group)) +
  geom_col() +
  geom_text(aes(label=count), hjust=-0.5, size=4)+
  scale_fill_manual(values = type_report_colour_pedsql_fl) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.5))  # add headroom for text
  )+
  coord_flip()+
  labs(title  = "PedsQL - Flowsheets*", y = "",x="", 
       caption="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        legend.position = "none")


pedsql_fl_type_report_quest_plot

# Create and Assign questionnaire ID -----------------------------
pedsql_fl3<-pedsql_fl2 %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","CYP"), labels=c("Unknown", "Proxy","CYP"))) %>% 
  mutate(group=str_trim(paste0(type_label," ",type_quest))) %>% 
  group_by(project_id) %>%
  mutate(order=dense_rank(date)) %>% 
  #Create questionnaire id
  mutate(questionnaire_id=paste0(project_id,"_",order,"_",type_quest,"_",type_label)) 


pedsql_fl3<-pedsql_fl3 %>% 
  #mutate(missing_report_quest=ifelse(is.na(type_report)|is.na(type_quest),1,0)) %>% #Remove any missing type of reporting or type of questionnaire
  #filter(missing_report_quest==0) %>% 
  left_join(demog %>% #This adds demographics data 
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name)) %>% 
  mutate(start=lubridate::ymd(birth_date), 
         end=lubridate::ymd(date), 
         age_questionnaire=round(lubridate::time_length(difftime(end, start), "years"))) %>% 
  mutate(age_check=case_when(age_questionnaire>1&age_questionnaire<19&type_quest=="2-18"~ "2-18",
  age_questionnaire<=2& type_quest=="infant"~ "infant",
  TRUE~"wrong type"))

table(pedsql_fl3$age_check)

pedsql_fl_type_quests_report<-pedsql_fl3 %>% 
  ungroup() %>% 
  select(questionnaire_id,age_check) %>% 
  distinct() %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check) %>% 
  summarise(count=n())

pedsql_fl_type_quests_report

write.csv(pedsql_fl_type_quests_report, here(results_folder,'pedsql_fl_step3_type_quest_report.csv'))

denom<-pedsql_fl3 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom


pedsql_fl_type_quest_all<-pedsql_fl3 %>% 
  ungroup() %>% 
  select(questionnaire_id, group, age_check) %>% 
  distinct()

pedsql_fl_type_quest_all

type_report_colour_pedsql_fl<-c("Unknown 2-18"= "#56B4E9", 
                                "Unknown infant" = "#56B4E9",
                                "CYP 2-18"= "#D55E00", 
                                "Proxy 2-18"="#5D3A9B")


pedsql_fl_quests_type<-pedsql_fl_type_quest_all %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check,group) %>% 
  mutate(count=n_distinct(questionnaire_id)) %>% 
  select(age_check, group,count) %>% 
  distinct() %>% 
  ggplot(aes(x =group ,y=count, group=group, fill=group)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  facet_grid(cols=vars(age_check), scales="free_x") + 
  scale_fill_manual(values = type_report_colour_pedsql_fl) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.3))  # add headroom for text
  ) +
  labs(title  = paste0("PedsQL - Flowsheets (n = ",denom$unique_patients,")"),
       y = "",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1, size=8),
        legend.position="none",
        axis.title.y=element_text(size=10))

pedsql_fl_quests_type

#Filter those with wrong type 
pedsql_fl4<-pedsql_fl3 %>% 
  filter(age_check!="wrong type") 


#Number of patients 

denom<-pedsql_fl4 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom

write.csv(denom, here(results_folder,'pedsql_fl_after_processing.csv'))

# Data quality checks ----------------------------------------------------

#Flag duplicate metrics 
pedsql_fl4 <-pedsql_fl4 %>%
  group_by(questionnaire_id,DisplayName,type) %>%
  mutate(num_items=n()) %>%
  mutate(flag_keep=case_when(num_items==1~1,
                             TRUE~0)) %>%   #This is for those with differing values for the same metric
  group_by(questionnaire_id) %>%
  #If any of the metrics in that questionnaire are flagged to be a duplicate that we need to remove then flag all the metrics for that questionnaire
  #So that we can remove that questionnaire
  mutate(flag_dup_metrics=case_when(any(flag_keep==0) ~ 1,
                                    TRUE ~ 0))

length(unique(pedsql_fl4$questionnaire_id[pedsql_fl4$flag_dup_metrics==1]))

quest_with_dup_metrics<-pedsql_fl4 %>% 
  select(questionnaire_id, flag_dup_metrics) %>% 
  distinct() %>% 
  group_by(flag_dup_metrics) %>% 
  summarise(count=n()) %>% 
  mutate(flag_dup_metrics=ifelse(flag_dup_metrics==1,"With duplicate metrics", "No duplicate metrics")) %>% 
  full_join(pedsql_fl4 %>% 
              select(questionnaire_id) %>% 
              distinct() %>% 
              ungroup() %>% 
              summarise(count=n()) %>% 
              mutate(flag_dup_metrics="Overall"))


quest_with_dup_metrics

write.csv(quest_with_dup_metrics, here(results_folder,'pedsql_fl4_step3_multiple_items.csv'))

#Check how many items in each questionnaire 
pedsql_fl4<-pedsql_fl4 %>% 
  group_by(questionnaire_id) %>% 
  mutate(num_items=n()) %>% 
  group_by(project_id, questionnaire_id) %>% 
  mutate(num_questionnaire_id=n_distinct(questionnaire_id))

pedsql_fl4 %>% 
  select(questionnaire_id,num_items) %>%
  distinct() %>% 
  tabyl(num_items)

pedsql_fl4 %>% 
  select(questionnaire_id,num_items,flag_dup_metrics) %>%
  distinct() %>% 
  tabyl(num_items,flag_dup_metrics)

num_quest_items<-pedsql_fl4 %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

write.csv(num_quest_items, here(results_folder,'pedsql_fl_step3_num_items.csv'))

num_quest_items

pedsql_fl_num_items<-pedsql_fl4 %>%
  ungroup() %>%
  select(questionnaire_id, num_items, flag_dup_metrics) %>%
  distinct() %>% 
  group_by(num_items, flag_dup_metrics) %>% 
  #summarise(count=n()) %>% 
  #mutate(num_items=as.factor(num_items)) %>% 
  mutate(dups=ifelse(flag_dup_metrics==0, "No duplicate metrics","With duplicate metrics")) %>% 
  full_join(pedsql_fl4 %>%
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

label_map <- setNames(pedsql_fl_num_items$dups_label, pedsql_fl_num_items$dups)

num_quest_items<-pedsql_fl_num_items %>% 
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

write.csv(num_quest_items, here(results_folder,'pedsql_fl_step3_num_items.csv'))



pedsql_fl_num_items_plot<-pedsql_fl_num_items%>% 
  ggplot(aes(y=num_items, x=dups, fill=dups)) +
  geom_boxplot(outlier.shape = 4)+
  scale_x_discrete(labels = label_map)+
  scale_colour_manual(values=thesis_colours)+
  labs(title = "PedsQL - Flowsheets",
       x = "",
       y = "") +
  #guides(colour = "none")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.text = element_text(size=8),
        #strip.text = element_text(size=6), 
        axis.text.x=element_text(size=8, angle=90), 
        axis.title.x=element_blank())


pedsql_fl_num_items_plot

pedsql_fl_type_scores<-pedsql_fl %>%
  ungroup() %>%
  select(Value) %>%
  mutate(Value=case_when(!is.na(ymd_hms(Value))~"Date", 
                         Value=="" ~ "Blank",
                         !str_detect(Value, "\\d")~"Text",
                         str_length(Value) >= 10 ~ "Text",
                         TRUE~"Numeric")) %>% 
  group_by(Value) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = Value, y=count,fill=Value)) +
  #geom_boxplot(fill = thesis_colours[1]) +
  geom_col()+
  scale_fill_manual(values=thesis_colours)+
  labs(title = "pedsql_fl",
       y = "Number of metrics", 
       x="Type of scores/values") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size=8),     # Remove x-axis title
    axis.text.x=element_text(size=8),   # Remove x-axis text
    axis.ticks.x = element_blank(), 
    legend.position = "none"
  )

pedsql_fl_type_scores


# Save final data set -----------------------------------------------------

saveRDS(pedsql_fl4, here('PROMs', 'clean_data','pedsql_fl.rds'))





