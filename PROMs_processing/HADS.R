rm(list=ls())

# Setup -------------------------------------------------------------------

pacman::p_load(here,dplyr, 
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

# Processing -------------------------------------------------------

# Step 1: Identify PROMs  -------------------------------------------------
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

nrow(hads_flowsheet)
nrow(hads_smartdata)

#Only in flowsheets 
hads<-hads_flowsheet

rm(list=c("hads_flowsheet", "hads_smartdata"))


metrics<-hads %>% 
  select(DisplayName) %>% 
  distinct()

write.csv(metrics, here(results_folder,'metrics_hads.csv'))

denom_start<-hads %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  summarise(n_uniq_patients=n_distinct(project_id), 
            n_uniq_obs=n_distinct(obs_id), 
            n_records=n())

denom_start

write.csv(denom_start, here(results_folder,'hads_step_1.csv'))


# Step 2 ------------------------------------------------------------------

# Order of questionnaire ---------------------------------------------------
hads2a<-hads %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(time=str_trim(substr(start_datetime,11,nchar(start_datetime)))) %>% 
  mutate(date_time=ymd_hms(start_datetime)) %>% 
  group_by(project_id) %>% 
  mutate(order=dense_rank(date))

order_of_observations<- hads2a %>% 
  select(project_id, order) %>% 
  distinct() %>% 
  mutate(order_of_obs=ifelse(order==1,"initial","follow-up")) %>% 
  group_by(order_of_obs) %>% 
  summarise(count=n())

order_of_observations

write.csv(order_of_observations, here(results_folder,'hads_step2_order_report.csv'))

hads_order<-order_of_observations %>% 
  mutate(order_label=factor(order_of_obs,levels=c("initial","follow-up"),labels=c("Initial","Follow-up"))) %>% 
  #mutate(type="CYP/Proxy") %>% 
  ggplot(aes(x = order_label,y=count, group=order_label, fill=order_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size=3) +
  scale_y_continuous(limits=c(0,300))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  labs(title  = "HADS", y = "Number of observations",x="")+
  theme_minimal()+
  theme_minimal()+
  theme(legend.position = "none",legend.title = element_blank(), 
        #axis.text.x=element_text(angle=30, hjust=1), 
        axis.title.y = element_text(size=8))


hads_order

#Flag patients with multiple times on the same date
hads2b<-hads2a %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  group_by(obs_id) %>% 
  mutate(flag_multiple_times=ifelse(n_distinct(time)>1,1,0)) %>% 
  group_by(project_id,date) %>% 
  mutate(order_times=dense_rank(date_time))

table(hads2b$flag_multiple_times)

table(hads2b$order_times)

hads_duplicate_times<-hads2b %>%
  ungroup() %>%
  mutate(multiple_times=ifelse(flag_multiple_times==1,"Multiple time stamps on the same date","Only one time stamp on the same date")) %>%
  select(project_id, date, multiple_times) %>%
  distinct() %>%
  group_by(multiple_times) %>%
  summarise(count=n())

hads_duplicate_times 

write.csv(hads_duplicate_times, here(results_folder,'hads_step_1_dup_times.csv'))


# Type of metric, reporting and version -----------------------------------

#Remove any duplicate scores by observations 

hads2b<-hads2b %>% 
  group_by(obs_id,DisplayName,type,Value) %>% 
  mutate(num_items_and_scores=n(),
         order_values=row_number()) %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  


table(hads2b$order_values)

hads_duplicate_scores<-hads2b %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values")) %>% 
  distinct(duplicates) %>% 
  group_by(duplicates) %>% 
  summarise(count=n())

write.csv(hads_duplicate_scores, here(results_folder,'hads_step_2_dup_scores.csv'))


hads_dup_scores<-hads2b %>% 
  ungroup() %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  %>% 
  group_by(duplicates) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = duplicates, y=count, fill=duplicates)) +
  geom_col(fill=thesis_colours[1:2]) +
  geom_text(aes(label=count),vjust=-0.5)+
  scale_y_continuous(limits=c(0,5500))+
  labs(title = "HADS", y = "Number of records", x="") +
  theme_minimal()+
  theme(axis.title.y=(element_text(size=8)))

hads_dup_scores

#Remove duplicate scores 
hads2c<-hads2b %>% 
  ungroup() %>% 
  filter(order_values==1)

#Metric, type of reporting and version

hads2<-hads2c %>% 
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
  mutate(metric=case_when(str_detect(DisplayName, "Score")~ "score", 
                          TRUE~"item"))


data_dictionary_hads<-hads2 %>%
  ungroup() %>%
  select(subscale,metric,DisplayName) %>%
  distinct()

write.csv(data_dictionary_hads, here(results_folder,'hads_metrics_recode.csv'))

type_report<-hads2 %>% 
  mutate(type_report="CYP") %>% 
  group_by(type_report) %>% 
  summarise(num_observations=n_distinct(obs_id)) 

type_report

write.csv(type_report, here(results_folder,'hads_step2_type_report.csv'))

  
hads_type_report<-type_report %>% 
  ggplot(aes(y = num_observations, x=type_report, fill=type_report)) +
  geom_col() +
  scale_fill_manual(values = type_report_colour)+
  geom_text(aes(label=num_observations), hjust=-0.5, size=4)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.5))  # add headroom for text
  ) +
  labs(title = "HADS", y = "", x="") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")

hads_type_report

# Step 3a: Create and assign questionnaire ID ------------------------------

hads3<-hads2 %>% 
  group_by(project_id) %>% 
  mutate(type_report="CYP") %>% 
  mutate(order=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order,"_",type_report))

# Step 3b: Attribute questionnaire ID and validate metrics/questionnaire -------

hads3<-hads3 %>% 
  left_join(demog %>% #Add demographics
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name)) %>% 
  ungroup() %>% 
  mutate(date=substr(start_datetime,1,10)) %>% 
  mutate(start=lubridate::ymd(birth_date), 
         end=lubridate::ymd(date), 
         age=round(lubridate::time_length(difftime(end, start), "years"))) 


#Number of patients 

denom<-hads3 %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())
denom

write.csv(denom, here(results_folder,'hads_after_processing.csv'))


#Number of questionnaires by type of reporting

hads_quests_type<-hads3 %>%
  ungroup() %>% 
  select(questionnaire_id, type_report) %>% 
  distinct() %>%
  group_by(type_report) %>% 
  summarise(unique_questionnaire=n()) %>% 
  ggplot(aes(x = type_report,y=unique_questionnaire, group=type_report, fill=type_report)) +
  geom_col(position="dodge") +
  scale_y_continuous(limits=c(0,350))+
  scale_fill_manual(values = type_report_colour) +
  geom_text(aes(label=unique_questionnaire), vjust=-0.5, size=3)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(title  = paste0("HADS (n = ",denom$unique_patients,")") , y = "Number of questionnaires",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        legend.position="none")

hads_quests_type

# Data quality checks ----------------------------------------------------
#Flag duplicate metrics 
hads3 <-hads3 %>%
  group_by(questionnaire_id,DisplayName,type) %>%
  mutate(num_items=n()) %>%
  mutate(flag_keep=case_when(num_items==1~1,
                             TRUE~0)) %>%   #This is for those with differing values for the same metric
  group_by(questionnaire_id) %>%
  #If any of the metrics in that questionnaire are flagged to be a duplicate that we need to remove then flag all the metrics for that questionnaire
  #So that we can remove that questionnaire
  mutate(flag_dup_metrics=case_when(any(flag_keep==0) ~ 1,
                                    TRUE ~ 0))

length(unique(hads3$questionnaire_id[hads3$flag_dup_metrics==1]))

quest_with_dup_metrics<-hads3 %>% 
  select(questionnaire_id, flag_dup_metrics) %>% 
  distinct() %>% 
  group_by(flag_dup_metrics) %>% 
  summarise(count=n()) %>% 
  mutate(flag_dup_metrics=ifelse(flag_dup_metrics==1,"With duplicate metrics", "No duplicate metrics")) %>% 
  full_join(hads3 %>% 
              select(questionnaire_id) %>% 
              distinct() %>% 
              ungroup() %>% 
              summarise(count=n()) %>% 
              mutate(flag_dup_metrics="Overall"))


quest_with_dup_metrics

write.csv(quest_with_dup_metrics, here(results_folder,'hads3_step3_multiple_items.csv'))

#Check how many items in each questionnaire 
hads3<-hads3 %>% 
  group_by(questionnaire_id) %>% 
  mutate(num_items=n()) %>% 
  group_by(project_id, questionnaire_id) %>% 
  mutate(num_questionnaire_id=n_distinct(questionnaire_id))

hads3 %>% 
  select(questionnaire_id,num_items) %>%
  distinct() %>% 
  tabyl(num_items)

hads3 %>% 
  select(questionnaire_id,num_items,flag_dup_metrics) %>%
  distinct() %>% 
  tabyl(num_items,flag_dup_metrics)

num_quest_items<-hads3 %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

write.csv(num_quest_items, here(results_folder,'hads3_step3_num_items.csv'))

num_quest_items

hads_num_items<-hads3 %>%
  ungroup() %>%
  select(questionnaire_id, num_items, flag_dup_metrics) %>%
  distinct() %>% 
  group_by(num_items, flag_dup_metrics) %>% 
  #summarise(count=n()) %>% 
  #mutate(num_items=as.factor(num_items)) %>% 
  mutate(dups=ifelse(flag_dup_metrics==0, "No duplicate metrics","With duplicate metrics")) %>% 
  full_join(hads3 %>%
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

label_map <- setNames(hads_num_items$dups_label, hads_num_items$dups)

hads_num_items_plot<-hads_num_items%>% 
  ggplot(aes(y=num_items, x=dups, fill=dups)) +
  geom_boxplot(outlier.shape = 4)+
  scale_x_discrete(labels = label_map)+
  scale_colour_manual(values=thesis_colours)+
  labs(title = "HADS",
       x = "",
       y = "") +
  #guides(colour = "none")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.text = element_text(size=8),
        #strip.text = element_text(size=6), 
        axis.text.x=element_text(size=8,angle=90), 
        axis.title.x=element_blank())


hads_num_items_plot

num_quest_items<-hads_num_items %>% 
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

write.csv(num_quest_items, here(results_folder,'hads_step3_num_items.csv'))


hads_type_scores<-hads3 %>%
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
  labs(title = "HADs",
       y = "Number of records", 
       x="Type of scores/values") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size=8),     # Remove x-axis title
    axis.text.x=element_text(size=8),   # Remove x-axis text
    axis.ticks.x = element_blank(), 
    legend.position = "none"
  )

hads_type_scores


# Save final data set -----------------------------------------------------

saveRDS(hads3, here('PROMs', 'clean_data','hads.rds'))
