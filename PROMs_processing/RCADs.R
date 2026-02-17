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

# Step 1: Identify PROMs  -------------------------------------------------
rcads_flowsheet<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "rcads"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

rcads_smartdata<-smart_data_elements %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "rcads"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

nrow(rcads_flowsheet)
nrow(rcads_smartdata)

#Only in flowsheets 
rcads<-rcads_flowsheet

rm(list=c("rcads_flowsheet", "rcads_smartdata"))

rcads_metrics<-rcads %>% 
  select(DisplayName,type) %>% 
  distinct() %>% 
  group_by(DisplayName) %>% 
  mutate(count=n()) %>% 
  group_by(type) %>% 
  mutate(count_type=n())

write.csv(rcads_metrics, here(results_folder,'rcads_uniq_metrics.csv'))

denom_start<-rcads %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  summarise(n_uniq_patients=n_distinct(project_id), 
            n_uniq_obs=n_distinct(obs_id), 
            n_records=n())

denom_start

write.csv(denom_start, here(results_folder,'rcads_step_1.csv'))


# Step 2 ------------------------------------------------------------------

# Order of questionnaire ---------------------------------------------------
rcads2a<-rcads %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(time=str_trim(substr(start_datetime,11,nchar(start_datetime)))) %>% 
  mutate(date_time=ymd_hms(start_datetime)) %>% 
  group_by(project_id) %>% 
  mutate(order=dense_rank(date))

order_of_observations<- rcads2a %>% 
  select(project_id, order) %>% 
  distinct() %>% 
  mutate(order_of_obs=ifelse(order==1,"initial","follow-up")) %>% 
  group_by(order_of_obs) %>% 
  summarise(count=n())

order_of_observations

write.csv(order_of_observations, here(results_folder,'rcads_step2_order_report.csv'))

rcads_order<-order_of_observations %>% 
  mutate(order_label=factor(order_of_obs,levels=c("initial","follow-up"),labels=c("Initial","Follow-up"))) %>% 
  #mutate(type="CYP/Proxy") %>% 
  ggplot(aes(x = order_label,y=count, group=order_label, fill=order_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size=3) +
  scale_y_continuous(limits=c(0,300))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  labs(title  = "RCADS", y = "Number of observations",x="")+
  theme_minimal()+
  theme_minimal()+
  theme(legend.position = "none",legend.title = element_blank(), 
        #axis.text.x=element_text(angle=30, hjust=1), 
        axis.title.y = element_text(size=8))


rcads_order

#Flag patients with multiple times on the same date
rcads2b<-rcads2a %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  group_by(obs_id) %>% 
  mutate(flag_multiple_times=ifelse(n_distinct(time)>1,1,0)) %>% 
  group_by(project_id,date) %>% 
  mutate(order_times=dense_rank(date_time))

table(rcads2b$flag_multiple_times)

table(rcads2b$order_times)

rcads_duplicate_times<-rcads2b %>%
  ungroup() %>%
  mutate(multiple_times=ifelse(flag_multiple_times==1,"Multiple time stamps on the same date","Only one time stamp on the same date")) %>%
  select(project_id, date, multiple_times) %>%
  distinct() %>%
  group_by(multiple_times) %>%
  summarise(count=n())

rcads_duplicate_times 

write.csv(rcads_duplicate_times, here(results_folder,'rcads_step_1_dup_times.csv'))

#Flag any duplicate scores by observations 

rcads2b<-rcads2b %>% 
  group_by(obs_id,DisplayName,type,Value) %>% 
  mutate(num_items_and_scores=n(),
         order_values=row_number()) %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  


table(rcads2b$order_values)

rcads_duplicate_scores<-rcads2b %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values")) %>% 
  distinct(duplicates) %>% 
  group_by(duplicates) %>% 
  summarise(count=n())

write.csv(rcads_duplicate_scores, here(results_folder,'rcads_step_2_dup_scores.csv'))


rcads_dup_scores<-rcads2b %>% 
  ungroup() %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  %>% 
  group_by(duplicates) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = duplicates, y=count, fill=duplicates)) +
  geom_col(fill=thesis_colours[1:2]) +
  geom_text(aes(label=count),vjust=-0.5)+
  scale_y_continuous(limits=c(0,28000))+
  labs(title = "RCADS", y = "Number of records", x="") +
  theme_minimal()+
  theme(axis.title.y=(element_text(size=8)))

rcads_dup_scores


#Remove duplicate scores 
rcads2c<-rcads2b %>% 
  ungroup() %>% 
  filter(order_values==1)

#Metric, type of reporting and version

rcads2<-rcads2c %>% 
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
    metric=case_when(str_detect(DisplayName, "Score")~ "score", 
                     !is.na(subscale)~"item", 
                     str_detect(DisplayName, "Scoring")~"score",
                     str_detect(DisplayName, "Gender")~"metadata",
                     str_detect(DisplayName, "School Year")~"metadata",
                     TRUE ~ NA_character_)) %>% 
  mutate(type_score=case_when(str_detect(DisplayName,"Raw Score")~ "raw", 
                              str_detect(DisplayName,"Scoring Row")~ "scoring_hide",
                              str_detect(DisplayName,"T Score")~ "t",
                              TRUE~metric)) %>% 
  mutate(subscale=case_when(!is.na(subscale)~subscale,
                            str_detect(DisplayName,"Obsessive|Obsession")~ "ocd", 
                            str_detect(DisplayName,"Panic Disorder|Panic")~ "panic_disorder",
                            str_detect(DisplayName,"Social Phobia")~ "soc_phobia",
                            str_detect(DisplayName,"Separation Anxiety")~ "sep_anxiety", 
                            str_detect(DisplayName,"Major Depression")~ "depression",
                            str_detect(DisplayName,"Generalised Anxiety")~ "gad",
                            str_detect(DisplayName,"Anxiety & Depression")~ "total_internalizing",
                            str_detect(DisplayName,"Total Anxiety")~ "total_anxiety",
                            str_detect(DisplayName, "Anxiety")~ "total_anxiety", 
                            str_detect(DisplayName, "Depression")~ "total_depression",
                            TRUE~"metadata")) 

rcads_data_dictionary<-rcads2 %>%
  ungroup() %>%
  select(DisplayName, type, subscale,type_report, metric, type_score) %>%
  distinct()

write.csv(rcads_data_dictionary, here(results_folder,'rcads_metrics_recode.csv'))

#Flag patients with missing reporting 
rcads2<-rcads2 %>%    
  group_by(obs_id) %>% 
  mutate(flag_miss_type=ifelse(any(is.na(type_report)),1,0))

length(unique(rcads2$obs_id[rcads2$flag_miss_type==1]))


#Report type of reporting 
type_report<-rcads2 %>% 
  select(obs_id, type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(num_observations=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "parent","child"), labels=c("Unknown", "Proxy","CYP")))  

write.csv(type_report, here(results_folder,'rcads_step2_type_report.csv'))

rcads_type_report<-type_report %>% 
  ggplot(aes(y = num_observations, x=type_label, fill=type_label)) +
  geom_col() +
  scale_fill_manual(values = type_report_colour)+
  geom_text(aes(label=num_observations), hjust=-0.5, size=4)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.5))  # add headroom for text
  ) +
  labs(title = "RCADS", y = "", x="") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")

rcads_type_report

# Step 3a: Create and assign questionnaire ID ------------------------------

rcads3<-rcads2 %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "parent","child"), labels=c("Unknown", "Proxy","CYP"))) %>% 
  group_by(project_id) %>% 
  mutate(order=dense_rank(date)) %>% 
  mutate(questionnaire_id=paste0(project_id,"_",order,"_",type_report))

# Step 3b: Attribute questionnaire ID and validate metrics/questionnaire-----------------------------

rcads3<-rcads3 %>% 
  filter(type_report!="NA") %>% 
  left_join(demog %>% #Add demographics
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name)) %>% 
  mutate(date=substr(start_datetime,1,10)) %>% 
  mutate(start=lubridate::ymd(birth_date), 
         end=lubridate::ymd(date), 
         age_questionnaire=round(lubridate::time_length(difftime(end, start), "years"))) %>% 
  mutate(age_check=ifelse(age_questionnaire<8|age_questionnaire>18,1,0)) 

table(rcads3$age_check)

rcads_type_quest<-rcads3 %>% 
  ungroup() %>% 
  select(questionnaire_id, type_report, age_check) %>% 
  distinct()

rcads_c1<-rcads_type_quest %>% 
  mutate(age_check=factor(ifelse(age_check==1,"wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check,type_report) %>% 
  mutate(count=n_distinct(questionnaire_id)) %>% 
  select(age_check, type_report,count) %>% 
  distinct() %>% 
  mutate(type_label=factor(type_report,levels=c("child","parent"), labels=c("CYP","Parent"))) %>% 
  ggplot(aes(x =type_label ,y=count, group=age_check, fill=age_check)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.2))  # add headroom for text
  ) +
  #facet_grid(cols=vars(age_check), scales="free_x") + 
  scale_fill_manual(values = thesis_colours[1:7]) +
  labs(title  = "RCADs", y = "",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="bottom",
        axis.title.y=element_text(size=8))

rcads_c1

rcads_type_quest_report<-rcads3 %>% 
  ungroup() %>% 
  select(questionnaire_id, type_report, age_check) %>% 
  distinct() %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check) %>% 
  summarise(count=n())

write.csv(rcads_type_quest_report, here(results_folder,'rcads_step3_type_quest_report.csv'))

rcads_type_quest_all<-rcads3 %>% 
  ungroup() %>% 
  select(questionnaire_id, type_report, age_check) %>% 
  distinct()

rcads_type_quest_all

rcads_quests_type<-rcads_type_quest_all %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check,type_report) %>% 
  mutate(count=n_distinct(questionnaire_id)) %>% 
  select(age_check, type_report,count) %>% 
  distinct() %>% 
  mutate(type_label=factor(type_report,levels=c("NA","child","parent","teacher"), labels=c("Unknown","CYP","Parent","Teacher"))) %>% 
  #mutate(quest_label=paste0(type_label,"",str_sub(type_quest,4))) %>% 
  ggplot(aes(x =type_label ,y=count, group=type_label, fill=type_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  facet_grid(cols=vars(age_check), scales="free_x") + 
  scale_fill_manual(values = type_report_colour) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.2))  # add headroom for text
  ) +
  labs(title  = "RCADS", y = "Number of unqiue questionnaires",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1, size=8),
        legend.position="none",
        axis.title.y=element_text(size=10))

rcads_quests_type


#Filter those with wrong type 
rcads4<-rcads3 %>% 
  filter(age_check==0) 


#Number of patients 

denom<-rcads4 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom


write.csv(denom, here(results_folder,'rcads_after_processing.csv'))


# Data quality checks ----------------------------------------------------

#Flag duplicate metrics 
rcads4 <-rcads4 %>%
  group_by(questionnaire_id,DisplayName,type) %>%
  mutate(num_items=n()) %>%
  mutate(flag_keep=case_when(num_items==1~1,
                             TRUE~0)) %>%   #This is for those with differing values for the same metric
  group_by(questionnaire_id) %>%
  #If any of the metrics in that questionnaire are flagged to be a duplicate that we need to remove then flag all the metrics for that questionnaire
  #So that we can remove that questionnaire
  mutate(flag_dup_metrics=case_when(any(flag_keep==0) ~ 1,
                                    TRUE ~ 0))

length(unique(rcads4$questionnaire_id[rcads4$flag_dup_metrics==1]))

quest_with_dup_metrics<-rcads4 %>% 
  select(questionnaire_id, flag_dup_metrics) %>% 
  distinct() %>% 
  group_by(flag_dup_metrics) %>% 
  summarise(count=n()) %>% 
  mutate(flag_dup_metrics=ifelse(flag_dup_metrics==1,"With duplicate metrics", "No duplicate metrics")) %>% 
  full_join(rcads4 %>% 
              select(questionnaire_id) %>% 
              distinct() %>% 
              ungroup() %>% 
              summarise(count=n()) %>% 
              mutate(flag_dup_metrics="Overall"))


quest_with_dup_metrics

write.csv(quest_with_dup_metrics, here(results_folder,'rcads4_step3_multiple_items.csv'))

#Check how many items in each questionnaire 
rcads4<-rcads4 %>% 
  group_by(questionnaire_id) %>% 
  mutate(num_items=n()) %>% 
  group_by(project_id, questionnaire_id) %>% 
  mutate(num_questionnaire_id=n_distinct(questionnaire_id))

rcads4 %>% 
  select(questionnaire_id,num_items) %>%
  distinct() %>% 
  tabyl(num_items)

rcads4 %>% 
  select(questionnaire_id,num_items,flag_dup_metrics) %>%
  distinct() %>% 
  tabyl(num_items,flag_dup_metrics)

num_quest_items<-rcads4 %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

write.csv(num_quest_items, here(results_folder,'rcads_step3_num_items.csv'))

num_quest_items

rcads_num_items<-rcads4 %>%
  ungroup() %>%
  select(questionnaire_id, num_items, flag_dup_metrics) %>%
  distinct() %>% 
  group_by(num_items, flag_dup_metrics) %>% 
  #summarise(count=n()) %>% 
  #mutate(num_items=as.factor(num_items)) %>% 
  mutate(dups=ifelse(flag_dup_metrics==0, "No duplicate metrics","With duplicate metrics")) %>% 
  full_join(rcads4 %>%
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

label_map <- setNames(rcads_num_items$dups_label, rcads_num_items$dups)

num_quest_items<-rcads_num_items %>% 
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

write.csv(num_quest_items, here(results_folder,'rcads_step3_num_items.csv'))


rcads_num_items_plot<-rcads_num_items%>% 
  ggplot(aes(y=num_items, x=dups, fill=dups)) +
  geom_boxplot(outlier.shape = 4)+
  scale_x_discrete(labels = label_map)+
  scale_colour_manual(values=thesis_colours)+
  labs(title = "RCADS",
       x = "",
       y = "") +
  #guides(colour = "none")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.text = element_text(size=8),
        #strip.text = element_text(size=6), 
        axis.text.x=element_text(size=8, angle=90), 
        axis.title.x=element_blank())


rcads_num_items_plot

rcads_type_scores<-rcads %>%
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
  labs(title = "RCADS",
       y = "Number of records", 
       x="Type of scores/values") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size=8),     # Remove x-axis title
    axis.text.x=element_text(size=8),   # Remove x-axis text
    axis.ticks.x = element_blank(), 
    legend.position = "none"
  )

rcads_type_scores


# Save final data set -----------------------------------------------------

saveRDS(rcads4, here('PROMs', 'clean_data','rcads.rds'))




