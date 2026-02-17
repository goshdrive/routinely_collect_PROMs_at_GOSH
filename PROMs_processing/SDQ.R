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

type_report_colour_sdq<-c("Unknown"= "#56B4E9", 
                          "CYP 11-17"= "#D55E00", 
                          "Parent 2-4"="#5D3A9B", 
                          "Parent 4-17"="#CC79A7", 
                          "Teacher 4-17"="#117733", 
                          "Teacher 2-4"="#DDCC77")

# Data load ---------------------------------------------------------------

proms_folder<-paste0('PROMS_Data_','2024-11-05')

flowsheet_rows<-read.csv(here(proms_folder,'flowsheet_rows.csv'))

demog<-read.csv(here(proms_folder, 'demographics.csv'))

smart_data_elements<-read.csv(here(proms_folder, 'smart_data_elements.csv'))

# Processing -------------------------------------------------------

# Step 1: Identify PROMs  -------------------------------------------------
sdq_flowsheet<-flowsheet_rows %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "sdq"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

sdq_smartdata<-smart_data_elements %>% 
  mutate(include = ifelse(
    rowSums(across(everything(), ~ replace_na(str_detect(str_to_lower(.), "sdq"), FALSE))) > 0,
    1,
    0
  )) %>% 
  filter(include==1)

nrow(sdq_flowsheet)

nrow(sdq_smartdata)

#Only in flowsheets 
sdq<-sdq_flowsheet

rm(list=c("sdq_flowsheet", "sdq_smartdata"))

metrics<-sdq %>% 
  select(DisplayName) %>% 
  distinct()

write.csv(metrics, here(results_folder,'metrics_sdq.csv'))

denom_start<-sdq %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  summarise(n_uniq_patients=n_distinct(project_id), 
            n_uniq_obs=n_distinct(obs_id), 
            n_records=n())

denom_start

write.csv(denom_start, here(results_folder,'sdq_step_1.csv'))

# Step 2 ------------------------------------------------------------------


# Order of questionnaire --------------------------------------------------
sdq2a<-sdq %>% 
  mutate(date=str_trim(substr(start_datetime,1,10))) %>% 
  mutate(time=str_trim(substr(start_datetime,11,nchar(start_datetime)))) %>% 
  mutate(date_time=ymd_hms(start_datetime)) %>% 
  group_by(project_id) %>% 
  mutate(order=dense_rank(date))


order_of_observations<- sdq2a %>% 
  select(project_id, order) %>% 
  distinct() %>% 
  mutate(order_of_obs=ifelse(order==1,"initial","follow-up")) %>% 
  group_by(order_of_obs) %>% 
  summarise(count=n())

order_of_observations

write.csv(order_of_observations, here(results_folder,'sdq_step2_order_report.csv'))

sdq_order<-order_of_observations %>% 
  mutate(order_label=factor(order_of_obs,levels=c("initial","follow-up"),labels=c("Initial","Follow-up"))) %>% 
  #mutate(type="CYP/Proxy") %>% 
  ggplot(aes(x = order_label,y=count, group=order_label, fill=order_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size=3) +
  scale_y_continuous(limits=c(0,1200))+
  scale_fill_manual(values = thesis_colours[1:4]) +
  labs(title  = "SDQ", y = "Number of observations",x="")+
  theme_minimal()+
  theme_minimal()+
  theme(legend.position = "none",legend.title = element_blank(), 
        #axis.text.x=element_text(angle=30, hjust=1), 
        axis.title.y = element_text(size=8))


sdq_order

#Flag patients with multiple times on the same date  
sdq2b<-sdq2a %>% 
  mutate(obs_id=paste0(project_id,"_",date)) %>% 
  group_by(obs_id) %>% 
  mutate(flag_multiple_times=ifelse(n_distinct(time)>1,1,0)) %>% 
  group_by(project_id,date) %>% 
  mutate(order_times=dense_rank(date_time))

table(sdq2b$flag_multiple_times)

table(sdq2b$order_times)

sdq_duplicate_times<-sdq2b %>%
  ungroup() %>%
  mutate(multiple_times=ifelse(flag_multiple_times==1,"Multiple time stamps on the same date","Only one time stamp on the same date")) %>%
  select(project_id, date, multiple_times) %>%
  distinct() %>%
  group_by(multiple_times) %>%
  summarise(count=n())

write.csv(sdq_duplicate_times, here(results_folder,'sdq_step_1_dup_times.csv'))

# Step 2 - Type of metric, reporting and version ----------------------------------------------------------

#Flag any duplicate scores by observations 

sdq2b<-sdq2b %>% 
  group_by(obs_id,DisplayName,type,Value) %>% 
  mutate(num_items_and_scores=n(),
         order_values=row_number()) %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  


table(sdq2b$order_values)

table(sdq2b$duplicates)

sdq_duplicate_scores<-sdq2b %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values")) %>% 
  group_by(duplicates) %>% 
  summarise(count=n())

sdq_duplicate_scores

write.csv(sdq_duplicate_scores, here(results_folder,'sdq_step_2_dup_scores.csv'))

sdq_dup_scores<-sdq2b %>% 
  ungroup() %>% 
  mutate(duplicates=ifelse(order_values==1,"Unique scores/values","Duplicate scores/values"))  %>% 
  group_by(duplicates) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = duplicates, y=count, fill=duplicates)) +
  geom_col(fill=thesis_colours[1:2]) +
  geom_text(aes(label=count),vjust=-0.5)+
  scale_y_continuous(limits=c(0,60000))+
  labs(title = "SDQ", y = "Number of records", x="") +
  theme_minimal()+
  theme(axis.title.y=(element_text(size=8)))

sdq_dup_scores

#Remove duplicate scores 
sdq2c<-sdq2b %>% 
  ungroup() %>% 
  filter(order_values==1)

#Identify metrics, type of reporting, type of questionnaire 

sdq2<-sdq2c %>% 
  mutate(across(where(is.character), tolower)) %>%
  mutate(type_quest_template_name=case_when(str_detect(TemplateName,"4-17")~"SDQ 4-17", 
                                            str_detect(TemplateName,"2-4")~"SDQ 2-4", 
                                            str_detect(TemplateName,"11-17")~"SDQ 11-17")) %>% 
  mutate(type_quest_type=case_when(str_detect(type,"4-17")~"SDQ 4-17", 
                                   str_detect(type,"2-4")~"SDQ 2-4", 
                                   str_detect(type,"11-17")~"SDQ 11-17")) %>% 
  mutate(type_quest_display_name=case_when(str_detect(DisplayName,"4-17")~"SDQ 4-17", 
                                           str_detect(DisplayName,"2-4")~"SDQ 2-4", 
                                           str_detect(DisplayName,"11-17")~"SDQ 11-17")) %>% 
  mutate(type_quest=ifelse(!is.na(type_quest_template_name), type_quest_template_name, 
                           ifelse(!is.na(type_quest_type), type_quest_type,
                                  type_quest_display_name))) %>% 
  mutate(type_quest=str_trim(type_quest)) %>% 
  select(-c(type_quest_template_name, type_quest_type, type_quest_display_name))


sdq2<-sdq2 %>% 
  mutate(DisplayName=tolower(DisplayName)) %>% 
  mutate(subscale=case_when(str_detect(DisplayName,"peer")~ "peer", 
                            str_detect(DisplayName,"emotional")~ "emotional",
                            str_detect(DisplayName,"hyperactivity")~ "hyperactivity", 
                            str_detect(DisplayName,"conduct")~ "conduct", 
                            str_detect(DisplayName,"prosocial")~ "prosocial",
                            str_detect(DisplayName,"total")~ "total",
                            str_detect(DisplayName,"impact")~ "impact", 
                            TRUE~NA)) %>% 
  mutate(metric=case_when(DisplayName=="completed by"~ "metadata", 
                          DisplayName=="do you have any other questions or concerns?"~ "free text comments",
                          DisplayName=="start/end of treatment"~"metadata",
                          DisplayName=="has coming to clinic been helpful in other ways e.g. providing information or making the problems more bearable?"~"other",
                          DisplayName=="since coming to clinic are your child's problems"~"other",
                          DisplayName=="follow up patient: has coming to clinic been helpful in other ways e.g. providing information or making the problems more bearable?"~"other",
                          DisplayName=="follow up patient:since coming to clinic are your problems:"~"other",
                          is.na(subscale)~ "item", 
                          TRUE~ "score")) %>% 
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
  mutate(subscale=case_when(!is.na(subscale)~ subscale, 
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
  select(project_id:metric) 



sdq2<-sdq2 %>% 
  group_by(obs_id) %>% 
  mutate(type_quest=case_when(!is.na(type_quest)~type_quest, 
                              #If a patient has missing type of questionnaire but all their metrics 
                              #have 11-17 as the type, then make that missing 11-17 
                              all(type_quest %in% c("SDQ 11-17", NA), na.rm = TRUE) ~ "SDQ 11-17",
                              TRUE ~ NA_character_)) %>% 
  #Type of reporting 
  mutate(type=tolower(type)) %>% 
  mutate(type_report=case_when(str_starts(DisplayName,"i ")~ "child",
                               str_detect(DisplayName,"t4")~"teacher", 
                               str_detect(DisplayName,"t2")~"teacher", 
                               TemplateName=="t gosh sdq teacher"~"teacher", 
                               str_detect(DisplayName,"p4")~"parent", 
                               str_detect(DisplayName,"p2")~"parent",
                               str_detect(TemplateName,"p4")~"parent", 
                               str_detect(TemplateName,"p2")~"parent", 
                               str_detect(TemplateName,"p 2")~"parent", 
                               str_detect(type,"p4")~"parent", 
                               str_detect(type,"p2")~"parent", 
                               str_detect(type,"17p")~"parent", 
                               type_quest=="SDQ 11-17"~"child")) %>% 
  mutate(type_quest=ifelse(type_report=="child","SDQ 11-17",type_quest)) %>% 
  group_by(obs_id) %>% 
  #If patients have metrics with missing type of reporting but all their other metrics only had all parent/all teacher 
  #then make that missing parent/teacher - if patients have both parent and teacher then leave missing metrics as they are 
  mutate(type_report=case_when(!is.na(type_report)~type_report, 
                               all(type_report %in% c("parent", NA), na.rm = TRUE) ~ "parent", 
                               all(type_report %in% c("teacher", NA), na.rm = TRUE) ~ "teacher",
                               TRUE ~ NA_character_)) %>% 
  ungroup()

sdq_data_dictionary<-sdq2 %>%
  ungroup() %>%
  select(type_quest,type_report, metric, subscale,DisplayName,type,TemplateName) %>%
  distinct() 

write.csv(sdq_data_dictionary, here(results_folder,'sdq_metrics_recode.csv'))


#Flag patients with missing type of reporting
sdq2<-sdq2 %>%    
  group_by(obs_id) %>% 
  mutate(flag_miss_type=ifelse(any(is.na(type_report)),1,0)) %>% 
  mutate(flag_miss_quest_type=ifelse(any(is.na(type_quest)),1,0))

length(unique(sdq2$obs_id[sdq2$flag_miss_type==1]))

length(unique(sdq2$obs_id[sdq2$flag_miss_quest_type==1]))


#Report type of reporting and questionnaire 
type_report<-sdq2 %>% 
  select(obs_id,type_report) %>% 
  distinct() %>% 
  group_by(type_report) %>% 
  summarise(num_observations=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "parent","child","teacher"), labels=c("Unknown", "Parent","CYP","Teacher"))) 
  

type_report 


sdq_type_report<-type_report %>% 
  ggplot(aes(y = num_observations, x=type_label, fill=type_label)) +
  geom_col() +
  scale_fill_manual(values = type_report_colour)+
  geom_text(aes(label=num_observations), hjust=-0.5, size=4)+
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(limits=c(0,1200))+
  labs(title = "SDQ", y = "Number of observations", x="") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")

sdq_type_report

#Type of questionnaire 
sdq_type_report_quest<-sdq2 %>% 
  select(obs_id,type_report, type_quest) %>% 
  distinct() %>% 
  group_by(type_report, type_quest) %>% 
  summarise(count=n()) %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "parent","child","teacher"), labels=c("Unknown", "Parent","CYP","Teacher"))) %>% 
  mutate(type_quest=str_sub(type_quest,start=4)) %>% 
  mutate(group=ifelse(is.na(type_quest)|type_report=="NA","Unknown",paste0(type_label, type_quest))) %>% 
  group_by(group) %>% 
  summarise(count=sum(count))

sdq_type_report_quest

sdq_type_report_quest_plot<-sdq_type_report_quest %>% 
  ggplot(aes(x = group,y=count, group=group, fill=group)) +
  geom_col() +
  geom_text(aes(label=count), hjust=-0.5, size=4)+
  scale_fill_manual(values = type_report_colour_sdq) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.5))  # add headroom for text
  ) +
  coord_flip()+
  labs(title  = "SDQ", y = "",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        legend.position = "none")


sdq_type_report_quest_plot


# Step 3 - Create and Assign questionnaire ID -----------------------------
sdq3<-sdq2 %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "parent","child","teacher"), labels=c("Unknown", "Parent","CYP","Teacher"))) %>% 
  mutate(type_quest_label=str_sub(type_quest,start=4)) %>% 
  mutate(group=paste0(type_label, type_quest_label)) %>% 
  group_by(project_id) %>%
  mutate(order=dense_rank(date)) %>% 
  #Create questionnaire id
  mutate(questionnaire_id=paste0(project_id,"_",order,"_",type_label,"_",type_quest)) 


sdq3<-sdq3 %>% 
  mutate(missing_report_quest=ifelse(is.na(type_report)|is.na(type_quest)|type_report=="NA",1,0)) %>%
  filter(missing_report_quest==0) %>% 
  left_join(demog %>% #This adds demographics data 
              select(project_id, birth_date,sex_name,ethnicity_nat_code:religion_name) %>% 
              mutate(project_id=tolower(project_id))) %>% 
  mutate(start=lubridate::ymd(birth_date), 
         end=lubridate::ymd(date), 
         age_questionnaire=round(lubridate::time_length(difftime(end, start), "years"))) %>% 
  mutate(age_check=case_when(age_questionnaire>1&age_questionnaire<5&type_quest=="SDQ 2-4"~ "2-4", 
                             age_questionnaire>3&age_questionnaire<18& type_quest=="SDQ 4-17"~ "4-17", 
                             age_questionnaire>10&age_questionnaire<18&type_quest=="SDQ 11-17"~"11-17",
                             TRUE~"wrong type")) 


table(sdq3$age_check)

#Number of patients 

denom<-sdq3 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom

sdq_type_quests_report<-sdq3 %>% 
  ungroup() %>% 
  select(questionnaire_id,age_check) %>% 
  distinct() %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check) %>% 
  summarise(count=n())

sdq_type_quests_report

write.csv(sdq_type_quests_report, here(results_folder,'sdq_step3_type_quest_report.csv'))

sdq_type_quest_all<-sdq3 %>% 
  ungroup() %>% 
  select(questionnaire_id, type_report, type_quest, age_check) %>% 
  distinct()

sdq_type_quest_all


sdq_quests_type<-sdq_type_quest_all %>% 
  mutate(age_check=factor(ifelse(age_check=="wrong type","wrong_type","right_type"), 
                          levels=c("right_type","wrong_type"),labels=c("Matching age","Not matching age"))) %>% 
  group_by(age_check,type_report, type_quest) %>% 
  mutate(count=n_distinct(questionnaire_id)) %>% 
  select(age_check, type_report, type_quest,count) %>% 
  distinct() %>% 
  mutate(type_label=factor(type_report,levels=c("NA","child","parent","teacher"), labels=c("Unknown","CYP","Parent","Teacher"))) %>% 
  mutate(quest_label=paste0(type_label,"",str_sub(type_quest,4))) %>% 
  ggplot(aes(x =quest_label ,y=count, group=quest_label, fill=quest_label)) +
  geom_col(position="dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  facet_grid(cols=vars(age_check), scales="free_x") + 
  scale_fill_manual(values = type_report_colour_sdq) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.3))  # add headroom for text
  ) +
  labs(title  = paste0("SDQ (n = ",denom$unique_patients,")"), 
       y = "",x="")+
  theme_minimal()+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1, size=8),
        legend.position="none",
        axis.title.y=element_text(size=10))

sdq_quests_type


#Filter those with wrong type 
sdq4<-sdq3 %>% 
  filter(age_check!="wrong type") 


#Number of patients 

denom<-sdq4 %>% 
  ungroup() %>% 
  summarise(unique_patients=n_distinct(project_id),
            unique_questionnaires=n_distinct(questionnaire_id), 
            number_of_records=n())

denom


write.csv(denom, here(results_folder,'sdq_after_processing.csv'))

# Data quality checks ----------------------------------------------------

#Flag duplicate metrics 
sdq4 <-sdq4 %>%
  group_by(questionnaire_id,DisplayName,type) %>%
  mutate(num_items=n()) %>%
  mutate(flag_keep=case_when(num_items==1~1,
                             TRUE~0)) %>%   #This is for those with differing values for the same metric
  group_by(questionnaire_id) %>%
  #If any of the metrics in that questionnaire are flagged to be a duplicate that we need to remove then flag all the metrics for that questionnaire
  #So that we can remove that questionnaire
  mutate(flag_dup_metrics=case_when(any(flag_keep==0) ~ 1,
                                    TRUE ~ 0))

length(unique(sdq4$questionnaire_id[sdq4$flag_dup_metrics==1]))

quest_with_dup_metrics<-sdq4 %>% 
  select(questionnaire_id, flag_dup_metrics) %>% 
  distinct() %>% 
  group_by(flag_dup_metrics) %>% 
  summarise(count=n()) %>% 
  mutate(flag_dup_metrics=ifelse(flag_dup_metrics==1,"With duplicate metrics", "No duplicate metrics")) %>% 
  full_join(sdq4 %>% 
              select(questionnaire_id) %>% 
              distinct() %>% 
              ungroup() %>% 
              summarise(count=n()) %>% 
              mutate(flag_dup_metrics="Overall"))


quest_with_dup_metrics

write.csv(quest_with_dup_metrics, here(results_folder,'sdq4_step3_multiple_items.csv'))

#Check how many items in each questionnaire 
sdq4<-sdq4 %>% 
  group_by(questionnaire_id) %>% 
  mutate(num_items=n()) %>% 
  group_by(project_id, questionnaire_id) %>% 
  mutate(num_questionnaire_id=n_distinct(questionnaire_id))

sdq4 %>% 
  select(questionnaire_id,num_items) %>%
  distinct() %>% 
  tabyl(num_items)

sdq4 %>% 
  select(questionnaire_id,num_items,flag_dup_metrics) %>%
  distinct() %>% 
  tabyl(num_items,flag_dup_metrics)

num_quest_items<-sdq4 %>% 
  select(questionnaire_id, num_items) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(
    mean_value = mean(num_items),
    max_value = max(num_items), 
    min_value = min(num_items),
    median_value = median(num_items)
  )

write.csv(num_quest_items, here(results_folder,'sdq_step3_num_items.csv'))

num_quest_items

sdq_num_items<-sdq4 %>%
  ungroup() %>%
  select(questionnaire_id, num_items, flag_dup_metrics) %>%
  distinct() %>% 
  group_by(num_items, flag_dup_metrics) %>% 
  #summarise(count=n()) %>% 
  #mutate(num_items=as.factor(num_items)) %>% 
  mutate(dups=ifelse(flag_dup_metrics==0, "No duplicate metrics","With duplicate metrics")) %>% 
  full_join(sdq4 %>%
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

label_map <- setNames(sdq_num_items$dups_label, sdq_num_items$dups)

num_quest_items<-sdq_num_items %>% 
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

write.csv(num_quest_items, here(results_folder,'sdq_step3_num_items.csv'))


sdq_num_items_plot<-sdq_num_items%>% 
  ggplot(aes(y=num_items, x=dups, fill=dups)) +
  geom_boxplot(outlier.shape = 4)+
  scale_x_discrete(labels = label_map)+
  scale_colour_manual(values=thesis_colours)+
  labs(title = "SDQ",
       x = "",
       y = "") +
  #guides(colour = "none")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.text = element_text(size=8),
        #strip.text = element_text(size=6), 
        axis.text.x=element_text(size=8, angle=90), 
        axis.title.x=element_blank())


sdq_num_items_plot

sdq_type_scores<-sdq %>%
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
  labs(title = "SDQ",
       y = "", 
       x="") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size=8),     # Remove x-axis title
    axis.text.x=element_text(size=8),   # Remove x-axis text
    axis.ticks.x = element_blank(), 
    legend.position = "none"
  )

sdq_type_scores


# Save final data set -----------------------------------------------------

saveRDS(sdq4, here('PROMs', 'clean_data','sdq.rds'))



