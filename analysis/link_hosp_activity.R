# install.packages('tidyverse')
rm(list=ls())

options(scipen=999)


#Setup -------------------------------------------------------------------
  
pacman::p_load(here, tidyverse,
               gtsummary, 
               lubridate, janitor, timeDate,
               GGally,ggraph,igraph,grid,scales,forcats, viridis,tidygraph,tidyverse,tibble,boot)
  

#create a new folder in your files depending on the current date for output
#update depending the date depending on the current date
results_folder<-here('PROMs', 'outputs', 'chapter6')

ifelse(!dir.exists(file.path(results_folder)), dir.create(results_folder), print('Outputs directory already exists'))

thesis_colours<-c("#56B4E9","#D55E00","#5D3A9B","#CC79A7","#117733","#DDCC77","#44AA99","#882255")


# Data load ---------------------------------------------------------------

proms_folder<-paste0('PROMS_Data_','2024-11-05')
#patient level 

demog<-read.csv(here(proms_folder, 'demographics.csv'))

#diag<-read.csv(here(proms_folder, 'diagnoses.csv'))

hosp_admi<-read.csv(here(proms_folder, 'hospital_admissions.csv'))

loc<-read.csv(here(proms_folder, 'location_factors.csv'))

op_activity<-read.csv(here(proms_folder, 'op_activity.csv'))

rcads<-readRDS(here('PROMs', 'clean_data','rcads.rds'))

sdq<-readRDS(here('PROMs', 'clean_data','sdq.rds'))

hads<-readRDS(here('PROMs', 'clean_data','hads.rds'))

pedsql_sd<-readRDS(here('PROMs', 'clean_data','pedsql_sd.rds'))

pedsql_fl<-readRDS(here('PROMs', 'clean_data','pedsql_fl.rds'))

chaq<-readRDS(here('PROMs', 'clean_data','chaq.rds'))

encounters_gosh<-readRDS(here('PROMs', 'clean_data','clean_encounters.rds'))

pats_proms<-readRDS(here('PROMs', 'clean_data','pats_proms.rds'))


# Outpatient encounters linkage -------------------------------------------


pats_with_proms<-unique(pats_proms$project_id)

op_activity_happened<-op_activity %>% 
  select(project_id:end_datetime, sptyp_name, attnd_nat_code, attnd_name, visit_name, spect_name, tfc_name) %>% 
  mutate(monthyear=format(as.Date(substr(start_datetime,1,10)),"%Y-%m")) %>% 
  mutate(project_id=tolower(project_id)) %>% 
  mutate(appt_cancelled_or_not_attend=case_when(str_detect(attnd_name,"cancelled")~1, 
                                       str_detect(attnd_name, "Did not attend")~1,
                                       str_detect(attnd_name, "could not be seen")~1, 
                                       TRUE~0))

table(op_activity_happened$appt_cancelled_or_not_attend)

op_activity_happened<-op_activity_happened %>% 
  filter(appt_cancelled_or_not_attend==0)

nrow(op_activity_happened)

op_activity_happened<-op_activity_happened %>% 
  filter(project_id %in% pats_with_proms) %>% 
  mutate(date_op=as.Date(start_datetime))

nrow(op_activity_happened)

check_appt_type<-op_activity_happened %>% 
  select(project_id,sptyp_name,visit_name) %>% 
  distinct() %>% 
  group_by(sptyp_name,visit_name) %>% 
  summarise(count=n())

nrow(op_activity_happened)

op_activity_short<-op_activity_happened %>% 
  select(project_id, date_op, monthyear, spect_name) %>% 
  distinct()

op_proms <- op_activity_short %>%
  right_join(pats_proms, by="project_id", relationship="many-to-many") %>% 
  mutate(
    # Ensure `date` and `date_op` are Date objects
    date = as.Date(date),
    date_op = as.Date(date_op),
    
    # Apply the rules
    rule1= date >= (date_op - days(14)) & date <= (date_op + days(14)),
    rule2= date >= (date_op %m-% days(28)) & date <= (date_op %m+% days(28)), 
    rule3= date >= (date_op %m-% months(3)) & date <= (date_op %m+% months(3)), 
    rule4= date >= (date_op %m-% months(6)) & date <= (date_op %m+% months(6)),
    
    #Create a variable to show which rules apply 
    type_link=case_when(rule1~"+/- 14 days", #date of PROM is +/- 14 days of date of appt 
                        rule2~"+/-28 days", # date of PROM is +/- 28 days of date of appt 
                        rule3~"+/-3 months", #date of PROM is +/- 3 months of date of appt 
                        rule4~"+/-within 6 months", #date of PROM is +/- 6 months of date of appt 
                        TRUE~"FALSE")) %>% 
  filter(type_link!=FALSE) %>%   #remove anything that has no link 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date))

nrow(op_proms)

#Check whether there are multiple appts within the same team that are linked to the same PROM 

check_multiple_appts<-op_proms %>% 
  group_by(obs_id, spect_name) %>% 
  mutate(count=n(), 
            order=row_number()) %>% 
  mutate(rule_true_count = rowSums(across(contains("rule"), ~ . == TRUE))) %>% 
  arrange(obs_id, spect_name) %>% 
  #filter(count>1) %>% 
  mutate(max_rule_flag = rule_true_count == max(rule_true_count)) %>%
  ungroup() 

table(check_multiple_appts$max_rule_flag)

#Only keep the the strictest link for each appt within the same team 

op_proms<-op_proms %>% 
  group_by(obs_id, spect_name) %>% 
  #Count the number of TRUE, the strictest linkage will have the most TRUE
  mutate(rule_true_count = rowSums(across(matches("^rule\\d+$"), ~ . == TRUE))) %>% 
  mutate(max_rule_flag = rule_true_count == max(rule_true_count)) %>%
  ungroup() %>% 
  filter(max_rule_flag==TRUE)

nrow(op_proms)


check_multiple_appts<-op_proms %>% 
  group_by(obs_id, spect_name) %>% 
  mutate(count=n(), 
         order=row_number()) %>% 
  mutate(rule_true_count = rowSums(across(matches("^rule\\d+$"), ~ . == TRUE))) %>% 
  mutate(prom_before=date<=date_op,
  date_diff=ifelse(prom_before,date_op-date,date-date_op)) %>% 
  arrange(obs_id, spect_name) %>% 
  mutate(min_date_flag = date_diff == min(date_diff)) %>%
  ungroup()

table(check_multiple_appts$min_date_flag)


#Only keep the appts that happened before or on the day of the PROM
op_proms<-op_proms %>% 
  group_by(obs_id, spect_name) %>% 
  mutate(count=n(), 
         order=row_number()) %>% #Number of duplicates is the same 
  mutate(prom_before=date<=date_op,
         date_diff=ifelse(prom_before,date_op-date,date-date_op)) %>% 
  arrange(obs_id, spect_name) %>% 
  group_by(obs_id, spect_name) %>% 
  mutate(min_date_flag = case_when(count==1~TRUE,
                          count>1&date_diff == min(date_diff)~TRUE)) %>%
  ungroup() %>% 
  filter(min_date_flag==TRUE)

check_multiple_appts<-op_proms %>% 
  group_by(obs_id, spect_name) %>% 
  mutate(count=n(), 
         order=row_number()) %>% 
  #filter(count>1) %>% 
  mutate(flag_duplicates=count>1, 
  flag_to_keep=flag_duplicates&prom_before) %>% 
  ungroup() %>% 
  arrange(obs_id,spect_name)

table(check_multiple_appts$flag_to_keep)

table(check_multiple_appts$count)

op_proms<-op_proms %>%
  group_by(obs_id, spect_name) %>%
  mutate(count=n(),
         order=row_number(),
         flag_duplicates=count>1) %>%
  mutate(flag_to_keep=case_when(count==1~TRUE,
                                flag_duplicates&prom_before~TRUE)) %>% 
  ungroup() %>%
  filter(flag_to_keep==TRUE)

length(unique(op_proms$project_id))
length(unique(op_proms$obs_id))

num_linked_obs<-op_proms %>% 
  select(obs_id,proms) %>% 
  group_by(proms) %>% 
  summarise(count_unique_observations=n_distinct(obs_id), 
            count_unique_encounters=n())

num_linked_obs

write.csv(num_linked_obs, here(results_folder,"num_linked_obs_op_proms.csv"))


#No more duplicates of the same team for the same PROM 

check_multiple_appts<-op_proms %>% 
  group_by(obs_id, spect_name) %>% 
  mutate(count=n(), 
         order=row_number()) 

table(check_multiple_appts$order)

length(unique(op_proms$project_id))

length(unique(op_proms$obs_id))


check_multiple_links<-op_proms %>% 
  group_by(obs_id) %>% 
  mutate(number_of_appointments_linked_to_questionnaire=n(), 
         order=row_number()) %>% 
  group_by(number_of_appointments_linked_to_questionnaire) %>% 
  summarise(total=n()) 

write.csv(check_multiple_links, here(results_folder, 'observations_with_multiple_links.csv'))

check_type_link<-op_proms %>%
  group_by(obs_id) %>% 
  mutate(count=n()) %>% 
  #Count the number of TRUE, the strictest linkage will have the most TRUE
  mutate(rule_true_count = rowSums(across(matches("^rule\\d+$"), ~ . == TRUE))) %>% 
  mutate(max_rule_flag = rule_true_count == max(rule_true_count)) %>%
  ungroup() 

table(check_type_link$rule_true_count, check_type_link$max_rule_flag)

table(check_type_link$max_rule_flag)

op_proms2<-op_proms %>%
  group_by(obs_id) %>% 
  #Count the number of TRUE, the strictest linkage will have the most TRUE
  mutate(rule_true_count = rowSums(across(matches("^rule\\d+$"), ~ . == TRUE))) %>% 
  mutate(max_rule_flag = rule_true_count == max(rule_true_count)) %>%
  ungroup() %>% 
  filter(max_rule_flag==TRUE) %>%  #Only keep the linkage with the strictest linkage 
  mutate(prom_before=date<=date_op,
         date_diff=ifelse(prom_before,date_op-date,date-date_op)) 
  
num_linked_obs_2<-op_proms2 %>% 
  select(obs_id,proms) %>% 
  group_by(proms) %>% 
  summarise(count_unique_observations=n_distinct(obs_id), 
            count_unique_encounters=n())

num_linked_obs_2

write.csv(num_linked_obs_2, here(results_folder,"num_linked_obs_op_proms2.csv"))

length(unique(op_proms$project_id))
length(unique(op_proms$obs_id))

#saveRDS(op_proms2, here('PROMs', 'clean_data','op_proms2.rds'))

#op_proms2<-readRDS(here('PROMs', 'clean_data','op_proms2.rds'))

#Visualise the lag

op_prom_linkage<-op_proms %>% 
  group_by(date_diff) %>% 
  summarise(count=n()) %>% 
  mutate(type_link="Standard linkage rules") %>% 
  full_join(op_proms2 %>% 
              group_by(date_diff) %>% 
              summarise(count=n()) %>% 
              mutate(type_link="Only keeping the strictest linkage")) %>% 
  mutate(type_link=factor(type_link, levels=c("Standard linkage rules","Only keeping the strictest linkage")))

count_op_proms_linkage <-op_prom_linkage %>% 
  pivot_wider(names_from=type_link,values_from=count) 

write.csv(count_op_proms_linkage, here(results_folder, 'days_lag.csv'))

p<-op_prom_linkage %>% 
  ggplot(aes(x = date_diff, y = count, colour=type_link)) +
  geom_line(linewidth = 1, alpha=0.5) +
  #geom_point(alpha = 0.5) +
  #coord_flip()+
  annotate("segment", x = 15, xend = 15, y = max(op_prom_linkage$count)*0.3,
           yend = max(op_prom_linkage$count)*0.09,
           arrow = arrow(length = unit(0.2, "inches")), colour = thesis_colours[3]) +
  annotate("text", x = 20, y = max(op_prom_linkage$count)*0.4,
           label = "Day 15 
           - point of variation", colour = thesis_colours[3])+
   scale_colour_manual(values=thesis_colours)+
  labs(
    title = "Number of days between encounters and observations",
    x = "Days difference",
    y = "Number of linked encounters", 
    colour="Type of linkage")+
  #facet_grid(rows=vars(proms), scales="free")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="bottom")

p
ggsave(here(results_folder,'days_lag.png'),plot = p, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")


length(unique(op_proms$project_id))
length(unique(op_proms$obs_id))

#Check if its the same 
length(unique(op_proms2$project_id))
length(unique(op_proms2$obs_id)) 

# Admissions linkage ------------------------------------------------------

hosp_admi<-hosp_admi %>% 
  select(project_id:end_datetime, patient_class, hospital_service, admission_type,expected_discharge_datetime) %>% 
  mutate(monthyear=format(as.Date(substr(start_datetime,1,10)),"%Y-%m")) %>% 
  mutate(project_id=tolower(project_id)) 

hosp_admi<-hosp_admi %>% 
  filter(project_id %in% pats_with_proms)

hosp_admi_proms <- hosp_admi %>%
  mutate(date_in=as.Date(start_datetime)) %>% 
  mutate(date_out=ifelse(expected_discharge_datetime!="",expected_discharge_datetime,end_datetime)) %>% 
  mutate(date_out=as.Date(date_out)) %>% 
  mutate(los=date_out-date_in) %>% 
  # Perform a cross join with df2 (merge by patient_id)
  inner_join(pats_proms, by = "project_id", relationship = "many-to-many") %>%
  rowwise() %>%
  mutate(
    # Ensure `date` and `date_op` are Date objects
    date_in = as.Date(date_in),
    date_out = as.Date(date_out),
    date = as.Date(date),
    
    # Apply the rules
    rule = case_when(
      los >= 1  ~ date >= date_in & date <= date_out,
      los == 0 ~ date == date_in & date == date_out,
      TRUE     ~ FALSE) 
  ) %>%
  # Filter rows where both rules are satisfied
  filter(rule) %>% 
  select(project_id, hospital_service:admission_type,monthyear:proms) %>% 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) 
  

length(unique(hosp_admi_proms$project_id))
length(unique(hosp_admi_proms$obs_id))

table(hosp_admi_proms$proms)

saveRDS(hosp_admi_proms, here('PROMs', 'clean_data','hosp_admi_proms.rds'))


num_linked_obs_admissions<-hosp_admi_proms %>% 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) %>% 
  select(obs_id,proms) %>% 
  group_by(proms) %>% 
  summarise(count_unique_observations=n_distinct(obs_id), 
            count_unique_encounters=n())

num_linked_obs_admissions

write.csv(num_linked_obs_admissions, here(results_folder,"num_linked_obs_admissions.csv"))



# Overall linkage ----------------------------------------------------------

#op_proms2<-readRDS(here('PROMs', 'clean_data','op_proms2.rds'))
#hosp_admi_proms<-readRDS(here('PROMs', 'clean_data','hosp_admi_proms.rds'))

#Type of linkage

pats_op_proms<-op_proms2 %>% 
  select(obs_id) %>%
  distinct() %>%
  mutate(op_link=1)

pats_hosp_proms<-hosp_admi_proms %>% 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) %>% 
  select(obs_id) %>%
  distinct() %>%
  mutate(admi_link=1)

pats_proms_link_check<-pats_proms %>% 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) %>% 
  left_join(pats_op_proms, by=c("obs_id")) %>% 
  left_join(pats_hosp_proms, by=c("obs_id")) %>% 
  mutate(flag_op_admi_link= ifelse(op_link==1&admi_link==1,1,0)) %>% 
  mutate(flag_op_only=ifelse(op_link==1&is.na(admi_link),1,0), 
         flag_admi_only=ifelse(admi_link==1&is.na(op_link),1,0)) 

table(pats_proms_link_check$op_link, pats_proms_link_check$flag_op_admi_link)

denom_linkage<-pats_proms_link_check %>% 
  group_by(proms) %>% 
  summarise(total_num_observations=n_distinct(obs_id), 
         total_linked_op_admi_data=sum(flag_op_admi_link, na.rm=TRUE), 
         total_op_link=sum(op_link, na.rm=TRUE), 
         total_admi_link=sum(admi_link, na.rm=TRUE), 
         total_op_only=sum(total_op_only,na.rm=TRUE), 
         total_admi_only=sum(total_admi_only,na.rm=TRUE),
         total_linked=sum(c(total_op_only,total_admi_only,total_linked_op_admi_data))) %>% 
  full_join(pats_proms_link_check %>% 
              mutate(all_link=ifelse(op_link==1&admi_link==1,1,0)) %>% 
              summarise(total_num_observations=n_distinct(obs_id), 
                        total_linked_op_admi_data=sum(flag_op_admi_link, na.rm=TRUE), 
                        total_op_link=sum(op_link, na.rm=TRUE), 
                        total_admi_link=sum(admi_link, na.rm=TRUE),
                        total_op_only=sum(total_op_only,na.rm=TRUE), 
                        total_admi_only=sum(total_admi_only,na.rm=TRUE), 
                        total_linked=sum(c(total_op_only,total_admi_only,total_linked_op_admi_data))) %>% 
              mutate(proms="Total")) 

denom_linkage

write.csv(denom_linkage, here(results_folder, 'linkage_denom.csv'))

plot_proms_link<-denom_linkage %>%
  select(-c(total_op_link, total_admi_link)) %>% 
  mutate(proms=case_when(proms=="chaq"~ "CHAQ", 
                         proms=="hads"~ "HADS", 
                         proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                         proms=="pedsql_sd"~ "PedsQL - SmartData", 
                         proms=="rcads"~ "RCADS",
                         proms=="sdq"~ "SDQ", 
                         TRUE~ proms)) %>% 
  pivot_longer(-c(proms, total_num_observations),names_to="type", values_to = "count") %>% 
  #filter(type!="total_linked_op_admi_data") %>% 
  mutate(prop=count/total_num_observations) %>% 
  mutate(prop_linked=ifelse(type=="total_linked",prop,NA)) %>% 
  fill(prop_linked,.direction="updown") %>% 
  mutate(labs=paste0(proms," (",round(prop_linked*100,1),"%)")) %>% 
  filter(type!="total_linked") %>% 
  mutate(type2=ifelse(type=="total_op_only", "Outpatients only", ifelse(type=="total_admi_only", "Admissions only", "Both"))) %>% 
  #mutate(proms_label=ifelse(proms=="Total", paste0(proms," (n = ",total_num_observations,")"),proms)) %>% 
  ggplot(aes(x=labs, y=prop, fill = type2))+
  geom_col()+
  scale_y_continuous(labels = percent_format()
                     # , limits = c(0, 1)
                     ) +
  scale_fill_manual(values=thesis_colours)+
  #facet_grid(cols=vars(type2),scales="free")+
  theme_minimal()+
  labs(x="PROMs", 
       y="Proportion of linked observations (%)", 
       title="", 
       fill="Type of Linkage")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
    axis.title.y = element_text(size = 9),
    panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
    strip.text.y = element_blank(),
    legend.position="bottom")

plot_proms_link

#ggsave(here(results_folder,'plot_proms_link.pdf'), plot = plot_proms_link, dpi = 300, width = 11, height = 6, units = "in")

ggsave(here(results_folder,'plot_proms_link.png'),plot = plot_proms_link, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")

##By teams

pats_op_proms<-op_proms2 %>% 
  select(project_id, date, obs_id, proms, team=spect_name) %>%
  mutate(team=str_trim(team)) %>% 
  distinct() %>%
  mutate(op_link=1)

pats_hosp_proms<-hosp_admi_proms %>% 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) %>% 
  select(project_id, date, obs_id, proms, team=hospital_service) %>%
  mutate(team=str_trim(team)) %>% 
  distinct() %>%
  mutate(admi_link=1)

pats_link<-pats_op_proms %>% 
  full_join(pats_hosp_proms) %>% 
  mutate(type_link= case_when(op_link==1&admi_link==1~"Both", 
                              op_link==1&is.na(admi_link)~"Outpatients only",
                              is.na(op_link)&admi_link==1~"Admissions only"))

proms_link_summary<-pats_link %>% 
  group_by(proms) %>% 
  mutate(total_obs_link=n_distinct(obs_id)) %>% 
  group_by(type_link,proms) %>% 
  mutate(total_link=n_distinct(obs_id)) %>% 
  group_by(team,type_link,proms) %>% 
  mutate(total_type_link=n_distinct(obs_id)) %>% 
  select(team, type_link,proms, total_obs_link, total_link, total_type_link) %>% 
  distinct() %>% 
  mutate(prop_type_link=total_type_link/total_link) %>% 
  mutate(team=str_trim(team)) %>% 
  filter(team!=""|!is.na(team)) %>% 
  filter(team!="") %>% 
  filter(!is.na(type_link)) %>% 
  mutate(proms=case_when(proms=="chaq"~ "CHAQ",
                         proms=="hads"~ "HADS",
                         proms=="pedsql_fl"~ "PedsQL - Flowsheets",
                         proms=="pedsql_sd"~ "PedsQL - SmartData",
                         proms=="rcads"~ "RCADS",
                         proms=="sdq"~ "SDQ",
                         TRUE~ proms)) %>%
  mutate(proms2=paste0(proms," (n =",total_obs_link,")"))

proms_link_summary$cat_percent <- cut(
   proms_link_summary$prop_type_link,
   breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   labels = c("0–20%", "21–40%", "41–60%", "61–80%", "81–100%"),
   include.lowest = TRUE
 )
 
write.csv(proms_link_summary, here(results_folder, 'proms_link_summary.csv'))


my_cols <- colorRampPalette(c("#D6D0CB", "#FF8700"))(5)
                                                 

scale_fill_manual( values = my_cols, name = "Percent of linked encounters (0–100%)", 
                   na.value = rgb(246/255, 246/255, 246/255) )    
                                                  

link_by_teams_heatmap<-proms_link_summary %>% 
  arrange(desc(team)) %>% 
  #filter(type_link!="both") %>% 
  #mutate(type_link=ifelse(type_link=="admi", "Admissions", "Outpatients")) %>% 
ggplot(aes(x = type_link, y = team, fill = cat_percent)) + 
  geom_tile(colour = "white", linewidth = 0.5, width = .9, height = .9) + 
  theme_minimal() +
  scale_fill_manual(
    values = my_cols,
    name = "Percent of linked observations (0–100%)",
    na.value = rgb(246/255, 246/255, 246/255)
  ) +
  facet_grid(cols=vars(proms), 
             labeller = labeller(
               col_var = label_wrap_gen(width = 24)
             )
  )+
  labs(x="Linkage",y="Team")+
  theme(
    #legend.position = c(0, -.15),
    legend.position = c(.25,-.15),
    #legend.justification = c(0.5, 0),
    legend.direction = "horizontal",
    legend.text = element_text(colour = "grey20"),
    plot.margin = grid::unit(c(.5, .5, 1.5, 1.5), "cm"),
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.text.x = element_text(size = 8, angle=30, hjust=1),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_text(angle = 90, size = 10),
    strip.placement = "outside",
    strip.clip = "off"
    #title = element_text(hjust = -.07, face = "bold", vjust = 1),
    #text = element_text(family = "Arial")
  ) 
    
link_by_teams_heatmap


ggsave(here(results_folder,'link_by_teams_heatmap.png'),plot = link_by_teams_heatmap, device = ragg::agg_png, 
       width = 8.27, height = 11.69,dpi= 600,units = "in", background="white")


# Network analysis --------------------------------------------------------

window_days    <- 90   # transitions within 90 days count as directed edges
top_k<- 3

#RCADs  
rcads_op <- op_proms2 %>%
  filter(proms == "rcads") %>%
  mutate(spect_name = ifelse(spect_name == "", "Unknown", spect_name),
    appt_date  = as.Date(date_op)) %>%
  select(project_id, spect_name, appt_date) %>%  
  distinct()

#Define Edges 
edges_dir <- rcads_op %>%
  inner_join(rcads_op, by = "project_id") %>%
  filter(spect_name.x != spect_name.y) %>%
  mutate(delta = as.integer(appt_date.y - appt_date.x)) %>%
  filter(delta == 0 | abs(delta) <= window_days) %>%
  distinct(project_id, spect_name.x, spect_name.y) %>%
  count(spect_name.x, spect_name.y, name = "patients") %>%
  arrange(desc(patients))

#Define Nodes 
vertices <- op_proms2 %>%
  filter(proms == "rcads") %>%
  mutate(spect_name = ifelse(spect_name == "", "Unknown", spect_name)) %>%
  group_by(spect_name) %>%
  summarise(appt_count = n(), .groups = "drop") %>%
  mutate(
    intensity = case_when(
      appt_count >= mean(appt_count) ~ "High",
      appt_count < mean(appt_count) ~ "Low",
    ),
    is_max = appt_count == max(appt_count))

#Build Graph 
graph <- graph_from_data_frame(
  d = edges_dir %>% rename(from = spect_name.x, to = spect_name.y),
  vertices = vertices,
  directed = TRUE
)

plot(graph)

# Edge attributes - weight of the edge corresponds with the number of shared patients 
E(graph)$patients <- edges_dir$patients 

#Invert weights - Stronger ties (more patients) should be "shorter"
E(graph)$weight <-1/E(graph)$patients

#Define centrality  
V(graph)$betweenness <- igraph::betweenness(
  graph,
  directed   = TRUE,
  normalized = TRUE,
  weights    = E(graph)$weight
)

plot(graph)

# Mark top 3 teams with the 
bt <- V(graph)$betweenness

V(graph)$is_bridge_hi <- V(graph)$betweenness >= 0.2 #Highlight connections with a lot of shared patients 

set.seed(1)  # for reproducible layout
p<-ggraph(graph, layout = "kk") +
  # Directed edges: width = number of patients transitioning
  geom_edge_fan(
    aes(width = patients),
    alpha  = 0.25,
    colour = "grey40",
    arrow     = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
    start_cap = ggraph::circle(3, "mm"),
    end_cap   = ggraph::circle(3, "mm")
  ) +
  scale_edge_width(range = c(1,3), breaks=c(1:3),name = "Transitions (unique patients)") +
  
  # Nodes: size = RCADS volume; fill = intensity; stroke = highlight bridges & max volume
  geom_node_point(
    aes(
      size   = appt_count,
      fill   = intensity,
      stroke = ifelse(is_bridge_hi, 2, ifelse(is_max, 4, 0.4))
    ),
    shape = 21, color = "black"
  ) +
  # Labels (bold for top bridges with β)
  geom_node_text(
    aes(x=x, 
        y=y+ifelse(is_max,0.4,ifelse(is_bridge_hi,-0.1,0)),
        label = ifelse(is_bridge_hi, paste0(name, "\nβ=", round(betweenness, 3)), name)),
    fontface = ifelse(V(graph)$is_max, "bold", 
                      ifelse(V(graph)$is_bridge_hi,"bold","plain")),
    size = ifelse(V(graph)$is_max, 5,3),
    repel = TRUE
  ) +
  scale_fill_manual(values = c("High" = "#D55E00", "Low" = "Grey", "At mean" = "#9E9E9E"),
                    name = "Volume of appointments vs. mean") +
  scale_size_continuous(range = c(3, 12), name = "Number of appointments linked to RCADS") +
  guides(edge_width = guide_legend(order = 1), fill = guide_legend(order = 2), size = guide_legend(order = 3)) +
  theme_void() +
  labs(
    title    = "Network of encounters linked to RCADs",
    subtitle = paste0("Window: ", window_days, " days; Top ", top_k, " teams outlined"))


p

ggsave(here(results_folder,'rcads_network.png'),plot = p, device = ragg::agg_png, 
       width = 11.69, height = 8.27,dpi= 600,units = "in", background="white")


#PedsQL

top_k<- 5

op_proms_all <- op_proms2 %>%
  filter(proms=="pedsql_sd") %>% 
  mutate(
    spect_name = ifelse(spect_name == "", "Unknown", spect_name),
    appt_date  = as.Date(date_op)  # <-- REPLACE with your appointment date column if needed
  ) %>%
  select(project_id, spect_name, appt_date) %>%  # <-- REPLACE patient_id if needed
  distinct()

#Define Edges 
edges_dir <- op_proms_all %>%
  inner_join(op_proms_all, by = "project_id") %>%
  filter(spect_name.x != spect_name.y) %>%
  mutate(delta = as.integer(appt_date.y - appt_date.x)) %>%
  filter(delta == 0 | abs(delta) <= window_days) %>%
  distinct(project_id, spect_name.x, spect_name.y) %>%
  count(spect_name.x, spect_name.y, name = "patients") %>%
  arrange(desc(patients)) %>% 
  filter(patients>3) #the median 

#Define Nodes 
vertices <- op_proms2 %>%
  filter(proms=="pedsql_sd") %>% 
  mutate(spect_name = ifelse(spect_name == "", "Unknown", spect_name)) %>%
  group_by(spect_name) %>%
  summarise(appt_count = n(), .groups = "drop") %>%
  mutate(rank=rank(-appt_count)) %>% 
  mutate(
    intensity = case_when(
      rank <= 5~ "Top 5",
      rank > 5 ~ "All",
    ),
    is_max = appt_count == max(appt_count)
  ) %>% 
  rename(name=spect_name)


#Build graph 
graph <- graph_from_data_frame(
  d = edges_dir %>% rename(from = spect_name.x, to = spect_name.y),
  vertices = vertices,
  directed = TRUE
)

plot(graph)

#Define edge weights 
E(graph)$patients <- edges_dir$patients

#Inverted weights 
E(graph)$weight <-1/E(graph)$patients

#Define centrality 
V(graph)$betweenness <- igraph::betweenness(
  graph,
  directed   = TRUE,
  normalized = TRUE,
  weights    = E(graph)$weight
)

plot(graph)

# Present top 5 teams 
bt <- V(graph)$betweenness
V(graph)$is_bridge_top <- rank(-bt, ties.method = "first") <= top_k 

set.seed(1)  # for reproducible layout
p<-ggraph(graph, layout = "kk") +
  # Directed edges: width = number of patients transitioning
  geom_edge_fan(
    aes(width = patients),
    alpha  = 0.25,
    colour = "grey40",
    arrow     = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
    start_cap = ggraph::circle(3, "mm"),
    end_cap   = ggraph::circle(3, "mm")
  ) +
  scale_edge_width(,name = "Transitions (unique patients)") +
  
  geom_node_point(
    aes(
      size   = appt_count,
      fill   = intensity,
      stroke = ifelse(is_bridge_top, 2, ifelse(is_max, 4, 0.4))
    ),
    shape = 21, color = "black"
  ) +
  # Labels (bold for top bridges with β)
  geom_node_text(
    aes(
      # x=x,
      y=y+ifelse(is_max,1,0),
      label = ifelse(intensity=="low",NA_character_,ifelse(is_bridge_top, paste0(name, "\nβ=", round(betweenness, 3)), name))),
    fontface = ifelse(V(graph)$is_max, "bold", 
                      ifelse(V(graph)$is_bridge_top,"bold","plain")),
    size = ifelse(V(graph)$is_max, 5,3),
    repel = TRUE, 
    na.rm=TRUE, 
    
    point.padding = 0.2,      # space around the point when repelling
    box.padding   = 0.3,      # space around the label box
    segment.color = "grey40", # leader line color
    segment.size  = 0.4,      # leader line thickness
    segment.alpha = 0.8,      # leader line transparency
    max.overlaps = Inf        # avoid dropping labels if needed
  ) +
  scale_fill_manual(values = c("Top 5" = "#D55E00", "All" = "#9E9E9E"),
                    name = "Volume of appointments") +
  scale_size_continuous(range = c(3, 12), name = "Number of appointments linked to RCADS") +
  guides(edge_width = guide_legend(order = 1), fill = guide_legend(order = 2), size = guide_legend(order = 3)) +
  theme_void() +
  labs(
    title    = "Network of encounters linked to PesdQL - Smartdata",
    subtitle = paste0("Window: ", window_days, " days; Top ", top_k, " teams outlined"))


p

ggsave(here(results_folder,'pedsql_network.png'),plot = p, device = ragg::agg_png, 
       width = 11.69, height = 8.72,dpi= 600,units = "in", background="white")
