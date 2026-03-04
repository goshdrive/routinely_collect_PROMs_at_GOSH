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


# Patients with PROMs -----------------------------------------------------

sdq_pats<-sdq %>% 
  ungroup() %>% 
  select(project_id,date,date_time, questionnaire_id, type_report, type_quest) 

rcads_pats<-rcads %>% 
  ungroup() %>% 
  select(project_id,date,date_time, questionnaire_id, type_report) 

hads_pats<-hads %>% 
  ungroup() %>% 
  select(project_id,date, date_time, questionnaire_id) 

pedsql_pats_fl<-pedsql_fl %>% 
  ungroup() %>% 
  select(project_id, date, date_time, questionnaire_id, type_report, type_quest)

pedsql_pats<-pedsql_sd %>% 
  ungroup() %>% 
 select(project_id,date,date_time, questionnaire_id, type_report, type_quest) 

chaq_pats<-chaq %>% 
  ungroup() %>% 
  select(project_id,date,date_time, questionnaire_id, type_report) 

questionnaire_proms<-sdq_pats %>% 
  select(questionnaire_id,date) %>% 
  mutate(date=as.Date(date)) %>% 
  distinct() %>% 
  mutate(proms="sdq") %>% 
  full_join(rcads_pats %>% 
              select(questionnaire_id,date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="rcads")) %>% 
  full_join(hads_pats %>% 
              select(questionnaire_id,date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="hads")) %>% 
  full_join(pedsql_pats_fl %>% 
              select(questionnaire_id,date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="pedsql_fl")) %>% 
  full_join(pedsql_pats %>% 
              select(questionnaire_id,date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="pedsql_sd")) %>% 
  full_join(chaq_pats %>% 
              select(questionnaire_id,date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="chaq"))

#Number of questionnaires 

questionnaire_denom<-questionnaire_proms %>% 
  group_by(proms) %>% 
  summarise(count=n())

write.csv(questionnaire_denom, here(results_folder, 'questionnaire_denom.csv'))

#Number of questionnaires per year 
questionnaire_denom_year<-questionnaire_proms %>% 
  ungroup() %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(proms,year,questionnaire_id) %>%
  group_by(proms,year) %>% 
  summarise(count_quest=n_distinct(questionnaire_id))


#All patients with PROMs 

pats_proms<-sdq_pats %>% 
  select(project_id, date) %>% 
  distinct() %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(proms="sdq") %>% 
  full_join(rcads_pats %>% 
              select(project_id, date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="rcads")) %>% 
  full_join(hads_pats %>% 
              select(project_id, date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="hads")) %>% 
  full_join(pedsql_pats_fl %>% 
              select(project_id, date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="pedsql_fl")) %>% 
  full_join(pedsql_pats %>% 
              select(project_id, date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="pedsql_sd")) %>% 
  full_join(chaq_pats %>% 
              select(project_id, date) %>% 
              mutate(date=as.Date(date)) %>% 
              distinct() %>% 
              mutate(proms="chaq")) %>% 
  mutate(project_id=tolower(project_id))

table(pats_proms$proms)

saveRDS(pats_proms, here('PROMs', 'clean_data','pats_proms.rds'))

#pats_proms<-readRDS(here('PROMs', 'clean_data','pats_proms.rds'))

pats_with_proms_denom<-pats_proms %>% 
  select(project_id, proms) %>% 
  distinct() %>% 
  group_by(proms) %>% 
  summarise(count=n())

write.csv(pats_with_proms_denom, here(results_folder, 'pats_with_proms_denom.csv'))

pats_with_proms_all<-pats_proms %>% 
  ungroup() %>% 
  select(project_id) %>% 
  distinct() %>% 
  summarise(count=n())

write.csv(pats_with_proms_all, here(results_folder, 'pats_with_proms_denom_all.csv'))

pats_with_more_than_one_prom<-pats_proms %>% 
  ungroup() %>% 
  select(project_id, proms) %>% 
  distinct() %>% 
  group_by(project_id) %>% 
  mutate(num_prom=n()) %>% 
  group_by(num_prom) %>% 
  summarise(count=n())

write.csv(pats_with_more_than_one_prom, here(results_folder, 'pats_with_more_than_one_prom.csv'))

num_observations<-pats_proms %>% 
  ungroup() %>% 
  select(project_id, proms,date) %>% 
  distinct() %>% 
  #group_by(proms) %>% 
  summarise(count=n())

write.csv(num_observations, here(results_folder, 'num_observations_denom.csv'))

num_observations_prom<-pats_proms %>% 
  ungroup() %>% 
  select(project_id, proms,date) %>% 
  distinct() %>% 
  group_by(proms) %>% 
  summarise(count=n())

#write.csv(num_observations, here(results_folder, 'num_observations_denom.csv'))

num_observations_year<-pats_proms %>% 
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) %>% 
  ungroup() %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(proms,year,obs_id) %>%
  distinct() %>% 
  group_by(proms,year) %>% 
  summarise(count_obs=n_distinct(obs_id))

num_patients_year<-pats_proms %>% 
  ungroup() %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(proms,year,project_id) %>%
  distinct() %>% 
  group_by(proms,year) %>% 
  summarise(count_pats=n_distinct(project_id))

#Number of patients with at least one encounter at GOSH in 2024 

encounters_year<-encounters_gosh %>% 
  ungroup() %>% 
  select(year,project_id) %>% 
  filter(year=="2024") %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(count_pats=n_distinct(project_id)) %>% 
  #mutate(pop="GOSH Patient Population") %>% 
  #mutate(proms=NA_character_) %>% 
  mutate(count_obs=NA_integer_, 
         count_quest=NA_integer_)


all_year<-merge(num_observations_year, questionnaire_denom_year)

all_year<-merge(all_year, num_patients_year)


write.csv(all_year, here(results_folder, 'num_observations_quests_year_denom.csv'))

write.csv(encounters_year,here(results_folder, 'num_pats_gosh_denom.csv'))

plot_observations<-all_year %>% 
  pivot_longer(-c(proms,year),names_to="type", values_to="count") %>% 
  mutate(lab=case_when(proms=="chaq"~ "CHAQ", 
                       proms=="hads"~ "HADS", 
                       proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                       proms=="pedsql_sd"~ "PedsQL - Smartdata", 
                       proms=="rcads"~ "RCADS",
                       proms=="sdq"~ "SDQ"),
        type2=ifelse(type=="count_obs","Observations", ifelse(type=="count_pats","Patients","Questionnaires"))) %>% 
  ggplot(aes(x=year, y=count, group=type2, colour=type2))+
  geom_line(alpha=0.5)+
  geom_point(alpha=0.5)+
  facet_grid(rows=vars(lab), scales="free", switch="y")+
  scale_colour_manual(values=thesis_colours)+
  labs(x="Year of observation",
       y="Count", 
       colour="")+
  theme_minimal()+
  theme(strip.text.y.left=element_text(angle=0), 
        strip.placement = "outside",         
        strip.clip      = "off")

plot_observations

ggsave(here(results_folder,'plot_observations.png'),plot = plot_observations, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")


# Tidy up IMD -------------------------------------------------------------

loc<-loc %>% 
  filter(tolower(project_id) %in% unique(pats_proms$project_id)) %>% 
  mutate(total_obs=n_distinct(project_id)) %>% 
  group_by(project_id) %>% 
  mutate(count=row_number(),
         recent_date=max(start_datetime), 
         flag_keep=start_datetime==recent_date) 

length(unique(loc$project_id))

loc_most_recent_start_date<-loc %>% 
  group_by(flag_keep) %>% 
  mutate(num_with_most_recent_start_date=n()) %>% 
  select(total_obs, flag_keep,num_with_most_recent_start_date) %>% 
  distinct()

write.csv(loc_most_recent_start_date, here(results_folder, 'loc_most_recent_start_date.csv'))

loc<-loc %>% 
  filter(flag_keep==TRUE)

length(unique(loc$project_id))

check<-loc %>% 
  filter(flag_keep==TRUE) %>% 
  group_by(project_id) %>% 
  mutate(count=n()) %>% 
  filter(count>1) #Still some people with multiple IMDs - in which case take the one with the latest end date

loc<-loc %>% 
  group_by(project_id) %>% 
  mutate(count=row_number(),
         recent_date=max(end_datetime), 
         flag_keep=end_datetime==recent_date)

loc_most_recent_end_date<-loc %>% 
  group_by(flag_keep) %>% 
  mutate(num_with_most_recent_end_date=n()) %>% 
  select(total_obs, flag_keep,num_with_most_recent_end_date) %>% 
  distinct()

write.csv(loc_most_recent_end_date, here(results_folder, 'loc_most_recent_end_date.csv'))

loc<-loc %>% 
  filter(flag_keep==TRUE) %>%  #Remove patients with multiple IMD 
  group_by(project_id) %>% 
  mutate(count=n()) 

length(unique(loc$project_id))

length(unique(pats_proms$project_id))

imd_report<-loc %>% 
  ungroup() %>% 
  select(project_id, count) %>% 
  group_by(count) %>% 
  summarise(n=n_distinct(project_id)) %>% 
  mutate(count=ifelse(count==1,"Only one IMD", "Multiple IMDs"))

write.csv(imd_report, here(results_folder,"imd_report.csv"))



# Link PROMs data with Demographics ---------------------------------------


pats_proms_demog<-pats_proms %>% 
  mutate(project_id=tolower(project_id)) %>% 
  left_join(demog %>% 
              mutate(project_id=tolower(project_id))) %>% 
   mutate(birth_date=as.Date(birth_date),
         age_at_time_of_questionnaire=floor(interval(birth_date, date) / years(1))) %>% 
  mutate(ethn=case_when(str_detect(ethnicity_name,"Mixed")~ "Mixed",
                        str_detect(ethnicity_name,"Asian")~ "Asian", 
                        str_detect(ethnicity_name,"Black")~ "Black",
                        str_detect(ethnicity_name,"White")~ "White",
                        str_detect(ethnicity_name,"Other")~ "Other",
                        str_detect(ethnicity_name,"Prefer")~ "Prefer not to say", 
                        TRUE~NA)) %>% 
  left_join(loc %>% 
              mutate(project_id=tolower(project_id)) %>% 
              select(project_id,imd_decile_2019), by="project_id") %>% 
  mutate(observation_id=paste0(project_id,"_",proms,"_",date)) %>% 
  group_by(observation_id) %>% 
  mutate(count=n(), 
         num_dups=row_number()) 
  
table(pats_proms_demog$count) 

table(pats_proms_demog$num_dups)

check_linked_pats<-pats_proms_demog %>% 
  ungroup() %>% 
  select(project_id,imd_decile_2019) %>% 
  distinct()

length(unique(check_linked_pats$project_id[!is.na(check_linked_pats$imd_decile_2019)]))

check_multiple_imd<-pats_proms_demog %>% 
  filter(count>1)

length(unique(check_multiple_imd$project_id))

#Check those with missing IMD is missing in the original location file 
miss_imd<-pats_proms_demog %>% 
  filter(is.na(imd_decile_2019))

pats_miss_imd<-unique(miss_imd$project_id)

check_miss_imd<-loc %>% 
  mutate(project_id=tolower(project_id)) %>% 
  filter(project_id %in% pats_miss_imd )

nrow(check_miss_imd)

imd_report_proms<-pats_proms_demog %>% 
  ungroup() %>% 
  select(project_id, count) %>% 
  distinct() %>% 
  group_by(count) %>% 
  summarise(n=n()) %>% 
  mutate(count=ifelse(count==1,"Only one IMD", "Multiple IMDs"))


write.csv(imd_report_proms, here(results_folder,"imd_report_proms.csv"))

#saveRDS(pats_proms_demog, here('PROMs', 'clean_data','pats_proms_demog.rds'))

# Patient groups completing PROMs  ----------------------------------------

#pats_proms_demog<-readRDS(here('PROMs', 'clean_data','pats_proms_demog.rds'))
#encounters_gosh<-readRDS(here('PROMs', 'clean_data','clean_encounters.rds'))

#Confidence intervals for median 

median_fun <- function(data, indices) {
  median(data[indices], na.rm = TRUE)
}

bootstrap_median_ci <- function(x, R = 2000, min_n = 30) {
  
  x <- x[!is.na(x)]
  n <- length(x)
  
  med <- median(x)
  
  # If sample size too small → no CI
  if (n < min_n) {
    return(
      tibble(
        n         = n,
        median    = med,
        ci_low    = NA_real_,
        ci_high   = NA_real_,
        ci_method = "not estimated (n < 30)"
      )
    )
  }
  
  boot_out <- boot(x, statistic = median_fun, R = R)
  ci <- boot.ci(boot_out, type = "perc")
  
  tibble(
    n         = n,
    median    = med,
    ci_low    = ci$percent[4],
    ci_high   = ci$percent[5],
    ci_method = "bootstrap percentile"
  )
}

age_ci_by_proms3 <- pats_proms_demog %>%
  mutate(obs_id=paste0(project_id,"_",proms,"_",date)) %>% 
  ungroup() %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(project_id, obs_id, proms, year, age_at_time_of_questionnaire) %>%
  distinct() %>% 
  group_by(project_id,year,proms) %>% 
  mutate(is_mean_age = age_at_time_of_questionnaire == mean(age_at_time_of_questionnaire, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(is_mean_age==TRUE) %>% 
  rename(age=age_at_time_of_questionnaire) %>% 
  full_join(encounters_gosh %>%  
              mutate(obs_id=paste0(project_id,"_",year)) %>% 
              select(project_id, obs_id, year, age=age_at_time_of_encounter) %>%
              filter(year=="2024") %>% 
              mutate(proms="GOSH patient population")) %>% 
  filter(!is.na(age)) %>%
  group_by(year, proms) %>%
  group_modify(~ bootstrap_median_ci(.x$age)) %>%
  ungroup()


age_ci_by_proms3

write.csv(age_ci_by_proms3, here(results_folder,"age_plots.csv"))

age_plots<-age_ci_by_proms3 %>%
  mutate(lab=case_when(proms=="chaq"~ "CHAQ", 
                       proms=="hads"~ "HADS", 
                       proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                       proms=="pedsql_sd"~ "PedsQL - SmartData", 
                       proms=="rcads"~ "RCADS",
                       proms=="sdq"~ "SDQ", 
                       TRUE~ "GOSH")) %>% 
  mutate(lab=factor(lab, levels=c("CHAQ","HADS","PedsQL - Flowsheets","PedsQL - SmartData",
                                  "RCADS","SDQ","GOSH"))) %>% 
  group_by(proms) %>% 
  mutate(total=sum(n)) %>% 
  #mutate(lab2 = paste0(lab, " (n =",format(total, big.mark = ",", scientific = FALSE),")")) %>% 
  ggplot(aes(x = year, group=lab)) +
  #geom_col(aes(y = n / 100, group=lab), fill = "grey80", alpha = 0.3) +  # scale for secondary axis
  geom_point(aes(y = median, group = lab, colour=lab)) +
  geom_line(aes(y = median, group = lab, colour=lab))+
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high, group=lab), colour="grey",alpha=0.3) +
  scale_colour_manual(values=thesis_colours)+
  scale_y_continuous(
    name = "Median Age",
    breaks=c(0:15),
      #,sec.axis = sec_axis(~ . * 100, name = "Number of observations")
  ) +
  labs(x="PROM", caption=
         "Note: For groups with fewer than 30 observations, confidence intervales were not estimated (no error bars shown).")+
  facet_grid(cols=vars(lab), scales="free", 
            labeller = labeller(
              col_var = label_wrap_gen(width = 30)
             ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="none", 
        strip.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.5,size = 9),
        strip.placement = "outside",
        strip.clip = "off"
  )


age_plots 

ggsave(here(results_folder,'age_plots.png'),plot = age_plots, device = ragg::agg_png, 
       width = 8.27, height = 11.69/1.5,dpi= 600,units = "in", background="white")

ethn_demog <- pats_proms_demog %>%
  ungroup() %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(project_id,proms,ethn) %>% 
  distinct() %>% 
  full_join(encounters_gosh %>%  
              select(year, project_id, ethn) %>% 
              filter(year=="2024") %>% 
              distinct() %>% 
              mutate(proms="GOSH patient population")) %>% 
  select(proms, ethn) %>% 
  mutate(ethn=case_when(is.na(ethn)~ "Not Known/Prefer not to say", 
                         ethn=="Prefer not to say"~ "Not Known/Prefer not to say", 
                         TRUE~ ethn)) %>% 
  filter(ethn!="Not Known/Prefer not to say") %>% 
  count(proms, ethn, name = "x") %>%        
  group_by(proms) %>%
  mutate(n = sum(x)) %>%                          # n = group total
  ungroup() %>%
  mutate(
    prop = x / n,
    # Wilson 95% CI (prop.test); set correct = TRUE for continuity correction
    ci_list  = map2(x, n, ~ prop.test(.x, .y, correct = TRUE)$conf.int),
    ci_low   = map_dbl(ci_list, 1),
    ci_high  = map_dbl(ci_list, 2)
  ) %>%
  select(-ci_list)

write.csv(ethn_demog, here(results_folder,"ethn_demog.csv"))


ethn_demog_missing<- pats_proms_demog %>%
  ungroup() %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(project_id,proms,ethn) %>% 
  distinct() %>% 
  full_join(encounters_gosh %>%  
              select(year,project_id, ethn) %>% 
              filter(year=="2024") %>% 
              distinct() %>% 
              mutate(proms="GOSH patient population")) %>% 
  select(proms, ethn) %>% 
  mutate(ethn=case_when(is.na(ethn)~ "Not Known/Prefer not to say", 
                        ethn=="Prefer not to say"~ "Not Known/Prefer not to say", 
                        TRUE~ ethn)) %>% 
  count(proms, ethn, name = "x") %>%        
  group_by(proms) %>%
  mutate(n = sum(x)) %>%                          # n = group total
  ungroup() %>%
  mutate(
    prop = x / n,
    # Wilson 95% CI (prop.test); set correct = TRUE for continuity correction
    ci_list  = map2(x, n, ~ prop.test(.x, .y, correct = TRUE)$conf.int),
    ci_low   = map_dbl(ci_list, 1),
    ci_high  = map_dbl(ci_list, 2)
  ) %>%
  select(-ci_list) %>% 
  filter(ethn=="Not Known/Prefer not to say") 


write.csv(ethn_demog_missing, here(results_folder,"ethn_demog_missing.csv"))


ethn_missing_plots<-ethn_demog_missing %>% 
  mutate(lab=case_when(proms=="chaq"~ "CHAQ", 
                       proms=="hads"~ "HADS", 
                       proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                       proms=="pedsql_sd"~ "PedsQL - SmartData", 
                       proms=="rcads"~ "RCADS",
                       proms=="sdq"~ "SDQ", 
                       TRUE~ "GOSH (2024)")) %>% 
  mutate(lab=factor(lab, levels=c("CHAQ","HADS","PedsQL - Flowsheets","PedsQL - SmartData",
                                  "RCADS","SDQ","GOSH (2024)"))) %>% 
  ggplot(aes(x = lab)) +
  #geom_col(aes(y = x / 2000), fill = "grey80", alpha = 0.3) +  # scale for secondary axis
  geom_point(aes(y = prop, colour=ethn)) +
  geom_line(aes(y=prop, group=1, colour=ethn))+
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high, colour=ethn), alpha=0.3) +
  scale_colour_manual(values=thesis_colours)+
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Patients (%)"
    #,sec.axis = sec_axis(~ . * 2000, name = "Number of patients")
  ) +
  labs(x="PROMs", 
      title="Proportion of patients with missing or not known ethncity")+
  #facet_grid(cols=vars(lab), scales="free")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="none", 
        strip.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.5,size = 9),
        strip.placement = "outside",
        strip.clip = "off")

ethn_missing_plots

ggsave(here(results_folder,'ethn_plots_missing.png'),plot = ethn_missing_plots, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")

my_cols <- colorRampPalette(c("#56B4E9","#D55E00"))(7)

ethn_plots<-ethn_demog %>%
  mutate(lab=case_when(proms=="chaq"~ "CHAQ", 
                       proms=="hads"~ "HADS", 
                       proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                       proms=="pedsql_sd"~ "PedsQL - SmartData", 
                       proms=="rcads"~ "RCADS",
                       proms=="sdq"~ "SDQ", 
                       TRUE~ "GOSH (2024)")) %>% 
  mutate(lab=factor(lab, levels=c("CHAQ","HADS","PedsQL - Flowsheets","PedsQL - SmartData",
                                  "RCADS","SDQ","GOSH (2024)"))) %>% 
  mutate(lab2 = paste0(lab, " 
                      (n =",format(n, big.mark = ",", scientific = FALSE),")")) %>% 
  ggplot(aes(x = lab)) +
  #geom_col(aes(y = x / 2000), fill = "grey80", alpha = 0.3) +  # scale for secondary axis
  #geom_point(aes(y = prop, colour = ethn)) +
  geom_col(aes(y=prop, colour=lab, fill=lab))+
  #geom_line(aes(y=prop, colour=ethn))+
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), colour="darkgrey", alpha=0.8) +
  scale_colour_manual(values=my_cols)+
  scale_fill_manual(values=my_cols)+
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Patients (%)"
    #,sec.axis = sec_axis(~ . * 2000, name = "Number of patients")
  ) +
  labs(x="Ethnicity")+
  facet_grid(cols=vars(ethn),scales="free")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="none", 
        strip.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.5,size = 9),
        strip.placement = "outside",
        strip.clip = "off")

ethn_plots

ggsave(here(results_folder,'ethn_plots.png'),plot = ethn_plots, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")



gender_demog <- pats_proms_demog %>%
  ungroup() %>% 
  select(project_id,proms,sex_name) %>% 
  distinct() %>% 
  full_join(encounters_gosh %>%  
              select(year,project_id, sex_name) %>% 
              filter(year=="2024") %>% 
              distinct() %>% 
              mutate(proms="GOSH patient population")) %>% 
  select(proms,sex_name) %>% 
  mutate(sex_name=case_when(is.na(sex_name)~ "Not Known", 
                            !(sex_name %in% c("Female","Male"))~"Not Known",
                        TRUE~ sex_name)) %>% 
  count(proms, sex_name, name = "x") %>%        # x = category count
  group_by(proms) %>%
  mutate(n = sum(x)) %>%                          # n = group total
  ungroup() %>%
  mutate(
    prop = x / n,
    # Wilson 95% CI (prop.test); set correct = TRUE for continuity correction
    ci_list  = map2(x, n, ~ prop.test(.x, .y, correct = TRUE)$conf.int),
    ci_low   = map_dbl(ci_list, 1),
    ci_high  = map_dbl(ci_list, 2)
  ) %>%
  select(-ci_list)


gender_demog

write.csv(gender_demog, here(results_folder,"gender_demog.csv"))


gender_plots<-gender_demog %>%
  mutate(lab=case_when(proms=="chaq"~ "CHAQ", 
                       proms=="hads"~ "HADS", 
                       proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                       proms=="pedsql_sd"~ "PedsQL - SmartData", 
                       proms=="rcads"~ "RCADS",
                       proms=="sdq"~ "SDQ", 
                       TRUE~ "GOSH (2024)")) %>% 
  mutate(lab=factor(lab, levels=c("CHAQ","HADS","PedsQL - Flowsheets","PedsQL - SmartData",
                                  "RCADS","SDQ","GOSH (2024)"))) %>% 
  mutate(lab2 = paste0(lab, " 
                      (n =",format(n, big.mark = ",", scientific = FALSE),")")) %>% 
  ggplot(aes(x = sex_name)) +
  #geom_col(aes(y = x / 2500), fill = "grey80", alpha = 0.3) +  # scale for secondary axis
  geom_col(aes(y = prop, colour = sex_name, fill=sex_name),alpha=0.3) +
  #geom_line(aes(y=prop, colour=sex_name))+
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), colour="darkgrey", alpha=0.8) +
  scale_colour_manual(values=thesis_colours)+
  scale_fill_manual(values=thesis_colours)+
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Patients (%)"
    #,sec.axis = sec_axis(~ . * 2500, name = "Number of patients")
  ) +
  labs(x="Gender")+
  facet_grid(cols=vars(lab), scales="free")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="none",
        strip.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5,size = 9),
        strip.placement = "outside",
        strip.clip = "off")

gender_plots

ggsave(here(results_folder,'gender_plots.png'),plot = gender_plots, device = ragg::agg_png, 
       width = 8.27, height = 11/1.5,dpi= 600,units = "in", background="white")

imd_demog <- pats_proms_demog %>%
  ungroup() %>% 
  select(project_id,proms,imd_decile_2019) %>% 
  distinct() %>% 
  full_join(encounters_gosh %>%  
              select(year, project_id, imd_decile_2019) %>% 
              filter(year=="2024") %>% 
              distinct() %>% 
              mutate(proms="GOSH patient population")) %>% 
  group_by(project_id,proms) %>% 
  mutate(count=n()) %>% 
  ungroup() %>% 
  mutate(imd_decile_2019=ifelse(count==2,NA,imd_decile_2019)) %>% 
  mutate(lab=case_when(is.na(imd_decile_2019)~ "Not Known", 
                                  imd_decile_2019==1 ~ "1 - Most Deprived", 
                                  imd_decile_2019==10 ~ "10 - Least Deprived",
                            TRUE~ as.character(imd_decile_2019))) %>%
  mutate(lab=factor(lab, levels=c("1 - Most Deprived","2","3","4","5","6","7","8","9","10 - Least Deprived", 
                             "Not Known"))) %>%
  select(project_id, proms, lab) %>% 
  distinct() %>% 
  count(proms, lab, name = "x") %>%        # x = category count
  group_by(proms) %>%
  mutate(n = sum(x)) %>%                          # n = group total
  ungroup() %>%
  mutate(
    prop = x / n,
    # Wilson 95% CI (prop.test); set correct = TRUE for continuity correction
    ci_list  = map2(x, n, ~ prop.test(.x, .y, correct = TRUE)$conf.int),
    ci_low   = map_dbl(ci_list, 1),
    ci_high  = map_dbl(ci_list, 2)
  ) %>%
  select(-ci_list)


imd_demog

write.csv(imd_demog, here(results_folder,"imd_demog.csv"))

my_cols <- colorRampPalette(c("#FF8700","#5D3A9B"))(10)

imd_plots<-imd_demog %>%
  filter(lab!="Not Known") %>% 
  mutate(group=case_when(proms=="chaq"~ "CHAQ", 
                       proms=="hads"~ "HADS", 
                       proms=="pedsql_fl"~ "PedsQL - Flowsheets", 
                       proms=="pedsql_sd"~ "PedsQL - SmartData", 
                       proms=="rcads"~ "RCADS",
                       proms=="sdq"~ "SDQ", 
                       TRUE~"GOSH (2024)")) %>% 
  mutate(group=factor(group, levels=c("CHAQ","HADS","PedsQL - Flowsheets","PedsQL - SmartData",
                                  "RCADS","SDQ","GOSH (2024)"))) %>% 
  mutate(group2 = paste0(group, " 
                      (n =",format(n, big.mark = ",", scientific = FALSE),")")) %>% 
  ggplot(aes(x = lab)) +
  #geom_col(aes(y = x ), fill = "grey80", alpha = 0.3) +  # scale for secondary axis
  #geom_point(aes(y = prop, colour = lab)) +
  geom_col(aes(y=prop, colour=lab, fill=lab),aes=0.5)+
  #geom_line(aes(y=prop, group=1, colour=lab))+
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), colour="darkgrey", alpha=0.3) +
  scale_colour_manual(values=my_cols)+
  scale_fill_manual(values=my_cols)+
  scale_y_continuous(
    limits = c(0, 0.25),
    breaks=c(0,0.15,0.25),
    labels = scales::percent_format(accuracy = 1),
    name = "Patients (%)"
    #,
    #sec.axis = sec_axis(~ . * 2000, name = "Number of patients")
  ) +
  labs(x="IMD")+
  facet_grid(col=vars(group), scales="free")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="none", ,
        strip.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.5,size = 9),
        strip.placement = "outside",
        strip.clip = "off")

imd_plots

ggsave(here(results_folder,'imd_plots.png'),plot = imd_plots, device = ragg::agg_png, 
       width = 8.27, height = 11.69/1.5,dpi= 600,units = "in", background="white")
