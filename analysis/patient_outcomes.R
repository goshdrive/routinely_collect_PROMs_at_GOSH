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


# Calculate mean scores ---------------------------------------------------

sdq_scores<-sdq %>% 
  select(project_id, questionnaire_id, type_quest, type_report, metric, subscale, Value) %>% 
  distinct() %>% 
  mutate(group= paste0(type_quest," - ",type_report)) %>% 
  mutate(score=as.numeric(Value)) %>%
  group_by(group) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>% 
  filter(metric=="score") %>% 
  group_by(questionnaire_id, metric, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,subscale,total_scores_in_each_quest, group) %>% 
  mutate(total=n(), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  select(metric,subscale, group, type_report, type_quest, total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests))

write.csv(sdq_scores, here(results_folder, 'sdq_scores.csv'))

rcads_scores <-rcads %>% 
  ungroup() %>% 
  mutate(score=as.numeric(Value)) %>% 
  group_by(type_report) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>%
  filter(metric=="score"&type_score=="t") %>% 
  group_by(questionnaire_id, metric, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,subscale, type_report, type_score, total_scores_in_each_quest) %>% 
  mutate(total=n(), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  select(metric, subscale, type_report,type_score, total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests))

write.csv(rcads_scores, here(results_folder, 'rcads_scores.csv'))

hads_scores <-hads %>% 
  ungroup() %>% 
  mutate(score=as.numeric(Value)) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>%
  filter(metric=="score") %>% 
  group_by(questionnaire_id, metric, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,subscale, total_scores_in_each_quest) %>% 
  mutate(total=n(), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  filter(metric=="score") %>% 
  select(metric, subscale, total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests))

write.csv(hads_scores, here(results_folder, 'hads_scores.csv'))


chaq_scores_text <-chaq %>% 
  ungroup() %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>% 
  filter(str_detect(subscale,"aids|devices|help"))  %>% 
  group_by(metric,subscale,StringValue) %>% 
  mutate(total=n()) %>% 
  select(metric, subscale, total_quests, StringValue, total) %>% 
  distinct() 

chaq_scores <-chaq %>% 
  ungroup() %>% 
  filter(should_be_duplicates==0)  %>% 
  mutate(score=ifelse(subscale=="overall_score",str_extract(StringValue, "(?<=\\=).*"),as.numeric(StringValue))) %>% 
  mutate(score=as.numeric(score)) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>% 
  filter(metric=="score") %>% 
  mutate(type_report=ifelse(is.na(type_report),"Unknown",type_report)) %>% 
  group_by(questionnaire_id, metric,type_report,subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,type_report, subscale,total_scores_in_each_quest) %>% 
  mutate(total=n(), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  select(metric, type_report, subscale, total_scores_in_each_quest,total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests))

write.csv(chaq_scores, here(results_folder, 'chaq_scores.csv'))

pedsql_sd_scores <-pedsql_sd %>% 
  ungroup() %>% 
  mutate(group= paste0(type_quest," - ",type_report)) %>% 
  group_by(group) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>% 
  mutate(score=as.numeric(StringValue)) %>% 
  filter(metric=="scales_scores"|metric=="summary_scores") %>% 
  group_by(questionnaire_id, metric,group, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,subscale, group,total_scores_in_each_quest) %>% 
  mutate(total=n_distinct(questionnaire_id), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  select(metric, subscale, group, total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests))

write.csv(pedsql_sd_scores, here(results_folder, 'pedsql_sd_scores.csv'))

pedsql_fl_scores <-pedsql_fl %>% 
  ungroup() %>% 
  mutate(type_report=ifelse(is.na(type_report),"NA",type_report)) %>% 
  mutate(type_label=factor(type_report,levels=c("NA", "proxy","CYP"), labels=c("Unknown", "Proxy","CYP"))) %>% 
  mutate(group= paste0(type_quest," - ",type_label)) %>% 
  group_by(group) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>% 
  mutate(score=as.numeric(Value)) %>% 
  filter(metric=="scale_score") %>% 
  group_by(questionnaire_id, metric,group, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,subscale, group,total_scores_in_each_quest) %>% 
  mutate(total=n_distinct(questionnaire_id), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  select(metric, subscale, group,total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests))


pedsql_fl_scores

write.csv(pedsql_sd_scores, here(results_folder, 'pedsql_sd_scores.csv'))

# Calculate completeness score ------------------------------------------------------------

all_scores<-sdq_scores %>% 
  select(metric, subscale, group, total, total_quests, total, prop_with_scores) %>%
  mutate(proms="SDQ") %>% 
  full_join(rcads_scores %>% 
              select(metric, subscale, type_report, total, total_quests) %>% 
              mutate(group=paste0("RCADS - ",type_report)) %>% 
              mutate(proms="RCADS")) %>% 
  full_join(hads_scores %>% 
              select(metric, subscale, total, total_quests) %>% 
              mutate(group="HADS") %>% 
              mutate(proms="HADS")) %>% 
  full_join(chaq_scores %>% 
              select(metric, subscale,type_report, total, total_quests) %>% 
              mutate(group=paste0("CHAQ - ",type_report)) %>% 
              mutate(proms="CHAQ")) %>% 
  full_join(pedsql_sd_scores %>% 
              select(metric, subscale, group, total, total_quests) %>% 
              mutate(proms="PedsQL - Smartdata")) %>% 
  full_join(pedsql_fl_scores %>% 
              select(metric, subscale, group, total, total_quests) %>% 
              mutate(proms="PedsQL - Flowsheets")) %>% 
    mutate(prop_with_scores=(total/total_quests)) %>% 
  select(-c(type_report,type_score,metric)) %>% 
  mutate(group2=paste0(group,"-",subscale))


my_cols <- colorRampPalette(c("#5D3A9B", "#FF8700"))(length(unique(all_scores$subscale)))

write.csv(all_scores, here(results_folder,'completness_scores.csv'))

completeness_plot<-all_scores %>% 
  filter(total_scores_in_each_quest==1) %>% 
  ggplot(aes(x=proms,y=prop_with_scores, colour=subscale)) +
  geom_hline(yintercept =0.5, linetype = "dashed", color = "#5D3A9B") +
  annotate("text", x="CHAQ", y = 0.55, label = "Cut off point", color = "#D55E00", size = 4)+
  geom_point(alpha=0.8)+
  scale_colour_manual(values=my_cols)+
  #scale_color_gradient(low = "#D6D0CB", high = "#FF8700")+
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "")+
  labs(x="PROMs", 
       y="Questionnaires with scores (%)")+
  theme_minimal()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

completeness_plot


ggsave(here(results_folder,'completeness_plot.png'),plot = completeness_plot, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")


# Visualize scores --------------------------------------------------------

sdq_scores_year<-sdq %>% 
  mutate(year=str_trim(substr(date,1,4))) %>% 
  select(project_id, year, questionnaire_id, type_quest, type_report, metric, subscale, Value) %>% 
  distinct() %>% 
  mutate(group= paste0(type_quest," - ",type_report)) %>% 
  mutate(score=as.numeric(Value)) %>%
  group_by(group,year) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>% 
  filter(metric=="score") %>% 
  group_by(questionnaire_id,year, metric, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,subscale,year,total_scores_in_each_quest, group) %>% 
  mutate(total=n(), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  select(metric,year,subscale, group, type_report, type_quest, total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  #mutate(prop_with_scores=round((total/total_quests)*100,2))
  mutate(prop_with_scores=(total/total_quests))


total_sdq_scores<-sdq_scores_year %>%
  mutate(subscale=sub("^.", toupper(substr(subscale, 1, 1)), subscale)) %>% 
  filter(total_scores_in_each_quest==1) %>% 
  filter(total>5) %>% 
  ggplot(aes(x = year)) +
  #geom_col(aes(y = prop_with_scores*100), fill = "grey80", alpha = 0.8) +  # scale for secondary axis
  geom_col(aes(y = mean_score, colour = subscale, fill=subscale)) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), colour="grey", alpha=0.3) +
  #geom_line(aes(y=mean_score, colour=subscale, group=subscale))+
  scale_colour_manual(values=thesis_colours)+
  scale_fill_manual(values=thesis_colours)+
  labs(x="Year", title="SDQ", y="Mean Score")+
  facet_grid(cols = vars(subscale), rows =vars(group), 
             switch="y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        panel.grid = element_blank(), 
        legend.position="none", 
        strip.placement = "outside",         # keep strips outside the panel
        strip.clip      = "off",             # avoid text being cut off
        strip.text.y.left = element_text(
          angle  = 0,                        # 0 = horizontal
          size   = 11,
          face   = "italic",
          margin = margin(t = 6, r = 6, b = 6, l = 6)
        ))

total_sdq_scores


total_sdq_scores<-sdq_scores_year %>%
  mutate(subscale=sub("^.", toupper(substr(subscale, 1, 1)), subscale)) %>% 
  filter(total_scores_in_each_quest==1) %>% 
  filter(total>5) %>% 
  ggplot(aes(x = year)) +
  #geom_col(aes(y = prop_with_scores*100), fill = "grey80", alpha = 0.8) +  # scale for secondary axis
  geom_point(aes(y = mean_score, colour = group, fill=group)) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), colour="grey", alpha=0.3) +
  geom_line(aes(y=mean_score, colour=group, group=group))+
  scale_colour_manual(values=thesis_colours)+
  scale_fill_manual(values=thesis_colours)+
  labs(x="Year", title="SDQ", y="Mean Score", 
       caption="Only scores with more than 5 observations are reported.")+
  facet_grid(cols = vars(subscale), 
             switch="y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        panel.grid = element_blank(), 
        legend.position="bottom", 
        legend.title = element_blank(),
        strip.placement = "outside",         # keep strips outside the panel
        strip.clip      = "off",             # avoid text being cut off
        strip.text.y.left = element_text(
          angle  = 0,                        # 0 = horizontal
          size   = 11,
          face   = "italic",
          margin = margin(t = 6, r = 6, b = 6, l = 6)
        ))

total_sdq_scores



ggsave(here(results_folder,'plot_total_sdq_scores.png'),plot = total_sdq_scores, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")

rcads_scores_order <-rcads %>%
  ungroup() %>%
  mutate(score=as.numeric(Value)) %>%
  group_by(type_report,order) %>%
  mutate(total_quests=n_distinct(questionnaire_id)) %>%
  filter(metric=="score"&type_score=="t") %>%
  group_by(questionnaire_id,order, metric, subscale) %>%
  mutate(total_scores_in_each_quest=n()) %>%
  group_by(metric,order,subscale, type_report, type_score, total_scores_in_each_quest) %>%
  mutate(total=n(),
         mean_score = mean(score, na.rm = TRUE),
         sd=sd(score),
         se=sd/sqrt(total),
         ci_lower=mean_score-qt(0.975,df=total-1)*se,
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>%
  select(metric,order, subscale, type_report,type_score, total_scores_in_each_quest, total_quests, total:ci_upper) %>%
  distinct() %>%
  mutate(prop_with_scores=(total/total_quests)) %>% 
  filter(total>5&total_scores_in_each_quest==1)

write.csv(rcads_scores_order, here(results_folder, 'rcads_scores_order.csv'))


total_rcads_scores<-rcads_scores_order %>%
  mutate(subscale=sub("^.", toupper(substr(subscale, 1, 1)), subscale)) %>% 
  mutate(subscale=str_replace(subscale, "_", " ")) %>% 
  mutate(subscale=case_when(subscale=="Gad"~"Generalized anxiety disorder", 
                            subscale=="Ocd"~"Obsessive compulsive disorder", 
                            subscale=="Sep anxiety"~"Separation anxiety", 
                            subscale=="Soc phobia"~"Social phobia",
                            TRUE~subscale)) %>% 
  mutate(type_report=sub("^.", toupper(substr(type_report, 1, 1)), type_report)) %>% 
  mutate(order=factor(order, levels=c(1,2), labels=c(1,2))) %>% 
  ggplot(aes(x = order)) +
  #geom_col(aes(y = prop_with_scores*100), fill = "grey80", alpha = 0.8) +  # scale for secondary axis
  geom_point(aes(y = mean_score, colour = type_report)) +
  #geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), colour="grey" ,alpha=0.3) +
  geom_line(aes(y=mean_score, group=type_report, colour=type_report), alpha=0.3)+
  geom_hline(
    yintercept = 65,
    linetype = "dotted",
    linewidth = 0.7,
    colour = "grey50", 
    alpha=0.5
  ) +
  geom_hline(
    yintercept = 70,
    linetype = "dotted",
    linewidth = 0.7,
    colour = "red", 
    alpha=0.5
  )+
  scale_y_continuous(limits=c(0,max(rcads_scores_order$ci_upper)))+
  labs(x="Order of questionnaires", title="RCADS", y="Mean Score", 
       caption="< 65: Normal   |   Grey line: 65–69: Borderline / Elevated (Top 3–7%)   |  Red line: ≥ 70: Clinical threshold / High severity (Top 2%)")+
  scale_colour_manual(values=thesis_colours)+
  facet_grid(cols=vars(subscale), 
             switch="y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.position="bottom", 
        legend.title=element_blank(),
        strip.placement = "outside",         # keep strips outside the panel
        strip.clip      = "off",             # avoid text being cut off
        strip.text.y.left = element_text(
          angle  = 0,                        # 0 = horizontal
          size   = 11,
          face   = "italic",
          margin = margin(t = 6, r = 6, b = 6, l = 6),
        ), 
        strip.text.x=element_text(angle=45))

total_rcads_scores

ggsave(here(results_folder,'plot_total_rcads_scores.png'),plot = total_rcads_scores, device = ragg::agg_png, 
       width = 8.27, height = 11.69/1.5,dpi= 600,units = "in", background="white")


rcads_scores_imd <-rcads %>%
  left_join(pats_proms_demog %>%
              filter(proms=="rcads"&count==1) %>%
              ungroup() %>%
              mutate(project_id=toupper(project_id)) %>%
              select(project_id, imd_decile_2019),
              by="project_id") %>%
  ungroup() %>%
  mutate(score=as.numeric(Value)) %>%
  group_by(type_report,imd_decile_2019) %>%
  mutate(total_quests=n_distinct(questionnaire_id)) %>%
  filter(metric=="score"&type_score=="t") %>%
  group_by(questionnaire_id,imd_decile_2019, metric, subscale) %>%
  mutate(total_scores_in_each_quest=n()) %>%
  group_by(metric,imd_decile_2019,subscale, type_report, type_score, total_scores_in_each_quest) %>%
  mutate(total=n(),
         mean_score = mean(score, na.rm = TRUE),
         sd=sd(score),
         se=sd/sqrt(total),
         ci_lower=mean_score-qt(0.975,df=total-1)*se,
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>%
  select(metric,imd_decile_2019, subscale, type_report,type_score, total_scores_in_each_quest, total_quests, total:ci_upper) %>%
  distinct() %>%
  mutate(prop_with_scores=(total/total_quests)) %>%
  filter(total>5&total_scores_in_each_quest==1) %>%
  filter(!is.na(imd_decile_2019))

write.csv(rcads_scores_imd, here(results_folder, 'rcads_scores_imd.csv'))

my_cols <- colorRampPalette(c("#FF8700","#5D3A9B"))(10)

total_rcads_scores_imd<-rcads_scores_imd %>%
  mutate(subscale=sub("^.", toupper(substr(subscale, 1, 1)), subscale)) %>% 
  mutate(subscale=str_replace(subscale, "_", " ")) %>% 
  mutate(subscale=case_when(subscale=="Gad"~"Generalized anxiety disorder", 
                            subscale=="Ocd"~"Obsessive compulsive disorder", 
                            subscale=="Sep anxiety"~"Separation anxiety", 
                            subscale=="Soc phobia"~"Social phobia",
                            TRUE~subscale)) %>% 
  mutate(type_report=sub("^.", toupper(substr(type_report, 1, 1)), type_report)) %>% 
  mutate(lab=case_when(imd_decile_2019==1 ~ "1 - Most Deprived",
                       imd_decile_2019==10 ~ "10 - Least Deprived",
                       TRUE~ as.character(imd_decile_2019))) %>%
  mutate(lab=factor(lab, levels=c("1 - Most Deprived","2","3","4","5","6","7","8","9","10 - Least Deprived"))) %>%
  ggplot(aes(x = lab)) +
  #geom_col(aes(y = prop_with_scores*100), fill = "grey80", alpha = 0.8) +  # scale for secondary axis
  #geom_col(aes(y = mean_score, colour = lab)) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), colour="grey" ,alpha=0.3) +
  geom_line(aes(y=mean_score, group=type_report, colour=type_report))+
  scale_y_continuous(limits=c(0,max(rcads_scores_imd$ci_upper)))+
  labs(x="IMD", title="RCADS", y="Mean Score")+
  scale_colour_manual(values=thesis_colours)+
  facet_grid(cols = vars(subscale),
             switch="y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        strip.placement = "outside",         # keep strips outside the panel
        strip.clip      = "off",             # avoid text being cut off
        strip.text.y.left = element_text(
          angle  = 0,                        # 0 = horizontal
          size   = 11,
          face   = "italic",
          margin = margin(t = 6, r = 6, b = 6, l = 6),
        ),
        strip.text.x=element_text(angle=45))

total_rcads_scores_imd

ggsave(here(results_folder,'plot_total_rcads_scores_imd.png'),plot = total_rcads_scores_imd, device = ragg::agg_png,
       width = 8.27, height = 11.69/1.5,dpi= 600,units = "in", background="white")




hads_scores_gender <-hads %>% 
  ungroup() %>% 
  mutate(score=as.numeric(Value)) %>% 
  group_by(sex_name) %>% 
  mutate(total_quests=n_distinct(questionnaire_id)) %>%
  filter(metric=="score") %>% 
  group_by(questionnaire_id, sex_name,metric, subscale) %>% 
  mutate(total_scores_in_each_quest=n()) %>% 
  group_by(metric,sex_name,subscale, total_scores_in_each_quest) %>% 
  mutate(total=n(), 
         mean_score = mean(score, na.rm = TRUE), 
         sd=sd(score), 
         se=sd/sqrt(total), 
         ci_lower=mean_score-qt(0.975,df=total-1)*se, 
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>% 
  filter(metric=="score") %>% 
  select(metric,sex_name, subscale, total_scores_in_each_quest, total_quests, total:ci_upper) %>% 
  distinct() %>% 
  mutate(prop_with_scores=(total/total_quests)) %>% 
  filter(total>5&total_scores_in_each_quest==1)

write.csv(hads_scores_gender, here(results_folder, 'hads_scores_gender.csv'))



mean_anxiety<-unlist(hads_scores[hads_scores$subscale == "anxiety", "mean_score"])[1]
mean_depression<-unlist(hads_scores[hads_scores$subscale == "depression", "mean_score"])[1]

 
total_hads_scores<-hads_scores_gender %>%
  mutate(subscale=sub("^.", toupper(substr(subscale, 1, 1)), subscale)) %>% 
  ggplot(aes(x = subscale)) +
  geom_col(aes(y = mean_score, colour = sex_name,fill=sex_name), position="dodge") +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper,group=sex_name),alpha=0.4,colour="darkgrey",
                position="dodge") +
  scale_colour_manual(values=thesis_colours)+
  scale_fill_manual(values=thesis_colours)+
  labs(x="Domain score", title="HADS", y="Mean Score")+
  # facet_grid(col = vars(subscale)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size=15),
        axis.text.y=element_text(size=15),
        strip.text=element_text(size=15),
        panel.grid = element_blank(), 
        legend.position="bottom", 
        legend.title = element_blank())

total_hads_scores

total_hads_scores<-total_hads_scores+ 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 7,
           fill = "darkgreen", alpha=0.2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7,  ymax = 10,
           fill = "#8B8000", alpha=0.2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10, ymax = 14,
           fill = "darkorange", alpha=0.2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 14, ymax = 21,
           fill = "darkred", alpha=0.2)+
  geom_hline(yintercept = c(7, 10, 14),
             linewidth = 0.5,
             colour = "grey50", lty="dotted")

total_hads_scores<-total_hads_scores+
   annotate(
    "text",
    x = Inf, y = c(3.5, 8.5, 12.5, 18),
    label = c(
      "Normal\n(0–7)",
      "Mild / borderline\n(8–10)",
      "Moderate / possible\n(11–14)",
      "Severe / probable\n(15–21)"
    ),
    hjust = 1.05,
    size = 3
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 35, 5.5, 5.5))

total_hads_scores

ggsave(here(results_folder,'plot_total_hads.png'),plot = total_hads_scores, device = ragg::agg_png, 
       width = 8.27, height = 11.69/2,dpi= 600,units = "in", background="white")


hads_scores_imd <-hads %>%
  left_join(pats_proms_demog %>%
              filter(proms=="hads"&count==1) %>%
              ungroup() %>%
              mutate(project_id=toupper(project_id)) %>%
              select(project_id, imd_decile_2019),
            by="project_id") %>%
  ungroup() %>%
  mutate(score=as.numeric(Value)) %>%
  group_by(type_report,imd_decile_2019) %>%
  mutate(total_quests=n_distinct(questionnaire_id)) %>%
  group_by(questionnaire_id,imd_decile_2019, metric, subscale) %>%
  mutate(total_scores_in_each_quest=n()) %>%
  group_by(metric,imd_decile_2019,subscale, total_scores_in_each_quest) %>%   
  mutate(total=n(),
         mean_score = mean(score, na.rm = TRUE),
         sd=sd(score),
         se=sd/sqrt(total),
         ci_lower=mean_score-qt(0.975,df=total-1)*se,
         ci_upper=mean_score+qt(0.975,df=total-1)*se) %>%
  select(metric,imd_decile_2019, subscale, total_scores_in_each_quest, total_quests, total:ci_upper) %>%
  distinct() %>%
  mutate(prop_with_scores=(total/total_quests)) %>%
  filter(total_scores_in_each_quest==1) %>%
  filter(!is.na(imd_decile_2019))

write.csv(hads_scores_imd, here(results_folder, 'hads_scores_imd.csv'))

my_cols <- colorRampPalette(c("#FF8700","#5D3A9B"))(10)

total_hads_scores_imd<-hads_scores_imd %>%
  mutate(subscale=sub("^.", toupper(substr(subscale, 1, 1)), subscale)) %>%
  mutate(lab=case_when(imd_decile_2019==1 ~ "1 - Most Deprived",
                       imd_decile_2019==10 ~ "10 - Least Deprived",
                       TRUE~ as.character(imd_decile_2019))) %>%
  mutate(lab=factor(lab, levels=c("1 - Most Deprived","2","3","4","5","6","7","8","9","10 - Least Deprived"))) %>%
  ggplot(aes(x = lab)) +
  #geom_col(aes(y = prop_with_scores*100), fill = "grey80", alpha = 0.8) +  # scale for secondary axis
  geom_col(aes(y = mean_score, colour = lab, fill=lab)) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), colour="grey" ,alpha=0.3) +
  #geom_line(aes(y=mean_score, group=type_report, colour=lab), alpha=0.3)+
  scale_y_continuous(limits=c(0,max(hads_scores_imd$ci_upper)))+
  labs(x="IMD", title="HADS", y="Mean Score")+
  scale_colour_manual(values=my_cols)+
  scale_fill_manual(values=my_cols)+
  facet_grid(cols = vars(subscale),
             switch="y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position="none",
        strip.placement = "outside",         # keep strips outside the panel
        strip.clip      = "off",             # avoid text being cut off
        strip.text.y.left = element_text(
          angle  = 0,                        # 0 = horizontal
          size   = 11,
          face   = "italic",
          margin = margin(t = 6, r = 6, b = 6, l = 6),
        ),
        strip.text.x=element_text(angle=45))

total_hads_scores_imd

ggsave(here(results_folder,'plot_total_rcads_scores_imd.png'),plot = total_rcads_scores_imd, device = ragg::agg_png,
       width = 8.27, height = 11.69/1.5,dpi= 600,units = "in", background="white")
