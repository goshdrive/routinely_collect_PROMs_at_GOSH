### Exploring the potential of Patient-Reported Outcome Measures (PROMs) collected as part of standard care at a large UK children’s hospital with electronic health records  

#### Project Status: [Ongoing]

#### Project Description

This study reports the different approaches in the collection, storage, access and usage of patient reported outcome measures (PROMs) data within a large UK Children’s Hospital (Great Ormond Street Hospital for Children NHS Foundation Trust), highlighting that there is great variability in the ability to identify, extract and process PROMs in the de-identified EHR data. 

This repository reports the steps taken to process the PROMs data to prepare the data for patient and questionnaire level analyses. 

It also presents exploratory descrptive statistics and linkages with hospital activity. 

#### Data Source 
NHS REC approval (REC reference:21/LO/0646) and local governance approvals at GOSH (22PE04) were obtained to access routine de-identified EHR data via the GOSH Digital Research Environment (DRE). 

Any observations that mentioned the name of the PROMs (from the list acquired from the Epic Team either in long or abbreviated form) in smart data or flowhsheets (data sets in Epic EHR at GOSH) and any patients with matching observations were included in the data extraction. emographic data were also extracted and linked for patients with identified PROMs from the de-identified EHR data. Due to governamnce processes the data is not available for publication. 

### PROMs processing steps 

The steps taken to process the PROMs data for further analyses were assessed. These steps are iterative but all are needed to succesfully process the data for further analyses. 

![process_figure](https://github.com/user-attachments/assets/0dd8e2f2-5905-44ce-a6ee-3df3b66c96a7)


### How does it work? 

The [procesing folder](https://github.com/goshdrive/routinely_collect_PROMs_at_GOSH/tree/main/PROMs_processing) includes the processing scripts. The analysis folder includes script to determine patient demographics, linkages to hospital activity and group outcomes. 

#### Processing 
The code follows the processing steps as outlined in the figure. 4 PROMs were identified in the  1 in smartdata, 3 in flowsheets and 1 in both smartdata and flowsheets. This was identified within Epic Caboodle by searching the short or long form of the PROM names. The PROMs includes Pediatric Quality of Life Inventory (PedsQL), Strenghts and Difficulties Questionnaire (SDQ), Hospital Anxiety and Depression Scale (HADS), Childhood Health Assessment Questionnaire (CHAQ) and Revised Anxiety Disorder Scale (RCADS). 

Using the time and date columns, the order of the questionnaires were identified. Any duplicate records were removed and type of metric, reporting and version of questionnaires were also identified. 

Questionnaire IDs were created and assigned to the metrics so that CYP/Proxy records can be identified. 

This resulted in data sets that are usable at patient and questionnaire level. It also results in data dictionaries, consistent metric names and 

Each PROM were processed individually. 

#### Analysis 

(descrptive.R)- This presents demographics of patients with PROMs data and how this compares with patients at GOSH. This includes age, gender, ethnicity and deprivation. 
(link_hospital_activity.R) - This links PROMs data to hospital activity (outpatient encounters and hospital admissions) and identifies which teams are most likely to be using PROMs. This includes network analysis to present potential collaboration networks. 
(patient_otutcomes.R) - This present the proportion of questionnaires with a valid score for all the different PROMs and visualises patient outcomes in different formats. 

### Requirements 


### Code Authors




