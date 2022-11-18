# Code to replicate KM survival curves and Cox PH Model
library(SurvMetrics)
library(caret)
library(survival)  
library(broom)
library(pec)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(survminer)
library(dplyr)
library(ranger)
library(ggfortify)
library(plyr)
library(prodlim)
library(lava)
library(pec)
library(riskRegression)

# load the dataset
data = read.csv("import.tbl_partner2.csv")

set.seed(123)

# Remove rows with NA or missing age, sex, region, country, income, outcome, missing time-to-event
data <- data[!(is.na(data$age) | data$age==""), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples
data <- data[!(is.na(data$outcome) | data$outcome==""), ] # 734282 samples
data <- data[!(is.na(data$dsstdy) | data$dsstdy==""), ] # 701663 samples

# Remove outliers for time-to-event (negative and above 200 days)
data <- data[!(data$dsstdy<0 | data$dsstdy>200), ] # 698692 samples

# Create death variable
data <- data %>%
  mutate(
    death = case_when(outcome == 'death' ~ 1,
                      TRUE ~ 0))

# Set event time to 200 for those alive
data[data$death == 0, "dsstdy"] <- 200

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy", "treat_mask_oxygen_therapy", "treat_nasal_oxygen_therapy",
       "ever_icu")][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                 "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                 "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                 "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                 "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy", "treat_mask_oxygen_therapy",
                                 "treat_nasal_oxygen_therapy", "ever_icu")])] <- FALSE

# Create PE variable
# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Convert age into custom categories
data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

# Regroup region variable
data <- data %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
                       region == 'Europe & Central Asia' ~ 'Europe & CA',
                       region == 'Latin America & Caribbean' ~ 'LA',
                       region == 'Middle East & North Africa' ~ 'MENA',
                       region == 'North America' ~ 'NAM',
                       region == 'South Asia' ~ 'SA',
                       region == 'Sub-Saharan Africa' ~ 'SSA'))

# Remove ethnicity, treatment, unknown variables, date to outcome, outcome
data <- data[, c("agegp10","sex", "region", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE",
                 "lborres_lab_ddimer", "lab_alt", "lab_bili", "lab_crp",
                 "lab_lym", "lab_neut", "lab_pt", "lab_urean", "lab_wbc", "vs_diabp", "vs_hr",
                 "vs_resp", "vs_sysbp", "vs_temp", "vs_oxysat", "dsstdy", "death")] # 698692 samples, 64 total variables

# Convert boolean to binary
data$comorbid_aids_hiv <- as.integer(as.logical(data$comorbid_aids_hiv))
data$comorbid_asthma <- as.integer(as.logical(data$comorbid_asthma))
data$comorbid_chronic_cardiac_disease <- as.integer(as.logical(data$comorbid_chronic_cardiac_disease))
data$comorbid_chronic_haematological_disease <- as.integer(as.logical(data$comorbid_chronic_haematological_disease))
data$comorbid_chronic_kidney_disease <- as.integer(as.logical(data$comorbid_chronic_kidney_disease))
data$comorbid_chronic_neurological_disorder <- as.integer(as.logical(data$comorbid_chronic_neurological_disorder))
data$comorbid_chronic_pulmonary_disease <- as.integer(as.logical(data$comorbid_chronic_pulmonary_disease))
data$comorbid_dementia <- as.integer(as.logical(data$comorbid_dementia))
data$comorbid_diabetes <- as.integer(as.logical(data$comorbid_diabetes))
data$comorbid_hypertension <- as.integer(as.logical(data$comorbid_hypertension))
data$comorbid_immunosuppression <- as.integer(as.logical(data$comorbid_immunosuppression))
data$comorbid_liver_disease <- as.integer(as.logical(data$comorbid_liver_disease))
data$comorbid_malignant_neoplasm <- as.integer(as.logical(data$comorbid_malignant_neoplasm))
data$comorbid_malnutrition <- as.integer(as.logical(data$comorbid_malnutrition))
data$comorbid_obesity <- as.integer(as.logical(data$comorbid_obesity))
data$comorbid_rheumatologic_disorder <- as.integer(as.logical(data$comorbid_rheumatologic_disorder))
data$comorbid_smoking <- as.integer(as.logical(data$comorbid_smoking))
data$comorbid_tuberculosis <- as.integer(as.logical(data$comorbid_tuberculosis))
data$symptomatic <- as.integer(as.logical(data$symptomatic))
data$symptoms_abdominal_pain <- as.integer(as.logical(data$symptoms_abdominal_pain))
data$symptoms_altered_consciousness_confusion <- as.integer(as.logical(data$symptoms_altered_consciousness_confusion))
data$symptoms_bleeding <- as.integer(as.logical(data$symptoms_bleeding))
data$symptoms_chest_pain <- as.integer(as.logical(data$symptoms_chest_pain))
data$symptoms_conjunctivitis <- as.integer(as.logical(data$symptoms_conjunctivitis))
data$symptoms_cough <- as.integer(as.logical(data$symptoms_cough))
data$symptoms_diarrhoea <- as.integer(as.logical(data$symptoms_diarrhoea))
data$symptoms_ear_pain <- as.integer(as.logical(data$symptoms_ear_pain))
data$symptoms_fatigue_malaise <- as.integer(as.logical(data$symptoms_fatigue_malaise))
data$symptoms_headache <- as.integer(as.logical(data$symptoms_headache))
data$symptoms_history_of_fever <- as.integer(as.logical(data$symptoms_history_of_fever))
data$symptoms_lost_altered_sense_of_smell <- as.integer(as.logical(data$symptoms_lost_altered_sense_of_smell))
data$symptoms_lost_altered_sense_of_taste <- as.integer(as.logical(data$symptoms_lost_altered_sense_of_taste))
data$symptoms_lymphadenopathy <- as.integer(as.logical(data$symptoms_lymphadenopathy))
data$symptoms_muscle_aches_joint_pain <- as.integer(as.logical(data$symptoms_muscle_aches_joint_pain))
data$symptoms_runny_nose <- as.integer(as.logical(data$symptoms_runny_nose))
data$symptoms_seizures <- as.integer(as.logical(data$symptoms_seizures))
data$symptoms_severe_dehydration <- as.integer(as.logical(data$symptoms_severe_dehydration))
data$symptoms_shortness_of_breath <- as.integer(as.logical(data$symptoms_shortness_of_breath))
data$symptoms_skin_rash <- as.integer(as.logical(data$symptoms_skin_rash))
data$symptoms_sore_throat <- as.integer(as.logical(data$symptoms_sore_throat))
data$symptoms_vomiting_nausea <- as.integer(as.logical(data$symptoms_vomiting_nausea))
data$symptoms_wheezing <- as.integer(as.logical(data$symptoms_wheezing))
data$PE <- as.integer(as.logical(data$PE))

# Remove ethnicity, treatment, unknown variables, date to outcome, outcome
data_no_lab <- data[, c("agegp10","sex", "region", "comorbid_aids_hiv", "comorbid_asthma",
                        "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                        "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                        "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                        "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                        "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                        "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                        "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                        "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                        "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                        "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                        "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                        "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                        "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                        "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE", "dsstdy", "death")]

# Rename column names for nicer plots
features <- c("Age", 'Sex', 'Region', 'AIDS', 'Asthma',
              "Chronic cardiac disease",
              'Chronic haematological disease',
              'Chronic kidney disease',
              'Chronic neurological disorder',
              'Chronic pulmonary disease', 'Dementia',
              'Diabetes', 'Hypertension','Immunosuppression', 'Liver disease',
              'Malignant neoplasm', 'Malnutrition',
              'Obesity', 'Rheumatologic disorder',
              'Smoking', 'Tuberculosis', 'Symptomatic',
              'Abdominal pain', 'Confusion',
              'Bleeding', 'Chest pain', 'Conjunctivitis',
              'Cough', 'Diarrhoea', 'Ear pain',
              'Fatigue malaise', 'Headache',
              'Fever', 'Lost or altered sense of smell',
              'Lost or altered sense of taste', 'Lymphadenopathy',
              'Muscle aches or joint pain', 'Runny nose',
              'Seizures', 'Severe dehydration',
              'Shortness of breath', 'Skin rash',
              'Sore throat', 'Vomiting or nausea', 'Wheezing', 'PE', 'Time', 'Death')
colnames(data_no_lab) <- make.names(features)

# KM Survival Curves
km_age_fit <- survfit(Surv(dsstdy, death) ~ agegp10, data=data)
survminer::ggsurvplot(km_age_fit, data=data, risk.table = TRUE, xlab = "Days After Admission", ylab = "Survival", 
                      legend.title = "Age", conf.int = TRUE, palette = 'Blues',
                      title = "KM Curve Age Stratified", conf.int.style = "step", 
                      legend = "right", surv.scale = "percent", break.x.by = 31, ylim = c(0.5, 1),
                      legend.labs=c("less than 20", "20-40", "40-60", "60-80", "more than 80"))

km_sex_fit <- survfit(Surv(dsstdy, death) ~ sex, data=data)
survminer::ggsurvplot(km_sex_fit, data=data, risk.table = TRUE, xlab = "Days After Admission", ylab = "Survival", 
                      legend.title = "Sex", conf.int = TRUE, palette = 'Blues',
                      title = "KM Curve Sex Stratified", conf.int.style = "step", 
                      legend = "right", surv.scale = "percent", break.x.by = 31, ylim = c(0.5, 1),
                      legend.labs=c("Female", "Male"))

km_region_fit <- survfit(Surv(dsstdy, death) ~ region, data=data)
survminer::ggsurvplot(km_region_fit, data=data, risk.table = TRUE, xlab = "Days After Admission", ylab = "Survival", 
                      legend.title = "Region", conf.int = FALSE, palette = 'Blues',
                      title = "KM Curve Region Stratified", conf.int.style = "step", 
                      legend = "right", surv.scale = "percent", break.x.by = 31, ylim = c(0.5, 1),
                      legend.labs=c("East Asia", "Europe and Central Asia", "Latin America", "MENA", "North America", "South Asia", "Sub-Saharan Africa"))

# Cox PH Model
full_no_labs = coxph(Surv(Time, Death) ~ Age +  Sex + Region + 
                       AIDS + Asthma + Chronic.cardiac.disease + 
                       Chronic.haematological.disease + Chronic.kidney.disease + 
                       Chronic.neurological.disorder + Chronic.pulmonary.disease + 
                       Dementia + Diabetes + Hypertension +
                       Immunosuppression + Liver.disease + Malignant.neoplasm + 
                       Malnutrition + Obesity + Rheumatologic.disorder + 
                       Smoking + Tuberculosis + Symptomatic + Abdominal.pain + 
                       Confusion + Bleeding + Chest.pain + 
                       Conjunctivitis + Cough + Diarrhoea + Ear.pain + 
                       Fatigue.malaise + Headache + Fever + 
                       Lost.or.altered.sense.of.smell + Lost.or.altered.sense.of.taste + 
                       Lymphadenopathy + Muscle.aches.or.joint.pain + Runny.nose + 
                       Seizures + Severe.dehydration + Shortness.of.breath + 
                       Skin.rash + Sore.throat + Vomiting.or.nausea + 
                       Wheezing + PE, 
                     data = data_no_lab, x = TRUE)

ggforest(
  full_no_labs,
  data = data_no_lab,
  main = "Hazard Ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.87,
  refLabel = "reference",
  noDigits = 2
)
