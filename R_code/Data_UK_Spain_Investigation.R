setwd("~/Desktop/COVID/data/")

invisible(lapply(c("tidyr","dplyr","plyr","ggplot2","reshape2",
                   "naniar","stringr","scales","lubridate",
                   "Hmisc","corrplot", "RColorBrewer", "data.table", 
                   "ggpubr", "DescTools", "ltm", "mix", "mice", "pscl",
                   "InformationValue", "survival", "survminer", "finalfit",
                   "forestmodel", "OddsPlotty"),
                 library, character.only = TRUE))

require(MASS)

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Get only Spain and UK
data <- data[(data$country=="United Kingdom" | data$country=="Spain"), ] # 288493 samples

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$agegp10) | data$agegp10==""), ] # 269784 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 269373 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 269373 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 269373 samples
data <- data[!(is.na(data$date_admit) | data$date_admit==""), ] # 754202 samples

# Standardise D-dimer lab measurements to microg/mL
setDT(data)[lborresu_lab_ddimer == "ng/mL", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/ml", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/ml FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "<b5>g/L", lborres_lab_ddimer := lborres_lab_ddimer*1000]
setDT(data)[lborresu_lab_ddimer == "ug/l", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/L", lborres_lab_ddimer := lborres_lab_ddimer/(1000*1000)]
setDT(data)[lborresu_lab_ddimer == "ug/L DDU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "<b5>g/mlFEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
data$lborres_lab_ddimer[data$lborres_lab_ddimer > 20] <- NA

# Convert back to dataframe for processing
class(data) <- class(as.data.frame(data))

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
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
       "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
       'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
       'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
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
                                                                          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE


# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Remove ethnicity, unknown variables, date to outcome, outcome, treatment, income
data <- data[, c("agegp10","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
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
                 "lborres_lab_ddimer", "lab_alt", "lab_bili", "lab_crp",
                 "lab_lym", "lab_neut", "lab_pt", "lab_urean", "lab_wbc", "vs_diabp", "vs_hr",
                 "vs_resp", "vs_sysbp", "vs_temp", "vs_oxysat", "PE")] # 269373 samples, 62 total variables

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

data <- data %>%
  mutate(
    sex = case_when(sex == 'Female' ~ 0,
                       sex == 'Male' ~ 1))

data <- data %>%
  mutate(
    country = case_when(country == 'Spain' ~ 0,
                        country == 'United Kingdom' ~ 1))

# Get the correlation matrix
corr_matrix <- cor(data)

# Only 11 complete cases
data_complete <- data[complete.cases(data), ]

# Extract top non-lab columns only
# Remove ethnicity, unknown variables, date to outcome, outcome, treatment, income
data <- data[, c("agegp10","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
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
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # Remove missing variables

# Fit a simple logistic regression model
glm.fit <- glm(PE ~ . -PE, data = data, family = binomial(link = logit))

summary(glm.fit)

plotty <- OddsPlotty::odds_plot(glm.fit, 
                                title = "Odds Plot",
                                subtitle = "Showing odds of PE based on various factors",
                                point_col = "#00f2ff",
                                error_bar_colour = "black",
                                point_size = .5,
                                error_bar_width = .8,
                                h_line_color = "red")

plot <- plotty$odds_plot #Returns the plot element from the list
plot <- plot + ggthemes::theme_economist() + theme(legend.position = "NULL")

# Add odds ratios to labels by calling the data list element
# The round function is used to return 2 decimal place values
plot + geom_text(label=round(plotty$odds_plot$data$OR, digits=2), 
                 hjust=0.1, vjust=1)


# Now try with train/test split
set.seed(101) # set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(data), size = floor(.8*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

glm.fit_train <- glm(PE ~ . -PE,
               data = train,
               family = binomial)

summary(glm.fit_train)

# R2 for how well the logistic regression performs
pscl::pR2(glm.fit_train)["McFadden"] # anything under 0.4 is not good, our model is not predictive

# Variable importance
caret::varImp(glm.fit_train)

# Collinearity check
#calculate VIF values for each predictor variable in our model
car::vif(glm.fit_train) # VIF values above 5 indicate severe multicollinearity

#calculate probability of PE for each sample in test dataset
predicted <- predict(glm.fit_train, test, type="response")

#find optimal cutoff probability to use to minimise misclassification error
optimal <- optimalCutoff(train$PE, predicted)[1]
optimal # 0.17

# Confusion matrix
confusionMatrix(test$PE, predicted) # does not predict any 1

#calculate sensitivity
sensitivity(test$PE, predicted)

#calculate specificity
specificity(test$PE, predicted)

#calculate total misclassification error rate
misClassError(test$PE, predicted, threshold=optimal) # very good once optimal, 0.0169

#plot the ROC curve
plotROC(test$PE, predicted)

# prediction
train$prediction <- predict(glm.fit_train, newdata = train, type = "response")

# distribution of the prediction score grouped by known outcome
ggplot(train, aes( prediction, fill = factor(PE))) + 
  geom_density(alpha = 0.4) +
  ggtitle( "Training Set's Predicted Score" ) + 
  theme_bw() + scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)]) +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  labs(fill = 'PE',
       x = "Prediction",
       y = "Density",
       title = "Training Set Predicted Score")

# Use the 0.17 cutoff value
test$prediction <- predict(glm.fit_train, newdata = test, type = "response" )
test <- test[ test$prediction >= 0.17, ]



# # Attempt EM imputation
# # Remove PE
# # Drop the old PE columns
# data_without_PE <-dplyr::select(data, -c('PE'))
# 
# imp.qp <- mice(data_without_PE, seed = 29725)
# 
# vnames <- c("vs_sysbp", "vs_diabp")
# cd1 <- mice::complete(imp.qp)[, vnames]
# cd2 <- mice::complete(imp.qp)[, vnames]
# typ <- factor(rep(c("blind imputation", "quickpred"),
#                   each = nrow(cd1)))
# mis <- ici(data_without_PE[, vnames])
# mis <- is.na(imp.qp$data$vs_sysbp) | is.na(imp.qp$data$vs_diabp)
# cd <- data.frame(typ = typ, mis = mis, rbind(cd1, cd2))
# xyplot(jitter(vs_diabp, 10) ~ jitter(vs_sysbp, 10) | typ,
#        data = cd, groups = mis,
#        col = c(mdc(1), mdc(2)),
#        xlab = "Systolic BP (mmHg)",
#        type = c("g","p"), ylab = "Diastolic BP (mmHg)",
#        pch = c(1, 19),
#        strip = strip.custom(bg = "grey95"),
#        scales = list(alternating = 1, tck = c(1, 0)))

##################################################################################################
# Logistic Regression of UK and Spain for PE without lab measurements
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Get only Spain and UK
data <- data[(data$country=="United Kingdom" | data$country=="Spain"), ] # 288493 samples

# Convert age decile into 5 groups instead
data <- data[!(is.na(data$agegp10) | data$agegp10==""), ] # 269784 samples

data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

ggplot(data, aes(x=agegp10)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age (Years)",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

# Remove rows with NA or missing sex, region, country, income
data <- data[!(is.na(data$sex) | data$sex==""), ] # 269373 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 254165 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 254165 samples
data <- data[!(is.na(data$date_admit) | data$date_admit==""), ] # 258129 samples

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
          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
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
                                                                                "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                                "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                                'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                                'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data <- data[, c("agegp10","sex","country", "date_admit", "comorbid_aids_hiv", "comorbid_asthma",
                       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                       "comorbid_hypertension", "comorbid_liver_disease", 
                       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                       "comorbid_rheumatologic_disorder", "comorbid_smoking",
                       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 254165 samples, 46 total variables

# Rename column names for nicer plots
features <- c("Age", 'Sex', 'Country', 'AIDS', 'Asthma',
              "Chronic cardiac disease",
              'Chronic haematological disease',
              'Chronic kidney disease',
              'Chronic neurological disorder',
              'Chronic pulmonary disease', 'Dementia',
              'Diabetes', 'Hypertension', 'Liver disease',
              'Malignant neoplasm', 'Malnutrition',
              'Obesity', 'Rheumatologic disorder',
              'Smoking', 'Symptomatic',
              'Abdominal pain', 'Confusion',
              'Bleeding', 'Chest pain', 'Conjunctivitis',
              'Cough', 'Diarrhoea', 'Ear pain',
              'Fatigue malaise', 'Headache',
              'Fever', 'Lost or altered sense of smell',
              'Lost or altered sense of taste', 'Lymphadenopathy',
              'Muscle aches or joint pain', 'Runny nose',
              'Seizures', 'Severe dehydration',
              'Shortness of breath', 'Skin rash',
              'Sore throat', 'Vomiting or nausea', 'Wheezing', 'PE')
colnames(data) <- make.names(features)

# Fit a simple logistic regression model
glm.fit <- glm(PE ~ . -PE, data = data, family = binomial(link = logit))

summary(glm.fit)

# Extract the odds ratios and the confidence intervals
exp(cbind(coef(glm.fit), confint(glm.fit)))

# Get the significance of multi-category variables like age
age1 <- glm(PE ~ . - PE - Age, data = data, family = binomial(link = logit))
age2 <- glm(PE ~ . -PE, data = data, family = binomial(link = logit))

anova(age1, age2, test="LRT")

plotty <- OddsPlotty::odds_plot(glm.fit,
                                point_col = "#7DAFD6",
                                error_bar_colour = "black",
                                point_size = .5,
                                error_bar_width = .8,
                                h_line_color = "black")

plot <- plotty$odds_plot #Returns the plot element from the list
plot <- plot + ggthemes::theme_economist() + theme(legend.position = "NULL")
plot <- plot + ggthemes::theme_base() + theme(legend.position = "NULL") + theme(plot.background = element_blank())

# Add odds ratios to labels by calling the data list element
# The round function is used to return 2 decimal place values
plot + geom_text(label=round(plotty$odds_plot$data$OR, digits=2), 
                 hjust=0.1, vjust=1)

ggsave('Logistic_Regression_Odds.png', width = 16, height = 14, dpi = 400)

##################################################################################################
# Logistic Regression of UK and Spain for PE without lab measurements WITH ALPHA VARIANT
##################################################################################################
setwd("~/Desktop/COVID/")

# load the dataset
data = read.csv("PE_Alpha.csv") # 241956 samples

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data <- data[, c("agegp10","sex","country", "alpha", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 254165 samples, 45 total variables

data$agegp10[data$agegp10 == "0"] <- "[0,20)"
data$agegp10[data$agegp10 == "1"] <- "[20,40)"
data$agegp10[data$agegp10 == "2"] <- "[40,60)"
data$agegp10[data$agegp10 == "3"] <- "[60,80)"
data$agegp10[data$agegp10 == "4"] <- "[80,120)"

data[c("alpha", "comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] <- lapply(data[c("alpha", "comorbid_aids_hiv", "comorbid_asthma",
                                                                                                        "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                                                        "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                                                        "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                                                        "comorbid_hypertension", "comorbid_liver_disease", 
                                                                                                        "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                                                        "comorbid_rheumatologic_disorder", "comorbid_smoking",
                                                                                                        "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                                                        "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                                                        "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                                                        "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                                                        "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                                                        "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                                                        "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                                                        "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")], as.logical)

### Alpha Variant frequency
data %>%
  ggplot(aes(x = alpha)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Post-alpha Variant",
       y = "Frequency")

data %>%
  group_by(PE, alpha) %>% 
  tally()

data_true_PE <- subset(data, PE == TRUE)
data_false_PE <- subset(data, is.na(PE))

sum(is.na(data$alpha))
sum(is.na(data_true_PE$alpha))
sum(is.na(data_false_PE$alpha))

# Rename column names for nicer plots
features <- c("Age", 'Sex', 'Country', 'Alpha variant', 'AIDS', 'Asthma',
              "Chronic cardiac disease",
              'Chronic haematological disease',
              'Chronic kidney disease',
              'Chronic neurological disorder',
              'Chronic pulmonary disease', 'Dementia',
              'Diabetes', 'Hypertension', 'Liver disease',
              'Malignant neoplasm', 'Malnutrition',
              'Obesity', 'Rheumatologic disorder',
              'Smoking', 'Symptomatic',
              'Abdominal pain', 'Confusion',
              'Bleeding', 'Chest pain', 'Conjunctivitis',
              'Cough', 'Diarrhoea', 'Ear pain',
              'Fatigue malaise', 'Headache',
              'Fever', 'Lost or altered sense of smell',
              'Lost or altered sense of taste', 'Lymphadenopathy',
              'Muscle aches or joint pain', 'Runny nose',
              'Seizures', 'Severe dehydration',
              'Shortness of breath', 'Skin rash',
              'Sore throat', 'Vomiting or nausea', 'Wheezing', 'PE')
colnames(data) <- make.names(features)

# Fit a simple logistic regression model
glm.fit <- glm(PE ~ . -PE, data = data, family = binomial(link = logit))

summary(glm.fit)

# Extract the odds ratios and the confidence intervals
exp(cbind(coef(glm.fit), confint(glm.fit)))

# Get the significance of multi-category variables like age
age1 <- glm(PE ~ . - PE - Age, data = data, family = binomial(link = logit))
age2 <- glm(PE ~ . -PE, data = data, family = binomial(link = logit))

anova(age1, age2, test="LRT")

plotty <- OddsPlotty::odds_plot(glm.fit,
                                point_col = "#7DAFD6",
                                error_bar_colour = "black",
                                point_size = .5,
                                error_bar_width = .8,
                                h_line_color = "black")

plot <- plotty$odds_plot #Returns the plot element from the list
plot <- plot + ggthemes::theme_economist() + theme(legend.position = "NULL")
plot <- plot + ggthemes::theme_base() + theme(legend.position = "NULL") + theme(plot.background = element_blank())

# Add odds ratios to labels by calling the data list element
# The round function is used to return 2 decimal place values
plot + geom_text(label=round(plotty$odds_plot$data$OR, digits=2), 
                 hjust=0.1, vjust=1)

ggsave('Logistic_Regression_Odds_alpha.png', width = 16, height = 14, dpi = 400)

