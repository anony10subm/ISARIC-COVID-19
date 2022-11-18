setwd("~/Desktop/COVID/data/")

invisible(lapply(c("tidyr","dplyr","plyr","ggplot2","reshape2",
                   "naniar","stringr","scales","lubridate",
                   "Hmisc","corrplot", "RColorBrewer", "skimr"),
                 library, character.only = TRUE))
require(gridExtra)

# load the dataset
data = read.csv("import.tbl_partner2.csv")

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$age) | data$age=="" | data$age=="NA"), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$country) | data$country==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples

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

# Plots of each of the variables with the first bin being missing (if categorical)
# Outcome
ggplot(data, aes(x=outcome)) + geom_bar(fill = "steelblue4") + 
  labs(x='Outcome') + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Outcome",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(outcome) %>% 
  tally()

outcome_per_country <- data %>%
  group_by(outcome, country) %>% 
  tally()

# Remove rows with NA outcome
data <- data[!(is.na(data$outcome) | data$outcome==""), ] # 734282 samples

# Create death variable
data <- data %>%
  mutate(
    death = case_when(outcome == 'death' ~ 1,
                    TRUE ~ 0))

# Create PE variable
data <- data %>%
  mutate(
    PE = case_when(PE == 'TRUE' ~ 1,
                   TRUE ~ 0))

# Death
ggplot(data, aes(x=death)) + geom_bar(fill = "steelblue4") + 
  labs(x='Outcome') + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Death",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

# PE
ggplot(data, aes(x=PE)) + geom_bar(fill = "#0F1D42") + 
  labs(x='PE') + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "PE",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

plot1 <- ggplot(data, aes(x=death)) + geom_bar(fill = "#0F1D42") + 
  labs(x='Outcome',
       y = "") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Death",
       y = "") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
plot2 <- ggplot(data, aes(x=PE)) + geom_bar(fill = "#0F1D42") + 
  labs(x='PE',
       y = "") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "PE",
       y = "") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
grid.arrange(plot1, plot2, ncol=2, left='Frequency')

# Create PE variable
# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Create Oxygen Therapy variable
# Combine into PE variable
data$treat_oxygen_therapy = data$treat_oxy_therapy | data$treat_oxygen_therapy | data$treat_mask_oxygen_therapy | data$treat_nasal_oxygen_therapy

# Drop the old oxygen therapy columns
data <-dplyr::select(data, -c('treat_oxy_therapy', 'treat_mask_oxygen_therapy', 'treat_nasal_oxygen_therapy'))

# Get the subset of data matching each patient group (target and control)
data_true_death <- subset(data, death == 1)
data_false_death <- subset(data, death == 0)

### Age
data %>%
  ggplot(aes(x = age, fill = factor(death))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Age at Admission (Years)",
       y = "Density",
       title = "Density of Age by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$age))
sum(is.na(data_true_death$age))
sum(is.na(data_false_death$age))

sum(data$age == "")
sum(data_true_death$age == "")
sum(data_false_death$age == "")

### Time-to-event

# Remove outliers for time-to-event (negative and above 720 days)
data <- data[!(data$dsstdy<0 | data$dsstdy>200), ] # 698692 samples

data %>%
  ggplot(aes(x = dsstdy, fill = factor(death))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Time-to-event",
       y = "Density",
       title = "Density of Time-to-event by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$dsstdy))
sum(is.na(data_true_death$dsstdy))
sum(is.na(data_false_death$dsstdy))

sum(data$dsstdy == "")
sum(data_true_death$dsstdy == "")
sum(data_false_death$dsstdy == "")

### Age as Group

data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

data <- data %>%
  mutate(
    death = case_when(death == 1 ~ 'Yes',
                      TRUE ~ 'No'))

data %>%
  ggplot(aes(x = agegp10, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Age at Admission (Years)",
       y = "Density",
       title = "Density of Age by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, agegp10) %>% 
  tally()

sum(is.na(data$agegp10))
sum(is.na(data_true_PE$agegp10))
sum(is.na(data_false_PE$agegp10))

sum(data$agegp10 == "")
sum(data_true_death$agegp10 == "")
sum(data_false_death$agegp10 == "")

### PE

data %>%
  ggplot(aes(x = PE, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "PE",
       y = "Density",
       title = "Distribution of PE by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, PE) %>% 
  tally()

### Sex

data %>%
  ggplot(aes(x = sex, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Sex",
       y = "Density",
       title = "Distribution of Sex by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, sex) %>% 
  tally()

### Country

data <- data[!(is.na(data$country) | data$country==""), ]

data %>%
  ggplot(aes(x = country, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Country",
       y = "Density",
       title = "Distribution of Country by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data_per_country <- data %>%
  group_by(death, country) %>% 
  tally()

summary <- ddply(data_true_death,.(country),summarise,freq=length(country))
country <- c("South Africa", "United Kingdom", "Pakistan", "India", "Canada")
freq <- c(96745, 53385, 2995, 1164, 1133)

data_ethnic <- data.frame(country, freq)
data_ethnic$country <- factor(data_ethnic$country)

ggplot(data_ethnic, aes(x=country, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Country Given Death",
       y = "Frequency")

summary <- ddply(data_false_death,.(country),summarise,freq=length(country))
country <- c("South Africa", "United Kingdom", "Spain", "Malaysia", "India")
freq <- c(329765, 189339, 7447, 6424, 4894)

data_ethnic <- data.frame(country, freq)
data_ethnic$country <- factor(data_ethnic$country)

ggplot(data_ethnic, aes(x=country, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Country Given no Death",
       y = "Frequency")

### Income

data <- data %>%
  mutate(
    income = case_when(income == 'High income' ~ 'HI',
                       income == 'Low income' ~ 'LI',
                       income == 'Lower middle income' ~ 'LMI',
                       income == 'Upper middle income' ~ 'UMI'))

data %>%
  ggplot(aes(x = income, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Income",
       y = "Density",
       title = "Distribution of Income by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, income) %>% 
  tally()

### Region

data <- data %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
                       region == 'Europe & Central Asia' ~ 'Europe & CA',
                       region == 'Latin America & Caribbean' ~ 'LA',
                       region == 'Middle East & North Africa' ~ 'MENA',
                       region == 'North America' ~ 'NAM',
                       region == 'South Asia' ~ 'SA',
                       region == 'Sub-Saharan Africa' ~ 'SSA'))

data %>%
  ggplot(aes(x = region, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Region",
       y = "Density",
       title = "Distribution of Region by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, region) %>% 
  tally()

data %>%
  group_by(symptomatic, region) %>% 
  tally()

data %>%
  ggplot(aes(x = region, fill = factor(symptomatic))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Symptoms',
       x = "Region",
       y = "Density",
       title = "Distribution of Symptoms by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidities
### AIDS/HIV

data %>%
  ggplot(aes(x = comorbid_aids_hiv, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "AIDS/HIV",
       y = "Density",
       title = "Distribution of AIDS/HIV by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_aids_hiv) %>% 
  tally()

### Asthma

data %>%
  ggplot(aes(x = comorbid_asthma, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Asthma",
       y = "Density",
       title = "Distribution of AIDS/HIV by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_asthma) %>% 
  tally()

sum(is.na(data$comorbid_asthma))
sum(is.na(data_true_death$comorbid_asthma))
sum(is.na(data_false_death$comorbid_asthma))

### Cardiac Disease

data %>%
  ggplot(aes(x = comorbid_chronic_cardiac_disease, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Cardiac Disease",
       y = "Density",
       title = "Distribution of Cardiac Disease by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_chronic_cardiac_disease) %>% 
  tally()

### Haematological Disease

data %>%
  ggplot(aes(x = comorbid_chronic_haematological_disease, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Haematological Disease",
       y = "Density",
       title = "Distribution of Haematological Disease by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_chronic_haematological_disease) %>% 
  tally()

### Kidney Disease

data %>%
  ggplot(aes(x = comorbid_chronic_kidney_disease, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Kidney Disease",
       y = "Density",
       title = "Distribution of Kidney Disease by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_chronic_kidney_disease) %>% 
  tally()

### Neurological Disease

data %>%
  ggplot(aes(x = comorbid_chronic_neurological_disorder, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Neurological Disease",
       y = "Density",
       title = "Distribution of Neurological Disease by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_chronic_neurological_disorder) %>% 
  tally()

### Pulmonary Disease

data %>%
  ggplot(aes(x = comorbid_chronic_pulmonary_disease, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Pulmonary Disease",
       y = "Density",
       title = "Distribution of Pulmonary Disease by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_chronic_pulmonary_disease) %>% 
  tally()

### Dementia

data %>%
  ggplot(aes(x = comorbid_dementia, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Dementia",
       y = "Density",
       title = "Distribution of Dementia by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_dementia) %>% 
  tally()

### Diabetes

data %>%
  ggplot(aes(x = comorbid_diabetes, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Diabetes",
       y = "Density",
       title = "Distribution of Diabetes by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_diabetes) %>% 
  tally()

### Hypertension

data %>%
  ggplot(aes(x = comorbid_hypertension, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Hypertension",
       y = "Density",
       title = "Distribution of Hypertension by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_hypertension) %>% 
  tally()

### Immunosuppression

data %>%
  ggplot(aes(x = comorbid_immunosuppression, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Immunosuppression",
       y = "Density",
       title = "Distribution of Immunosuppression by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_immunosuppression) %>% 
  tally()

### Liver Disease

data %>%
  ggplot(aes(x = comorbid_liver_disease, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Liver Disease",
       y = "Density",
       title = "Distribution of Liver Disease by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_liver_disease) %>% 
  tally()

### Malignant Neoplasm

data %>%
  ggplot(aes(x = comorbid_malignant_neoplasm, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Malignant Neoplasm",
       y = "Density",
       title = "Distribution of Malignant Neoplasm by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_malignant_neoplasm) %>% 
  tally()

### Malnutrition

data %>%
  ggplot(aes(x = comorbid_malnutrition, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Malnutrition",
       y = "Density",
       title = "Distribution of Malnutrition by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_malnutrition) %>% 
  tally()

### Obesity

data %>%
  ggplot(aes(x = comorbid_obesity, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Obesity",
       y = "Density",
       title = "Distribution of Obesity by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_obesity) %>% 
  tally()

### Rheumatologic

data %>%
  ggplot(aes(x = comorbid_rheumatologic_disorder, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Rheumatologic",
       y = "Density",
       title = "Distribution of Rheumatologic by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_rheumatologic_disorder) %>% 
  tally()

### Smoking

data %>%
  ggplot(aes(x = comorbid_smoking, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Smoking",
       y = "Density",
       title = "Distribution of Smoking by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_smoking) %>% 
  tally()

### Tuberculosis

data %>%
  ggplot(aes(x = comorbid_tuberculosis, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Tuberculosis",
       y = "Density",
       title = "Distribution of Tuberculosis by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, comorbid_tuberculosis) %>% 
  tally()

### Symptoms
### Symptomatic

data %>%
  ggplot(aes(x = symptomatic, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Symptomatic",
       y = "Density",
       title = "Distribution of Symptomatic by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptomatic) %>% 
  tally()

### Abdominal Pain

data %>%
  ggplot(aes(x = symptoms_abdominal_pain, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Abdominal Pain",
       y = "Density",
       title = "Distribution of Abdominal Pain by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_abdominal_pain) %>% 
  tally()

### Confusion

data %>%
  ggplot(aes(x = symptoms_altered_consciousness_confusion, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Confusion",
       y = "Density",
       title = "Distribution of Confusion by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_altered_consciousness_confusion) %>% 
  tally()

### Bleeding

data %>%
  ggplot(aes(x = symptoms_bleeding, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Bleeding",
       y = "Density",
       title = "Distribution of Bleeding by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_bleeding) %>% 
  tally()

### Chest Pain

data %>%
  ggplot(aes(x = symptoms_chest_pain, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Chest Pain",
       y = "Density",
       title = "Distribution of Chest Pain by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_chest_pain) %>% 
  tally()

### Conjunctivitis

data %>%
  ggplot(aes(x = symptoms_conjunctivitis, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Conjunctivitis",
       y = "Density",
       title = "Distribution of Conjunctivitis by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_conjunctivitis) %>% 
  tally()

### Cough

data %>%
  ggplot(aes(x = symptoms_cough, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Cough",
       y = "Density",
       title = "Distribution of Cough by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_cough) %>% 
  tally()

### Diarrhoea

data %>%
  ggplot(aes(x = symptoms_diarrhoea, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Diarrhoea",
       y = "Density",
       title = "Distribution of Diarrhoea by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_diarrhoea) %>% 
  tally()

### Ear Pain

data %>%
  ggplot(aes(x = symptoms_ear_pain, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Ear Pain",
       y = "Density",
       title = "Distribution of Ear Pain by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_ear_pain) %>% 
  tally()

### Fatigue

data %>%
  ggplot(aes(x = symptoms_fatigue_malaise, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Fatigue",
       y = "Density",
       title = "Distribution of Fatigue by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_fatigue_malaise) %>% 
  tally()

### Headache

data %>%
  ggplot(aes(x = symptoms_headache, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Headache",
       y = "Density",
       title = "Distribution of Headache by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_headache) %>% 
  tally()

### Fever

data %>%
  ggplot(aes(x = symptoms_history_of_fever, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Fever",
       y = "Density",
       title = "Distribution of Fever by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_history_of_fever) %>% 
  tally()

### Lost Sense of Smell

data %>%
  ggplot(aes(x = symptoms_lost_altered_sense_of_smell, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Lost Sense of Smell",
       y = "Density",
       title = "Distribution of Lost Sense of Smell by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_lost_altered_sense_of_smell) %>% 
  tally()

### Lost Sense of Taste

data %>%
  ggplot(aes(x = symptoms_lost_altered_sense_of_taste, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Lost Sense of Taste",
       y = "Density",
       title = "Distribution of Lost Sense of Taste by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_lost_altered_sense_of_taste) %>% 
  tally()

### Lymphadenopathy

data %>%
  ggplot(aes(x = symptoms_lymphadenopathy, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Lymphadenopathy",
       y = "Density",
       title = "Distribution of Lymphadenopathy by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_lymphadenopathy) %>% 
  tally()

### Muscle Aches and Joint Pain

data %>%
  ggplot(aes(x = symptoms_muscle_aches_joint_pain, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Muscle Aches and Joint Pain",
       y = "Density",
       title = "Distribution of Muscle Aches and Joint Pain by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_muscle_aches_joint_pain) %>% 
  tally()

### Runny Nose

data %>%
  ggplot(aes(x = symptoms_runny_nose, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Runny Nose",
       y = "Density",
       title = "Distribution of Runny Nose by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_runny_nose) %>% 
  tally()

### Seizures

data %>%
  ggplot(aes(x = symptoms_seizures, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Seizures",
       y = "Density",
       title = "Distribution of Seizures by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_seizures) %>% 
  tally()

### Severe Dehydration

data %>%
  ggplot(aes(x = symptoms_severe_dehydration, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Severe Dehydration",
       y = "Density",
       title = "Distribution of Severe Dehydration by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_severe_dehydration) %>% 
  tally()

### Shortness of Breath

data %>%
  ggplot(aes(x = symptoms_shortness_of_breath, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Shortness of Breath",
       y = "Density",
       title = "Distribution of Shortness of Breath by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_shortness_of_breath) %>% 
  tally()

### Skin Rash

data %>%
  ggplot(aes(x = symptoms_skin_rash, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Skin Rash",
       y = "Density",
       title = "Distribution of Skin Rash by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_skin_rash) %>% 
  tally()

### Sore Throat

data %>%
  ggplot(aes(x = symptoms_sore_throat, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Sore Throat",
       y = "Density",
       title = "Distribution of Sore Throat by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_sore_throat) %>% 
  tally()

### Vomiting

data %>%
  ggplot(aes(x = symptoms_vomiting_nausea, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Vomiting",
       y = "Density",
       title = "Distribution of Vomiting by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_vomiting_nausea) %>% 
  tally()

### Wheezing

data %>%
  ggplot(aes(x = symptoms_wheezing, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Wheezing",
       y = "Density",
       title = "Distribution of Wheezing by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, symptoms_wheezing) %>% 
  tally()

### Treatment
### Extracorporeal

data %>%
  ggplot(aes(x = treat_extracorporeal, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Extracorporeal",
       y = "Density",
       title = "Distribution of Extracorporeal by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_extracorporeal) %>% 
  tally()

### High Flow Nasal Cannula

data %>%
  ggplot(aes(x = treat_high_flow_nasal_cannula, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "High Flow Nasal Cannula",
       y = "Density",
       title = "Distribution of High Flow Nasal Cannula by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_high_flow_nasal_cannula) %>% 
  tally()

### Inhaled Nitric Oxide

data %>%
  ggplot(aes(x = treat_inhaled_nitric_oxide, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Inhaled Nitric Oxide",
       y = "Density",
       title = "Distribution of Inhaled Nitric Oxide by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_inhaled_nitric_oxide) %>% 
  tally()

### Invasive Ventilation

data %>%
  ggplot(aes(x = treat_invasive_ventilation, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Invasive Ventilation",
       y = "Density",
       title = "Distribution of Invasive Ventilation by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_invasive_ventilation) %>% 
  tally()

### Non-invasive Ventilation

data %>%
  ggplot(aes(x = treat_non_invasive_ventilation, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Non-invasive Ventilation",
       y = "Density",
       title = "Distribution of Non-invasive Ventilation by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_non_invasive_ventilation) %>% 
  tally()

### Oxygen Therapy

data %>%
  ggplot(aes(x = treat_oxygen_therapy, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Oxygen Therapy",
       y = "Density",
       title = "Distribution of Oxygen Therapy by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_oxygen_therapy) %>% 
  tally()

### Respiratory Support

data %>%
  ggplot(aes(x = treat_respiratory_support, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Respiratory Support",
       y = "Density",
       title = "Distribution of Respiratory Support by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_respiratory_support) %>% 
  tally()

### Tracheostomy

data %>%
  ggplot(aes(x = treat_tracheostomy, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "Tracheostomy",
       y = "Density",
       title = "Distribution of Tracheostomy by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, treat_tracheostomy) %>% 
  tally()

### ICU Admission

data %>%
  ggplot(aes(x = ever_icu, fill = factor(death))) +
  geom_bar() +
  theme_bw() +
  labs(fill = 'Death',
       x = "ICU Admission",
       y = "Density",
       title = "Distribution of ICU Admission by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(death, ever_icu) %>% 
  tally()

### Lab Measurements
### D-dimer

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

data %>%
  ggplot(aes(x = lborres_lab_ddimer, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "D-dimer",
       y = "Density",
       title = "Distribution of D-dimer by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lborres_lab_ddimer))
sum(is.na(data_false_death$lborres_lab_ddimer))

### ALT

data %>%
  ggplot(aes(x = lab_alt, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "ALT",
       y = "Density",
       title = "Distribution of ALT by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_alt))
sum(is.na(data_false_death$lab_alt))

### Bilirubin

data %>%
  ggplot(aes(x = lab_bili, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Bilirubin",
       y = "Density",
       title = "Distribution of Bilirubin by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_bili))
sum(is.na(data_false_death$lab_bili))

### CRP

data %>%
  ggplot(aes(x = lab_crp, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "CRP",
       y = "Density",
       title = "Distribution of CRP by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_crp))
sum(is.na(data_false_death$lab_crp))

### Lymphocytes

data %>%
  ggplot(aes(x = lab_lym, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Lymphocytes",
       y = "Density",
       title = "Distribution of Lymphocytes by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_lym))
sum(is.na(data_false_death$lab_lym))

### Neutrophils

data %>%
  ggplot(aes(x = lab_neut, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Neutrophils",
       y = "Density",
       title = "Distribution of Neutrophils by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_neut))
sum(is.na(data_false_death$lab_neut))

### Platelets

data %>%
  ggplot(aes(x = lab_pt, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Platelets",
       y = "Density",
       title = "Distribution of Platelets by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_pt))
sum(is.na(data_false_death$lab_pt))

### Urean

data %>%
  ggplot(aes(x = lab_urean, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Urean",
       y = "Density",
       title = "Distribution of Urean by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_urean))
sum(is.na(data_false_death$lab_urean))

### WBC

data %>%
  ggplot(aes(x = lab_wbc, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "WBC",
       y = "Density",
       title = "Distribution of WBC by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$lab_wbc))
sum(is.na(data_false_death$lab_wbc))

### DBP

data %>%
  ggplot(aes(x = vs_diabp, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "DBP",
       y = "Density",
       title = "Distribution of DBP by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_diabp))
sum(is.na(data_false_death$vs_diabp))

### HR

data %>%
  ggplot(aes(x = vs_hr, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "HR",
       y = "Density",
       title = "Distribution of HR by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_hr))
sum(is.na(data_false_death$vs_hr))

### Oxygen Saturation (Therapy)

data %>%
  ggplot(aes(x = vs_oxysat_oxygen_therapy, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Oxygen Saturation",
       y = "Density",
       title = "Distribution of Oxygen Saturation by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_oxysat_oxygen_therapy))
sum(is.na(data_false_death$vs_oxysat_oxygen_therapy))

### Oxygen Saturation (Room Air)

data %>%
  ggplot(aes(x = vs_oxysat_room_air, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Oxygen Saturation",
       y = "Density",
       title = "Distribution of Oxygen Saturation by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_oxysat_room_air))
sum(is.na(data_false_death$vs_oxysat_room_air))

### RR

data %>%
  ggplot(aes(x = vs_resp, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "RR",
       y = "Density",
       title = "Distribution of RR by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_resp))
sum(is.na(data_false_death$vs_resp))

### SBP

data %>%
  ggplot(aes(x = vs_sysbp, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "SBP",
       y = "Density",
       title = "Distribution of SBP by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_sysbp))
sum(is.na(data_false_death$vs_sysbp))

### Temperature

data %>%
  ggplot(aes(x = vs_temp, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Temperature",
       y = "Density",
       title = "Distribution of Temperature by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_temp))
sum(is.na(data_false_death$vs_temp))

### Oxygen Saturation

data %>%
  ggplot(aes(x = vs_oxysat, fill = factor(death))) +
  geom_density(alpha=0.4) +
  theme_bw() +
  labs(fill = 'Death',
       x = "Oxygen Saturation",
       y = "Density",
       title = "Distribution of Oxygen Saturation by Death") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data_true_death$vs_oxysat))
sum(is.na(data_false_death$vs_oxysat))

# Summary Statistics
skim(data_false_death)


##################################################################
#Simple logistic regression
##################################################################

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data <- data[, c("agegp10","sex","region", "comorbid_aids_hiv", "comorbid_asthma",
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
                 "PE", "death")] # 734282 samples, 47 total variables

# Rename column names for nicer plots
features <- c("Age", 'Sex', 'Region', 'AIDS', 'Asthma',
              "Chronic cardiac disease",
              'Chronic haematological disease',
              'Chronic kidney disease',
              'Chronic neurological disorder',
              'Chronic pulmonary disease', 'Dementia',
              'Diabetes', 'Hypertension', 'Immunosuppression', 'Liver disease',
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
              'Sore throat', 'Vomiting or nausea', 'Wheezing', 'PE', 'Death')
colnames(data) <- make.names(features)

# Fit a simple logistic regression model
glm.fit <- glm(Death ~ . -Death, data = data, family = binomial(link = logit))

summary(glm.fit)

# Extract the odds ratios and the confidence intervals
exp(cbind(coef(glm.fit), confint(glm.fit)))

# Get the significance of multi-category variables like age and region
age1 <- glm(Death ~ . -Death - Age, data = data, family = binomial(link = logit))
age2 <- glm(Death ~ . -Death, data = data, family = binomial(link = logit))

anova(age1, age2, test="LRT")

region1 <- glm(Death ~ . -Death -Region, data = data, family = binomial(link = logit))
region2 <- glm(Death ~ . -Death, data = data, family = binomial(link = logit))

anova(region1, region2, test="LRT")

plotty <- OddsPlotty::odds_plot(glm.fit,
                                point_col = "#7DAFD6",
                                error_bar_colour = "black",
                                point_size = .5,
                                error_bar_width = .8,
                                h_line_color = "red")

plot <- plotty$odds_plot #Returns the plot element from the list
plot <- plot + ggthemes::theme_economist() + theme(legend.position = "NULL")
plot <- plot + ggthemes::theme_base() + theme(legend.position = "NULL") + theme(plot.background = element_blank())

# Add odds ratios to labels by calling the data list element
# The round function is used to return 2 decimal place values
plot + geom_text(label=round(plotty$odds_plot$data$OR, digits=2), 
                 hjust=0.1, vjust=1)
ggsave('Logistic_Regression_Odds_Death.png', width = 16, height = 14, dpi = 400)
