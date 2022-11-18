setwd("~/Desktop/COVID/data/")

invisible(lapply(c("tidyr","dplyr","plyr","ggplot2","reshape2",
                   "naniar","stringr","scales","lubridate", "lemon",
                   "Hmisc","corrplot", "RColorBrewer", "skimr", "gridExtra", "grid"),
                 library, character.only = TRUE))

# load the dataset
data = read.csv("import.tbl_partner2.csv")

# Get only Spain and UK
data <- data[(data$country=="United Kingdom" | data$country=="Spain"), ] # 288493 samples

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$age) | data$age==""), ] # 269784 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 269373 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 269373 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 269373 samples

# Convert age into custom categories
data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

# Replace NA with FALSE for PE
data[c("pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism")][is.na(data[c("pulmonary_embolism_or_dvt",
                                                                          "thromboembolsim", "pulmonary_embolism")])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Get the subset of data matching each patient group (target and control)
data_true_PE <- subset(data, PE == TRUE)
data_false_PE <- subset(data, PE == FALSE)

data_true_PE %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_fatigue_malaise))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Fatigue (PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data_false_PE %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptomatic))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Fatigue (No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

# Symptomatic
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptomatic))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Symptomatic (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptomatic))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Abdominal Pain
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_abdominal_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Abdominal Pain (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_abdominal_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Confusion
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_altered_consciousness_confusion))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Confusion (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_altered_consciousness_confusion))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Bleeding
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_bleeding))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Bleeding (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_bleeding))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Chest Pain
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_chest_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Chest Pain (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_chest_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Conjunctivitis
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_conjunctivitis))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Conjunctivitis (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_conjunctivitis))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Cough
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_cough))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Cough (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_cough))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Diarrhoea
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_diarrhoea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Diarrhoea (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_diarrhoea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Ear Pain
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_ear_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Ear Pain (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_ear_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Fatigue
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_fatigue_malaise))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Fatigue (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_fatigue_malaise))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Headache
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_headache))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Headache (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_headache))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Fever
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_history_of_fever))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Fever (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_history_of_fever))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Lost Sense of Smell
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_smell))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Lost Sense of Smell (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_smell))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Lost Sense of Taste
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_taste))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Lost Sense of Taste (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_taste))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Lymphadenopathy
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_lymphadenopathy))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Lymphadenopathy (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_lymphadenopathy))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Muscle Aches and Joint Pain
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_muscle_aches_joint_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Muscle Aches and Joint Pain (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_muscle_aches_joint_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Runny Nose
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_runny_nose))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Runny Nose (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_runny_nose))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Seizures
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_seizures))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Seizures (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_seizures))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Severe Dehydration
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_severe_dehydration))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Severe Dehydration (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_severe_dehydration))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Shortness of Breath
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_shortness_of_breath))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Shortness of Breath (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_shortness_of_breath))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Skin Rash
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_skin_rash))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Skin Rash (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_skin_rash))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Sore Throat
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_sore_throat))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Sore Throat (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_sore_throat))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Vomiting
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_vomiting_nausea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Vomiting (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_vomiting_nausea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Wheezing
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(symptoms_wheezing))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Wheezing (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(symptoms_wheezing))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Comorbidities
# AIDS/HIV
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_aids_hiv))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "AIDS/HIV (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_aids_hiv))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Asthma
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_asthma))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Asthma (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_asthma))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Cardiac Disease
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_cardiac_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Cardiac Disease (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_cardiac_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Haematological Disease
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_haematological_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Haematological Disease (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_haematological_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Kidney Disease
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_kidney_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Kidney Disease (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_kidney_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Neurological Disease
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_neurological_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Neurological Disease (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_neurological_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Pulmonary Disease
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_pulmonary_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Pulmonary Disease (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_chronic_pulmonary_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Dementia
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_dementia))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Dementia (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_dementia))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Diabetes
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_diabetes))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Diabetes (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_diabetes))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Hypertension
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_hypertension))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Hypertension (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_hypertension))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Immunosuppression
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_immunosuppression))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Immunosuppression (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_immunosuppression))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Liver Disease
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_liver_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Liver Disease (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_liver_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Malignant Neoplasm
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_malignant_neoplasm))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Malignant Neoplasm (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_malignant_neoplasm))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Malnutrition
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_malnutrition))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Malnutrition (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_malnutrition))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Obesity
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_obesity))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Obesity (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_obesity))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Rheumatologic
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_rheumatologic_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Rheumatologic Disorder (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_rheumatologic_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Smoking
plot1 <- ggplot(data_true_PE, aes(x = factor(agegp10), fill = factor(comorbid_smoking))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Smoking (PE/No PE)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_PE, aes(x = factor(agegp10), fill = factor(comorbid_smoking))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

###############################################################################
# Death
###############################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv")

# Remove rows with NA or missing age, sex, region, country, income, outcome
data <- data[!(is.na(data$age) | data$age==""), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples
data <- data[!(is.na(data$outcome) | data$outcome==""), ] # 734282 samples

# Create death variable
data <- data %>%
  mutate(
    death = case_when(outcome == 'death' ~ 1,
                      TRUE ~ 0))

# Convert age into custom categories
data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

# Replace NA with FALSE for PE
data[c("pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism")][is.na(data[c("pulmonary_embolism_or_dvt",
                                                              "thromboembolsim", "pulmonary_embolism")])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Get the subset of data matching each patient group (target and control)
data_true_death <- subset(data, death == 1)
data_false_death <- subset(data, death == 0)

# Symptomatic
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptomatic))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Symptomatic (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptomatic))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Abdominal Pain
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_abdominal_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Abdominal Pain (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_abdominal_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Confusion
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_altered_consciousness_confusion))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Confusion (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_altered_consciousness_confusion))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Bleeding
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_bleeding))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Bleeding (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_bleeding))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Chest Pain
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_chest_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Chest Pain (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_chest_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Conjunctivitis
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_conjunctivitis))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Conjunctivitis (Death/No Death))") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_conjunctivitis))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Cough
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_cough))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Cough (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_cough))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Diarrhoea
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_diarrhoea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Diarrhoea (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_diarrhoea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Ear Pain
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_ear_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Ear Pain (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_ear_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Fatigue
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_fatigue_malaise))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Fatigue (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_fatigue_malaise))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Headache
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_headache))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Headache (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_headache))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Fever
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_history_of_fever))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Fever (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_history_of_fever))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Lost Sense of Smell
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_smell))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Lost Sense of Smell (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_smell))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Lost Sense of Taste
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_taste))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Lost Sense of Taste (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_taste))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Lymphadenopathy
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_lymphadenopathy))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Lymphadenopathy (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_lymphadenopathy))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Muscle Aches and Joint Pain
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_muscle_aches_joint_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Muscle Aches and Joint Pain (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_muscle_aches_joint_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Runny Nose
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_runny_nose))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Runny Nose (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_runny_nose))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Seizures
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_seizures))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Seizures (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_seizures))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Severe Dehydration
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_severe_dehydration))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Severe Dehydration (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_severe_dehydration))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Shortness of Breath
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_shortness_of_breath))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Shortness of Breath (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_shortness_of_breath))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Skin Rash
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_skin_rash))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Skin Rash (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_skin_rash))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Sore Throat
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_sore_throat))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Sore Throat (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_sore_throat))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Vomiting
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_vomiting_nausea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Vomiting (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_vomiting_nausea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Wheezing
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(symptoms_wheezing))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Wheezing (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(symptoms_wheezing))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Comorbidities
# AIDS/HIV
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_aids_hiv))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "AIDS/HIV (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_aids_hiv))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Asthma
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_asthma))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Asthma (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_asthma))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Cardiac Disease
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_cardiac_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Cardiac Disease (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_cardiac_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Haematological Disease
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_haematological_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Haematological Disease (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_haematological_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Kidney Disease
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_kidney_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Kidney Disease (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_kidney_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Neurological Disease
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_neurological_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Neurological Disease (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_neurological_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Pulmonary Disease
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_pulmonary_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Pulmonary Disease (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_chronic_pulmonary_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Dementia
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_dementia))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Dementia (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_dementia))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Diabetes
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_diabetes))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Diabetes (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_diabetes))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Hypertension
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_hypertension))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Hypertension (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_hypertension))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Immunosuppression
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_immunosuppression))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Immunosuppression (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_immunosuppression))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Liver Disease
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_liver_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Liver Disease (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_liver_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Malignant Neoplasm
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_malignant_neoplasm))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Malignant Neoplasm (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_malignant_neoplasm))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Malnutrition
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_malnutrition))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Malnutrition (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_malnutrition))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Obesity
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_obesity))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Obesity (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_obesity))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Rheumatologic
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_rheumatologic_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Rheumatologic Disorder (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_rheumatologic_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

# Smoking
plot1 <- ggplot(data_true_death, aes(x = factor(agegp10), fill = factor(comorbid_smoking))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "Smoking (Death/No Death)") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
plot2 <- ggplot(data_false_death, aes(x = factor(agegp10), fill = factor(comorbid_smoking))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "",
       fill = "") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])
grid_arrange_shared_legend(plot1, plot2, ncol=2, left='Proportion')

