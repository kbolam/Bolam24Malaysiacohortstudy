## Cleaning the data frame 
#Malaysia cohort BOLAM24 


#renaming/creating the variables (run everytime)
# # creating and naming the CVD variable 

cohort$CVD <- NA
cohort$CVD[cohort$sr_heartf=="Yes"]<- 1
cohort$CVD[cohort$sr_heartf=="No"]<- 0
cohort$CVD[cohort$sr_mi=="Yes"]<- 1
cohort$CVD[cohort$sr_stroke=="Yes"]<- 1
table(cohort$CVD)

##rename the other variables and assign missing values NA. 

cohort$ethnicity <- (cohort$Race)
cohort$ethnicity <- ifelse(cohort$ethnicity %in% c("Malay", "Chinese", "Indian"), cohort$ethnicity, NA)

cohort$income<- (cohort$Update_H_Income)
cohort$income <- ifelse(cohort$income %in% c("<500", "500-999", "1000-1499", "1500-1999", "2000-2499", "2500-2999", "3000-3999","4000-4999","5000-6999","7000-9999","10000-12999", "13000-14999", ">=15000"), cohort$income, NA)

cohort$education <- (cohort$Education_Level)
cohort$education <- ifelse(cohort$education %in% c("Primary Education", "Secondary Education", "Tertiary Education"), cohort$education, NA)

cohort$hypertension <- (cohort$sr_hypertension)
cohort$hypertension <- ifelse(cohort$hypertension %in% c("Yes", "No"), cohort$hypertension, NA)

cohort$hyperlipidemia <- (cohort$sr_hyperlipid)
cohort$hyperlipidemia <- ifelse(cohort$hyperlipidemia %in% c("Yes", "No"), cohort$hyperlipidemia, NA)

cohort$diabetes <- (cohort$sr_diabetes)
cohort$diabetes <- ifelse(cohort$diabetes %in% c("Yes", "No"), cohort$diabetes, NA)

cohort$kidney <- (cohort$sr_kidney)
cohort$kidney <- ifelse(cohort$kidney %in% c("Yes", "No"), cohort$kidney, NA)

cohort$asthma <- (cohort$sr_asthma)
cohort$asthma <- ifelse(cohort$asthma %in% c("Yes", "No"), cohort$asthma, NA)

cohort$Gender <- ifelse(cohort$Gender %in% c("Female", "Male"), cohort$Gender, NA)

cohort$age <- (cohort$Age_at_Dexa_Scan)

cohort$CVD <- ifelse(cohort$CVD %in% c(1, 0), cohort$CVD, NA)

## Assigning missing data as NA (Applicable to income and education)

cohort$income[cohort$income=="Not Done Questionnaire"]<-NA
cohort$education[cohort$education=="Not Available"]<- NA


## Checking for missing data 
sum(is.na(cohort$ethnicity))
sum(is.na(cohort$Gender))
sum(is.na(cohort$income))  #36 missing (not done questionnaire)
sum(is.na(cohort$education)) #19 missing (not available)
sum(is.na(cohort$hypertension))
sum(is.na(cohort$hyperlipidemia))
sum(is.na(cohort$diabetes))
sum(is.na(cohort$CVD))
sum(is.na(cohort$kidney))
sum(is.na(cohort$asthma))

# creating a cleaned variable 

library(dplyr)
cohort <- cohort %>% filter(!is.na(education)) %>% filter(!is.na(income))