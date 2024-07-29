#Malaysia cohort script 
#Date - started on 09/07/2024
#Author - Katie Bolam 

setwd(")
cohort<-read.csv("", na.strings=c(""))

# Getting to know the data script: establishing the types of variables and basic summary stats. 

summary(cohort$Race)
table(cohort$Race)
summary(cohort$Gender)
table(cohort$Gender)
sum(is.na(cohort$Gender))
sum(is.na(cohort$Race))
sum(is.na(cohort$Update_H_Income))
summary(cohort$Update_H_Income)
table(cohort$Update_H_Income)
summary(cohort$Education_Level)
table(cohort$Education_Level)
sum(is.na(cohort$Education_Level))
sum(is.na(cohort$sr_hypertension))
table(cohort$sr_hypertension)
table(cohort$sr_hyperlipid)
sum(is.na(cohort$sr_hyperlipid))
table(cohort$sr_diabetes)
table(cohort$sr_stroke, cohort$sr_heartf, cohort$sr_mi)
table(cohort$sr_heartf)
table(cohort$sr_mi)
table(cohort$sr_kidney)
sum(is.na(cohort$sr_kidney))
sum(is.na(cohort$sr_asthma))
table(cohort$sr_asthma)
sd(cohort$Age_at_Dexa_Scan)
var(cohort$Age_at_Dexa_Scan)
hist(cohort$Age_at_Dexa_Scan, col=blues9, ylab = 'frequency', xlab = 'Age', main = 'Cohort Age')

# Any CVD y/n #

cohort$CVD <- NA
cohort$CVD[cohort$sr_heartf=="Yes"]<- 1
cohort$CVD[cohort$sr_heartf=="No"]<- 0
cohort$CVD[cohort$sr_mi=="Yes"]<- 1
cohort$CVD[cohort$sr_stroke=="Yes"]<- 1
table(cohort$CVD)
table(cohort$CVD)

# Number of CVD total (MI, HeartF and Stroke)

cohort$sr_heartf_2[cohort$sr_heartf=="Yes"]<- 1
cohort$sr_heartf_2[cohort$sr_heartf=="No"]<- 0
cohort$sr_mi_2[cohort$sr_mi=="Yes"]<- 1
cohort$sr_mi_2[cohort$sr_mi=="No"]<- 0
cohort$sr_stroke_2[cohort$sr_stroke=="Yes"]<- 1
cohort$sr_stroke_2[cohort$sr_stroke=="No"]<- 0
cohort$CVD_total <- rowSums(cohort[,58:60])
table(cohort$CVD_total)

#Cleaning the Variable (outliers of age)

Q1age <- quantile(cohort$Age_at_Dexa_Scan, 0.25)
print(Q1age)
Q3age <- quantile(cohort$Age_at_Dexa_Scan, 0.75)
print(Q3age)
IQRage <- IQR(cohort$Age_at_Dexa_Scan)
outliersage <- subset(cohort, cohort$Age_at_Dexa_Scan<(Q1age - 1.5*IQR), cohort$Age_at_Dexa_Scan>(Q3age + 1.5*IQR))

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

#### TABLE 1A. STATISTICS ####


#Association of variables (count)

table(cohort$Gender)
by(cohort$Gender, cohort$ethnicity, table)

table(cohort$education)
by(cohort$education, cohort$ethnicity, table)

table(cohort$hypertension)
by(cohort$hypertension, cohort$ethnicity, table)

table(cohort$hyperlipidemia)
by(cohort$hyperlipidemia, cohort$ethnicity, table)

table(cohort$diabetes)
by(cohort$diabetes, cohort$ethnicity, table)

table(cohort$CVD)
by(cohort$CVD, cohort$ethnicity, table)

table(cohort$kidney)
by(cohort$kidney, cohort$ethnicity, table)

table(cohort$age)
by(cohort$age, cohort$ethnicity, table)

table(cohort$asthma)
by(cohort$asthma, cohort$ethnicity, table)

## age by overall then by ethnicity (mean and SD)

mean(cohort$age, na.rm = TRUE)
sd(cohort$age)
mean_age_by_ethnicity <- tapply(cohort$age, cohort$ethnicity, FUN = mean)
print(mean_age_by_ethnicity)
sd_age_by_ethnicity <- tapply(cohort$age, cohort$ethnicity, FUN = sd)
print(sd_age_by_ethnicity)

## Income (grouping to get overall and then by ethnicity - run everytime)
# 1 = lowest incomes 2 = middle incomes  3 = upper incomes

cohort$grouped_income <- NA
cohort$grouped_income[cohort$income %in% c("<500", "500-999", "1000-1499", "1500-1999", "2000-2499" )] <- 1
cohort$grouped_income[cohort$income %in% c("2500-2999", "3000-3999","4000-4999")] <- 2
cohort$grouped_income[cohort$income %in% c("5000-6999","7000-9999")] <- 3 
cohort$grouped_income[cohort$income %in% c("10000-12999", "13000-14999", ">=15000")] <- 4
table(cohort$grouped_income)

## income groups by ethnicity

by(cohort$grouped_income, cohort$ethnicity, table)

# Association tests (getting p-values)
## chi2 test for gender by ethnicity 

gender_by_ethnicity_table <- table(cohort$Gender, cohort$ethnicity)
g_by_e <- chisq.test(gender_by_ethnicity_table)

## ANOVA test for mean age by ethnicity 

anova_result_age_ethnicity <- aov(cohort$age ~ cohort$ethnicity, data = cohort)
summary(anova_result_age_ethnicity)
p_value_age_ethnicity <- summary(anova_result_age_ethnicity)[[1]]$`Pr(>F)`[1]
print(p_value_age_ethnicity)

## chi2 test for disease prevalence by ethnicity 

hypertension_by_ethnicity_table <- table(cohort$hypertension, cohort$ethnicity)
hp_by_ethnicity <- chisq.test(hypertension_by_ethnicity_table)
print(hp_by_ethnicity)

hyperlipidemia_by_ethnicity_table <- table(cohort$hyperlipidemia, cohort$ethnicity)
hl_by_ethnicity <- chisq.test(hyperlipidemia_by_ethnicity_table)
print(hl_by_ethnicity)

diabetes_by_ethnicity_table <- table(cohort$diabetes, cohort$ethnicity)
d_by_ethnicity <- chisq.test(diabetes_by_ethnicity_table)
print(d_by_ethnicity)

CVD_by_ethnicity_table <- table(cohort$CVD, cohort$ethnicity)
CVD_by_ethnicity <- chisq.test(CVD_by_ethnicity_table)
print(CVD_by_ethnicity)

kidney_by_ethnicity_table <- table(cohort$kidney, cohort$ethnicity)
k_by_ethnicity <- chisq.test(kidney_by_ethnicity_table)
print(k_by_ethnicity)

asthma_by_ethnicity_table <- table(cohort$asthma, cohort$ethnicity)
a_by_ethnicity <- chisq.test(asthma_by_ethnicity_table)
print(a_by_ethnicity)

#chi test for income by ethnicity

income_by_ethnicity_table <- table(cohort$income, cohort$ethnicity)
income_by_ethnicity <- chisq.test(income_by_ethnicity_table)
print(income_by_ethnicity)

#chi test for education by ethnicity 

education_by_ethnicity_table <- table(cohort$education, cohort$ethnicity)
education_by_ethnicity <- chisq.test(education_by_ethnicity_table)
print(education_by_ethnicity)

##### TABLE 1B STATISTICS #####

#mean and SD of age by gender

mean_age_by_gender <- tapply(cohort$age, cohort$Gender, FUN = mean)
print(mean_age_by_gender)
sd_age_by_gender <- tapply(cohort$age, cohort$Gender, FUN = sd)
print(sd_age_by_gender)

#grouped income by gender (using grouped income created for table 1A statistics)
by(cohort$grouped_income, cohort$Gender, table)

#count and percentage of other variables 

by(cohort$education, cohort$Gender, table)
by(cohort$hypertension, cohort$Gender, table)
by(cohort$hyperlipidemia, cohort$Gender, table)
by(cohort$diabetes, cohort$Gender, table)
by(cohort$CVD, cohort$Gender, table)
by(cohort$kidney, cohort$Gender, table)
by(cohort$asthma, cohort$Gender, table)

## ANOVA test for mean age by gender

anova_result_age_gender <- aov(cohort$age ~ cohort$Gender, data = cohort)
summary(anova_result_age_gender)
p_value_age_gender <- summary(anova_result_age_gender)[[1]]$`Pr(>F)`[1]
print(p_value_age_gender)

# chi test income by gender

income_by_gender_table <- table(cohort$income, cohort$Gender)
income_by_gender <- chisq.test(income_by_gender_table)
print(income_by_gender)

# chi test education by gender

education_by_gender_table <- table(cohort$education, cohort$Gender)
education_by_gender <- chisq.test(education_by_gender_table)
print(education_by_gender)

## chi2 test for disease prevalence by gender

hypertension_by_gender_table <- table(cohort$hypertension, cohort$Gender)
hp_by_gender <- chisq.test(hypertension_by_gender_table)
print(hp_by_gender)

hyperlipidemia_by_gender_table <- table(cohort$hyperlipidemia, cohort$Gender)
hl_by_gender <- chisq.test(hyperlipidemia_by_gender_table)
print(hl_by_gender)

diabetes_by_gender_table <- table(cohort$diabetes, cohort$Gender)
d_by_gender <- chisq.test(diabetes_by_gender_table)
print(d_by_gender)

CVD_by_gender_table <- table(cohort$CVD, cohort$Gender)
CVD_by_gender <- chisq.test(CVD_by_gender_table)
print(CVD_by_gender)

kidney_by_gender_table <- table(cohort$kidney, cohort$Gender)
k_by_gender <- chisq.test(kidney_by_gender_table)
print(k_by_gender)

asthma_by_gender_table <- table(cohort$asthma, cohort$Gender)
a_by_gender <- chisq.test(asthma_by_gender_table)
print(a_by_gender)


# REGRESSION #
## Uni variable logistic regression, exposure ethnicity, reference group Malay. 
cohort$ethnicity <- factor(cohort$ethnicity)
cohort$ethnicity <- relevel(cohort$ethnicity, ref = 'Malay')

#Outcome hypertension 
cohort$hypertension[cohort$hypertension %in% c("Yes")] <- 1
cohort$hypertension[cohort$hypertension %in% c("No")] <- 0
cohort$hypertension <- as.numeric(cohort$hypertension)
Unimodel <- glm(hypertension ~ ethnicity, family = 'binomial', data = cohort)
summary(Unimodel)
exp(cbind(coef(Unimodel), confint(Unimodel)))
## co variates sex and age 
model2 <- glm(hypertension ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model2)
exp(cbind(coef(model2), confint(model2)))
# co variates sex, age, income and education 
model3 <- glm(hypertension ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model3)
exp(cbind(coef(model3), confint(model3)))

## Outcome hyperlipidaemia 
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("Yes")] <- 1
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("No")] <- 0
cohort$hyperlipidemia <- as.numeric(cohort$hyperlipidemia)
model4 <- glm(hyperlipidemia ~ ethnicity, family = 'binomial', data = cohort)
summary(model4)
exp(cbind(coef(model4), confint(model4)))
## co variates sex and age 
model5 <- glm(hyperlipidemia ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model5)
exp(cbind(coef(model5), confint(model5)))
# co variates sex, age, income and education 
model6 <- glm(hyperlipidemia ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model6)
exp(cbind(coef(model6), confint(model6)))

#Outcome diabetes 
cohort$diabetes[cohort$diabetes %in% c("Yes")] <- 1
cohort$diabetes[cohort$diabetes %in% c("No")] <- 0
cohort$diabetes <- as.numeric(cohort$diabetes)
model7 <- glm(diabetes ~ ethnicity, family = 'binomial', data = cohort)
summary(model7)
exp(cbind(coef(model7), confint(model7)))
## co variates sex and age 
model8 <- glm(diabetes ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model8)
exp(cbind(coef(model8), confint(model8)))
# co variates sex, age, income and education 
model9 <- glm(diabetes ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model9)
exp(cbind(coef(model9), confint(model9)))

#outcome CVD 
# Already assigned 1 and 0 values when CVD variable created 
cohort$CVD <- as.numeric(cohort$CVD)
model10 <- glm(CVD ~ ethnicity, family = 'binomial', data = cohort)
summary(model10)
exp(cbind(coef(model10), confint(model10)))
# Co variates sex and age 
model11 <- glm(CVD ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model11)
exp(cbind(coef(model11), confint(model11)))
# Co variates sex, age, income and education 
model12 <- glm(CVD ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model12)
exp(cbind(coef(model12), confint(model12)))

#outcome kidney 
cohort$kidney[cohort$kidney %in% c("Yes")] <- 1
cohort$kidney[cohort$kidney %in% c("No")] <- 0
cohort$kidney <- as.numeric(cohort$kidney)
model13 <- glm(kidney ~ ethnicity, family = 'binomial', data = cohort)
summary(model13)
exp(cbind(coef(model13), confint(model13)))
exp(cbind(Odds_Ratio = coef(model13), confint(model13)))
# Co variates sex and age 
model14 <- glm(kidney ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model14)
exp(cbind(coef(model14), confint(model14)))
# Co variates sex, age, income and education 
model15 <- glm(kidney ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model15)
exp(cbind(coef(model15), confint(model15)))

#outcome asthma 
cohort$asthma[cohort$asthma %in% c("Yes")] <- 1
cohort$asthma[cohort$asthma %in% c("No")] <- 0
cohort$asthma <- as.numeric(cohort$asthma)
model16 <- glm(asthma ~ ethnicity, family = 'binomial', data = cohort)
summary(model16)
exp(cbind(coef(model16), confint(model16)))
exp(cbind(Odds_Ratio = coef(model16), confint(model16)))
# Co variates sex and age 
model17 <- glm(asthma ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model17)
exp(cbind(coef(model17), confint(model17)))
# Co variates sex, age, income and education 
model18 <- glm(asthma ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model18)
exp(cbind(coef(model18), confint(model18)))


# REGRESSION 2  #
## Uni variable logistic regression, exposure ethnicity, reference group Indian. 
cohort$ethnicity <- factor(cohort$ethnicity)
cohort$ethnicity <- relevel(cohort$ethnicity, ref = 'Indian')

#Outcome hypertension 
cohort$hypertension[cohort$hypertension %in% c("Yes")] <- 1
cohort$hypertension[cohort$hypertension %in% c("No")] <- 0
cohort$hypertension <- as.numeric(cohort$hypertension)
indiaUnimodel <- glm(hypertension ~ ethnicity, family = 'binomial', data = cohort)
summary(indiaUnimodel)
exp(cbind(coef(indiaUnimodel), confint(indiaUnimodel)))
## co variates sex and age 
indiamodel2 <- glm(hypertension ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel2)
exp(cbind(coef(indiamodel2), confint(indiamodel2)))
# co variates sex, age, income and education 
indiamodel3 <- glm(hypertension ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel3)
exp(cbind(coef(indiamodel3), confint(indiamodel3)))

## Outcome hyperlipidaemia 
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("Yes")] <- 1
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("No")] <- 0
cohort$hyperlipidemia <- as.numeric(cohort$hyperlipidemia)
indiamodel4 <- glm(hyperlipidemia ~ ethnicity, family = 'binomial', data = cohort)
summary(indiamodel4)
exp(cbind(coef(indiamodel4), confint(indiamodel4)))
## co variates sex and age 
indiamodel5 <- glm(hyperlipidemia ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel5)
exp(cbind(coef(indiamodel5), confint(indiamodel5)))
# co variates sex, age, income and education 
indiamodel6 <- glm(hyperlipidemia ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel6)
exp(cbind(coef(indiamodel6), confint(indiamodel6)))

#Outcome diabetes 
cohort$diabetes[cohort$diabetes %in% c("Yes")] <- 1
cohort$diabetes[cohort$diabetes %in% c("No")] <- 0
cohort$diabetes <- as.numeric(cohort$diabetes)
indiamodel7 <- glm(diabetes ~ ethnicity, family = 'binomial', data = cohort)
summary(indiamodel7)
exp(cbind(coef(indiamodel7), confint(indiamodel7)))
## co variates sex and age 
indiamodel8 <- glm(diabetes ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel8)
exp(cbind(coef(indiamodel8), confint(indiamodel8)))
# co variates sex, age, income and education 
indiamodel9 <- glm(diabetes ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel9)
exp(cbind(coef(indiamodel9), confint(indiamodel9)))

#outcome CVD 
# Already assigned 1 and 0 values when CVD variable created 
cohort$CVD <- as.numeric(cohort$CVD)
indiamodel10 <- glm(CVD ~ ethnicity, family = 'binomial', data = cohort)
summary(indiamodel10)
exp(cbind(coef(indiamodel10), confint(indiamodel10)))
# Co variates sex and age 
indiamodel11 <- glm(CVD ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel11)
exp(cbind(coef(indiamodel11), confint(indiamodel11)))
# Co variates sex, age, income and education 
indiamodel12 <- glm(CVD ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel12)
exp(cbind(coef(indiamodel12), confint(indiamodel12)))

#outcome kidney 
cohort$kidney[cohort$kidney %in% c("Yes")] <- 1
cohort$kidney[cohort$kidney %in% c("No")] <- 0
cohort$kidney <- as.numeric(cohort$kidney)
indiamodel13 <- glm(kidney ~ ethnicity, family = 'binomial', data = cohort)
summary(indiamodel13)
exp(cbind(coef(indiamodel13), confint(indiamodel13)))
exp(cbind(Odds_Ratio = coef(indiamodel13), confint(indiamodel13)))
# Co variates sex and age 
indiamodel14 <- glm(kidney ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel14)
exp(cbind(coef(indiamodel14), confint(indiamodel14)))
# Co variates sex, age, income and education 
indiamodel15 <- glm(kidney ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel15)
exp(cbind(coef(indiamodel15), confint(indiamodel15)))

#outcome asthma 
cohort$asthma[cohort$asthma %in% c("Yes")] <- 1
cohort$asthma[cohort$asthma %in% c("No")] <- 0
cohort$asthma <- as.numeric(cohort$asthma)
indiamodel16 <- glm(asthma ~ ethnicity, family = 'binomial', data = cohort)
summary(indiamodel16)
exp(cbind(coef(indiamodel16), confint(indiamodel16)))
exp(cbind(Odds_Ratio = coef(indiamodel16), confint(indiamodel16)))
# Co variates sex and age 
indiamodel17 <- glm(asthma ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel17)
exp(cbind(coef(indiamodel17), confint(indiamodel17)))
# Co variates sex, age, income and education 
indiamodel18 <- glm(asthma ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(indiamodel18)
exp(cbind(coef(indiamodel18), confint(indiamodel18)))


# REGRESSION #
## Uni variable logistic regression, exposure ethnicity, reference group Malay. 
cohort$ethnicity <- factor(cohort$ethnicity)
cohort$ethnicity <- relevel(cohort$ethnicity, ref = 'Malay')

#Outcome hypertension 
cohort$hypertension[cohort$hypertension %in% c("Yes")] <- 1
cohort$hypertension[cohort$hypertension %in% c("No")] <- 0
cohort$hypertension <- as.numeric(cohort$hypertension)
Unimodel <- glm(hypertension ~ ethnicity, family = 'binomial', data = cohort)
summary(Unimodel)
exp(cbind(coef(Unimodel), confint(Unimodel)))
## co variates sex and age 
model2 <- glm(hypertension ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model2)
exp(cbind(coef(model2), confint(model2)))
# co variates sex, age, income and education 
model3 <- glm(hypertension ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model3)
exp(cbind(coef(model3), confint(model3)))

## Outcome hyperlipidaemia 
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("Yes")] <- 1
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("No")] <- 0
cohort$hyperlipidemia <- as.numeric(cohort$hyperlipidemia)
model4 <- glm(hyperlipidemia ~ ethnicity, family = 'binomial', data = cohort)
summary(model4)
exp(cbind(coef(model4), confint(model4)))
## co variates sex and age 
model5 <- glm(hyperlipidemia ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model5)
exp(cbind(coef(model5), confint(model5)))
# co variates sex, age, income and education 
model6 <- glm(hyperlipidemia ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model6)
exp(cbind(coef(model6), confint(model6)))

#Outcome diabetes 
cohort$diabetes[cohort$diabetes %in% c("Yes")] <- 1
cohort$diabetes[cohort$diabetes %in% c("No")] <- 0
cohort$diabetes <- as.numeric(cohort$diabetes)
model7 <- glm(diabetes ~ ethnicity, family = 'binomial', data = cohort)
summary(model7)
exp(cbind(coef(model7), confint(model7)))
## co variates sex and age 
model8 <- glm(diabetes ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model8)
exp(cbind(coef(model8), confint(model8)))
# co variates sex, age, income and education 
model9 <- glm(diabetes ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model9)
exp(cbind(coef(model9), confint(model9)))

#outcome CVD 
# Already assigned 1 and 0 values when CVD variable created 
cohort$CVD <- as.numeric(cohort$CVD)
model10 <- glm(CVD ~ ethnicity, family = 'binomial', data = cohort)
summary(model10)
exp(cbind(coef(model10), confint(model10)))
# Co variates sex and age 
model11 <- glm(CVD ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model11)
exp(cbind(coef(model11), confint(model11)))
# Co variates sex, age, income and education 
model12 <- glm(CVD ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model12)
exp(cbind(coef(model12), confint(model12)))

#outcome kidney 
cohort$kidney[cohort$kidney %in% c("Yes")] <- 1
cohort$kidney[cohort$kidney %in% c("No")] <- 0
cohort$kidney <- as.numeric(cohort$kidney)
model13 <- glm(kidney ~ ethnicity, family = 'binomial', data = cohort)
summary(model13)
exp(cbind(coef(model13), confint(model13)))
exp(cbind(Odds_Ratio = coef(model13), confint(model13)))
# Co variates sex and age 
model14 <- glm(kidney ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model14)
exp(cbind(coef(model14), confint(model14)))
# Co variates sex, age, income and education 
model15 <- glm(kidney ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model15)
exp(cbind(coef(model15), confint(model15)))

#outcome asthma 
cohort$asthma[cohort$asthma %in% c("Yes")] <- 1
cohort$asthma[cohort$asthma %in% c("No")] <- 0
cohort$asthma <- as.numeric(cohort$asthma)
model16 <- glm(asthma ~ ethnicity, family = 'binomial', data = cohort)
summary(model16)
exp(cbind(coef(model16), confint(model16)))
exp(cbind(Odds_Ratio = coef(model16), confint(model16)))
# Co variates sex and age 
model17 <- glm(asthma ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
summary(model17)
exp(cbind(coef(model17), confint(model17)))
# Co variates sex, age, income and education 
model18 <- glm(asthma ~ ethnicity + Gender + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model18)
exp(cbind(coef(model18), confint(model18)))


# REGRESSION 3 #
## Uni variable logistic regression, exposure gender, reference group Male. 
cohort$Gender <- factor(cohort$Gender)
cohort$Gender<- relevel(cohort$Gender, ref = 'Male')

#Outcome hypertension 
cohort$hypertension[cohort$hypertension %in% c("Yes")] <- 1
cohort$hypertension[cohort$hypertension %in% c("No")] <- 0
cohort$hypertension <- as.numeric(cohort$hypertension)
genderUnimodel <- glm(hypertension ~ Gender, family = 'binomial', data = cohort)
summary(genderUnimodel)
exp(cbind(coef(genderUnimodel), confint(genderUnimodel)))
## co variates ethnicity and age 
gendermodel2 <- glm(hypertension ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel2)
exp(cbind(coef(gendermodel2), confint(gendermodel2)))
# co variates ethnicity, age, income and education 
gendermodel3 <- glm(hypertension ~ Gender + ethnicity + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel3)
exp(cbind(coef(gendermodel3), confint(gendermodel3)))

## Outcome hyperlipidaemia 
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("Yes")] <- 1
cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("No")] <- 0
cohort$hyperlipidemia <- as.numeric(cohort$hyperlipidemia)
gendermodel4 <- glm(hyperlipidemia ~ Gender, family = 'binomial', data = cohort)
summary(gendermodel4)
exp(cbind(coef(gendermodel4), confint(gendermodel4)))
## co variates sex and age 
gendermodel5 <- glm(hyperlipidemia ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel5)
exp(cbind(coef(gendermodel5), confint(gendermodel5)))
# co variates sex, age, income and education 
gendermodel6 <- glm(hyperlipidemia ~ Gender + ethnicity + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel6)
exp(cbind(coef(gendermodel6), confint(gendermodel6)))

#Outcome diabetes 
cohort$diabetes[cohort$diabetes %in% c("Yes")] <- 1
cohort$diabetes[cohort$diabetes %in% c("No")] <- 0
cohort$diabetes <- as.numeric(cohort$diabetes)
gendermodel7 <- glm(diabetes ~ Gender, family = 'binomial', data = cohort)
summary(gendermodel7)
exp(cbind(coef(gendermodel7), confint(gendermodel7)))
## co variates age and ethnicity 
gendermodel8 <- glm(diabetes ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel8)
exp(cbind(coef(gendermodel8), confint(gendermodel8)))
# co variates ethnicity , age, income and education 
gendermodel9 <- glm(diabetes ~ Gender + ethnicity + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel9)
exp(cbind(coef(gendermodel9), confint(gendermodel9)))

#outcome CVD 
# Already assigned 1 and 0 values when CVD variable created 
cohort$CVD <- as.numeric(cohort$CVD)
gendermodel10 <- glm(CVD ~ Gender, family = 'binomial', data = cohort)
summary(gendermodel10)
exp(cbind(coef(gendermodel10), confint(gendermodel10)))
# Co variates sex and age 
gendermodel11 <- glm(CVD ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel11)
exp(cbind(coef(gendermodel11), confint(gendermodel11)))
# Co variates sex, age, income and education 
gendermodel12 <- glm(CVD ~ Gender + ethnicity + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel12)
exp(cbind(coef(gendermodel12), confint(gendermodel12)))

#outcome kidney 
cohort$kidney[cohort$kidney %in% c("Yes")] <- 1
cohort$kidney[cohort$kidney %in% c("No")] <- 0
cohort$kidney <- as.numeric(cohort$kidney)
gendermodel13 <- glm(kidney ~ Gender, family = 'binomial', data = cohort)
summary(gendermodel13)
exp(cbind(coef(gendermodel13), confint(gendermodel13)))
exp(cbind(Odds_Ratio = coef(gendermodel13), confint(gendermodel13)))
# Co variates sex and age 
gendermodel14 <- glm(kidney ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel14)
exp(cbind(coef(gendermodel14), confint(gendermodel14)))
# Co variates sex, age, income and education 
gendermodel15 <- glm(kidney ~ Gender + ethnicity + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel15)
exp(cbind(coef(gendermodel15), confint(gendermodel15)))

#outcome asthma 
cohort$asthma[cohort$asthma %in% c("Yes")] <- 1
cohort$asthma[cohort$asthma %in% c("No")] <- 0
cohort$asthma <- as.numeric(cohort$asthma)
gendermodel16 <- glm(asthma ~ Gender, family = 'binomial', data = cohort)
summary(gendermodel16)
exp(cbind(coef(gendermodel16), confint(gendermodel16)))
exp(cbind(Odds_Ratio = coef(gendermodel16), confint(gendermodel16)))
# Co variates ethnicity and age 
gendermodel17 <- glm(asthma ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel17)
exp(cbind(coef(gendermodel17), confint(gendermodel17)))
# Co variates ethnicity, age, income and education 
gendermodel18 <- glm(asthma ~ Gender + ethnicity + age + income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel18)
exp(cbind(coef(gendermodel18), confint(gendermodel18)))
