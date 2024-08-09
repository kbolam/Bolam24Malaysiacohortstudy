#### TABLE 1. STATISTICS ####

#table 1a 

#Association of variables (count)
table(cohort$ethnicity)

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
table(cohort$Gender)

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