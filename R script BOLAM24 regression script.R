#Regression script 
#Malysia Cohort BOLAM24 

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
model3 <- glm(hypertension ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
model6 <- glm(hyperlipidemia ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
model9 <- glm(diabetes ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
model12 <- glm(CVD ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
model15 <- glm(kidney ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
model18 <- glm(asthma ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(model18)
exp(cbind(coef(model18), confint(model18)))

# exporting results 
install.packages("broom")
install.packages("writexl")
library(broom)
library(writexl)
library(dplyr)
Unimodel <- tidy(Unimodel)
model2 <- tidy(model2)
model3 <- tidy(model3)
model4 <- tidy(model4)
model5 <- tidy(model5)
model6 <- tidy(model6)
model7 <- tidy(model7)
model8 <- tidy(model8)
model9 <- tidy(model9)
model10 <- tidy(model10)
model11 <- tidy(model11)
model12 <- tidy(model12)
model13 <- tidy(model13)
model14 <- tidy(model14)
model15 <- tidy(model15)
model16 <- tidy(model16)
model17 <- tidy(model17)
model18 <- tidy(model18)
results.ethnicity <- bind_rows(Unimodel, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13, model14, model15, model16, model17, model18)
write.csv(results.ethnicity, "regression_results_ethnicity.csv")

# REGRESSION 2  #
## Uni variable logistic regression, exposure ethnicity, reference group Indian. 
#cohort$ethnicity <- factor(cohort$ethnicity)
#cohort$ethnicity <- relevel(cohort$ethnicity, ref = 'Indian')

#Outcome hypertension 
#cohort$hypertension[cohort$hypertension %in% c("Yes")] <- 1
#cohort$hypertension[cohort$hypertension %in% c("No")] <- 0
#cohort$hypertension <- as.numeric(cohort$hypertension)
#indiaUnimodel <- glm(hypertension ~ ethnicity, family = 'binomial', data = cohort)
#summary(indiaUnimodel)
#exp(cbind(coef(indiaUnimodel), confint(indiaUnimodel)))
## co variates sex and age 
#indiamodel2 <- glm(hypertension ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel2)
#exp(cbind(coef(indiamodel2), confint(indiamodel2)))
# co variates sex, age, income and education 
#indiamodel3 <- glm(hypertension ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel3)
#exp(cbind(coef(indiamodel3), confint(indiamodel3)))

## Outcome hyperlipidaemia 
#cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("Yes")] <- 1
#cohort$hyperlipidemia[cohort$hyperlipidemia %in% c("No")] <- 0
#cohort$hyperlipidemia <- as.numeric(cohort$hyperlipidemia)
#indiamodel4 <- glm(hyperlipidemia ~ ethnicity, family = 'binomial', data = cohort)
#summary(indiamodel4)
#exp(cbind(coef(indiamodel4), confint(indiamodel4)))
### co variates sex and age 
#indiamodel5 <- glm(hyperlipidemia ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel5)
#exp(cbind(coef(indiamodel5), confint(indiamodel5)))
# co variates sex, age, income and education 
#indiamodel6 <- glm(hyperlipidemia ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel6)
#exp(cbind(coef(indiamodel6), confint(indiamodel6)))

#Outcome diabetes 
#cohort$diabetes[cohort$diabetes %in% c("Yes")] <- 1
#cohort$diabetes[cohort$diabetes %in% c("No")] <- 0
#cohort$diabetes <- as.numeric(cohort$diabetes)
#indiamodel7 <- glm(diabetes ~ ethnicity, family = 'binomial', data = cohort)
#summary(indiamodel7)
#exp(cbind(coef(indiamodel7), confint(indiamodel7)))
## co variates sex and age 
#indiamodel8 <- glm(diabetes ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel8)
#exp(cbind(coef(indiamodel8), confint(indiamodel8)))
# co variates sex, age, income and education 
#indiamodel9 <- glm(diabetes ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel9)
#exp(cbind(coef(indiamodel9), confint(indiamodel9)))

#outcome CVD 
# Already assigned 1 and 0 values when CVD variable created 
#cohort$CVD <- as.numeric(cohort$CVD)
#indiamodel10 <- glm(CVD ~ ethnicity, family = 'binomial', data = cohort)
#summary(indiamodel10)
#exp(cbind(coef(indiamodel10), confint(indiamodel10)))
# Co variates sex and age 
#indiamodel11 <- glm(CVD ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel11)
#exp(cbind(coef(indiamodel11), confint(indiamodel11)))
# Co variates sex, age, income and education 
#indiamodel12 <- glm(CVD ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel12)
#exp(cbind(coef(indiamodel12), confint(indiamodel12)))

#outcome kidney 
#cohort$kidney[cohort$kidney %in% c("Yes")] <- 1
#cohort$kidney[cohort$kidney %in% c("No")] <- 0
#cohort$kidney <- as.numeric(cohort$kidney)
#indiamodel13 <- glm(kidney ~ ethnicity, family = 'binomial', data = cohort)
#summary(indiamodel13)
#exp(cbind(coef(indiamodel13), confint(indiamodel13)))
#exp(cbind(Odds_Ratio = coef(indiamodel13), confint(indiamodel13)))
# Co variates sex and age 
#indiamodel14 <- glm(kidney ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel14)
#exp(cbind(coef(indiamodel14), confint(indiamodel14)))
# Co variates sex, age, income and education 
#indiamodel15 <- glm(kidney ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel15)
#exp(cbind(coef(indiamodel15), confint(indiamodel15)))

#outcome asthma 
#cohort$asthma[cohort$asthma %in% c("Yes")] <- 1
#cohort$asthma[cohort$asthma %in% c("No")] <- 0
#cohort$asthma <- as.numeric(cohort$asthma)
#indiamodel16 <- glm(asthma ~ ethnicity, family = 'binomial', data = cohort)
#summary(indiamodel16)
#exp(cbind(coef(indiamodel16), confint(indiamodel16)))
#exp(cbind(Odds_Ratio = coef(indiamodel16), confint(indiamodel16)))
# Co variates sex and age 
#indiamodel17 <- glm(asthma ~ ethnicity + Gender + age, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel17)
#exp(cbind(coef(indiamodel17), confint(indiamodel17)))
# Co variates sex, age, income and education 
#indiamodel18 <- glm(asthma ~ ethnicity + Gender + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
#summary(indiamodel18)
#exp(cbind(coef(indiamodel18), confint(indiamodel18)))


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
gendermodel3 <- glm(hypertension ~ Gender + ethnicity + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
gendermodel6 <- glm(hyperlipidemia ~ Gender + ethnicity + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
gendermodel9 <- glm(diabetes ~ Gender + ethnicity + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
gendermodel12 <- glm(CVD ~ Gender + ethnicity + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel12)
exp(cbind(coef(gendermodel12), confint(gendermodel12)))

#outcome kidney 
cohort$kidney[cohort$kidney %in% c("Yes")] <- 1
cohort$kidney[cohort$kidney %in% c("No")] <- 0
cohort$kidney <- as.numeric(cohort$kidney)
gendermodel13 <- glm(kidney ~ Gender, family = 'binomial', data = cohort)
summary(gendermodel13)
exp(cbind(coef(gendermodel13), confint(gendermodel13)))
# Co variates sex and age 
gendermodel14 <- glm(kidney ~ Gender + ethnicity + age, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel14)
exp(cbind(coef(gendermodel14), confint(gendermodel14)))
# Co variates sex, age, income and education 
gendermodel15 <- glm(kidney ~ Gender + ethnicity + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
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
gendermodel18 <- glm(asthma ~ Gender + ethnicity + age + grouped_income + education, family = 'binomial'(link = "logit"), data = cohort)
summary(gendermodel18)
exp(cbind(coef(gendermodel18), confint(gendermodel18)))

# Export the results 
genderUnimodel <- tidy(genderUnimodel)
gendermodel2 <- tidy(gendermodel2)
gendermodel3 <- tidy(gendermodel3)
gendermodel4 <- tidy(gendermodel4)
gendermodel5 <- tidy(gendermodel5)
gendermodel6 <- tidy(gendermodel6)
gendermodel7 <- tidy(gendermodel7)
gendermodel8 <- tidy(gendermodel8)
gendermodel9 <- tidy(gendermodel9)
gendermodel10 <- tidy(gendermodel10)
gendermodel11 <- tidy(gendermodel11)
gendermodel12 <- tidy(gendermodel12)
gendermodel13 <- tidy(gendermodel13)
gendermodel14 <- tidy(gendermodel14)
gendermodel15 <- tidy(gendermodel15)
gendermodel16 <- tidy(gendermodel16)
gendermodel17 <- tidy(gendermodel17)
gendermodel18 <- tidy(gendermodel18)
results.gender <- bind_rows(genderUnimodel, gendermodel2, gendermodel3, gendermodel4, gendermodel5, gendermodel6, gendermodel7, gendermodel8, gendermodel9, gendermodel10, gendermodel11, gendermodel12, gendermodel13, gendermodel14, gendermodel15, gendermodel16, gendermodel17, gendermodel18)
write.csv(results.gender, "regression_results_gender.csv")
