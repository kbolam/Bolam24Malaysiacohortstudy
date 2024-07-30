
setwd("J:/Forest Plots")
forest.data.chinese <- read.csv('J:/Forest Plots/forestplotdata (katie).csv')

#install packages

install.packages("ggplot2")
library(ggplot2)
install.packages("dplR")
library(dplyr)
install.packages("forcats")
library(forcats)
install.packages("forestplot")
library(forestplot)

#add color to the OR points 

forest.data.chinese$color <- ifelse(forest.data.chinese$Disease_outcome %in% c("Hypertension fully adjusted", "Diabetes fully adjusted", "CVD fully adjusted", "Hyperlipidaemia fully adjusted", "Kidney Disease fully adjusted", "Asthma fully adjusted"), "blue", "black")

# Convert Disease_outcome to a factor with specified levels
forest.data.chinese$Disease_outcome <- factor(forest.data.chinese$Disease_outcome,
                                             levels = c("Hypertension minimally adjusted", "Hypertension fully adjusted", "Hyperlipidaemia minimally adjusted", "Hyperlipidaemia fully adjusted",
                                                        "Diabetes minimally adjusted", "Diabetes fully adjusted", "CVD minimally adjusted", "CVD fully adjusted", "Kidney Disease minimally adjusted", "Kidney Disease fully adjusted",
                                                        "Asthma minimally adjusted", "Asthma fully adjusted"))

forest.data.chinese$color <- ifelse(forest.data.chinese$Disease_outcome %in% c("Hypertension fully adjusted", "Diabetes fully adjusted", "CVD fully adjusted", "Hyperlipidaemia fully adjusted", "Kidney Disease fully adjusted", "Asthma fully adjusted"), "blue", "black")

dev.off() # Prevents any non-compatible plotting occurring at the same time (prevents graphics errors)

forestplot.chinese <- ggplot(forest.data.chinese, aes(x = Chinese_OR, y = Disease_outcome)) +
  geom_point(aes(color = color)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = color), height = 0.1) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", alpha = 0.5) +
  scale_color_identity() +
  labs(title = "Risk of Disease Outcome by Ethnicity (Chinese)",
       x = "Odds Ratio (95% CI)",
       y = "Disease Outcome") +
  theme_minimal()

print(forestplot.chinese)

# Indian forest plot 

forest.data.indian <- read.csv("J:/Forest Plots/Forestplot data indian.csv")

#add color to the OR points 

forest.data.indian$color <- ifelse(forest.data.indian$Disease_outcome %in% c("Hypertension fully adjusted", "Diabetes fully adjusted", "CVD fully adjusted", "Hyperlipidaemia fully adjusted", "Kidney Disease fully adjusted", "Asthma fully adjusted"), "purple", "black")

# Convert Disease_outcome to a factor with specified levels
forest.data.indian$Disease_outcome <- factor(forest.data.indian$Disease_outcome,
                                              levels = c("Hypertension minimally adjusted", "Hypertension fully adjusted", "Hyperlipidaemia minimally adjusted", "Hyperlipidaemia fully adjusted",
                                                         "Diabetes minimally adjusted", "Diabetes fully adjusted", "CVD minimally adjusted", "CVD fully adjusted", "Kidney Disease minimally adjusted", "Kidney Disease fully adjusted",
                                                         "Asthma minimally adjusted", "Asthma fully adjusted"))
#Create the plot 

dev.off() # Prevents any non-compatible plotting occurring at the same time (prevents graphics errors)

forestplot.indian <- ggplot(forest.data.indian, aes(x = Indian_OR, y = Disease_outcome)) +
  geom_point(aes(color = color)) +
  geom_errorbarh(aes(xmin = lowerindian, xmax = upperindian, color = color), height = 0.1) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", alpha = 0.5) +
  scale_color_identity() +
  labs(title = "Risk of Disease Outcome by Ethnicity (Indian)",
       x = "Odds Ratio (95% CI)",
       y = "Disease Outcome") +
  theme_minimal()

print(forestplot.indian)
     

#Forest data Comparison 

df1 <- read.csv('J:/Forest Plots/Forestplot data indian.csv')
df2 <- read.csv('J:/Forest Plots/forestplotdata (katie).csv')
forest.data.comparison <- bind_rows(df1, df2)

'Indian OR' <- forest.data.comparison$Indian_OR
'Chinese OR' <- forest.data.comparison$Chinese_OR

#add color to the OR points 

forest.data.comparison$color <- ifelse(forest.data.comparison$Disease_outcome %in% c("Hypertension fully adjusted", "Diabetes fully adjusted", "CVD fully adjusted", "Hyperlipidaemia fully adjusted", "Kidney Disease fully adjusted", "Asthma fully adjusted"), "blue", "black")

# Convert Disease_outcome to a factor with specified levels
forest.data.comparison$Disease_outcome <- factor(forest.data.indian$Disease_outcome,
                                             levels = c("Hypertension minimally adjusted", "Hypertension fully adjusted", "Hyperlipidaemia minimally adjusted", "Hyperlipidaemia fully adjusted",
                                                        "Diabetes minimally adjusted", "Diabetes fully adjusted", "CVD minimally adjusted", "CVD fully adjusted", "Kidney Disease minimally adjusted", "Kidney Disease fully adjusted",
                                                        "Asthma minimally adjusted", "Asthma fully adjusted"))
#Create the plot 

#make the plot 

forestplot.comparison <- ggplot(forest.data.comparison) +
  geom_point(aes(x = Chinese_OR, y = Disease_outcome, color = "Chinese"), position = position_nudge(y = 0.0)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = Disease_outcome, color = 'Chinese'), height = 0.1, position = position_nudge(y = 0.0)) +
  geom_point(aes(x = Indian_OR, y = Disease_outcome, color = "Indian"), position = position_nudge(y = -0.15)) +
  geom_errorbarh(aes(xmin = lowerindian, xmax = upperindian, y = Disease_outcome, color = 'Indian'), height = 0.15, position = position_nudge(y = -0.15)) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("Chinese" = "blue", "Indian" = "purple")) +
  labs(title = "Risk of Disease Outcome by Ethnicity (Comparison)",
       x = "Odds Ratio (95% CI)",
       y = "Disease Outcome") +
  theme_minimal()

print(forestplot.comparison)

#Forest plot Gender

forest.data.gender <- read.csv("J:/Forest Plots/forest plot data gender.csv")

#add color to the OR points 

forest.data.gender$color <- ifelse(forest.data.gender$Disease.outcome %in% c("Hypertension fully adjusted", "Diabetes fully adjusted", "CVD fully adjusted", "Hyperlipidaemia fully adjusted", "Kidney Disease fully adjusted", "Asthma fully adjusted"), "orange", "black")

# Convert Disease_outcome to a factor with specified levels
forest.data.gender$Disease_outcome <- factor(forest.data.gender$Disease.outcome,
                                              levels = c("Hypertension minimally adjusted", "Hypertension fully adjusted", "Hyperlipidaemia minimally adjusted", "Hyperlipidaemia fully adjusted",
                                                         "Diabetes minimally adjusted", "Diabetes fully adjusted", "CVD minimally adjusted", "CVD fully adjusted", "Kidney Disease minimally adjusted", "Kidney Disease fully adjusted",
                                                         "Asthma minimally adjusted", "Asthma fully adjusted"))


dev.off() # Prevents any non-compatible plotting occurring at the same time (prevents graphics errors)

forest.data.indian$Disease_outcome <- factor(forest.data.indian$Disease.outcome,
                                             levels = c("Hypertension minimally adjusted", "Hypertension fully adjusted", "Hyperlipidaemia minimally adjusted", "Hyperlipidaemia fully adjusted",
                                                        "Diabetes minimally adjusted", "Diabetes fully adjusted", "CVD minimally adjusted", "CVD fully adjusted", "Kidney Disease minimally adjusted", "Kidney Disease fully adjusted",
                                                        "Asthma minimally adjusted", "Asthma fully adjusted"))
#Create the plot 

dev.off() # Prevents any non-compatible plotting occurring at the same time (prevents graphics errors)

forestplot.gender <- ggplot(forest.data.gender, aes(x = Female_OR, y = Disease.outcome)) +
  geom_point(aes(color = color)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = color), height = 0.1) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", alpha = 0.5) +
  scale_color_identity() +
  labs(title = "Risk of Disease Outcome by Gender",
       x = "Odds Ratio (95% CI)",
       y = "Disease Outcome") +
  theme_minimal()

print(forestplot.gender)

