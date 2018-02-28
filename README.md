# Kaggle_Titanic
Notes on working through the analysis of survival for the Titanic data set from Kaggle 

Working in R Studio

Goal: Develop a model to predict whether a person would have survived the sinking of the Titanic (determine the factors that, in combination, contributed to survival)

STEP 1: SETUP

//libraries imported
library(ggplot2) //for data visualization//
library(dplyr) //set of data manipulation functions//
library(nlme) //linear modeling package ** MAY NOT NEED THIS//
library(readxl) //has "subset" function that allows you to generate subset table for rows that have given value in a specific column//

//figured out working directory using gwtwd()
//set working directory using setwd()

train <- read.csv("train.csv") //imported training table and assigned it to object name "train"

**STEP 2: Examine characteristics of survivors (start with class, sex, & age, based on anecdotal reports of who survived) 

survivors <- subset(train, train$Survived==1) //create subset table 'survivors' containing only those who survived (not strictly necessary, but simplifies each line of code somewhat)

**STEP 2a: generate graph showing distributions of survivors by sex

ggplot(survivors, aes(survivors$Sex)) + geom_bar(survivors$Sex) //creates a bar chart with count of male and female survivors)
//eyeballing shows that there were about twice as many female survivors as male

**STEP 2b: get an actual count of male and female survivors

survsex <- survivors$Sex //Generate a subset table that includes only data from the "sex" column of the 'sirvivors' table (may not be strictly necessary, but for neatness' sake)

sum(survsex=="female") //generates a sum of the rows for which sex = "female" (quotations bc string)
//233 female survivors 

sum(survsex=="male") //number of male survivors
//109 male survivors


**STEP 2c: get total number of male and female passengers

sum(train$Sex=="female") //314 female passengers

sum(train$Sex=="male") //577 male passengers

**STEP 2d: Check whether proprtionally more females survived than males (out of total number of each)

233/314  //~74.2% of the female passengers survived
109/577 //only ~18.9% of the male passengers survived

**INTERIM CONCLUSION: proportionally more female than male passengers survived (large different in percentages indicates that this is definitely a variable to consider in larger model)


**Step 2d: generate graph showing distributions of survivors by age
ggplot(survivors, aes(survivors$Age)) + geom_density(survivors$Age) //creates a density plot approximating count of survivors of different ages)
//WHY IS DENSITY plot here more appropriate than histogram?  Perhaps because it allows for more fine-grained age display? 
//eyeballing shows that there were about twice as many female survivors as male

