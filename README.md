# Kaggle_Titanic
Notes on working through the analysis of survival for the Titanic data set from Kaggle 

Working in R Studio

# Goal: Develop a model to predict whether a person would have survived the sinking of the Titanic (determine the factors that, in combination, contributed to survival)

# STEP 1: SETUP

### libraries imported
library(ggplot2)  //for data visualization

library(dplyr)    //set of data manipulation functions

library(nlme)   //linear modeling package (MAY NOT NEED THIS)

library(readxl)  //has "subset" function that allows you to generate subset table for rows that have given value in a specific column

### figured out working directory using gwtwd()
### set working directory using setwd()

train <- read.csv("train.csv") //imported training table and assigned it to object name "train"

# STEP 2: Examine characteristics of survivors (start with class, sex, & age, based on anecdotal reports of who survived) 

survivors <- subset(train, train$Survived==1)
	//create subset table 'survivors' containing only those who survived (not strictly necessary, but simplifies subsequent lines)

# STEP 2a: generate graph showing distributions of survivors by sex

ggplot(survivors, aes(survivors$Sex)) + geom_bar(survivors$Sex) 
  // creates a bar chart with count of male and female survivors)
  // eyeballing shows that there were about twice as many female survivors as male

# STEP 2b: get an actual count of male and female survivors

survsex <- survivors$Sex  
  // Generate a subset table that includes only data from the "sex" column of the 'sirvivors' table (may not be strictly necessary, but for neatness' sake)

sum(survsex=="female")   // generates a sum of the rows for which sex = "female" (quotations bc string)
  // 233 female survivors 

sum(survsex=="male")   //number of male survivors
  // 109 male survivors


# STEP 2c: get total number of male and female passengers

sum(train$Sex=="female")   // 314 female passengers

sum(train$Sex=="male")   // 577 male passengers

# STEP 2d: Check whether proprtionally more females survived than males (out of total number of each)

233/314    // ~74.2% of the female passengers survived
109/577   // only ~18.9% of the male passengers survived

**INTERIM CONCLUSION: proportionally more female than male passengers survived (large different in percentages indicates that this is definitely a variable to consider in larger model)


# Step 2d: generate graph showing distributions of survivors by age

age <- survivors$Age   # create variable for age (for ease of reference)
ggplot(survivors, aes(x=age)) + geom_density(aes(y=..count..))   // density plot approximating count of survivors of different ages
  //WHY IS DENSITY plot more appropriate than histogram?  Allows for more fine-grained/continuous age display 
  //bimodal, skewed distribution, indicating that suvivors clustered primarily in the 20-40yo range, with second, smaller peak under 10 yo

# STEP 2e: determine distribution of ages of all passengers

allpassage <- train$Age  # variable for age for table containing all passengers
ggplot(train, aes(x=allpassage)) + geom_density(aes(y=..count..))   / plot of all passangers, by age
  // ALL PASSENGESR BY AGE PLOT LOOKS VERY SIMILAR to survivor by age plot, but want to compare the two more directly

ggplot(train, aes(x=allpassage)) + geom_density(aes(y=..count.., group=train$Survived))    # OVERLAYS both plots, with two lines grouped by "survivial" factor
  # THE ABOVE WORKS, but it is somewhat difficult to distinguish the two distributions (bc both same color)
  # Am trying to figure out how to use reshape package and melt function to restructure the data so I can make the two distributions different colors

**INTERIM CONCLUSION (from b/w graph): proportionally more children (under 10) survived than other age groups

# STEP 2f: generate graph showing distribution of survivors by class

class <- survivors$Pclass    # creates variable for class from survivors data subset 
ggplot(survivors, aes(class)) + geom_bar(class)   # plots the number of survivors in a bar chart by class (1st, 2nd, 3rd)
# Class is a discrete var, so bar plot most appropriate
# More first-class passengers survived than other classes, then 3rd, then 2nd

# CHECK distribution of all passengers by class
allclass <- train$Pclass   # variable for class from full train dataset
ggplot(train, aes(allclass)) + geom_bar(aes(allclass))   # plots the number of passengers by class

**INTERIM CONCLUSION: from a visual comparison of the two plots, it looks like proportionally more 1st class passengers survived than 3rd or second class passengers (could do same calculation of actual proportions as did with sex, but moving on to model now)
  #COULD ALSO MAYBE DO A CHI-square test, or log likelihood, but should be able to do that as part of the larger model 


**STEP 3: generate a logistic regression model of survival with age, sex, and class as predictors

allsex <- train$Sex  ## created two new variables referencing sex and age in the training dataset
allage <- train$Age

model_age_sex_class <- glm(formula = survived ~ allage + allsex + allclass, data = train, family = "binomial")

## OUTPUT FOR THIS MODEL:
Call:
glm(formula = survived ~ allage + allsex + allclass, family = "binomial", 
    data = train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7270  -0.6799  -0.3947   0.6483   2.4668  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  5.056006   0.502128  10.069  < 2e-16 ***
allage      -0.036929   0.007628  -4.841 1.29e-06 ***
allsexmale  -2.522131   0.207283 -12.168  < 2e-16 ***
allclass    -1.288545   0.139259  -9.253  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 964.52  on 713  degrees of freedom
Residual deviance: 647.29  on 710  degrees of freedom
  (177 observations deleted due to missingness) # THIS IS A PROBLEM - losing a lot of data here
AIC: 655.29

Number of Fisher Scoring iterations: 5

# There will be several more models with different variables, and models will be compared according to model fit (what the best criterion to use here?  NEed to refresh knowedlge on that)

# POINTS TO CONSIDER when constructing the model
    # potential sparseness of Age variable (177 passengers did not have ages listed, out of 891 total passengers)
      ## NEED TO IMPUTE VALUES FOR THIS (examine R tutorial linked on competition page)
    # MULTICOLLINEARITY (check correlation matrix between for predictors)

# OTHER FACTORS to consider
    # family size, parental status, cabin (some closer to impact?), point of embarkation (not sure how theoretically this might be relevant, but tutorial considers it)       

**STEP 4: actually use the final 
