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

STEP 2: Examine characteristics of survivors (start with class, gender, & age, based on anecdotal reports of who survived) 

survivors <- subset(train, train$Survived==1) //create subset table 'survivors' containing only those who survived

//generate graphs showing distributions of survivors by class, gender, & age
