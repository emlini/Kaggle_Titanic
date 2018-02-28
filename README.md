# Kaggle_Titanic
Notes on working through the analysis of survival for the Titanic data set from Kaggle 

Working in R Studio

Goal: develop a model to predict whether a person has survived

//library(ggplot2) for data visualization
//library(dplyr) set of data manipulation functions
//library(nlme) linear modeling package ** MAY NOT NEED THIS
//library(readxl) has "subset" function that allows you to generate subset table for rows that have given value in a specific column

//libraries imported

//figured out working directory using gwtwd()
//set working directory using setwd()

train <- read.csv("train.csv")
//imported training table and assigned it to object name "train"
