#Created: 2019-03-21
#Author: Alex Zajichek
#Package: cheese
#Description: Script for building dataset found in /data

#Load packages
require(tidyverse)

#Read dataset from UCI repository
heart_disease <-
    read_delim(
        file = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
        delim = ",",
        col_names = FALSE
    ) %>%
    
    #Rename/clean columns
    transmute(
        Age = X1,
        Sex = 
            factor(X2) %>%
            fct_recode(
                Male = "1",
                Female = "0"
            ),
        ChestPain =
            factor(X3) %>%
            fct_recode(
                `Typical angina` = "1",
                `Atypical angina` = "2",
                `Non-anginal pain` = "3",
                Asymptomatic = "4"
            ),
        BP = X4,
        Cholesterol = X5,
        BloodSugar = as.logical(X6),
        MaximumHR = X8,
        ExerciseInducedAngina =
            factor(X9) %>%
            fct_recode(
                Yes = "1",
                No = "0"
            ),
        HeartDisease =
            factor(X14) %>%
            fct_collapse(
                No = "0",
                Yes = c("1", "2", "3", "4")
            )
    )


