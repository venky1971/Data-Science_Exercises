
install.packages("tidyverse")
library(tidyverse)
library (dplyr)

#Get and set working directory
getwd()
setwd("F:/Janaki & Venky/Data Science/Data Science Exercises/Exercise 2_Data Wrangling")

# 1. Load file to titanic_original.csv

titanic_data1<- read.csv("titanic_original.csv")
View(titanic_data1)

# 2. Insert 'S' if embarked = ""
titanic_data1$embarked[titanic_data1$embarked == ""] <- 'S'

#View(titanic_data$embarked[titanic_data$embarked != "S" && titanic_data$embarked != "C"])
#View(titanic_data$embarked[titanic_data$embarked == "Q" ])

# 3. Calculate the mean of the Age column and use that value to populate the missing values

# find the missing rows.
age_missingvalues <- titanic_data1$age[titanic_data1$age == "NA"]
View (age_missingvalues)

# Find the mean for the rows that has values.
mean_age <- mean(titanic_data1$age, na.rm=TRUE)
View (mean_age)

# Assign mean values to missing rows.
titanic_data1$age[is.na(titanic_data1$age) == TRUE] <- mean_age

#NOT WORKING ****   titanic_data1$age[age_missingvalues] <- mean_age
View(titanic_data1)

# 3(b) Think about other ways you could have populated the missing values in the age column. Why would you pick any of those over the mean (or not)?

# 4. Boat column - Fill empty slots with a dummy value e.g. 'None' or 'NA'

titanic_data1$boat[titanic_data1$boat == ""] <- "NA"

# Did the job with a warning message as 
# Warning message:
#In `[<-.factor`(`*tmp*`, titanic_data1$boat == " ", value = c(13L,  :invalid factor level, NA generated


# 5. Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

titanic_data1$has_cabin_number <- titanic_data1$cabin
titanic_data1$has_cabin_number[titanic_data1$has_cabin_number == ""] <- NA
titanic_data1$has_cabin_number <- is.na(titanic_data1$has_cabin_number)
titanic_data1$has_cabin_number <- ifelse(titanic_data1$has_cabin_number == TRUE, 0, 1)
View(titanic_data1)
View(titanic_data1$has_cabin_number)

titanic_clean <- titanic_data1
# 6. Create CSV file with titanic_clean data.
write.csv(titanic_clean, 'titanic_clean.csv')


















