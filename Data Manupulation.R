
# Get working directory of the current project.
getwd()

# Pointing the working directory where the data file resides.
#setwd("F:/Janaki & Venky/Data Science/Projects/Foundation Capstone Project")
setwd("F:/Janaki & Venky/Data Science/Data Science Exercises/Exercise 1_ Data Wrangling")

# Data Import to R and creates the dataframe mydata.
mydata <- read.csv("refine_original_2.CSV")

mydata
colnames(mydata)

#help(read.csv)


# 1. clean up brand names

install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)

c_company <- mydata %>% 
  mutate(lower_company = tolower(company),
         first_letter = substr(lower_company, 0, 1), 
         clean_company = ifelse(first_letter == "p", "phillips", 
                                ifelse(first_letter == "f", "phillips",
                                       ifelse(first_letter == "a", "akzo",
                                              ifelse(first_letter == "v", "van Houten",
                                                     ifelse(first_letter == "u", "unliever", first_letter))))))

c_company

#Create Vector/Dataframe for product code and product name
Category <- data.frame(product_code = c("p", "v", "x", "q"),product_number = c("Smartphone", "TV", "Laptop", "Tablet")) 
Category

# 2. seperate one column to multiple columns.
Final <- c_company %>%
  separate(Product.code...number, into = c('product_code','product_number'), sep = "-")

View(Final)

# Either use leftjoin or merge to combine 2 dataframes.

#Join_data <- Final_data %>%
# left_join(Final_data, Category, by = "product_code")

join<-merge(Final, Category, "product_code")

View(join)

#Combine columns address, city, country to full_address
caddress <- join %>%
  unite(full_address, address:country, sep = ',')

View(caddress)

#Create binary variables
result <- caddress %>%
  mutate(company_phillips = ifelse(clean_company == 'phillips',1,0)) %>%
  mutate(company_akzo = ifelse(clean_company == 'akzo',1,0)) %>% 
  mutate(company_van_houten = ifelse(clean_company == 'van houten',1,0)) %>% 
  mutate(company_unilever = ifelse(clean_company == 'unilever',1,0)) %>% 
  
  mutate(product_smartphone = ifelse(product_number.y == 'Smartphone',1,0)) %>% 
  mutate(product_tv = ifelse(product_number.y == 'Tv',1,0)) %>% 
  mutate(product_laptop = ifelse(product_number.y == 'Laptop',1,0)) %>% 
  mutate(product_tablet = ifelse(product_number.y == 'Tablet',1,0)) 

View(result)

#Remove redundant variables
#select(-c(company, product_code, product_categories))

#print .csv file
write.csv(result, 'Result.csv', row.names = FALSE)
