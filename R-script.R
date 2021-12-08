# ads analysis


# A Kenyan entrepreneur has created an online cryptography course and
#would want to advertise it on her blog. She currently targets audiences
#originating from various countries. In the past, she ran ads to advertise
#a related course on the same blog and collected data in the process. 
#She would now like to employ your services as a Data Science Consultant
#to help her identify which individuals are most likely to click on her ads. 

# installing packages needed 
library(dplyr)
library(ggplot2)
library("data.table")



# loading the dataset provided
ads<- read.csv('/home/francis/Downloads/advertising.csv')
head(ads)


# checking for duplicate rows in the dataset
duplicated_rows <- ads[duplicated(ads),]

duplicated_rows
# there are no duplicate rows or columns in the dataset

# checking for null values in the dataset
colSums(is.na(ads))

# There are no missing values in the dataset



# finding average income range of people being targeted 
income.mean <- mean(ads$Area.Income)

income.mean
# The average income of people targeted is 55,000.

#Checking for outliers in the income column
boxplot(ads$Area.Income)

# checking the frequency of income in the ads dataset using a histogram
hist(ads$Area.Income)

# The income range thet is most frequent is from 50,000 to 70,000.

# Finding the average age of people in our dataset
ads.age <- mean(ads$Age)

ads.age

# The average age of the people in the dataset is 36 years.

# Checking for outliers in the dataset
boxplot(ads$Age)

#there are no outliers in the age column

#checking the most frequent age in the ads dataset using a histogram.
hist(ads$Age)

# the most frequent age ranges between 25 and 40 years.

# We can see the most recurrent city with mode, this will show use the city
# that is most popular with the ads.
# mode function
mode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

# getting the mode
top.city.mode <- mode(ads$City)
top.city.mode

# The most popular city in terms of ads shown is Lisamouth

# We can find the most recurring country using mode as well

top.country <- mode(ads$Country)
top.country
# From the above we can see that Czech Republic has the most ads shown to them


# Checking the distribution in terms of gender where 1 is Male and 0 is Female
gender <- (ads$Male)
gender.frequency <- table(gender)
gender.frequency
# plotting to visualize the distribution
barplot(gender.frequency)

#From the table and bar graph above, we can see that there are more female
#viewers than male.
##plotting to see Covarience 

#Plotting to see the daily internet usage
daily_internet_usage <- ads$Daily.Internet.Usage
area_income <- ads$Area.Income
cov(daily_internet_usage,area_income)

#
age <- ads$Age
internet_usage_frequency <- ads$Daily.Internet.Usage
cov(age,internet_usage_frequency)
