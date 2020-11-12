## Author:  Iris Orizar
## Date:    November 11 2020
## Version: 2011v1.0
## Data source: https://archive.ics.uci.edu/ml/datasets/Student+Performance
## Citation for using the data base:
##     P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.
##
## Data is from UCI Machine Learning Repository which aims to
##  predict student performance in secondary education (high
##  school) including alcohol consumption.
##
## There are two data sets:
##     student-por.csv   <- student's performance in Portuguese language
##     student-mat.csv   <- student's performance in Math

##------------------------------------------------------------
##------------------------------------------------------------

#--working directory
setwd("~/IODS2020/IODS-project/data")

#-- Install/load required packages
library(dplyr)

#---------- Data exploration

por<-read.csv("student-por.csv", header = TRUE, sep=";")
#por$id<- por %>% mutate(id = 1000 +row_number())

str(por)
dim(por)                #dim 649 x 33
head(por, 5)
colnames(por)

mat<-read.csv("student-mat.csv", header = TRUE, sep=";")
#mat$id <- mat %>% mutate(id = 2000 + row_number())

str(mat)
dim(mat)                #dim 395 x 33       
head(mat, 5)
colnames(mat)

#-- check similarity of the two data sets
identical(por, mat)
all_equal(por, mat)
identical(names(por), names(mat))

## por have more observations than mat
## both have the same columns (33)

#---------- Join the two data sets

#
#-- access the dplyr library
library(dplyr)

#-- common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#-- join the two datasets by the selected identifiers
data <- inner_join(mat, por, by = join_by, suffix =c(".math", ".por"))

#-- explore the new data set
glimpse(data)
dim(data)   # 382 x 53

#-- data with only the joined columns
alc <- select(data, one_of(join_by))

#-- data set of columns not included in join_by
n.alc <- colnames(mat)[!colnames(mat) %in% join_by]

#-- for loop
for(column_name in n.alc) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(data, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

#-- check alc
glimpse(alc)
dim(alc)    #382 x 33
head(alc)
tail(alc)

#-- define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

#-- define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

#-- check
glimpse(alc)
dim(alc)     # 382 x 35
colnames(alc)

##-- saving the data set

write.table(alc,"alc.table")

##--

data<-read.table("alc.table")

dim(data)
str(data)
head(data, 5)

##-- END of Data wrangling
##==========================================================
##
## Data analysis

##-- Data exploration
data<-read.table("alc.table")

str(data)
dim(data)
colnames(data)

#-- graphical exploration of the chosen predictor variables
#-- failures, sex, absences, freetime
library(ggplot2)
str(data)
g1 <- ggplot(data, aes(x = high_use, y = failures, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("failures")

# initialise a plot of high_use and absences
g2<-ggplot(alc, aes(x = high_use, y = absences, col = sex)) 
g2 + geom_boxplot() + ylab("absences")


## fitting model using glm()
m1 <- glm(high_use ~ failures*sex + absences*sex + freetime*sex, 
          data = alc, family = "binomial")

# print out a summary of the model
summary(m1)
drop1(m1)

# print out the coefficients of the model
m2<- glm(high_use ~ failures + absences*sex + freetime*sex, 
         data = alc, family = "binomial")
summary(m2)
drop1(m2)

