########################################################
#####           Iris Orizar
#####             2020 Nov 03
########################################################
#
##-----     Data wrangling exercise

data<-read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt",
                 sep = "\t", header = TRUE)

#-- dimension
# 183 x 60

dim(data)     

#-- structure

str(data)

#-----     Data wrangling     --------------------------------------
#install.packages('dplyr')
library(dplyr)

#-- create 'attitude' column
data$attitude<-data$Attitude/10

#-- create 'deep' column
deep_q<-c("D03", "D11", "D19", "D27", "D07", "D14", 
          "D22", "D30","D06",  "D15", "D23", "D31")
deep_c<-select(data, one_of(deep_q))
data$deep<-rowMeans(deep_c)

#-- create 'stra' column
strategic_q <- c("ST01","ST09","ST17","ST25","ST04","ST12",
                         "ST20","ST28")

strategic_c<-select(data, one_of(strategic_q))
data$stra<-rowMeans(strategic_c)

#create 'surf' column
surface_q <- c("SU02","SU10","SU18","SU26", "SU05","SU13",
                       "SU21","SU29","SU08","SU16","SU24","SU32")
surface_c<-select(data, one_of(surface_q))
data$surf<-rowMeans(surface_c)

#-- columns to keep
keep_columns <- c("gender","Age","attitude", "deep", 
                  "stra", "surf", "Points")

#-- create a new dataset
l2014<-select(data, one_of(keep_columns))

#-- check structure
str(l2014)

#colnames(l2014)

#-- changing column names

colnames(l2014)[2] <- 'age'
colnames(l2014)[7] <- 'points'

#-- check new column names
names(l2014)

#-- Exclude observations where the exam points variable is zero
l2014 <- filter(l2014, points > 0)
#head(l2014)

#check dimension
# dim(l2014)
# 166 x 7

#-- set working directory
setwd("~/IODS2020/IODS-project/data")
head(l2014[-1,])
#-- saving work
# ?write.csv
write.csv(l2014, file = 'learning2014.csv', row.names = FALSE)

#-- check the data

data<-read.csv('learning2014.csv', header = TRUE)

dim(data)
str(data)
head(data)

#-- END of Data Wrangling
#-- continue for Regression analysis
####################################################################
#####         Visualization and regression Analysis
#####---------------------------------------------------------------

# inspect the data
data<-read.csv('learning2014.csv', header = TRUE)

dim(data)
str(data)
head(data)

#load/intall required packages
#install.packages('ggplot2')
library(dplyr)
library(ggplot2)

#initialize plot
p1 <- ggplot(data, aes(x = attitude, y = points, 
                       col = gender))

#-- choose type of visualization (e.g. geom_point)
p2 <- p1 + geom_point()
p2

#-- add regression line
p3 <- p2 + geom_smooth(method = 'lm')

#-- adding Main Title for the plot
p4<- p3 + ggtitle("Student's attitude vs exam points")
p4

##-- Performing simple linear regression

# a scatter plot of points versus attitude
qplot(attitude, points, data = l2014) + geom_smooth(method = "lm")

# fit a linear model
my_model <- lm(points ~ attitude, data = l2014)

# print out a summary of the model
summary(my_model)

##-- Perform multiple linear regression
# create an plot matrix with ggpairs()
ggpairs(l2014, lower = list(combo = wrap("facethist", 
                                                bins = 20)))

# create a regression model with multiple explanatory variables
m2 <- lm(points ~ attitude + age + surf, data = l2014)

# print out a summary of the model
summary(m2)

