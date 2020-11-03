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

#-- END
