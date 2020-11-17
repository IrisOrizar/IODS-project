#' Author: Iris Orizar
#' Date:Nov 17 2020
#' Data source:  http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv
#'               http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv
#'               

#' load packages

library(dplyr)
library(tidyr)


#' Load the data set

#' Human development data set
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

#' 195 observations x 8 variables
dim(hd)
str(hd)

#' check for missing values
colSums(is.na(hd))
#there are 7 nas in HDI.Rank and GNI.per.Capita.Rank.Minus.HDI.Rank

#'summary
summary(hd)
colnames(hd)
#' Renaming

#' renaming variables
hd<- hd %>% 
  rename(
    HDI = Human.Development.Index..HDI.,
    LE = Life.Expectancy.at.Birth,
    educ.Year = Expected.Years.of.Education,
    mean.edu.Y = Mean.Years.of.Education,
    GNI.C = Gross.National.Income..GNI..per.Capita,
    GNI.HDI = GNI.per.Capita.Rank.Minus.HDI.Rank
  )

colnames(hd)
#' Gendere equality data set
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#' 195 observations x 10 variables
dim(gii)
str(gii)

#' check for missing values
colSums(is.na(gii))

#s' summary
summary(gii)
colnames(gii)

#'renaming variables and creating new variables
#' GRSE  <- ratio of female and male populations with secondary education for each country
#' GRLFP <- labour force participation of females and males in each country
#' 

gii <- gii %>%
  rename(
    GII = Gender.Inequality.Index..GII.,                
    MMratio = Maternal.Mortality.Ratio,
    ABR = Adolescent.Birth.Rate,
    PRP = Percent.Representation.in.Parliament,
    PSEF = Population.with.Secondary.Education..Female.,
    PSEM = Population.with.Secondary.Education..Male.,
    LFPRF = Labour.Force.Participation.Rate..Female.,
    LFPRM = Labour.Force.Participation.Rate..Male.
  ) %>%
  mutate(
    GRSE = PSEF/PSEM,
           GRLFP = LFPRF/LFPRM)

colnames(gii)


#'Combine the data sets

#-- common columns to use as identifiers

data <- inner_join(hd, gii, by = "Country")

dim(data)
str(data)

#'END of Data Wrangling
