#' Author: Iris Orizar
#' Date:Nov 17 2020
#' Modified: Nov 26 2020
#' Data source:  http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv
#'               http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv
#' 
###############################################################################              
#'                                            
#' NOTE:  Data wrangling for WEEK 5 starts at line 95
#' 
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

#'-- save the file
#'
write.csv(data, 'human.csv', row.names = FALSE)

#'END of Data Wrangling
###############################################################################
###############################################################################
#'Week 5 Data wrangling
#'
#' read the human data
data <- read.csv('human.csv', header = TRUE, sep = ',')

dim(data)
str(data)
head(data, 5)

#' mutate GNI.C to numeric
data <- data %>% mutate(
  GNI.C = as.numeric(gsub(",",".", gsub("\\.", "", GNI.C)))
)

str(data)
head(data, 5)

#' Select the following columns:
#' 'Country' 'GRSE' 'GRLFP' 'educ.Year' 'LE'
#' 'GNI.C' 'MMratio' 'ABR' 'PRP'

w.dat <- data %>% dplyr::select(c('Country', 'GRSE', 'GRLFP', 'educ.Year',
                                  'LE', 'GNI.C', 'MMratio', 'ABR', 'PRP'))

str(w.dat)
dim(w.dat)
colSums(is.na(w.dat))

#' Removing rows with NAs
complete.cases(w.dat)
data.frame(w.dat[-1], comp = complete.cases(w.dat))
human<-filter(w.dat, complete.cases(w.dat) == TRUE)

colSums(is.na(human))
dim(human)


#' Remove observations which relate to regions instead of countries
tail(human, 10)

#' the last 7 rows are regions instead of countries
last <- nrow(human) - 7

#' remove rows for regions
human <- human[1:last, ]

#' use countries as rownames
rownames(human) <- human$Country

#' remove column Country
human <- select(human, -Country)

head(human, 5)
dim(human)

write.csv(human, 'human2.csv', row.names = FALSE)

#####   END OF DATA WRANGLING FOR WEEK 5