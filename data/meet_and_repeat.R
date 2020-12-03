#' Author:     Iris Orizar
#' Date:       Dec 2 2020
#' 
#' 

#'   DATA WRANGLING
BPRS<-read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt",
                 sep = "", header = TRUE)
RATS<-read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt",
                 sep = "", header = TRUE)


#' EXPLORE THE DATA
dim(BPRS); colnames(BPRS); str(BPRS)
dim(RATS); colnames(RATS); str(RATS)

summary(BPRS); summary(RATS)

#' LOAD PACKAGES
library(dplyr)
library(tidyr)


#' BPRS data
BPRS <- BPRS %>%
  mutate(
    ftreatment = as.factor(treatment),
    fsubject = as.factor(subject)
  )
str(BPRS)

#' convert from wide to long
l.BPRS <- BPRS %>% dplyr::select(c(-treatment, -subject)) %>%
                                   gather(key = weeks, value = bprs, -ftreatment, -fsubject) %>%
  dplyr::mutate(week = as.integer(substr(weeks, 5,5)))

head(l.BPRS, 5)
tail(l.BPRS, 5)
str(l.BPRS)

write.csv(l.BPRS, 'l_BPRS.csv', row.names = FALSE)

#' check the saved data file
#' l.BPRS <- read.csv('l_BPRS.csv', header = TRUE)
#' head(l.BPRS)
#' str(l.BPRS)
#' ftreatment and fsubject are not factors in l.BPRS
#' 
#' 
#' RATS data set
RATS <- RATS %>%
  mutate(
    fID = as.factor(ID),
    fGroup = as.factor(Group)
  )
str(RATS)

#' convert from wide to long
l.RATS <- RATS %>% dplyr::select(c(-ID, -Group)) %>%
  gather(key = time, value = weight, -fID, -fGroup) %>%
  dplyr::mutate(week = as.integer(substr(time, 3,4)))

head(l.RATS, 5)
tail(l.RATS, 5)

write.csv(l.RATS, 'l_RATS.csv', row.names = FALSE)

#' check the saved data file
#' l.RATS <- read.csv('l_RATS.csv', header = TRUE)
#' head(l.RATS)
#' str(l.RATS)
