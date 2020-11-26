data<-read.csv('human2.csv', header = TRUE, sep = ',')

dim(data)
str(data)

library(GGally)
library(corrplot)

ggpairs(data)
summary(data)

cor(data) %>% corrplot()


#' non-standardized data
pca_human <- prcomp(data)


# draw a biplot of the principal component representation and the original variables
biplot(pca_human, choices = 1:2, cex = c(0.3,0.3), col = c("grey40", "deeppink2"))


#' standard data
data_std <- scale(data)

#' pca of data_std
pca_stdHuman <- prcomp(data_std)

#'bi-plot
biplot(pca_stdHuman, choices = 1:2, cex = c(0.3,0.3), col = c("grey40", "deeppink2"))


#' Factominer
library(FactoMineR)

data(tea)
dim(tea)
str(tea)
summary(tea)
colSums(is.na(tea))

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))
#colnames(tea_time)
# look at the summaries and structure of the data
summary(tea_time)
str(tea_time)

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
  facet_wrap("key", scales = "free") 

# multiple correspondence analysis
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
plot(mca, invisible=c("ind"))
