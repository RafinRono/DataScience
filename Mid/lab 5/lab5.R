dataset <- read.csv("C:/Users/student/Documents/rono/lab 5/input.csv", header=TRUE, sep=',')
library(dplyr)

vars <- c('sepal_length', 'sepal_width', 'petal_length', 'petal_width')
head(dataset[vars])

dataset

head <- head(dataset)
head

summary(dataset[vars])

dataset$species <- factor(dataset$species, 
                          levels = c('setosa', 'versicolor', 'virginica'),
                          labels = c(1,2,3))

dataset$species <- factor(dataset$species, 
                          levels = c(1,2,3),
                          labels = c('setosa', 'versicolor', 'virginica'))

dataset

str(dataset)

s<-dataset$sepal_length
sd(s)

lapply(dataset[vars], sd)
sapply(dataset[vars], sd)

edited <- edit(dataset) # remove some data aside from the species col values
head(edited)

head(is.na(edited))
colSums(is.na(edited)) # shows the total number of missing data per col
which(is.na(edited$sepal_width)) # shows he row number where the value is missing

remove <- na.omit(edited) #remove all rows with NA value
remove
colSums(is.na(remove))











































