dataset <- read.csv("C:/Users/student/Documents/rono/lab 4/input.csv", header=TRUE, sep=',')
dataset <- data.frame(dataset)
mode(dataset)

# install.packages("dplyr")
library(dplyr)

filter(dataset, petal_width<3)

stats <- data.frame(player=c('A', 'B', 'C', 'D', 'A', 'A'),
                    runs=c(100 ,200, 408, 19, 56, 100),
                    wickets=c(17, 20, NA, 5, 2, 17))

stats
filter(stats, runs<300)
distinct(stats) #removes duplicate rows
distinct(stats, player, .keep_all = TRUE) #removes duplicates based on a col, shows al the data
distinct(stats, player, .keep_all = FALSE) #removes duplicates based on a col, shows just the one col
distinct(stats, wickets, .keep_all = TRUE) 

arrange(stats, runs)
arrange(stats, player)
arrange(stats, desc(wickets)) #desc order

select(stats, player, wickets) # returns a subset

stats <- rename(stats, runs_scored=runs) # rename a col
stats <- rename(stats, runs = runs_scored) # rename a col

mutate(stats, avg = runs/6) # keeps old records
transmute(stats, avg = runs/6) # deletes and returns a new data data frame

# edit(dataset)

summarize(stats, sum(runs), mean(runs))


























