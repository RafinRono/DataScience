X <- readline(prompt = "Enter anything: ")
Y <- readline(prompt = "Enter a Number: ")
Y <- as.double(Y)
print(X)
print(Y)

carID <- c(1, 2, 3, 4)
carName <- c("Ferrari", "Lamborghini", "McLaren", "Pagani")
prodYear <- c(2004, 2019, 2015, 1998)

carData <- data.frame(carID, carName, prodYear)
carData

carData['topSpeed'] <- c(210, 240, 220, 190)
fuel <- c('hybrid', 'diesel', 'electricity', 'patrol')
carData <- cbind(carData, fuel)

carData[nrow(carData) + 1,] = list(5,"Bugatti", 2023, 250, "petrol")
carData             

carData <- carData[-3,]
carData <- carData[, -3]

carData[2:3]
carData[c('fuel','carID')]
carData[c(2, 4)]
carData[c(2, 4), c(1,3)]

test <- factor("A", "A+", "B", "C")
test

a <- "tetsing a list"
b <- c(67, 12, 44, 0)
c <- matrix(1:15, nrow=5)
d <- c("hey", "there", "look")
myList <- list(title=a, ages=b, c, fame=d)
myList
myList[[2]]

item <- list("mango", "apple")
item <- append(item,'orange')
item
newItem <- item[-1]
newItem

testDF <- data.frame(ID = numeric(), gender=character(), age=integer())
testDF <- edit(testDF)
testDF
write.csv(testDF, file="J:/R/Workspace/lab 3/test.csv")

dataset <- read.csv("J:/R/Workspace/lab 3/Iris.csv", header = TRUE, stringsAsFactors = FALSE)
dataset

dataset[10:20,]
dataset$Species == "Iris-setosa"
dataset[dataset$Species == "Iris-setosa", ]
subset(dataset, PetalLengthCm < 2)
subset(dataset, SepalWidthCm >= 3 & Species=="Iris-virginica")



