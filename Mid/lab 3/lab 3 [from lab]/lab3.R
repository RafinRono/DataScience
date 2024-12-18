carID <- c(1:4)
prodYear <- c(2004, 2006, 2009, 2021)
type <- c('convertibe', 'hybrid', 'diesel', 'elecric')
model <- c('Aventador', 'Enzo', 'Roadster 2.0', 'P1')
carData <- data.frame(carID, prodYear, type, model)
carData

carData['topSpeed'] <- c(175, 205, 210, 230)
carData[nrow(carData) + 1,] = c(1, 2010, 'diesel', 'Zonda', 240)

carData[1:2]
carData[c('model', 'topSpeed')]

a <- "tsing a list"
b <- c(12, 65, 43, 32)
c <- matrix(1:15, nrow=3)
d <- c('my', 'name', 'is', 'blank')
myList <- list(title=a, ages=b, c, test=d)
myList
myList[[2]]

item <- list('mango', 'apple')
append(item, 'orange')

item
newItem <- item[-1]
newItem

var1 = readline(prompt='Enter a string:')
var2 = readline(prompt='Enter a value:')

var2 = as.double(var2)
print(var1)
print(var2)

x = scan()
print(x)

y = scan()
print(y)

z = scan(what=character())
print(z)

Z = scan(what=" ")
print(Z)

mydata <- data.frame(age=numeric(0), gender=character(0), weight=numeric(0))
mydata <- edit(mydata)
mydata

write.csv(mydata, "C:/Users/student/Documents/rono/lab 3/test.csv")
dataset <- read.csv("C:/Users/student/Documents/rono/lab 3/input.csv", header=TRUE, sep=',')
dataset

dataset[5:10,]
dataset[, 1:4]
dataset[1:10, 2:4]
dataset[c(5, 7),]
dataset$species

subset(dataset, species=='virginica')
subset(dataset, sepal_width > 3 & species=='virginica')









