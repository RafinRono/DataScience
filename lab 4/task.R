dataset <- read.csv("C:/Users/student/Documents/rono/lab 4/input.csv", header=TRUE, sep=',')

dataset
normalize <- function(x, min, max) {
  value <-  (x - min)/(max - min)
  return (value)
}

max(dataset$sepal_length)
min(dataset$sepal_length)

mutate(dataset, 
          sepal_length = normalize(sepal_length, min(dataset$sepal_length), max(dataset$sepal_length)),
          sepal_width = normalize(sepal_width , min(dataset$sepal_width ), max(dataset$sepal_width )),
          petal_length = normalize(petal_length , min(dataset$petal_length ), max(dataset$petal_length )),
          petal_width = normalize(petal_width , min(dataset$petal_width), max(dataset$petal_width)))
