switch(2, "rono", 13, "meraz")
switch(3, "rono", 13, "meraz")
switch(2, "red", 'blue', "green")

switch("third", 'third' = "rono", 'fourth' = 13, 'rich' = "meraz")

i <- 1
while(i < 13) {
  print(i)
  i <- i+1
}

i <- 1
while(i < 16) { #prints 1 2 3 4
  print(i)
  if (i == 4) break
  i <- i+1
}

i <- 1
while(i < 16) { #prints 1 2 3 
  print(i)
  i <- i+1
  if (i == 4) break
}

i <- 0
while(i < 6) {
  i <- i+1
  if (i == 3) next
  print(i)
}

for (x in 1:10) {
  print(x)
}

for (x in 1:10) {
  if (x %% 2 == 1) print(x)
}

for (x in 1:2) {
  for (y in 1:3) {
    print(x*y)
  }
}

for (x in 1:2) {
  for (y in 1:3) {
    print(x*y)
  }
  break
}

for (x in 1:2) {
  for (y in 1:3) {
    print(x*y)
    break
  }
}

add_numbers <- function(a, b) {
  sum <- a+b
return (sum)
}
print(add_numbers(4, 5))
print(add_numbers(54, -25))

a <- c(1, 2, 3, 4)
b <- c('one', 'two', 'three', 'four')
c <- c(TRUE, FALSE, TRUE, TRUE)

a[0]
b[0]
c[0]

a[1]
b[2]
c[3]

a[2:3]
b[3:4]
c[1:3]
c[1:3][2]

# save.image("~/rono/lab 2/lab2.RData")

sum(a)
mean(a)
min(a)
max(a)
sd(a)
mode(a)

max(b)

name <- "hello world, how are you"
nchar(name)
length(name)

name <- c('hello', 'world', 'there', 'try')
nchar(name)
length(name)

a
a + 2
a * 3

d = a + c(4, 6, 43, 3)
d

X <- c(32, 6, 2, 19)
X
sort(X)
X

X <- c("MY", "name", 'is', 'Rono') 
X 
sort(X)
sort(X , decreasing = TRUE)

X[0]
X[1]
sort(X , decreasing = TRUE)[2]
X
X[1:3]
X[c(1, 3)]

vec <- c( 1 : 4, "test", "money",
       TRUE, 45.7, 56, 34, 56)
print(vec)

vec[0]

mymatrix_bycol <- matrix(1:25, nrow =5, ncol=5)
mymatrix_bycol

mymatrix_byrow <- matrix(1:20, nrow =5, ncol=6, byrow = TRUE)
mymatrix_byrow

mymatrix <- matrix(1:30)
mymatrix

mymatrix <- matrix(1:30, nrow = 5, ncol = 6, byrow = TRUE, dimnames = list(c('A', 'B', 'C', 'D', 'E'), c('U', 'V','W','X','Y','Z')))
mymatrix

mymatrix[3,]
mymatrix[, 5]
mymatrix[1, 5]
mymatrix[3, 4]
mymatrix[3, c(2, 5)]
mymatrix[1:2,]
mymatrix[1:2,2:3]
mymatrix[, 1:2]

myarray <- array(1:24, c(2,4))
myarray

myarray <- array(1:24, c(2,4,3))
myarray

myarray[1, 2, 3]

carID <- c(1:4)
prodYear <- c(2004, 2006, 2009, 2021)
type <- c('convertibe', 'hybrid', 'diesel', 'elecric')
carData <- data.frame(carID, prodYear, type)
carData














































































