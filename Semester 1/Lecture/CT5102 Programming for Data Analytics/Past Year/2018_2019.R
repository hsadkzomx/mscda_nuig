#1A
e    <- new.env() 
e$f1 <- function()1 
y    <- e$f1()


#3B
myarray <- function (n){   
  structure(list(data=vector(mode = "numeric", length = n)),             class="myarray") 
  }

m <- myarray(10)
attributes(m)

summary.myarray <- function(x){
  #class(x) <- class(x)[-1]
  cat("Array Size = ", length(x$data))
  cat("\nValues = [", x$data, "]")
}

summary(m)

`[.myarray` <- function(o, i){
  o$data[i]
}

m[1:4]

`[<-.myarray` <- function(o, i, value1){
  o$data[i] <- value1
  o
}
m[1:4]<-7:10 

`<.myarray` <- function(o, v){
  o$data < v
}
m<1

`<=.myarray` <- function(o, v){
  o$data <= v
}
m<=1

`>.myarray` <- function(o, v){
  o$data > v
}
m>1

`>=.myarray` <- function(o, v){
  o$data >= v
}
m>=1

`==.myarray` <- function(o, v){
  o$data == v
}
m==0

`!=.myarray` <- function(o, v){
  o$data != v
}
m!=0

#4B
c(10, 20, TRUE, "TRUE") 
typeof(c(T,T,F,0))
typeof(unlist(list(10, 20, TRUE, "TRUE")))

sapply(seq(-10, 10, .1), function(x){
  1/ (1 + exp(-x))
})

library(purrr)
library(tibble)
map_df(c(1, 4, 7), ~{tibble(Old = .x, New = .x+10)})

map_df(c(1, 4, 7), ~tibble(Old = .x, New = .x+10))
  
map_df(c(1, 4, 7), ~tibble(Old = ., New = .+10))
