#1A
library(dplyr) 
library(ggplot2) 
library(tidyr)

search()

#1B
f1 <- function (x){   
  function (y){     
    y^x   
  } 
  }
f1(2)(4)


#1C
x <- 100
f1 <- function(x1){   
  f2 <- function(x2){     
    x <<- x1 + x2  + 3
  }   
  f2(10) 
}
Y <- f1(100)

#3A
methods("mean")

#3B
structure(list(a=1:3,b=4:7), class = c("my_class"))

#3C
mod <- lm(eruptions ~ waiting, data=faithful) 
attributes(mod)

#3D
coef(mod)
coef


#3E
class(mod) <- c("my_lm", class(mod))

coef.my_lm <- function(x){
  cat("Welcome to coef() for the class my_lm\nHere is the output from coef() for the class lm\n")
  class(x) <- class(x)[-1]
  coef(x)
}

coef(mod)

#4A
mtcars[c(T,F),3:4] 
temp <-as.data.frame(mpg)
temp[temp$class=="compact",c("cty", "cyl")] 

#4B
m <- 1:6
m
attr(m, "dim") <- c(2, 3)
dim(m)
rownames(m) <- paste0(c("Row "), 1:2)
colnames(m) <- paste0(c("Col "), 1:3)
m

#4C
l <- list(a=1:2,b="Hello",c=list(d=3:4,e=c(T,F)))

str(l[1:2])

str(l[[2]])

str(l[[3]])

str(l[[3]][1:2])

str(l[[3]][[1]][2])


#4D
my_table <- function(x){
  table(x)
}
my_table(c(1,1,2,3,4,4,5,5)) 

my_table <- function(v){
  un_vals <- unique(v)
  ans <- vector(mode="integer", length=length(un_vals))
  names(ans) <- as.character(un_vals)
  for(i in seq_along(v)){
    ans[as.character(v[i])] <-  ans[as.character(v[i])] + 1
  }
  ans
}
