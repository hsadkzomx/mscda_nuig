# CT5102 Programming for DA 
# Past year - 2020/2022
# 
# 


library(aimsir17) 
library(ggplot2) 
library(dplyr)

mean <- function(x)x^2 

mean(1:5)

base::mean(1:5)

global::mean(1:5)

#2B
#mpg
ggplot(mpg, aes(x=displ, y=cty, colour=class))+geom_point()+facet_grid(fl~cyl)

#2C1
library(purrr)
library(tidyr)
tb <- observations |>
  filter(station %in% c('BELMULLET', 'DUBLIN AIRPORT', 'ROCHES POINT')) |>
  group_by(station, day, month) |>
  summarise(daily_rain = sum(rain),
            min_pressure = min(msl)) |>
  ungroup() |>
  group_by(station) |>
  nest()

pluck(pull(tb,data),3) %>% slice(1:3) 

#2C2
tb <- mutate(tb, LM = map(data, 
                    ~lm(daily_rain~min_pressure, data=.)))

tb <- mutate(tb, R.Sq = map_dbl(LM,
                          ~summary(.)$r.squared))|>
  arrange(desc(R.Sq))

#3A
x <- 1:5
attributes(x)
attr(x, "names") <- c("a", "b", "c", "d", "e")
structure(1:5, names=c("a", "b", "c", "d", "e"))

#3B
library(pryr)
typeof(mtcars)
class(mtcars)
otype(mtcars)

#3C
otype(summary)

methods(summary)
summary.factor

#3D
my_df <- function(x){
  structure(x, class=c("my_df", class(x)))
}

d <- my_df(mtcars) 

class(d)

dim(mtcars)[1]

summary.my_df <- function(x){
    class(x) <- class(x)[-1]
    cat("The column names are", names(x))
    
    cat("\nThe number of rows are", dim(x)[1])
    
    cat("\nHere is a summary of the columns\n")
    
    summary(x)
  }
summary(d) 

summary.my_df <- function(x){
  class(x) <- class(x)[-1]
  cat("The column names are", colnames(x))
  
  cat("\nThe number of rows are", nrow(x))
  
  cat("\nHere is a summary of the columns\n")
  
  summary(x)
}
summary(d) 

nrow(mtcars)

#4A
A <- 100; B <- 20
f1<-function(a){   
  B <- 100   
  f2<-function(b){     
    A <<- 200+b   #<--- determiner  
    B <<- 1000-b  #<--- determiner  
  }   
  f2(a) 
  }
f1(B)
#A=220, B=980

#4B
l <- list(c(T,F),1:5,list(1:2,6:7))
is.list(l[3])
is.list(l[[3]])
l[[3]][[1]]

str(l[1])
str(l[[2]])
str(l[[2]][1])

#4C
pow <- function(x){
  function(y){
    y**x
  }
}

p2 <- pow(2)
p2(5)

#4D
x <- seq(-100, 100, .1)
y <- sapply(x, function(x, a, b, c, d){
  a*x^3 + b*x^2 + c*x + d
}, 2, 3, -4, 100)
y <- map_dbl(x, function(x, a, b, c, d){
  a*x^3 + b*x^2 + c*x + d
}, 2, 3, -4, 100)
