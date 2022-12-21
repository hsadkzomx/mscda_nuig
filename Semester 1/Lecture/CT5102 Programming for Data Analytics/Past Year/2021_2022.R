# CT5102 Programming for DA 
# Past year - 2021/2022
# 

#1A
library(ggplot2) 
library(dplyr)
sum <- function(x){
  base::sum(x[-1]) 
}

sum(1:5)
search()
parent.env(ggplot2()) 
where("sum")
base::sum(1:5)

#1B
f1 <- function (x){   
  function (y){     
    x-y   
  } 
  }
y <- f1(3)(2) 		

#2A in paper

#2B + paper
View(mpg)
ggplot(mpg, aes(x = displ, y=hwy, colour=manufacturer, fill=manufacturer, shape=drv)) + geom_point() + facet_wrap(~class)

#2C
library(aimsir17)
month_r <- observations |> group_by(station, month) |> summarise(TotalRain = sum(rain, na.rm=TRUE)) |> ungroup() 
arrange(month_r,month,station)

month_c <- left_join(month_r, select(stations, station, county))
arrange(month_c,month,station)

county_avr <- month_c |> group_by(month, county) |> summarise(MeanRain = mean(TotalRain, na.rm=T)) |> ungroup() |> arrange(desc(MeanRain))
county_avr

#3A + paper
mod <- lm(hwy~displ,data=mpg) 

class(mod)

summary(mod)

class(mod) <- c("my_lm", "lm")

summary.my_lm <- function(x){
  cat("Summary Coefficients\n")
  print(coef(mod))
}

summary(mod)

#3B
library(aimsir17)
stat_test <- stations 
class(stat_test) 
head(stat_test) 
class(stat_test) <- c("my_s", class(stat_test))
class(stat_test)

head.my_s <- function(o){
  class(o) <- class(o)[-1]
  head(o,3)
}

tail.my_s <- function(o){
  class(o) <- class(o)[-1]
  tail(o,3)
}

tail(stat_test)


#4A in paper

#4B
library(purrr)
# y	=	3x2	-10x	+	100
#	input	range	of	 [-100,	+100],	in	steps	of	0.1	
x <- seq(-100,100,.1)
y <- map_dbl(x, ~3*.x^2-10*.x+100)

#4C
c4 <- group_by(mpg, class)
map_df(group_split(c4), ~{
  tibble(NObs = nrow(.x),
         CarClass = first(.x$class),
         AvrHwy = mean(.x$hwy),
         AvrCty = mean(.x$cty)
  )
})

#4D
library(tidyr)
d4 <- group_by(mpg, manufacturer) |> nest() |> head()

d4 <- mutate(d4, RSquared = map_dbl(data, 
                                    ~{
                                      obj = lm(hwy~displ, data=.)
                                      summary(obj)$r.squared
                                      }))
d4



