# 9.1
x <- 1:100
attr(x, "dim") = c(10, 10)
#dim(x) <- c(10, 10)
str(x)
attributes(x)
attributes(x[10,,drop=F])

#s3
class(x)
class(c('A', 'B'))
class(1:10)

summary
UseMethod("summary")

summary.lm

# 9.2
print
# to override print
print.summary <- summary #making print.summary a generic function
# or
print.summary <- function(x){ #making print.summary a class function
  summary(x)
}
x <- 1:10
class(x) <- "summary"
print(x)
attributes(x)
class(x) <- NULL
print(x)

print.summary

#----------------------------------------------
print # has UseMethod, so it's a generic function
lm # has no UseMethod, so it's not a generic function
#----------------------------------------------

methods("summary") #show all the classes under this generic function

#> mean ---> is GF
#  function (x, ...) 
#  UseMethod("mean")
#  <bytecode: 0x110021708>
#  <environment: namespace:base>
#> max ---> not GF
#  function (..., na.rm = FALSE)  .Primitive("max")
#> min ---> not GF
#  function (..., na.rm = FALSE)  .Primitive("min")
#> 

mean

# 9.3
print.df1 <- function(x) {
  print(Sys.time())
  print(data.frame(x))
  }
d <- structure(data.frame(mtcars), class=c("df1", "data.frame"))
class(d)
d[1:2,]

attributes(d[1:2,])

class(data.frame)

methods("print")

class(d) <- class(d)[-1]


g <- function(o){
  UseMethod("g")
}

g.a <- function(o){
  cat("This is functio g.a\n", o)
}

g.default <- function(o){
  cat("defaultttt", o)
}

x <- structure(1:5, class = c("a"))
x <- 1:5
class(x) <- c("a")
attr(x, "class") <- "b"
class(x)

g(x)
print(x)
attributes(x)

methods("g")



?cat






















