#' ---
#' title: "Assignment 9 - CT5102 Using S3"
#' subtitle: "Chin Zhe Jing 22221970"
#' output:
#'   pdf_document
#' ---
#' 

library(dplyr)
library(ggplot2)
library(purrr)

d1 <- mpg |> group_by(class)

d1

my_mpg_lms <- function(x){
  temp <- x |> group_split()
  mods1 <- map(temp, ~lm(cty~displ, data=.))
  class(mods1) <- "my_mpg_lms"
  names(mods1) <- group_keys(x)[[1]]
  mods1
}

mods1 <- my_mpg_lms(d1)

length(mods1)

class(mods1)

names(mods1)

str(mods1[[1]])

str(mods1[[7]])

summary.my_mpg_lms <- function(x){
  cat("The following are the model groups\n")
  cat(names(x), "\n\n")
  cat("Here are the results...\n")
  
  walk2(names(x),x,~{
    cat("Model #", which(.x==names(x)), "Group", .x, "Obs =", nobs(.y), "\n")
    print(summary(.y))
    cat("=======================================================\n\n")
  })
}

summary(mods1)

d2 <- mpg %>% group_by(manufacturer)
mods2 <- my_mpg_lms(d2)
names(mods2)

summary(mods2)
