#lecture 3 week 3 20 sept exercise

library(repurrrsive)
View(sw_films)

#films directed by "George Lucas"

#sol 1
directors <- c("George Lucas")
films <- c()
for (i in seq_along(sw_films)) {
  if(sw_films[[i]]$director %in% directors)
    films[i] <- sw_films[[i]]$title
  else
    films[i] <- ""
}

# sol 2
has_gl <- vector(mode='logical', length=length(sw_films))
for (i in seq_along(sw_films)) {
  has_gl[i] <- sw_films[[i]]$director == directors
}
sw_films1 <- sw_films[has_gl]
names_gl <- c() #vector(mode='character', length=length(sw_films1))
for (i in seq_along(sw_films1)) {
  names_gl[i] <- sw_films1[[i]]$title
}
