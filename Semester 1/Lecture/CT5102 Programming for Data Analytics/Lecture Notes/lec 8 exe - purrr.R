#Week 9 - Lec 8 Purrr
#
library(purrr)
library(ggplot2)
library(tidyverse)
library(aimsir17)


glimpse(mpg)

mv <- mpg |> select(cty, hwy, displ) |>
  map(mean)

mv2 <- mpg |> select(cty, hwy, displ) |>
  map_dbl(mean)

mv3 <- mpg |> select(cty, hwy, displ) |>
  sapply(mean) #return vector

mv4 <- mpg |> select(cty, hwy, displ) |>
  lapply(mean) #return list

a1 <- observations |> lapply(function(x)sum(is.na(x)))
a2 <- observations |> map(function(x)sum(is.na(x)))
a3 <- observations |> map(~sum(is.na(.x)))
a4 <- observations |> map(~sum(is.na(.)))

d1 <- observations |> map_df(~tibble(class=class(.),
                                     length=length(.),
                                     ndistinct=n_distinct(.)),
                             .id="Column")

p1 <- mpg |> ggplot(aes(displ, cty, colour=class)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~class)
p1

mpg |> group_split(class) -> tmp
str(tmp)

tmp |> map_df(~{
  mod <- lm(.$cty~.$displ)
  summ <- summary(mod)
  tibble(Class=first(.$class),
         Obs=nrow(.),
         Intercept=mod$coefficients[1],
         Slope=mod$coefficients[2],
         RSquared=summ$r.squared,
         AdjSquared=summ$adj.r.squared)
})


mpg |> group_split(class) |> map_df(~{
  mod <- lm(.$hwy~.$cyl)
  summ <- summary(mod)
  tibble(Class=first(.$class),
         Obs=nrow(.),
         Intercept=mod$coefficients[1],
         Slope=mod$coefficients[2],
         RSquared=summ$r.squared,
         AdjSquared=summ$adj.r.squared)
})

#lm(dependant~independant)

mpg |> ggplot(aes(cyl, hwy, colour=class)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~class)

mpg |> group_by(class) |> nest() |> mutate(lm_obj=map(data, ~lm(cty~displ,data=.x)))
#nest() -> to archive results in column

o1 <- observations |> group_by(station, month, day) |>
  summarise(MeanWDSP = mean(wdsp, na.rm=T),
            MeanMSL = mean(msl, na.rm=T)) |>
  ungroup()
plots <- o1 |> group_split(station) |>
  map(~ggplot(.,aes(MeanMSL, MeanWDSP))+geom_point()+geom_smooth()+
        ggtitle(.$station))
plots[[10]]

library(ggpubr)
ggarrange(plotlist =  plots)

#|> nest() |> mutate(lm_obj=map(data, ~lm(wdsp~msl,data=.x)))


