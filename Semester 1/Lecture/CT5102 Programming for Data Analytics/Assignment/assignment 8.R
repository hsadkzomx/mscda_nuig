#' ---
#' title: "Assignment 8 - CT5102 Using purrr for a Data Science Workflow"
#' subtitle: "Chin Zhe Jing 22221970"
#' output:
#'   pdf_document
#' ---
#' 

library(aimsir17)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(ggpubr)
library(randomcoloR)
set.seed(200)

ener <- eirgrid17 |> group_by(year, month, day, hour) |>
  summarise(AvrWindGen = mean(IEWindGeneration))  |> ungroup()
ener

glimpse(ener)


ds <- ener |> left_join(observations, by = c("year", "month", "day", "hour"))
ds

glimpse(ds)

sum(!complete.cases(ds))

ds <- ds[complete.cases(ds),] |> ungroup() |> sample_frac(0.01)
ds

glimpse(ds)

sum(!complete.cases(ds))

ds_n <- ds |> group_by(station) |> nest()  |> ungroup()
ds_n

glimpse(ds_n)

ds_n <- ds_n |> mutate(LM = map(data,
                        ~lm(AvrWindGen~wdsp, data = . )))
ds_n

ds_n <- ds_n |>
  mutate(R_SQ = map_dbl(LM,
                        ~summary(.)$r.squared)) |>
  arrange(desc(R_SQ))
ds_n

ds_n <- ds_n |> mutate(Plots = map2(data, station,
                            ~ggplot(.x, aes(x=wdsp, y=AvrWindGen, colour=randomColor()))+
                              geom_point()+geom_smooth(colour=randomColor(luminosity = "light"))+
                              scale_colour_manual(values=randomColor(length(.y)))+
                              xlab("Speed")+ylab("Power")+
                              labs(title = .y)+ 
                              theme_classic()+theme(plot.title = element_text(size=6))
))
ds_n

ggarrange(plotlist = ds_n$Plots, legend = "none")




