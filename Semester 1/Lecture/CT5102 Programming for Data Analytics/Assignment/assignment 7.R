#' ---
#' title: "Assignment 7 - CT5102 Relational data with dplyr"
#' subtitle: "Chin Zhe Jing 22221970"
#' output:
#'   pdf_document
#' ---
#' 

library(aimsir17)
library(ggplot2)
library(dplyr)
library(tidyr)

glimpse(observations)
obs <- observations |> 
  filter(station %in% c("MACE HEAD", "DUBLIN AIRPORT", "SherkinIsland")) |>
  mutate(Season = case_when(
    month %in% c(11, 12, 1) ~ "Winter",
    month %in% c(2, 3, 4)  ~ "Spring",
    month %in% c(5, 6, 7)  ~ "Summer",
    month %in% c(8, 9, 10)  ~ "Autumn"))

obs

glimpse(obs)

glimpse(eirgrid17)

ener <- eirgrid17 |>
  group_by(year, month, day, hour) |>
  summarise(IE = mean(IEDemand, na.rm=T),
            NI = mean(NIDemand, na.rm=T),
            CheckObs = n())


ener

glimpse(ener)


set.seed(100)

ds <- left_join(ener, obs) |> 
  ungroup() |>
  sample_frac(0.1)

ds

glimpse(ds)

ds <- ds |>
  select(station, month, temp, Season, IE, NI)

ds

ds1 <- ds |>
  pivot_longer(-(station:Season),
               names_to="Area",
               values_to='Demand')

ds1

ggplot(ds1, aes(x=temp, y=Demand, colour=Area))+
  geom_point()+geom_smooth(method='lm')+
  facet_grid(station~Season)

ggplot(ds1, aes(x=temp, y=Demand, colour=Area))+
  geom_point()+geom_smooth(method='lm')+
  facet_grid(station~month)

cor_season <- ds1 |>
  group_by(station, Season, Area) |>
  summarise(corr = cor(Demand, temp))

cor_season <- cor_season |> ungroup() |>
  pivot_wider(c('station', 'Season'),
            names_from = Area,
            names_glue = "Corr_{Area}",
            values_from = corr) |>
    mutate(Diff = Corr_IE - Corr_NI)

slice(cor_season,1:nrow(cor_season))

cor_month <- ds1 |>
  group_by(station, month, Area) |>
  summarise(corr = cor(Demand, temp))

cor_month <- cor_month |> ungroup() |>
  pivot_wider(c('station', 'month'),
              names_from = Area,
              names_glue = "Corr_{Area}",
              values_from = corr) |>
  mutate(Diff = Corr_IE - Corr_NI)

slice(cor_month,1:nrow(cor_month))






