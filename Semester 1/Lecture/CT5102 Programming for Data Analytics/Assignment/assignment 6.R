#' ---
#' title: "Assignment 6 - CT5102 Transforming data with dplyr"
#' subtitle: "Chin Zhe Jing 22221970"
#' output:
#'   pdf_document
#' ---
#' 

library(aimsir17)
library(ggplot2)
library(dplyr)

#'a new tibble that is a summary of the total rainfall and the average
#'temperature for each day (and for each station)
s_data <- observations |>
  group_by(station, day, month) |>
  summarize(TotalRain = sum(rain, na.rm=T), 
            AvrTemp = mean(temp, na.rm=T))

s_data
glimpse(s_data)

#'to calculate the daily changes in temperature
#'and rainfall, for each station
s_data_diff <- ungroup(s_data) |>
  arrange(station, month, day) |>
  group_by(station) |>
  mutate(RainDiff = TotalRain - lag(TotalRain),
         AbsRainDiff = abs(TotalRain - lag(TotalRain)),
         MeanTempDiff = AvrTemp - lag(AvrTemp),
         AbsMeanTempDiff = abs(AvrTemp - lag(AvrTemp)))
 
s_data_diff 
glimpse(s_data_diff)

print(n=30, s_data_diff |> filter(station == "ATHENRY", month == 5, day == 31))
  
arrange(s_data_diff,desc(AbsRainDiff)) |> slice(1:5)
  
arrange(s_data_diff,desc(AbsMeanTempDiff)) |> slice(1:5)
  
#'a new output tibble out which generates the following monthly summaries for 
#'each weather station (average, standard deviation, minumim and maxiumm).  
out <- s_data_diff |>
  group_by(station, month) |>
  summarize(AvrDiffTemp = mean(MeanTempDiff, na.rm=T), 
            SDDiffTemp = sd(MeanTempDiff, na.rm=T),
            MinDiffTemp = min(MeanTempDiff, na.rm=T),
            MaxiffTemp = max(MeanTempDiff, na.rm=T),
            AvrDiffRain = mean(RainDiff, na.rm=T),
            SDDiffRain = sd(RainDiff, na.rm=T),
            MinDiffRain = min(RainDiff, na.rm=T),
            MaxDiffRain = max(RainDiff, na.rm=T))
out  
glimpse(out)
  

ggplot(out, aes(x = month, 
                y = AvrDiffTemp)) +
  geom_ribbon(aes(ymin = MinDiffTemp,
                  ymax = MaxiffTemp),
              alpha=0,
              colour="blue") +
  geom_ribbon(aes(ymin = AvrDiffTemp - SDDiffTemp,
                  ymax = AvrDiffTemp + SDDiffTemp),
              alpha=0.3,
              fill = "red") +
  geom_line(colour="black") +
  geom_point() +
  facet_wrap(~station) +
  coord_cartesian(ylim = c(-5,5)) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(out, aes(x = month, 
                y = AvrDiffRain)) +
  geom_ribbon(aes(ymin = MinDiffRain,
                  ymax = MaxDiffRain),
              alpha=0,
              colour="red") +
  geom_ribbon(aes(ymin = AvrDiffRain - SDDiffRain,
                  ymax = AvrDiffRain + SDDiffRain),
              alpha=0.3,
              fill = "blue") +
  geom_line(colour="black") +
  geom_point() +
  facet_wrap(~station) +
  coord_cartesian(ylim = c(-20,20)) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))







