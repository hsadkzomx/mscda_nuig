library(aimsir17)
library(dplyr)
library(ggplot2)
library(ggpubr)

glimpse(observations)
glimpse(stations)

join1 <- inner_join(observations, stations)
glimpse(join1)

glimpse(eirgrid17)
c71o <- observations |> filter(station == 'MACE HEAD', month==10)
c71e <- eirgrid17 |> filter(month==10) |>
  group_by(year, month, day, hour) |>
  summarise(mean_wind_energy = mean(IEWindGeneration, na.rm=T),
            checkObs = n()) |>
  ungroup()
c71 <- inner_join(c71o, c71e, by=c("year", "month", "day", "hour"))
c71

p1 <- ggplot(c71, aes(x = wdsp, y = mean_wind_energy)) +
  geom_point() + geom_smooth(method=lm) + geom_jitter()

lm1 <- lm(mean_wind_energy~wdsp, data=c71)

p2 <- ggplot(c71, aes(x = temp, y = mean_wind_energy)) +
  geom_point() + geom_smooth(method=lm) + geom_jitter()

lm2 <- lm(mean_wind_energy~temp, data=c71)

p3 <- ggplot(c71, aes(x = rain, y = mean_wind_energy)) +
  geom_point() + geom_smooth(method=lm) + geom_jitter()

p4 <- ggplot(c71, aes(x = msl, y = mean_wind_energy)) +
  geom_point() + geom_smooth(method=lm) + geom_jitter()

ggarrange(p1, p2, p3, p4, nrow = 2, ncol=2)

c71o_all <- observations |> filter(month==10)
c71e_all <- eirgrid17 |> filter(month==10) |>
  group_by(year, month, day, hour) |>
  summarise(meanWE = mean(IEWindGeneration, na.rm=T),
            checkObs = n()) |>
  ungroup()
c71_all <- inner_join(c71o_all, c71e_all, by=c("year", "month", "day", "hour"))
c71_all

ggplot(c71_all, aes(x=wdsp, y=meanWE))+
  geom_point() + geom_jitter() + geom_smooth(method="lm")+
  facet_wrap(~station)

c71_all |>
  group_by(station) |> 
  summarise(corr_wdsp = cor(meanWE, wdsp),
            corr_temp = cor(meanWE, temp),
            corr_msl = cor(meanWE, msl),
            corr_rain = cor(meanWE, rain)
            ) |>
  arrange(desc(corr_wdsp))

mh <- filter(c71_all, station=="MALIN HEAD")

lm2 <- lm(meanWE~wdsp, data=mh)
summary(lm2)
summary(lm1)
#View(c71)
#

c72o <- observations |> filter(station %in% c('BELMULLET', 'DUBLIN
AIPRORT', 'VALENTIA OBSERVATORY'), month==10) |>
  group_by(year, month, day, hour) |>
  summarise(minDailyTemp = min(temp, na.rm=T),
            checkObs = n()) |>
  ungroup()
c72e <- eirgrid17 |> filter(month==10) |>
  group_by(year, month, day, hour) |>
  summarise(maxDailyEnergy = max(IEDemand, na.rm=T),
            checkObs = n()) |>
  ungroup()
c72 <- inner_join(c72o, c72e, by=c("year", "month", "day", "hour"))















































