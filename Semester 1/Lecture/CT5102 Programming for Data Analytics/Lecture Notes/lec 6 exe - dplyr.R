library(dplyr)
library(nycflights13)

x <- flights
x |> filter(arr_delay>= 120)
x |> filter(dest %in% c('IAH', 'HOU'))
x |> filter(month %in% 7:9)
x |> filter(arr_delay > 120 & dep_delay <= 0)

x |> arrange(desc(distance))
x |> arrange((distance))

x |> select(contains("time"))

library(aimsir17)
library(magrittr)
observations
res <- observations %>% 
  filter(month==10 & year == 2017)  %>% 
  filter(station=="ROCHES POINT") %>%
  arrange(desc(wdsp)) %>%
  slice(1:5)

res

new_flights <- flights %>% mutate(OnTime = dep_delay <= 0) %>% filter(origin %in% c("EWR", "JFK", "LGA"))
library(ggplot2)
ggplot(new_flights, aes(x=OnTime, colour = origin, fill=origin)) +
  geom_bar()

new_observations <- observations %>% filter(station=="MACE HEAD" & month == 10 &
                                              day== 16)
ggplot(new_observations, aes(x=date, y=rain)) +
  geom_line()


new_observations2 <- observations %>%   filter(station %in% c("MACE HEAD", "DUBLIN AIRPORT", "BELMULLET")) %>%
  group_by(station, month) %>%
  summarize(TotalRainFall = sum(rain, na.rm=T)) %>% 
  arrange(month)
new_observations2
ggplot(new_observations2, aes(x=month, y=TotalRainFall, colour = station)) +
  geom_point() + geom_line()

