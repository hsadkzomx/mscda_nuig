library(aimsir17)
library(ggplot2)
library(tidyverse)
#library(bbplot) -> bbc_style() (other color)

View(observations)

jan <- observations %>% 
  subset(station %in% c('MACE HEAD', 'DUBLIN AIRPORT', 'SherkinIsland') )%>% 
  subset(month == 1)
ggplot(plot, aes(x=date, y=temp, size=rain, colour=station)) + geom_point() + 
  #geom_smooth() + 
  xlab("Time") + ggtitle(("My plot")) +
  geom_hline(yintercept = 0, colour = "red", size=3)

jan$date[5] - jan$date[1]


ophelia <- tibble(observations) |> 
  subset(station %in% c('ROCHES POINT', 'SherkinIsland', 'VALENTIA OBSERVATORY') &
         month==10 & day==16)
ophelia

ggplot(ophelia, aes(x=date, y=msl, colour=station)) + 
  geom_point() + geom_line() + 
  ggtitle(("mean sea level atmospheric pressure"))

ggplot(ophelia, aes(x=date, y=msl, colour=station)) + 
  geom_point() + geom_line() + 
  facet_wrap(~station) +
  ggtitle(("mean sea level atmospheric pressure"))

ggplot(ophelia, aes(x=date, y=wdsp, colour=station)) + 
  geom_point() + geom_line() + 
  ggtitle(("wind speed"))

ggplot(ophelia, aes(x=date, y=wddir, colour=station)) + 
  geom_point() + geom_line() + 
  ggtitle(("wind direction"))

ggplot(ophelia, aes(x=date, y=temp, colour=station)) + 
  geom_point() + geom_line() + 
  ggtitle(("temperature"))

ggplot(ophelia, aes(x=date, y=wdsp, colour=station)) + 
  geom_point() + geom_smooth() + 
  ggtitle(("wind speed + smooth"))

ggplot(ophelia, aes(x=wddir, y=wdsp)) + 
  geom_point() + geom_smooth(method='lm') + 
  ggtitle(("wind direction vs speed"))


