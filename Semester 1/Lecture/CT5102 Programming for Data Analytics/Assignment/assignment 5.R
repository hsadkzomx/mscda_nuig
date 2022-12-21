#' ---
#' title: "Assignment 5 - CT5102 Visualisation with ggplot2 - Flights data"
#' subtitle: "Chin Zhe Jing 22221970"
#' output:
#'   pdf_document
#' ---
#' The aim of this assignment is to use ggplot2 to visualise flight data, which is contained in the package
#' nycflights13. Note that you may have to follow up and search the internet to see the instructions for using
#' certain features, for example using lubridate and finding out how to position a legend in ggplot2. 
#' Please ensure that your outputs exactly match the graphs. 
#' For example, the command scale_x_continuous(n.breaks = 15) can be used to format the x-axis in a number of the plots.
#' 
#' 
library(nycflights13)
library(ggplot2)
library(lubridate)
library(tibble)

#'The data is contained in the tibble flights, and the idea is to create a new tibble d based on this data.
#'Notice that not all the columns are used, and that there are two new columns, MonthName and Hour.
data <- tibble(flights)
data$MonthName <- month(data$time_hour, label = TRUE, abbr = TRUE)
data$Hour <-as.factor(data$hour)

d <- subset(data, select=c('month',
                           'MonthName',
                           'Hour',
                           'origin',
                           'day',
                           'dep_delay',
                           'arr_delay',
                           'air_time',
                           'distance',
                           'carrier'))

d
glimpse(d)

#'1. Plot a boxplot for the departure delays by month.
d_dep_delay_month <- d |> 
  subset(origin %in% c('EWR', 
                       'JFK', 
                       'LGA') &
           dep_delay>=-30 & dep_delay<=30)
ggplot(d_dep_delay_month, aes(x=MonthName, y=dep_delay, colour = origin)) +
  geom_boxplot() +
  xlab("Month") + 
  ylab("Departure Delay") +
  ggtitle(("Departure Delays by Month (range -30 to 30 minutes shown)")) + 
  theme(legend.position="top")

#'2. Plot a boxplot for the departure delays by hour of the day
#'xlim(levels(data$Hour)) force show all data points
ggplot(subset(d, origin %in% c('EWR', 
                               'JFK', 
                               'LGA') &
                dep_delay>=-30 & dep_delay<=30), 
       aes(x=Hour, y=dep_delay, colour = origin)) +
  geom_boxplot() +
  xlab("Hour") + 
  xlim(levels(data$Hour)) +
  ylab("Departure Delay") +
  ggtitle(("Departure Delays by Hour of Day (range -30 to 30 minutes shown)")) + 
  theme(legend.position="bottom")

#'3. Plot a faceted histogram of the air time of flights by origin. Note that three rows should be used, and there
#'is no legend.
ggplot(subset(d, origin %in% c('EWR', 
                               'JFK', 
                               'LGA')), 
       aes(x=air_time, colour = origin, fill = origin)) +
  geom_histogram(show.legend = FALSE) +
  xlab("Time in Air") + 
  scale_x_continuous(breaks = seq(0, 700, by = 50)) +
  ylab("Number of Flights") +
  ggtitle(("Histogram of Airtime from each Airport")) + 
  facet_wrap(~origin, ncol=1)

#'4. Plot a faceted histogram of the flight distance by origin. Note that three rows should be used, and
#'there is no legend.
ggplot(subset(d, origin %in% c('EWR', 
                               'JFK', 
                               'LGA')), 
       aes(x=distance, colour = origin, fill = origin)) +
  geom_histogram(show.legend = FALSE) +
  xlab("Distance") + 
  scale_x_continuous(breaks = seq(0, 5000, by = 500)) +
  ylab("Number of Flights") +
  ggtitle(("Histogram of Airtime from each Airport")) + 
  facet_wrap(~origin, ncol=1)

#'5. Plot a bar chart showing the number of flights per carrier
ggplot(subset(d, origin %in% c('EWR', 
                               'JFK', 
                               'LGA')), 
       aes(x=carrier, colour = origin, fill = origin, position = "fill")) +
  geom_bar() +
  xlab("Carrier") + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("Number of Flights") +
  ggtitle(("Count of flights from the different origins"))

#'6. With a seed value of 100, select a random sample of 3000 from the tibble, and store this in the variable
#'d1.
set.seed(100)
d1_data <- tibble(flights)
d1 <- d1_data[sample(nrow(d1_data), 3000), ]
head(d1)
summary(d1)

#'7. Plot the departure delay v arrival delay
#'Use geom_point
ggplot(subset(d1, origin %in% c('EWR', 
                                'JFK', 
                                'LGA')), 
       aes(x=dep_delay, 
           y=arr_delay,
           colour = origin, 
           position = "fill")) +
  geom_point() +
  geom_smooth() +
  xlab("Departure Delay") + 
  ylab("Arrival Delay") +
  ggtitle(("Departure delay v Arrival Delay for N = 3000"))

#'8. Plot the distance v Air Time 
#'Use method = lm
ggplot(subset(d1, origin %in% c('EWR', 
                                'JFK', 
                                'LGA')), 
       aes(x=distance, 
           y=air_time,
           colour = origin, 
           position = "fill")) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Distance") + 
  ylab("Air Time") +
  ggtitle(("Distance v Air Time for N = 3000"))
