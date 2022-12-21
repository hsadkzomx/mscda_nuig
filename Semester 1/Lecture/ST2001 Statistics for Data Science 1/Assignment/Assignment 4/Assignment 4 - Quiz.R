# From a group of 5 women and 10 men, how many different committees consisting of 4 women and 3 men can be formed?
# 
choose(5, 4) * choose(10, 3)
choose(9, 2) * choose(6, 3)

# If A and B are two mutually exclusive events with P(A) equals 0.11 and P (B) equals 0.52. Find P(AUB)'.
# P(AUB)' = P(A'nB')
# since is disjoint, hence P(AUB) = P(A)+P(B) = 0.63
# hence P(AUB)' = 1-0.63 = 0.37
1 - (0.11+0.52)
1 - (0.27+0.29)

# Of the microprocessors manufactured by a certain process, 11% are defective. Five microprocessors are chosen at random. Assume they function independently. What is the probability that they all work?
# P(D) = 0.11, P(ND) = 0.89
(1-0.11)^5  
(1-0.19)^5
  
# In a particular manufacturing lab there are two production lines running in parallel. The probability that the first production line stops is 0.76 while the probability that the second production line stops is 0.77. If the production lines work independently of each other what is the probability that both lines are working at any given time ?
(1-0.76) * (1-0.77)
(1-0.61) * (1-0.57)

# Two consecutive traffic lights have been synchronized to make a run of green lights more likely. In particular, if a driver finds the first light to be red, the second light will be green with probability 0.9, and if the first light is green the second will be green with probability 0.69. If the probability of finding the first light green is 0.62, find the probability that a driver will find both lights green.
# P(G|R) = 0.9, P(G|G) = 0.69, P(G) = 0.62
0.62 * 0.69
0.6 * 0.71

# The prevalence of a certain disease is 0.09. A diagnostic test has been developed to test for the disease in question which returns a positive reading with probability 0.92 amongst those where it is known they have the disease and returns a negative result with probability 0.99 for those that are known not to have the disease. A person is suspected to have the disease, takes the test and the test comes back positive. What is the probability that they have the disease given that the test came back positive ?
# P(d)=0.09, P(d')=0.91, 
# P(t|d)=0.92, P(t'|d) = 0.08
# P(t'|d')=0.99, P(t|d')=0.01
# P(t) = P(t|d) * P(d) + P(t|d') * P(d') = 0.0919
# P(d|t) = ?
# P(d|t) = P(t|d) * P(d) / P(t)
(0.92*0.09) / ((0.92*0.09) + (0.01*0.91))

# A discrete random variable X is defined by the following probability distribution
## define a vector containing x values:
x <- c(0, 5 , 7, 9)
## define a vector containing probabilities:
probs <- c(0.14, 0.19, 0.24, 0.43)
## the expected value then can be calculated as:
mu <- sum(x*probs)
mu
#E(2X+5)  
2*mu + 5
#E(3X-8)  
3*mu - 8
## the E(X^2) then can be calculated as:
EX2 <- sum(x^2*probs)
EX2
## the variance can be calculated as:
Var <- EX2 - mu^2
Var
#Var(4X-/+100)
4*Var
## the standard deviation can then be calculated as:
SD <- sqrt(Var)
SD

## define a vector containing x values:
x <- c(1, 4 , 5, 7)
## define a vector containing probabilities:
probs <- c(0.1, 0.25, 0.3, 0.35)
## the expected value then can be calculated as:
mu <- sum(x*probs)
mu
#E(2X+5)  
4*mu + 5
#E(3X-8)  
2*mu - 3
## the E(X^2) then can be calculated as:
EX2 <- sum(x^2*probs)
EX2
## the variance can be calculated as:
Var <- EX2 - mu^2
Var
#Var(4X-/+100)
4*Var
## the standard deviation can then be calculated as:
SD <- sqrt(Var)
SD

# The probability of a bus arriving on time at a particular bus station is 0.79. A sample of size 9 has been studied. What is the probability that 8 buses arrive on time?
dbinom(8, 9, 0.79)
dbinom(9, 10, 0.76)

# When circuit boards used in the manufacture of compact disc players are tested, the long run percentage of defectives is 4.8%. A sample of 25 has been tested. What is the probability that there will be 5 or less defectives ?
# should start from 0
sum(dbinom(1:5, 25, 0.048)) #WRONG
sum(dbinom(0:5, 25, 0.048))
sum(dbinom(0:5, 20, 0.06))

# If you assume that the mean number of goals scored in the Airtricty league is 2.768 and the number of goals scored in a game follows a Poisson process, what is the probability that the number of goals scored in a random game is 3 ?
dpois(3, 2.768)
dpois(4, 2.759)

# Arrivals at a walk-in optometry department in a shopping centre have been found to be Poisson distributed with a mean of 2.26 potential customers arriving per hour. Assuming that the Poisson distribution is reasonable for this situation, where X is the number of arrivals during a given hour. Calculate the probability of at least 10 customers between 2pm and 6pm? Give the answer to the two decimal places.
# 2 to 6 = 5hrs <- WRONG
# dpois(10, 5*2.26) <- WRONG
# should be 6-2 = 4 hrs
# at least 10 (include 10), so need to sum 0-9 and minus it with 1
1-sum(dpois(0:9, 4*2.26))
1-sum(dpois(0:10, 4*2.64))

# Lifetimes of batteries in a certain application are normally distributed with mean 53 hours and standard deviation 6 hours. Find the probability that a randomly chosen battery lasts greater than 49 hours.
1-pnorm(49, 53, 6)
pnorm(49, 53, 6, FALSE)
pnorm(44, 48, 6, FALSE)

# A process manufactures ball bearings whose diameters are normally distributed with mean 2.9 cm and standard deviation 0.01cm. Specifications call for the diameter to be in the interval 2.9 plus-or-minus 0.01 cm. What proportion of the ball bearings will meet the specification?
pnorm(2.91, 2.9, 0.01) - pnorm(2.89, 2.9, 0.01)
pnorm(3+0.01, 3, 0.01) - pnorm(3-0.01, 3, 0.01)

# The reaction time of a driver to visual stimulus is normally distributed with a mean of 0.55 seconds and a standard deviation of 0.02 seconds. What is the reaction time that is exceeded 7% of the time ?
# becuz exceeded, so take upper tail
qnorm(0.07,0.55, 0.02, FALSE) 
qnorm(0.05,0.43, 0.08, FALSE) 



t.test()



















