install.packages('dslabs')
args(log)

log(10, 10)
log(10)
log(2)
co2
pi
Inf
library(dslabs)
class(murders$region)
levels(murders$region)
sort(murders$total)
order(murders$total)

ind <- order(murders$total)
murders$abb[ind]
max(murders$total)
i_max <- which.max(murders$total)
i_max

murder_rate <- murders$total / murders$population * 100000 
ind <- murder_rate <= 0.71
murders$state[ind]
ind <- which(murders$state == 'California')
murder_rate[ind]

ind <- match(c("New York", "Florida", "Texas"), murders$state)
ind
murder_rate[ind]


which(murders$state %in% c("New York", "Florida", "Texas"))

pop_in_mil <- murders$population / 10^6
total_gun_murders <- murders$total
plot(pop_in_mil, total_gun_murders)

with(murders, plot(population, total))
library(dplyr)
murders <- mutate(murders, rate = total / population * 10^5)
hist(murders$rate)
murders$state[which.max(murders$rate)]

boxplot(rate ~ region, data = murders)
