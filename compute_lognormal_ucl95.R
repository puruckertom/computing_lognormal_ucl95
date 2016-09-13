rm(list=ls())
library(boot)
set.seed(1)

data <- c(3100,700,17702,9295,1734,13000)
hist(data)

#normal mean
mean(data)
exp(log(mean(data)))

#normal sd
sd(data)

#confidence interval
max(data)
t.test(data, conf.level = 0.95)$conf.int[2]

log_data <- log(data)

#lognormal mean
mean(log_data)
exp(mean(log_data))

sd(log_data)
exp(sd(log_data))

#http://stats.stackexchange.com/questions/33382/how-do-i-calculate-a-confidence-interval-for-the-mean-of-a-log-normal-data-set
# Statistic (MLE)
samp.mean = function(dat) return(mean(dat))

# Bootstrap
boots.out = boot(data=log_data, statistic=function(d, ind){samp.mean(d[ind])}, R = 10000)
#logspace
plot(density(boots.out$t))
#normalspace
plot(density(exp(boots.out$t)))

# 4 types of Bootstrap confidence intervals
#lower
boot.ci(boots.out, conf = 0.95, type = "basic")[4][[1]][4]
#upper
boot.ci(boots.out, conf = 0.95, type = "basic")[4][[1]][5]
exp(boot.ci(boots.out, conf = 0.95, type = "basic")[4][[1]][5])
#exp(boot.ci(boots.out, conf = 0.95, type = "basic")[4][[1]][5])
#[1] 11432.57