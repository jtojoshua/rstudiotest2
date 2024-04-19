# find the probability that the mean
> mean <- 5
> sd <-1
> n<-25
> lowerlimit<-4.7
> upperlimit<-5.1
> zlower<-(lowerlimit-mean)/(sd/sqrt(n))
> zupper<-(upperlimit-mean)/(sd/sqrt(n))
> pro<-zupper-zlower
> pro2<- pnorm(zupper)-pnorm(zlower)
 
# given
n <- 30
s <- 0.81
confidence_level <- 0.96
 
# dof
df <- n - 1
 
# chi-squared
chi2_lower <- qchisq((1 - confidence_level) / 2, df, lower.tail = TRUE)
chi2_upper <- qchisq(1 - (1 - confidence_level) / 2, df, lower.tail = TRUE)
 
# lower and upper
variance_lower <- (df * s^2) / chi2_upper
variance_upper <- (df * s^2) / chi2_lower
 
# interval
sd_lower <- sqrt(variance_lower)
sd_upper <- sqrt(variance_upper)
 
# Print
cat("The 96% confidence interval for the standard deviation is between", sd_lower, "and", sd_upper, "ounces.\n")
 
# scores
pre_test <- c(30, 29, 29, 22, 29, 30, 34, 32, 28, 16)
post_test <- c(28, 31, 32, 23, 31, 31, 32, 33, 28, 20)
 
# Calculate the differences
differences <- post_test - pre_test
 
# mean and standard deviation
mean_diff <- mean(differences)
sd_diff <- sd(differences)
 
# Number of observations
n <- length(differences)
 
# Find the t-critical value
t_critical <- qt(0.95, df = n-1)  # two-tailed, so 0.95 instead of 0.90
 
# margin of error
margin_error <- t_critical * (sd_diff / sqrt(n))
 
# confidence interval
lower_bound <- mean_diff - margin_error
upper_bound <- mean_diff + margin_error
 
# Print
cat("The 90% confidence interval for the mean difference is (", lower_bound, ",", upper_bound, ")\n")
 
 
