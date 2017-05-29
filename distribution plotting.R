library(ggplot2)
?stat_function


set.seed(1492)
df <- data.frame(
  x = rnorm(100))
x <- df$x

base <- ggplot(df, aes(x)) + geom_density()
base + stat_function(fun = dnorm, colour = "red")
base + stat_function(fun = dnorm, colour = "red", args = list(mean = 3))

# Plot functions without data
# Examples adapted from Kohske Takahashi

# Plot a normal curve
ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm)

# To specify a different mean or sd, use the args parameter to supply new values
myplot <- ggplot(data.frame(x = c(-1, 20)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 3, sd = .7),colour = "red") +
  stat_function(fun = dnorm, args = list(mean = 7, sd = 1),colour = "blue") + 
  stat_function(fun = dnorm, args = list(mean = 10, sd = 1.5),colour = "green") +
  stat_function(fun = dnorm, args = list(mean = 13, sd = 3),colour = "black")
plot(myplot)



myplot2 <- ggplot(data.frame(x = c(0,75)), aes(x)) +
  stat_function(fun = plogis, args = list(0.5, location = 40),colour = "red") +
  stat_function(fun = plogis, args = list(1, location = 40),colour = "blue")  +
  stat_function(fun = plogis, args = list(3, location = 40),colour = "green") +
  stat_function(fun = plogis, args = list(5, location = 40),colour = "black") 


plot(myplot2)


# generate n samples from the skew logistic
# distribution with parameter Theta

r.skewlog <- function(n, Theta)
{
  U <- runif(n)
  return( -log( (1/U)^(1/Theta) - 1) )
}
X <- r.skewlog(1000, 4)

# empirical CDF
v <- seq(-10, 10, length=1000)
emp.cdf <- rep(0, 1000)
for(i in 1:1000) emp.cdf[i] <- mean(X <= v[i])

# true CDF
true.cdf <- (1 + exp(-v))^(-4)

plot(v, emp.cdf, xlab="X", ylab="F(X)", main="Empirical vs True CDF", col=2,type="l")
lines(v,true.cdf,col=4)



