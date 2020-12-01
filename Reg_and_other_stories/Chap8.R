packages <- c("ggplot2","ggsci","dplyr","insight","bayestestR","arm","bayesplot",
              "rstanarm"  # rstanarm uses Stan as statistical inference engine
              # to fit regression models
)
lapply(packages, require, character.only = TRUE)

### ---- Chapter 8, fitting regression models ---- ###
### Three methods: least squares, maximum likelihhod and Bayesian inference

### Least squares ----
# Point is to identify the a and b that minimises the residual sum of squares

## Write a function to calculate residual sum of squares
rss <- function(x, y, a, b) {
  resid <- y - (a + b*x)
  return(sum(resid^2))
}
# Test it with different values for a and b
N <- 100
a <- 5
b <- 7
x <- runif(N, 0, 50)
y <- a + b*x
fake <- data.frame(x,y)
plot(fake$x, fake$y)
rss(fake$x, fake$y, 5, 7)/100000
# ----

### Excercises ----
elections <- read.delim("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat",sep = " ")
fit8.1 <- stan_glm(vote ~ growth, data=elections)
print(fit8.1)
rss(elections$growth, elections$vote, 46.3, 3.1)

## 8.1 - Least squares
# plot sum of squares of residuals given a, with b fixed at best estimate
a <- seq(46,46.99,0.01)
b <- 3.05
rss.a <- rep(0, 100)
for (i in 1:100) {
  x <- rss(elections$growth, elections$vote, a[i], b)
  rss.a[i] <- x
}
rss.a <- data.frame(cbind(rss.a,a))
p1 <- ggplot(rss.a,aes(a,rss.a)) + geom_point() + theme_bw() +
  geom_vline(xintercept=fit8.1$coefficients[1], col="red")

# plot sum of squares of residuals given b, with a fixed at best estimate 
a <- 46.27
b <- seq(2.5,3.49,0.01)
rss.b <- rep(0, 100)
for (i in 1:100) {
  x <- rss(elections$growth, elections$vote, a, b[i])
  rss.b[i] <- x
}
rss.b <- data.frame(cbind(rss.b,b))
p2 <- ggplot(rss.b,aes(b,rss.b)) + geom_point() + theme_bw() +
  geom_vline(xintercept=fit8.1$coefficients[2], col="red")
gridExtra::grid.arrange(p1,p2,nrow=1)

## 8.2 Maximum likelihood

# Write a function to calculate log-likelihood given data, a, b and sigma
m <- fit8.1$coefficients[1] + fit8.1$coefficients[2]*elections$growth
sigma <- sqrt((1/nrow(elections))*(sum(m^2)))

logLfnx <- function(x, y, a, b, s) {
  m <- a + b*x
  ll <- (1/sqrt(2*pi*s))*(exp(-0.5*((m - y)/s)^2))
  return(sum(ll))
}

# varying a
a <- seq(46,46.99,0.01)
b <- 3.05
logLfnx.a <- rep(0, 100)
for (i in 1:100) {
  x <- logLfnx(elections$growth, elections$vote, a[i], b, sigma)
  logLfnx.a[i] <- x
}
logLfnx.a <- data.frame(cbind(logLfnx.a,a))
p1 <- ggplot(logLfnx.a,aes(a,logLfnx.a)) + geom_point() + theme_bw() +
  geom_vline(xintercept=fit8.1$coefficients[1], col="red")

# varying b
a <- 46.27
b <- seq(2.5,3.49,0.01)
logLfnx.b <- rep(0, 100)
for (i in 1:100) {
  x <- logLfnx(elections$growth, elections$vote, a, b[i], sigma)
  logLfnx.b[i] <- x
}
logLfnx.b <- data.frame(cbind(logLfnx.b,b))
p2 <- ggplot(logLfnx.b,aes(b,logLfnx.b)) + geom_point() + theme_bw() +
  geom_vline(xintercept=fit8.1$coefficients[2], col="red")

gridExtra::grid.arrange(p1,p2,nrow=1)

## 8.3 - Least absolute deviation
rad <- function(x, y, a, b) {
  resid <- y - (a + b*x)
  return(sum(resid))
}
# plot sum of absolute values of residuals varying a
a <- seq(46,46.99,0.01)
b <- 3.05
rad.a <- rep(0, 100)
for (i in 1:100) {
  x <- rad(elections$growth, elections$vote, a[i], b)
  rad.a[i] <- x
}
rad.a <- data.frame(cbind(rad.a,a))
p1 <- ggplot(rad.a,aes(a,rad.a)) + geom_point() + theme_bw() +
  geom_vline(xintercept=fit8.1$coefficients[1], col="red")

# plot sum of absolute values of residuals varying a
a <- 46.27
b <- seq(2.5,3.49,0.01)
rad.b <- rep(0, 100)
for (i in 1:100) {
  x <- rad(elections$growth, elections$vote, a, b[i])
  rad.b[i] <- x
}
rad.b <- data.frame(cbind(rad.b,b))
p2 <- ggplot(rad.b,aes(b,rad.b)) + geom_point() + theme_bw() +
  geom_vline(xintercept=fit8.1$coefficients[2], col="red")
gridExtra::grid.arrange(p1,p2,nrow=1)

# ----




