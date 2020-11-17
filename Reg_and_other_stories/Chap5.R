
# chapter 5, exercises (section 5.7 pag 76) 

### ---- 5.1 Discrete probability simulation ----

# a) function that simulates 60% chance of scoring (independent throws), run 
#     until two thows missed in a row. 
set.seed(1)
accuracy <- 0.6
player.a <- function(x){
  for (t in 1:x) {
    missed <- 0 
    shots <- 0
    scored <- 0
    while  (missed < 2) {
      throw <- runif(1)
      if (throw <= accuracy){
        shots = shots + 1
        scored = scored + 1
      }
      else {
        missed = missed + 1
        shots = shots + 1
      }
    }
    return(shots)
  }
}
player.a(1)


# b) for loop to simulate process 1000 times, return mean and sd of number of shots,
#     plot histogram
N <- 1000
ex.b <- rep(NA, N)
for (i in 1:N) {
  ex.b[i] <- player.a(1)
}
p5.1.b <- ggplot() + aes(ex.b) + geom_histogram(binwidth=1, colour="black", fill="white") + 
  ggtitle(paste0("mean = ",round(mean(ex.b),2),"; sigma = ",round(sd(ex.b),2)))

# c) scatterplot of n of shots and proportion successful 
total <- NULL
success <- NULL
for (t in 1:N) {
  missed <- 0 
  shots <- 0
  scored <- 0
  while  (missed < 2) {
    throw <- runif(1)
    if (throw <= 0.6){
      shots = shots + 1
      scored = scored + 1
    }
    else {
      missed = missed + 1
      shots = shots + 1
    }
  }
  total[t] <- shots
  success[t] <- scored/shots
}
ex.c <- data.frame(cbind(total,success))
p5.1.c <- ggplot(ex.c,aes(x = total, y= success)) + geom_point()

gridExtra::grid.arrange(p5.1.b, p5.1.c, nrow=1)

# ----

### --- 5.2 Continuous probability simulation ----

# men's weight normally distributed with mean 5.13 and sd 0.17; 
# women's near-normal with mean 4.96 and sd 0.2. 
# Suppose 10 random adults step in elevator with max capacity 1750. 
# What is the probability total weight exceeds limit

ex.5.2 <- NULL
max.capacity <- 1750
draws <- 10000
for (i in 1:draws) {
  N <- 10
  male <- rbinom(N, 1, 0.5)
  weight <- ifelse(male == 1, rnorm(N, 5.13, 0.17), rnorm(N, 4.96, 0.2))
  weight <- exp(weight)
  if (sum(weight) >= max.capacity) {
    ex.5.2[i] <- 1
  }
  else {ex.5.2[i] <- 0}
}
sum(ex.5.2)/draws

### --- 5.3 Binomial distribution 
# Player takes 10 shots with 40% probability per shot (independent)

# a) R code to compute probability makes 3 out of 10 shots
ex.5.3.a <- dbinom(3, 10, prob = 0.4)

# b) R function to simulate 10 shots 10,000 times. Check probability of making 3 
#     is close to exact probability computed in (a)
ex.5.3.b <- NULL
draws <- 1000
for (i in 1:draws) {
  N <- 10
  scored <- rbinom(N, 1, 0.4)
  ex.5.3.b[i] <- sum(scored)
}
sum(ex.5.3.b == 3)/draws

