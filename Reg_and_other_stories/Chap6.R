packages <- c("ggplot2","ggsci","dplyr",
              "rstanarm"  # rstanarm uses Stan as statistical inference engine
                          # to fit regression models
              )
lapply(packages, require, character.only = TRUE)

### ---- Chapter 6, background on regression ---- ###

## fitting regression to data

# simplest model

x <- 1:20       # fake data points
n <- length(x)
a <- 0.2        # intercept
b <- 0.3        # slope
sigma <- 0.5    # sd of error
y <- a + b*x + sigma*rnorm(n) # simple linear model

fake <- data.frame(x, y)
fit.1 <- stan_glm(y ~ x, data = fake)  # stan fit generalised linear model
print(fit.1, digits = 2)
plot(fake$x, fake$y, main = "Data and fitted regression line")
a_hat <- coef(fit.1)[1]
b_hat <- coef(fit.1)[2]
abline(a_hat, b_hat)
x_bar <- mean(fake$x)
text(x_bar, a_hat + b_hat * x_bar,
     paste("y = ",round(a_hat, 2), "+", round(b_hat, 2), "* x"), adj=0)

# regression to the mean 

n <- 1000       # fake data for midterm and final exams of 1,000 students
true_ability <- rnorm(n, 50, 10)
noise_1 <- rnorm(n, 0, 10)
noise_2 <- rnorm(n, 0, 10)
mideterm <- true_ability + noise_1
final <- true_ability + noise_2
exams <- data.frame(mideterm, final)

fit_1 <- stan_glm(final ~ mideterm, data = exams)
plot(mideterm, final, xlab = "Midterm scores", ylab = "Final scores")
abline(coef(fit_1)) # exemplifies regression fallacy
                    # i.e. assuming causality that is entirely spurious, as a
                    # result of 'true ability' + random noise

### exercises

## 6.2 function y = a + bx + error that takes as input a,b,n,sigma and outputs
#      the data, prints fitted regression and plots the data/model
simple_linear <- function(a, b, n, sigma, to.return){
  
  x <- runif(n, min = 0, max = 100)
  y <- a + b*x + sigma*rnorm(n) 
  
  df <- data.frame(x, y)
  
  fit.df <- stan_glm(y ~ x, data = df)
  print(fit.df, digits = 2)
  plot(df$x, df$y, main = "Data and fitted regression line")
  a_hat <- coef(fit.df)[1]
  b_hat <- coef(fit.df)[2]
  abline(a_hat, b_hat, col="red", lty=3)
  x_bar <- mean(df$x)
  text(x_bar, a_hat + b_hat * x_bar,
       paste("          y = ",round(a_hat, 2), "+", round(b_hat, 2), "* x"), adj=0)
  
  if (to.return == "df"){return(df)}
  if (to.return == "fit"){
    fit.df <- as.data.frame(fit.df)
    return(fit.df)
  }
}

ex.6.3 <- simple_linear(20, 1.2, 1000, 0.5, "df")

seq.n <- seq(0, 500, by=10)
seq.n <- seq.n[-1]
ex.6.4 <- matrix(NA,nrow = length(seq.n),ncol = 4)
for (i in seq_along(seq.n)) {
  
  n <- seq.n[i]
  
  run <- simple_linear(30, 0.54, n, 2, "fit")
  
  ex.6.4[i,1] <- mean(run$x)
  ex.6.4[i,2] <- mad(run$x)
  ex.6.4[i,3] <- mean(run$`(Intercept)`)
  ex.6.4[i,4] <- mad(run$`(Intercept)`)
}
ex.6.4 <- as.data.frame(ex.6.4)
ex.6.4 <- cbind(seq.n, ex.6.4)
plot.6.4 <- ggplot(ex.6.4, aes(x=seq.n))

ggplot(reshape2::melt(ex.6.4, id.vars="seq.n"),
       aes(seq.n, value, colour = variable)) + geom_line() + 
  facet_wrap(~variable, scales = "free_y") + theme_bw()



