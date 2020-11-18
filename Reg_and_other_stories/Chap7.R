packages <- c("ggplot2","ggsci","dplyr","insight","bayestestR",
              "rstanarm"  # rstanarm uses Stan as statistical inference engine
              # to fit regression models
)
lapply(packages, require, character.only = TRUE)

### ---- Chapter 7, linear regression with single predictor ---- ###

### Exercises

## 7.2 Fake-data simulation and regression ----
# Fake data
N <- 100
a <- 5
b <- 7
x <- runif(N, 0, 50)
sigma <- 3
y <- a + b*x + sigma*rnorm(N)
fake <- data.frame(x,y)

# Fit a regression line and display output
fit.1 <- stan_glm(y ~ x, data = fake)
summary(fit.1)
posteriors <- get_parameters(fit.1)
colnames(posteriors) <- c("intercept","x")

p.model <- ggplot(fake,aes(x=x,y=y)) + geom_point() + theme_bw() +
  geom_abline(intercept=coef(fit.1)[1], slope=coef(fit.1)[2],color="red") +
  annotate(geom="text", x=mean(fake$x), y=mean(fake$y), vjust=-12, 
           label=paste("y = ",round(mean(fake$x), 2), "+", 
                       round(mean(fake$y), 2), "* x"), colour = "red", size = 4) +
  ggtitle("Data and fitted regression line")
p.dist <- ggplot(posteriors, aes(x = x)) +
  ggtitle("x posteriors") + geom_density(fill = "red",alpha = 0.1) +
  # The MAP in purple
  geom_vline(xintercept=map_estimate(posteriors$x), color="purple", size=1) +
  annotate(geom="text", x=map_estimate(posteriors$x),y=10, hjust=-0.5, vjust=-11,
           label=paste("MAP = ",round(map_estimate(posteriors$x),2)), size=4,
           color="purple")
gridExtra::grid.arrange(p.model,p.dist,nrow=1)

# ----

## 7.3 Fake-data simulation and fitting the wrong model ----
# Fake data
N <- 100
a <- 5
b <- 7
c <- 2
x <- runif(N, 0, 50)
sigma <- 3
y <- a + b*x + c*x^2 + sigma*rnorm(N)
fake <- data.frame(x,y)

# Fit a regression line and display output
fit.2 <- stan_glm(y ~ x, data = fake)
summary(fit.2)
posteriors <- get_parameters(fit.2)
colnames(posteriors) <- c("intercept","x")

p.model <- ggplot(fake,aes(x=x,y=y)) + geom_point() + theme_bw() +
  geom_abline(intercept=coef(fit.2)[1], slope=coef(fit.2)[2],color="red") +
  annotate(geom="text", x=mean(fake$x), y=mean(fake$y), vjust=-12, hjust=0.7, 
           label=paste("y = ",round(mean(fake$x), 2), "+", 
                       round(mean(fake$y), 2), "* x"), colour = "red", size = 4) +
  ggtitle("Data and fitted regression line")
p.dist <- ggplot(posteriors, aes(x = x)) +
  ggtitle("x posteriors") + geom_density(fill = "red",alpha = 0.1) +
  # The MAP in purple
  geom_vline(xintercept=map_estimate(posteriors$x), color="purple", size=1) +
  annotate(geom="text", x=map_estimate(posteriors$x),y=2, hjust=-0.1,
           label=paste("MAP = ",round(map_estimate(posteriors$x),2)), size=4,
           color="purple")
gridExtra::grid.arrange(p.model,p.dist,nrow=1)




