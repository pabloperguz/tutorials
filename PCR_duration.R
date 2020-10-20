df <- read.csv("omar_data.csv")
N <- 1000
dist.data <- matrix(nrow=N,ncol=2)

for (i in 1:N) {
  while (TRUE) {
    edata <- NULL
    for (j in 1:12) {
      
      n <- df$n[j]
      out <- data.frame(x = rnorm(n, mean=df$Time[j], sd=df$sd[j]))
      edata <- rbind(edata,out)
    }
    
    fit <- try(fitdistrplus::fitdist(edata$x,"gamma",lower=c(0,0),method = "mle",
                                     start = list(scale=0.1,shape=0.1)),silent=T)
    if(!is(fit, 'try-error')) break
  }
  dist.data[i,1] <- fit$estimate[1]
  dist.data[i,2] <- fit$estimate[2]
  rm(edata)
}


edata <- data.frame(y = rgamma(1000, shape=mean(dist.data[,2]), scale=mean(dist.data[,1])))
fitdistrplus::plotdist(edata$y, histo=T, demp=T)
summary(edata$y)

rate <- exp(mean(dist.data[,1])); myshape <- 1
for (ii in 1:myshape)
  edata <- transform(edata, y = y + rexp(1000, rate = rate))
fit <- VGAM::vglm(y ~ 1, VGAM::erlang(shape = myshape), data = edata, trace = TRUE)
coef(fit, matrix = TRUE)
VGAM::Coef(fit) 
1/rate
summary(fit)


# Time to negative PCR 
library(rriskDistributions)
p <- rev(df$Prop[!is.na(df$Proportion)])
q <- df$Days[!is.na(df$Proportion)]
plot(q,p)

# Test best distribution fit and get parameters
fit.results <- rriskFitdist.perc(p, q, show.output = FALSE)
plotDiagnostics.perc(fit.results, tolPlot = 10)

get.gamma.par(p, q, show.output = TRUE, plot = TRUE, tol = 0.001)

edata.2 <- data.frame(y = rgamma(1000, shape=params[1], scale=1/params[2]))
fitdistrplus::plotdist(edata.2$y, histo=T, demp=T)
summary(edata$y)
