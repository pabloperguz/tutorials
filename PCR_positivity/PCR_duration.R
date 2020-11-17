
# attach packages
packages <- c("VGAM","ggplot2","rriskDistributions",
              "survival","survminer","ggsci","flexsurv","stats")
lapply(packages, require, character.only = TRUE)

# load data from Omar et al.
df <- read.csv("omar_data.csv")

# fit parameters directly
p <- rev(df$Prop[!is.na(df$Proportion)])
q <- df$Days[!is.na(df$Proportion)]

fit.results <- rriskFitdist.perc(p, q, show.output = FALSE)
params <- as.numeric(get.gamma.par(p,q),show.output = FALSE, plot = FALSE)
edata <- data.frame(y = rgamma(1000, shape=params[1], scale=1/params[2]))
fitdistrplus::plotdist(edata$y, histo=T, demp=T)
summary(edata$y)

# Fit gamma-accelerated failure time model
edata.surv <- Surv(edata$y)
fit.edata <- survfit(edata.surv~1,type="kaplan-meier", conf.type="log-log")

print(fit.edata,print.rmean=T)
ggsurvplot(fit.edata,data=edata,
           palette="lancet",
           ggtheme=theme_bw(),
           surv.median.line="hv",
           legend.title="Time to SARS-CoV-2 negative",
           legend.labs="",xlab="Days since symptoms onset",
           ylab="Probability still positive")
fit.gamma.edata <- flexsurvreg(edata.surv~1, dist="gamma")
fit.gamma.edata


# Obtain ccdf of PCR positivity
q <- seq(0, 1000, l = 21)
df.edata <- data.frame(x = fit.edata$surv, y = fit.edata$cumhaz, 
                       z = fit.edata$time)
ccdf_pcr <- data.frame()
for (i in 1:21) {
  if (i == 1){out = 0}
  else if (i>1){
    x = q[i]
    out = df.edata$y[x]
  }
  ccdf_pcr[1,i] <- out
}
rownames(ccdf_pcr) <- "pcr_pos"

# obtain mean, median and IQR of progression durations
progression_durations <- data.frame(param = "m_PosNeg",
                                    Q2.5 = df.edata$z[25],
                                    Q50 = df.edata$z[500],
                                    Q97.5 = df.edata$z[974],
                                    mean = mean(df.edata$z))

# pass parameters on to Lilith's functions
source("fit_Erlang.R") ## this is kept offline

# Print outputs
op_res # scaled Erlang parameters (time in dd)
scaled_gamma # re-scaled gamma over mean duration (true time)
progression_durations$mean






## Time to negative PCR 
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
