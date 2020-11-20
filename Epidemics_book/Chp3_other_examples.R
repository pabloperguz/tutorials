packages <- c("deSolve","ggplot2","ggsci","stats","epimdr","bbmle",
              "scales")
lapply(packages, require, character.only = TRUE)

#### --- Further chapter 3 examples --- ####

### 3.6.1 Influenza A/H1N1 1977 ----

head(flu)  # cases correspond to daily number of bed-confined boys, not incidence

# Serial interval is estimated at 2-3 days and the infectious period at 5 days
# Plotted data shows a log-linear increase in cases in the first 5 days
nat <- ggplot(flu,aes(x=day, y=cases)) + geom_point() + geom_line() +
  ylab("In bed") + theme_bw() + ggtitle("Natural scale") + 
  scale_x_continuous(breaks = seq(min(flu$day),max(flu$day),2))
log <- ggplot(flu,aes(x=day, y=cases)) + geom_point() + geom_line() +
  scale_y_log10() + ylab("In bed") + theme_bw() + ggtitle("Log scale") +
  scale_x_continuous(breaks = seq(min(flu$day),max(flu$day),2))
gridExtra::grid.arrange(nat,log,nrow=1)

# Get a quick calculation of R0 from the boarding school flu outbreak
fit.flu=lm(log(cases)~day, subset=day<=5, data=flu)
r=fit.flu$coef["day"]
V=c(2, 3)
V*r+1

# R_0 for pandemic flu is usually between 1.5 and 2.5. However, this was a closed
# epidemic in a boarding-school, were contact rates were likely much higher than
# in a community setting

# ----

### 3.6.2 Ebola Sierra Leone 2014–2015 ----

summary(ebola)  # cases are back-calculated from CDC's cum_cases which led to 
                # negative numbers for some days. These were set to zero

# Serial interval is around 15 days, incubation 11 days and time to 
# hospitalisation 5 days. The infectious period begins approximately 2.5 days 
# before the onset of symptoms. Mean time to death or discharge is 5 and 11 days,
# respectively. 
scaleFactor <- floor(max(ebola$cum_cases) / max(ebola$cases))
nat <- ggplot(ebola,aes(x=day)) + geom_point(aes(y=cases)) + 
  geom_line(aes(y=cases),lty=3) +
  geom_line(aes(y=cum_cases/scaleFactor),col="red") +
  scale_y_continuous("Incidence", 
    sec.axis = sec_axis(~.*scaleFactor,
                        labels = comma),
    labels = comma) +
  theme(axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")) + ggtitle("Natural scale")
log <- ggplot(ebola,aes(x=day, y=cum_cases)) + geom_line(col="red") +
  scale_y_log10() + theme_bw() + ggtitle("Log scale") +
  scale_y_continuous("Cumulative incidence", labels = comma) +
  theme(axis.title.y.left=element_text(color="red"),
        axis.text.y.left=element_text(color="red"))
gridExtra::grid.arrange(nat,log,nrow=1)

# Use the regression correction method to estimate R_0 for the first 100 days
fit.ebo = lm(log(cum_cases)~day, subset = day<=100, data = ebola)
r=fit.ebo$coef["day"]
V = 15
f = (5+2.5)/V
V * r + 1 + f * (1 - f) * (V * r)^2

# Use the removal method to estimate parameters for S_0 and Beta
# cases will have to be aggregated into bi-weekly, roughly corresponding to the
# serial interval, so we can apply the removal method
cases=sapply(split(ebola$cases,
                   floor((ebola$day-.1)/14)), sum)
sum(cases)

# Negative log-likelihood in chain-binomial model
llik.cb = function(S0, beta, I) {
  n = length(I)
  S = floor(S0 - cumsum(I[-n]))
  p = 1 - exp(-beta * (I[-n])/S0)
  L = -sum(dbinom(I[-1], S, p, log = TRUE))
  return(L)
}
fit=mle2(llik.cb, start=list(S0=20000, beta=2),
         method="Nelder-Mead",data = list(I = cases))
summary(fit)



# ----