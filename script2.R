library(tidyverse)
library(brms)
library(rethinking)
library(EnvStats)


m = brm(bf(I ~ exp(log_i0 + a*log(M)) - E*(1/(8.62*1e-5*Temp))),
)


met.rate <- function(log_i0_mu, log_i0_err, 
                     a_mu, a_err, 
                     E_mu, E_err,
                     M, Temp) {
  log_i0 = rnorm(1, log_i0_mu, log_i0_err)
  a = rnorm(1, a_mu, a_err)
  E = rnorm(1, E_mu, E_err)
  
  I = exp(log_i0 + a*log(M) - E*(1/(8.62*1e-5*(Temp+273))))
  return(I)
}

met.rate(24.581475, 1.427089,
         0.5652537, 0.021186,
         0.7093476, 0.035668,
         100, 20)

vec = vector(mode = "numeric", 100000)
for (i in 1:100000) {
  vec[i] = met.rate(24.581475, 1.427089,
                    0.5652537, 0.021186,
                    0.7093476, 0.035668,
                    100, 20)
}

hist((vec[vec >= HPDI(vec, .95)[1] &
            vec <= HPDI(vec, .95)[2]]
), breaks = 1000)


exp(24.581475 + 0.5652537*log(100) - 0.7093476*(1/(8.62*1e-5*(20+273))))





n = 1000

log_i0 = rnorm(n, 24.581475, 1.427089)
a      = rnorm(n, 0.5652537, 0.021186)
E      = rnorm(n, 0.7093476, 0.035668)

log_i0 = rnormTrunc(n, 24.581475, 1.427089, 
                    min = qnorm(.05, 24.581475, 1.427089),
                    max = qnorm(.95, 24.581475, 1.427089))

a      = rnormTrunc(n, 0.5652537, 0.021186, 
                    min = qnorm(.05, 0.5652537, 0.021186),
                    max = qnorm(.95, 0.5652537, 0.021186))

E      = rnormTrunc(n, 0.7093476, 0.035668, 
                    min = qnorm(.05, 0.7093476, 0.035668),
                    max = qnorm(.95, 0.7093476, 0.035668))

plot( NULL , xlim=range(0.72, 1000) , ylim=c(-20,20) ,
      xlab="mass (mg)" , ylab="metabolic rate (J/h)" )
for ( i in 1:n ) { 
  curve((log_i0[i] + a[i]*log(x) - E[i]*(1/(8.62*1e-5*(20+273)))),
        from = 0.72 , to = 1000 , add=TRUE ,
        col = col.alpha("black",0.2) )
}

plot( NULL , xlim=range(0.72, 1000) , ylim=c(-0,20) ,
      xlab="mass (mg)" , ylab="metabolic rate (J/h)" )

for ( i in 1:n ) { 
  curve(exp(log_i0[i] + a[i]*log(x) - E[i]*(1/(8.62*1e-5*(20+273)))),
        from = 0.72 , to = 1000 , add=TRUE ,
        col = col.alpha("black",0.2) )
}
curve(exp(24.581475 + 0.5652537*log(x) - 0.7093476*(1/(8.62*1e-5*(20+273)))),
      from = .72, to = 1000, add = TRUE,
      col = "red")
curve(exp(24.581475 + 0.5652537*log(x) - 0.7093476*(1/(8.62*1e-5*(5+273)))),
      from = .72, to = 1000, add = TRUE,
      col = "green")
curve(exp(24.581475 + 0.5652537*log(x) - 0.7093476*(1/(8.62*1e-5*(35+273)))),
      from = .72, to = 1000, add = TRUE,
      col = "purple")



curve(exp(mean(log_i0) + mean(a)*log(x) - mean(E)*(1/(8.62*1e-5*(20+273)))),
      from = .72, to = 1000, add = TRUE,
      col = "green")

curve(exp(mean(log_i0) + mean(a)*log(x) - mean(E)*(1/(8.62*1e-5*(20+273)))),
      from = .72, to = 1000, add = TRUE,
      col = "green")
curve(exp(mean(log_i0) + mean(a)*log(x) - mean(E)*(1/(8.62*1e-5*(20+273)))),
      from = .72, to = 1000, add = TRUE,
      col = "green")
















library(tidyverse)

decay <- function(t, t2) exp(-(t/t2))

signal_loss <- list(water = 2500, white_matter = 100)

# Adapted from function reference: 
# https://ggplot2.tidyverse.org/reference/stat_function.html
ggplot(data.frame(x = c(0, 10000)), aes(x)) +
  stat_function(fun = function(x) decay(x, signal_loss$water), colour = "blue") +
  stat_function(fun = function(x) decay(x, signal_loss$white_matter), colour = "red")