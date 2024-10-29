library(tidyverse)
library(nlme)

ehnes = read.csv("Ehnes2011.csv")

ehnes$this.study = ifelse(ehnes$original.study == "this study","this","other")

linear.m = lme(log(met.rate.Jh) ~ 1 
               + log(mass.mg) 
               + I(1/(8.62*1e-5*(273.15+temp.C))),
               random = ~ 1|original.study,
               method = "REML",
               data = ehnes)
summary(linear.m)

phylo.m = lme(log(met.rate.Jh) ~ 0 
              + group.2
              + group.2:log(mass.mg) 
              + group.2:I(1/(8.62*1e-5*(273.15+temp.C))),
              random = ~ 1|original.study,
              method = "REML",
              data = ehnes)
summary(phylo.m)


library(brms)
library(tidybayes)
library(tidyr)
library(modelr)
library(ggdist)

linear.mb = brm(log(met.rate.Jh) ~ 1 
                + log(mass.mg) 
                + I(1/(8.62*1e-5*(273.15+temp.C)))
                + (1|original.study)
                ,
                #family = student(),
                chains = 4,
                iter = 2000,
                cores = 4,
                backend = "cmdstanr", 
                data = ehnes)
summary(linear.mb)

ehnes %>%
  data_grid(mass.mg = seq_range(mass.mg, 
                                n = 101), 
            temp.C) %>%
  add_epred_draws(linear.mb, 
                  ndraws = 100, 
                  re_formula = NA) %>% 
  ggplot(aes(x = log(mass.mg))) +
  stat_lineribbon(aes(y = (.epred)),
                  color = "red") +
  geom_point(data = ehnes, aes(y = log(met.rate.Jh),
                               x = log(mass.mg)),
             alpha = .25) +
  scale_fill_brewer(palette = "Greys") +
  theme_classic()

ehnes %>%
  data_grid(mass.mg = seq_range(mass.mg, 
                                n = 101), 
            temp.C) %>%
  add_epred_draws(linear.mb, 
                  ndraws = 100, 
                  re_formula = NA) %>% 
  ggplot(aes(x = log(mass.mg))) +
  stat_lineribbon(aes(y = (.epred)),
                  color = "red") +
  geom_point(data = ehnes, aes(y = log(met.rate.Jh),
                               x = log(mass.mg),
                               color = this.study),
             alpha = .25) +
  scale_fill_brewer(palette = "Greys") +
  theme_classic()

## predicted responses of 
newdata <- data.frame(
  original.study = factor(c("this study")),
  mass.mg = c(150),
  temp.C = c(10, 20)
)
pp <- posterior_predict(linear.mb, newdata = newdata)
str(pp)

predicted <- linear.mb %>%  
  predicted_draws(newdata = newdata,
                  seed = 12345)
ggplot(predicted, aes(x = (.prediction),
                      fill = as.factor(temp.C),
                      alpha = .25)) +
  stat_halfeye() +
  theme_classic()

phylo.mb = brm(log(met.rate.Jh) ~ 0
                + group.2
                + group.2:log(mass.mg) 
                + group.2:I(1/(8.62*1e-5*(273.15+temp.C)))
                + (1|original.study),
                chains = 4,
                iter = 2000,
                cores = 4,
                backend = "cmdstanr", 
                data = ehnes)
summary(phylo.mb)
## predicted responses of an individual of 100mg at 20 degrees C for three different taxa
newdata <- data.frame(
  original.study = factor(c("this study")),
  group.2 = factor(c("Insecta","Chilopoda","Isopoda")),
  mass.mg = c(100),
  temp.C = c(20)
)
predicted <- phylo.mb %>%  
  predicted_draws(newdata = newdata,
                  seed = 12345)
ggplot(predicted, aes(x = (.prediction),
                      fill = as.factor(group.2),
                      alpha = .25)) +
  stat_halfeye() +
  theme_classic()
