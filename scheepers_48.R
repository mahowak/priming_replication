library(tidyverse)
library(brms)
library(lme4)
library(shinystan)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) { exp(x)/(1+exp(x))}
###########

simulate.df.simple = function(ns, ni, beta1) {
  nsubj = ns
  subjs = tibble(subj.intercept = rnorm(nsubj, 0, 1.5),
                 subj.b1 = rnorm(nsubj, 0, .14)) %>%
    mutate(subj = 1:n())
  
  nitems = ni
  items = tibble(item.intercept = rnorm(nitems, 0, 1.5),
                 item.b1 = rnorm(nitems, 0, .14)) %>%
    mutate(item = 1:n())
  
  beta.int = rnorm(1, -.33, .1)
  beta = rnorm(1, beta1, .05)

  newd = expand.grid(subj = (seq(1,ns)), 
                     item = (seq(1,ni))) %>%
    mutate(rand1=runif(n()),
           rand2 = runif(n()),
           condcode = ifelse(rand1 > .5, .5, -.5)) %>%
    sample_n(.60 * n()) %>%
    left_join(subjs) %>%
    left_join(items) %>%
    mutate(fitted = inv.logit(beta.int +
                                (beta + subj.b1 + item.b1) * condcode + 
                                subj.intercept +
                                item.intercept ),
           respcode = as.integer(runif(n()) < fitted))
  
  return(newd)
}


d = simulate.df.simple(24, 24, .2)

priors.scheepers <-c(set_prior("normal(-.3, .1)", class = "Intercept"),
                     set_prior("normal(0, .5)", class = "b"),
                     set_prior("normal(0, .05)", class = "sd"),
                     set_prior("lkj(2)", class = "L"),
                     set_prior("normal(0, .5)", class = "sd", group="subj", coef="Intercept" ),
                     set_prior("normal(0, .5)", class = "sd", group="item", coef="Intercept" ))

priors.scheepers.null <-c(set_prior("normal(-.3, .1)", class = "Intercept"),
                     set_prior("normal(0, .05)", class = "sd"),
                     set_prior("lkj(2)", class = "L"),
                     set_prior("normal(0, .5)", class = "sd", group="subj", coef="Intercept" ),
                     set_prior("normal(0, .5)", class = "sd", group="item", coef="Intercept" ))

l = brm(respcode  ~ condcode +
          (condcode | subj) +
          (condcode | item),
        family="bernoulli",
        data=d,
        cores =2,
        chains=2,
        iter=2000,
        prior = priors.scheepers, 
        save_all_pars = T)

l0 = brm(respcode  ~ 1 +
               (condcode | subj) +
               (condcode | item),
             family="bernoulli",
             data=d,
             cores =2,
             chains=2,
             iter=2000,
             prior = priors.scheepers.null, 
             save_all_pars = T)

bayes_factor(l, l0)

bf.new.null = 1
a = NULL
startpoint = 200
ni = 48
beta1 = .2

for (ns in c(500)) {
  for (it in seq(1, 2)) {
    newd_ = simulate.df.simple(ns, ni, beta1)
    for (curnum in seq(startpoint, ns, 100)) {
      if (curnum == startpoint | (bf.new.null > (1/6) & bf.new.null < 6)) {
        newd = filter(newd_, subj <= curnum)

        print('fitting l.null')
        l.null = update(l0, newdata=newd,
                        cores=8, chains=8, iter=curnum * 40, save_all_pars=T,
                        warmup = 1000)
	print('fitting l.new')	
        l.new = update(l, newdata=newd, cores=8,
                       chains=8, iter=curnum * 40, save_all_pars=T,
                       warmup = 1000)
        bf.new.null = bayes_factor(l.new, l.null)[1]
        a = cbind(curnum, ni, it, bf.new.null)
        print(a)
        write.table( a, file="results_scheepers_48.csv", append = T, sep=",", row.names=F, col.names=F)
      }
    }
  }
}


