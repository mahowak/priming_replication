# This script runs the power analysis, usinb brms. It is currently
# set up to run in parallel using 8 cores.
library(tidyverse)
library(brms)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) { exp(x)/(1+exp(x))}
###########

# read in Corley data, set DO to dep var,
# do contrast coding as described in analysis plan
d = read_tsv("Corley_Scheepers/Corley_Scheepers_2002.txt") %>%
    filter(prime_resp %in% c("DO", "PO"),
           target_resp %in% c("DO", "PO")) %>%
    rename(subj = Subject_ID) %>%
    mutate(condcode = ifelse(prime_resp == "DO", .5, -.5),
           respcode = ifelse(target_resp == "DO", 1, 0),
           boost = ifelse(cond %in% c("01", "02"), .5, -.5),
           c.boost.prime = case_when(condcode == .5 & boost == .5 ~ .5,
                                      condcode == -.5 & boost == .5 ~ -.5,
                                      TRUE ~ 0),
           c.noboost.prime = case_when(condcode == .5 & boost == -.5 ~ .5,
                                     condcode == -.5 & boost == -.5 ~ -.5,
                                     TRUE ~ 0),
           c.boost = case_when(boost == .5 ~ .5,
                             boost == -.5 ~ -.5),
           c.prime = case_when(condcode == .5 ~ .5,
                             condcode == -.5 ~ -.5))

# set priors for power analysis 
priors <-c(set_prior("normal(-.3, .1)", class = "Intercept"),
                  set_prior("normal(0, .5)", class = "b"),
                  set_prior("normal(0, .05)", class = "sd"),
                  set_prior("lkj(2)", class = "L"),
                  set_prior("normal(0, .5)", class = "sd", group="subj", coef="Intercept" ),
                  set_prior("normal(0, .5)", class = "sd", group="item", coef="Intercept" ))

# full model for power analysis
l.1 = brm(respcode  ~ c.boost.prime + c.noboost.prime + c.boost +
                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
                      (c.boost.prime + c.noboost.prime + c.boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=2000,
                    prior = priors, 
                    save_all_pars = T)

# simpler model for power analysis (without c.noobost.prime fixef)
l.1.null = brm(respcode  ~ c.boost.prime  + c.boost +
                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
                      (c.boost.prime + c.noboost.prime + c.boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=2000,
                    prior = priors, 
                    save_all_pars = T)

# function for simulating a dataframe with ns subjects,
# ni items, beta1 (prime with boost) and beta2 (prime with no boost)

simulate.df.simple = function(nsubj, nitems, beta1, beta2) {
  subjs = tibble(subj.intercept = rnorm(nsubj, 0, 1.5), subj.b1 = rnorm(nsubj, 0, .14),
                 subj.b2 = rnorm(nsubj, 0, .14)) %>%
    mutate(subj = 1:n())
  
  items = tibble(item.intercept = rnorm(nitems, 0, 1.5), item.b1 = rnorm(nitems, 0, .14),
                 item.b2 = rnorm(nitems, 0, .14)) %>%
    mutate(item = 1:n())
  
  beta.int = rnorm(1, -.33, .1)
  beta.c.boost.prime = rnorm(1, beta1, .05)
  beta.c.noboost.prime = rnorm(1, beta2, .1)
  beta.c.boost = 0
  
  newd = expand.grid(subj = (seq(1,ns)), 
                     item = (seq(1,ni))) %>%
    mutate(rand1=runif(n()),
           rand2 = runif(n()),
           condcode = ifelse(rand1 > .5, .5, -.5),
           boost = ifelse(rand2 > .5, .5, -.5),
           c.boost.prime = case_when(condcode == .5 & boost == .5 ~ .5,
                                     condcode == -.5 & boost == .5 ~ -.5,
                                     TRUE ~ 0),
           c.noboost.prime = case_when(condcode == .5 & boost == -.5 ~ .5,
                                       condcode == -.5 & boost == -.5 ~ -.5,
                                       TRUE ~ 0),
           c.boost = case_when(boost == .5 ~ .5,
                               boost == -.5 ~ -.5),
           c.prime = case_when(condcode == .5 ~ .5,
                               condcode == -.5 ~ -.5)) %>%
    sample_n(.60 * n()) %>%
    left_join(subjs) %>%
    left_join(items) %>%
    mutate(fitted = inv.logit(beta.int +
                                (beta.c.boost.prime + subj.b1 + item.b1) * c.boost.prime + 
                                (beta.c.noboost.prime + subj.b2 + item.b2) * c.noboost.prime + 
                                (beta.c.boost) * c.boost + 
                                subj.intercept +
                                item.intercept ),
           respcode = as.integer(runif(n()) < fitted))
  
  return(newd)
}

# set parameters
bf.new.null = 1
a = NULL
startpoint = 200
ni = 48
beta1 = .88
beta2 = .29

# run power analysis, appending output to exp1_power_analysis.csv
for (ns in c(500)) {
  for (it in seq(1, 2)) {
    newd_ = simulate.df.simple(ns, ni, beta1, beta2)
    for (curnum in seq(startpoint, ns, 100)) {
      if (curnum == startpoint | (bf.new.null > (1/6) & bf.new.null < 6)) {
        newd = filter(newd_, subj <= curnum) # filter to include nsubj
        l.new = update(l.1, newdata=newd, cores=8,
                       chains=8, iter=curnum * 40, save_all_pars=T,
                       warmup = 1000)
        l.null = update(l.1.null, newdata=newd,
                        cores=8, chains=8, iter=curnum * 40, save_all_pars=T,
                        warmup = 1000)
        boost.bigger = mean(fixef(l.new, summary = F)[, "c.boost.prime"] > 
                              fixef(l.new, summary = F)[, "c.noboost.prime"])
        bf.new.null = bayes_factor(l.new, l.null)[1]
        a = cbind(curnum, ni, it, bf.new.null, boost.bigger)
        print(a)
	      write.table( a, file="exp1_power_analysis.csv", append = T, sep=",", row.names=F, col.names=F)
      }
    }
  }
}

