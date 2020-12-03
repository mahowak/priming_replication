library(tidyverse)
library(brms)
library(lme4)
library(shinystan)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) { exp(x)/(1+exp(x))}
###########

# todo: use corley 322, 324
d = read_csv("raw_data_merged_with_master.csv") 

# Pickering & Branigan (1998), depvar is DO

# get Corley data
d = read_tsv("Corley_Scheepers/Corley_Scheepers_2002.txt")

d = filter(d, prime_resp %in% c("DO", "PO"),
           target_resp %in% c("DO", "PO")) %>%
  mutate(condcode = ifelse(prime_resp == "DO", .5, -.5),
         respcode = ifelse(target_resp == "DO", 1, 0),
         boost = ifelse(cond %in% c("01", "02"), .5, -.5)) %>%
  rename(subj = Subject_ID)

subjs = group_by(d, subj, condcode, boost) %>%
  summarise(m=mean(respcode)) %>%
  pivot_wider(id_cols=subj, names_from=c(condcode, boost), values_from = m) 
names(subjs) = c("subj", "noprime_noboost", "noprime_boost", "prime_noboost", 'prime_boost')


d.sum = group_by(d, condcode, boost) %>% summarise(m=mean(respcode),
                                                   effect=logit(m)) 
d.sum  %>% group_by(boost) %>% arrange(condcode) %>%
  mutate(x = effect - lag(effect))

priors.corley <-c(set_prior("normal(-.3, .1)", class = "Intercept"),
                  set_prior("normal(0, .5)", class = "b"),
                  set_prior("normal(0, .05)", class = "sd"),
                  set_prior("lkj(2)", class = "L"),
                  set_prior("normal(0, .5)", class = "sd", group="subj", coef="Intercept" ),
                  set_prior("normal(0, .5)", class = "sd", group="item", coef="Intercept" ))

priors.corley.simple <-c(set_prior("normal(0, 2)", class = "Intercept"),
                         set_prior("normal(0, 2)", class = "b"),
                         set_prior("normal(0, .1)", class = "sd"))


d = mutate(d, c.boost.prime = case_when(condcode == .5 & boost == .5 ~ .5,
                                        condcode == -.5 & boost == .5 ~ -.5,
                                        TRUE ~ 0),
           c.noboost.prime = case_when(condcode == .5 & boost == -.5 ~ .5,
                                       condcode == -.5 & boost == -.5 ~ -.5,
                                       TRUE ~ 0),
           c.boost = case_when(boost == .5 ~ .5,
                               boost == -.5 ~ -.5),
           c.prime = case_when(condcode == .5 ~ .5,
                               condcode == -.5 ~ -.5))

d$boost = as.factor(d$boost)
d$condcode = as.factor(d$condcode)
contrasts(d$boost) = c(-.5, .5)
contrasts(d$condcode) = c(-.5, .5)


l.corley.cont = brm(respcode  ~ c.boost.prime + c.noboost.prime + c.boost +
                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
                      (c.boost.prime + c.noboost.prime + c.boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=2000,
                    prior = priors.corley, 
                    save_all_pars = T)

l.corley.cont.null = brm(respcode  ~ c.boost.prime  + c.boost +
                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
                      (c.boost.prime + c.noboost.prime + c.boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=2000,
                    prior = priors.corley, 
                    save_all_pars = T)

# l.corley.cont.simple = brm(respcode  ~ c.boost.prime + c.noboost.prime + c.boost +
#                       (c.boost.prime + c.noboost.prime  || subj) +
#                       (c.boost.prime + c.noboost.prime  || item),
#                     family="bernoulli",
#                     data=d,
#                     cores=2,
#                     chains=2,
#                     iter=8000,
#                     prior = priors.corley.simple, 
#                     save_all_pars = T)
# 
# l.corley.cont.simple.nested = brm(respcode  ~ c.boost.prime  + c.noboost.prime + c.boost +
#                              (c.boost.prime + c.noboost.prime  || subj) +
#                              (c.boost.prime + c.noboost.prime  || item),
#                            family="bernoulli",
#                            data=d,
#                            cores=2,
#                            chains=2,
#                            iter=8000,
#                            prior = priors.corley.simple, 
#                            save_all_pars = T)
# 
# l.corley.cont.simple.nested.null = brm(respcode  ~ c.boost.prime + c.boost +
#                                     (c.boost.prime + c.noboost.prime  || subj) +
#                                     (c.boost.prime + c.noboost.prime  || item),
#                                   family="bernoulli",
#                                   data=d,
#                                   cores=2,
#                                   chains=2,
#                                   iter=8000,
#                                   prior = priors.corley.simple, 
#                                   save_all_pars = T)


# 
# l.corley.cont.simple.nested = brm(respcode  ~ c.boost.prime + c.noboost.prime + c.boost +
#                              (c.boost.prime + c.noboost.prime  || subj) +
#                              (c.boost.prime + c.noboost.prime  || item),
#                            family="bernoulli",
#                            data=d,
#                            cores=2,
#                            chains=2,
#                            iter=8000,
#                            prior = priors.corley.simple, 
#                            save_all_pars = T)
# 
# l.corley.cont.noboostprime = brm(respcode  ~ c.boost.prime  + c.boost +
#                                    (c.boost.prime + c.noboost.prime + c.boost | subj) +
#                                    (c.boost.prime + c.noboost.prime + c.boost | item),
#                                  family="bernoulli",
#                                  data=d,
#                                  cores =2,
#                                  chains=2,
#                                  iter=8000,
#                                  prior = priors.corley, 
#                                  save_all_pars = T)
# 
# l.corley.cont.noboostprime.simple = brm(respcode  ~ c.boost.prime  + c.boost +
#                                    (c.boost.prime + c.noboost.prime  || subj) +
#                                    (c.boost.prime + c.noboost.prime  || item),
#                                  family="bernoulli",
#                                  data=d,
#                                  cores =2,
#                                  chains=2,
#                                  iter=8000,
#                                  prior = priors.corley.simple, 
#                                  save_all_pars = T)


# l.corley.cont.nullboostprime = brm(respcode  ~ c.noboost.prime  + c.boost +
#                                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
#                                      (c.boost.prime + c.noboost.prime + c.boost | item),
#                                    family="bernoulli",
#                                    data=d,
#                                    cores =2,
#                                    chains=2,
#                                    iter=8000,
#                                    prior = priors.corley, 
#                                    save_all_pars = T)
# 
# l.corley.cont.nullinteraction = brm(respcode  ~ c.boost  + c.prime +
#                                       (c.boost.prime + c.noboost.prime + c.boost | subj) +
#                                       (c.boost.prime + c.noboost.prime + c.boost | item),
#                                     family="bernoulli",
#                                     data=d,
#                                     cores =2,
#                                     chains=2,
#                                     iter=8000,
#                                     prior = priors.corley, 
#                                     save_all_pars = T)



#bf1 = bayes_factor(l.corley.cont, l.corley.cont.noboostprime)
#bf2 = bayes_factor(l.corley.cont, l.corley.cont.nullboostprime)
#bf3 = bayes_factor(l.corley.cont, l.corley.cont.nullinteraction)



# l.corley.null = brm(respcode  ~ condcode + boost +
#                       (condcode * boost | subj) +
#                       (condcode * boost | item),
#                     family="bernoulli",
#                     data=d,
#                     cores =2,
#                     chains=2,
#                     iter=2000,
#                     prior = priors.corley, 
#                     save_all_pars = T)

simulate.df.simple = function(ns, ni, beta1, beta2) {
  nsubj = ns
  # intercept:intercept intercept:condcode intercept:boost intercept:interaction
  # condcode:intercept condcode:condcode condcode:boost condcode:interaction
  # boost:intercept boost:condcode boost:boost boost:interaction
  # interaction:intercept interaction:condcode interaction:boost interaction:interaction
  subjs = tibble(subj.intercept = rnorm(nsubj, 0, 1), subj.b1 = rnorm(nsubj, 0, .04),
                 subj.b2 = rnorm(nsubj, 0, .04)) %>%
    mutate(subj = 1:n())
  
  nitems = ni
  items = tibble(item.intercept = rnorm(nitems, 0, 1), item.b1 = rnorm(nitems, 0, .04),
                 item.b2 = rnorm(nitems, 0, .04)) %>%
    mutate(item = 1:n())
  
  beta.int = rnorm(1, -.33, .1)
  beta.c.boost.prime = rnorm(1, beta1, .05)
  beta.c.noboost.prime = rnorm(1, beta2, .1)
  beta.c.boost = 0 #rnorm(1, 0, .1)
  
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


# simulate.df = function(ns, ni, beta1, beta2, lx) {
#   nsubj = ns
#   # intercept:intercept intercept:condcode intercept:boost intercept:interaction
#   # condcode:intercept condcode:condcode condcode:boost condcode:interaction
#   # boost:intercept boost:condcode boost:boost boost:interaction
#   # interaction:intercept interaction:condcode interaction:boost interaction:interaction
#   subjs = data.frame(MASS::mvrnorm(n=nsubj, mu=c(0, 0, 0, 0),
#                                    Sigma=matrix(sapply(c("Intercept", "c.boost.prime", "c.noboost.prime", "c.boost"),
#                                                        function(x) {VarCorr(lx)$subj$cov[,,x][,"Estimate"]}),
#                                                 ncol=4))) %>%
#     rename(subj.intercept = X1, subj.b1 = X2,
#            subj.b2 = X3, subj.b3 = X4) %>%
#     mutate(subj = 1:n())
# 
#   nitems = ni
#   items = data.frame(MASS::mvrnorm(n=nitems, mu=c(0, 0, 0, 0),
#                                    matrix(sapply(c("Intercept", "c.boost.prime", "c.noboost.prime", "c.boost"),
#                                                  function(x) {VarCorr(lx)$item$cov[,,x][,"Estimate"]}), ncol=4))) %>%
#     rename(item.intercept = X1, item.b1 = X2,
#            item.b2 = X3, item.b3 = X4) %>%
#     mutate(item = 1:n())
# 
#   beta.int = rnorm(1, -.33, .1)
#   beta.c.boost.prime = rnorm(1, beta1, .1)
#   beta.c.noboost.prime = rnorm(1, beta2, .1)
#   beta.c.boost = rnorm(1, 0, .1)
# 
#   newd = expand.grid(subj = (seq(1,ns)),
#                      item = (seq(1,ni))) %>%
#     mutate(rand1=runif(n()),
#            rand2 = runif(n()),
#            condcode = ifelse(rand1 > .5, .5, -.5),
#            boost = ifelse(rand2 > .5, .5, -.5),
#            c.boost.prime = case_when(condcode == .5 & boost == .5 ~ .5,
#                                      condcode == -.5 & boost == .5 ~ -.5,
#                                      TRUE ~ 0),
#            c.noboost.prime = case_when(condcode == .5 & boost == -.5 ~ .5,
#                                        condcode == -.5 & boost == -.5 ~ -.5,
#                                        TRUE ~ 0),
#            c.boost = case_when(boost == .5 ~ .5,
#                                boost == -.5 ~ -.5),
#            c.prime = case_when(condcode == .5 ~ .5,
#                                condcode == -.5 ~ -5)) %>%
#     sample_n(.60 * n()) %>%
#     left_join(subjs) %>%
#     left_join(items) %>%
#     mutate(fitted = inv.logit(beta.int +
#                                 (beta.c.boost.prime + subj.b1 + item.b1) * c.boost.prime +
#                                 (beta.c.noboost.prime + subj.b2 + item.b2) * c.noboost.prime +
#                                 (beta.c.boost + subj.b3 + item.b3) * c.boost +
#                                 subj.intercept +
#                                 item.intercept ),
#            respcode = as.integer(runif(n()) < fitted))
# 
#   return(newd)
# }


bf.new.null = 1
a = NULL
startpoint = 200
ni = 48
beta1 = .88
beta2 = .29

for (ns in c(800)) {
  for (it in seq(1, 3)) {
    newd_ = simulate.df.simple(ns, ni, beta1, beta2)
    for (curnum in seq(startpoint, ns, 100)) {
      if (curnum == startpoint | (bf.new.null > (1/6) & bf.new.null < 6)) {
        newd = filter(newd_, subj <= curnum)
        l.new = update(l.corley.cont, newdata=newd, cores=12,
                       chains=12, iter=curnum * 20, save_all_pars=T,
                       warmup = 1000)
        l.null = update(l.corley.cont.null, newdata=newd,
                        cores=12, chains=12, iter=curnum * 20, save_all_pars=T,
                        warmup = 1000)
        boost.bigger = mean(fixef(l.new, summary = F)[, "c.boost.prime"] > 
                              fixef(l.new, summary = F)[, "c.noboost.prime"])
        bf.new.null = bayes_factor(l.new, l.null)[1]
        a = cbind(curnum, ni, it, bf.new.null, boost.bigger)
        print(a)
	write.table( a, file="results_corley_20201126.csv", append = T, sep=",", row.names=F, col.names=F)
        write.csv(a, file="results_gensimple_testfull_20201124_ucsb_xxx.csv", append=T)
      }
    }
  }
}




# 
# bf.new.null = 1
# a = NULL
# startpoint = 200
# for (ns in c(800)) {
#   for (it in seq(1, 1000)) {
#     newd_ = simulate.df.simple(ns, ni, .73, .27)
#     for (curnum in seq(startpoint, ns, 100)) {
#       if (curnum == startpoint | (bf.new.null > (1/6) & bf.new.null < 6)) {
#         ni = 48
#         newd = filter(newd_, subj <= curnum)
#         l.new = update(l.corley.cont.simple.nested, newdata=newd, cores=16,
#                        chains=16, iter=curnum * 20, save_all_pars=T)
#         l.null = update(l.corley.cont.simple.nested.null, newdata=newd,
#                         cores=16, chains=16, iter=curnum * 20, save_all_pars=T)
#         boost.bigger = mean(fixef(l.new, summary = F)[, "c.boost.prime"] > 
#                               fixef(l.new, summary = F)[, "c.noboost.prime"])
#         bf.new.null = bayes_factor(l.new, l.null)[1]
#         a = rbind(a, cbind(curnum, ni, it, bf.new.null, boost.bigger))
#         print(a)
#         write.csv(a, file="results_simple_2.csv")
#       }
#     }
#   }
# }
# 
# 
# 
