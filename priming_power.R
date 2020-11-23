library(tidyverse)
library(brms)
library(lme4)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) { exp(x)/(1+exp(x))}
###########

# todo: use corley 322, 324

d = read_csv("~/db/ToUsePrimingPapers/Analysis/raw_data_merged_with_master.csv") 

# get a df with LB (-.5 for no boost, .5 for boost),
# condcode: -.5 for less frequent construction, .5 for more frequent
# respcode: 0 for less frequent construction, 1 for more frequent
# Paper.ID: unique paper id
# Study.ID: unique experiment id within paper, converted to numeric
ha = d %>%
  group_by(StudyID, subj) %>%
  filter(Dep.measure %in% c("HA"), Target.task == "WSC", L1 == "L1L1") %>%
  ungroup() %>% 
  mutate(StudyID = as.numeric(as.factor(StudyID))) %>%
  dplyr::select(subj, item, LB, respcode, condcode, Paper.ID, StudyID) %>%
  group_by(Paper.ID, StudyID) %>%
  mutate(numsubjs = length(unique(subj)),
         subj = paste(Paper.ID, StudyID, subj),
         item = paste(Paper.ID, StudyID, item)) 

do = d %>%
  group_by(StudyID, subj) %>%
  filter(Dep.measure %in% c("DO", "PO"), Target.task == "WSC", L1 == "L1L1") %>%
  ungroup() %>% 
  mutate(StudyID = as.numeric(as.factor(StudyID))) %>%
  dplyr::select(subj, item, LB, respcode, condcode, Paper.ID, StudyID) %>%
  group_by(Paper.ID, StudyID) %>%
  mutate(numsubjs = length(unique(subj)),
         subj = paste(Paper.ID, StudyID, subj),
         item = paste(Paper.ID, StudyID, item)) 

group_by(do, Paper.ID) %>%
  mutate(n.lb=length(unique(LB))) %>%
  filter(n.lb > 1) %>%
  group_by(Paper.ID, condcode, LB) %>% summarise(m=mean(respcode)) 

e.ha = glmer(respcode ~ condcode  + (condcode |subj) +
               (condcode  | item) +
               (condcode |Paper.ID),
             data=ha,
             family='binomial')

e.do = glmer(respcode ~ condcode  * LB + (condcode + LB |subj) +
               (condcode + LB | item) +
               (condcode  + LB|Paper.ID),
             data=do,
             family='binomial')


priors.do <-c(set_prior("normal(0, 2)", class = "Intercept"),
           set_prior("normal(0, 2)", class = "b"),
           set_prior("normal(0, .5)", class = "sd"),
           set_prior("lkj(2)", class = "L"))

do$boost = do$LB
l.do = brm(respcode  ~ condcode * boost +
             (condcode * boost | subj) +
             (condcode * boost | item) +
             (condcode * boost | Paper.ID), 
        family="bernoulli",
        data=do,
        cores =2,
        chains=2,
        iter=2000,
        prior = priors.do)
################

l.logit = load("~/db/ToUsePrimingPapers/public_data/current_lmer.rda")
summary(current_lmer)
# 
m = read_csv("meta_analysis_with_predictions.csv")
filter(m, grepl("Corley", ExperimentID))

 filter(m, X.2 == 323) %>% dplyr::select(predicted, Lexical.boost., Dep.measure)
 filter(m, X.2 == 325) %>% dplyr::select(predicted, Lexical.boost., Dep.measure)
 
# Pickering, depvar is DO
filter(m, X.2 == 444) %>% dplyr::select(predicted, Lexical.boost., Dep.measure)
filter(m, X.2 == 446) %>% dplyr::select(predicted, Lexical.boost., Dep.measure)
filter(m, X.2 == 448) %>% dplyr::select(predicted, Lexical.boost., Dep.measure)

# Scheepers
filter(m, X.2 == 469) %>% dplyr::select(predicted, Lexical.boost.)
filter(m, X.2 == 471) %>% dplyr::select(predicted, Lexical.boost.)

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

priors.corley <-c(set_prior("normal(0, 2)", class = "Intercept"),
              set_prior("normal(0, 2)", class = "b"),
              set_prior("normal(0, .1)", class = "sd"),
              set_prior("lkj(2)", class = "L"))

lmer.corley = glmer(respcode  ~ condcode * boost +
                     (condcode * boost | subj) +
                     (condcode * boost | item),
                   family="binomial",
                   data=d)

lmer.corley.null = glmer(respcode  ~ condcode + boost +
                     (condcode * boost | subj) +
                     (condcode * boost | item),
                   family="binomial",
                   data=d)

anova(lmer.corley, lmer.corley.null)

l.corley = brm(respcode  ~ condcode * boost +
              (condcode * boost | subj) +
              (condcode * boost | item),
            family="bernoulli",
            data=d,
            cores =2,
            chains=2,
            iter=2000,
            prior = priors.corley,
            save_all_pars = T)

l.corley.null = brm(respcode  ~ condcode + boost +
                 (condcode * boost | subj) +
                 (condcode * boost | item),
               family="bernoulli",
               data=d,
               cores =2,
               chains=2,
               iter=2000,
               prior = priors.corley, 
               save_all_pars = T)

d = mutate(d, c.boost.prime = case_when(condcode == .5 & boost == .5 ~ 1,
                                      condcode == -.5 & boost == .5 ~ -1,
                                      TRUE ~ 0),
           c.noboost.prime = case_when(condcode == .5 & boost == -.5 ~ 1,
                                     condcode == -.5 & boost == -.5 ~ -1,
                                     TRUE ~ 0),
           c.boost = case_when(boost == .5 ~ 1,
                               boost == -.5 ~ -1),
           c.prime = case_when(condcode == .5 ~ 1,
                               condcode == -.5 ~ -1))

l.corley.cont = brm(respcode  ~ c.boost.prime + c.noboost.prime + c.boost +
                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
                      (c.boost.prime + c.noboost.prime + c.boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=8000,
                    prior = priors.corley, 
                    save_all_pars = T)

l.corley.cont.noboostprime = brm(respcode  ~ c.boost.prime  + c.boost +
                      (c.boost.prime + c.noboost.prime + c.boost | subj) +
                      (c.boost.prime + c.noboost.prime + c.boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=8000,
                    prior = priors.corley, 
                    save_all_pars = T)

l.corley.cont.nullboostprime = brm(respcode  ~ c.noboost.prime  + c.boost +
                                   (c.boost.prime + c.noboost.prime + c.boost | subj) +
                                   (c.boost.prime + c.noboost.prime + c.boost | item),
                                 family="bernoulli",
                                 data=d,
                                 cores =2,
                                 chains=2,
                                 iter=8000,
                                 prior = priors.corley, 
                                 save_all_pars = T)

l.corley.cont.nullinteraction = brm(respcode  ~ c.boost  + c.prime +
                                     (c.boost.prime + c.noboost.prime + c.boost | subj) +
                                     (c.boost.prime + c.noboost.prime + c.boost | item),
                                   family="bernoulli",
                                   data=d,
                                   cores =2,
                                   chains=2,
                                   iter=8000,
                                   prior = priors.corley, 
                                   save_all_pars = T)



bf1 = bayes_factor(l.corley.cont, l.corley.cont.noboostprime)
bf2 = bayes_factor(l.corley.cont, l.corley.cont.nullboostprime)
bf3 = bayes_factor(l.corley.cont, l.corley.cont.nullinteraction)



l.corley.null = brm(respcode  ~ condcode + boost +
                      (condcode * boost | subj) +
                      (condcode * boost | item),
                    family="bernoulli",
                    data=d,
                    cores =2,
                    chains=2,
                    iter=2000,
                    prior = priors.corley, 
                    save_all_pars = T)

hypothesis(l.corley, "condcode > 0")
hypothesis(l.corley, "condcode:boost > 0")
hypothesis(l.corley, "Intercept - .5 * condcode - .5 * boost + .25 * condcode:boost > 0")

pp_check(l.corley)
pp_check(l.corley, type="scatter_avg_grouped", group="subj")

l.corley.noboost = brm(respcode  ~ condcode  +
                 (condcode  | subj) +
                 (condcode  | item),
               family="bernoulli",
               data=filter(d, boost == -.5),
               cores =2,
               chains=2,
               iter=2000,
               prior = priors.corley)

l.corley.between = brm(respcode  ~ condcode * boost +
                         (condcode  | subj) +
                         (condcode * boost  | item),
                       family="bernoulli",
                       data=d,
                       cores =2,
                       chains=2,
                       iter=2000,
                       prior = priors.corley)


 predict.grid = expand.grid(condcode = c(.5, -.5), boost = c(.5, -.5))
 posterior.corley = cbind(predict.grid, data.frame(t(posterior_predict(l.corley, 
                                                                       newdata=predict.grid, re_formula=NA))))
# 
 gather(posterior.corley, variable, value, -condcode, -boost) %>%
   group_by(condcode, boost) %>%
   summarise(m=mean(value)) 
 
lm.corley = glm(data=d,family="binomial", respcode  ~ condcode * boost)

logit = function(p) {log(p) - log(1-p)}

# compute from paper
boost.prime = logit(.38/(.38 + .29)) - logit(.22/(.22 + .47)) 
noboost.prime = logit(.29/(.35 + .29)) - logit(.25/(.25 + .40)) 
mean.prime = mean(c(boost.prime, noboost.prime))
boost.prime
noboost.prime
noboost.prime
0 + mean.prime * .5 - .25 * boosteffect

0 + mean.prime * -.5 + .25 *boosteffect

boosteffect = logit(.38/(.38 + .29)) - logit(.22/(.22 + .47)) - (logit(.29/(.35 + .29)) - logit(.25/(.25 + .40)) )

-.56 + 1.38 * .5


cond = .4
interact = .4
boost.prime = inv.logit(.5 * cond + interact * .25)
boost.noprime = inv.logit(-.5 * cond - interact * .25)
noboost.prime = inv.logit(.5 * cond - interact * .25)
noboost.noprime = inv.logit(-.5 * cond + interact * .25)
print(c(boost.prime, boost.noprime, noboost.prime, noboost.noprime))

######################### Pickering and Branigan/Corley and Scheepers lmer power analysis
lmer.simulate.pb = function(ns, ni, niter, lx) {
  powers = NULL
  for (i in 1:niter) {
    nsubj = ns
    # intercept:intercept intercept:condcode intercept:boost intercept:interaction
    # condcode:intercept condcode:condcode condcode:boost condcode:interaction
    # boost:intercept boost:condcode boost:boost boost:interaction
    # interaction:intercept interaction:condcode interaction:boost interaction:interaction
    subjs = data.frame(MASS::mvrnorm(n=nsubj, mu=c(0, 0, 0, 0),
                               Sigma=matrix(sapply(c("Intercept", "condcode", "boost", "condcode:boost"),
                                                   function(x) {VarCorr(lx)$subj$cov[,,x][,"Estimate"]}),
                                            ncol=4))) %>%
      rename(subj.intercept = X1, subj.condcode = X2,
             subj.boost = X3, subj.interaction = X4) %>%
      mutate(subj = 1:n())
    
    nitems = ni
    items = data.frame(MASS::mvrnorm(n=nitems, mu=c(0, 0, 0, 0),
                                     matrix(sapply(c("Intercept", "condcode", "boost", "condcode:boost"),
                                                   function(x) {VarCorr(lx)$item$cov[,,x][,"Estimate"]}), ncol=4))) %>%
      rename(item.intercept = X1, item.condcode = X2,
             item.boost = X3, item.interaction = X4) %>%
      mutate(item = 1:n())
    
    beta.int = -.45
    beta.cond = .40
    beta.boost = 0
    beta.interaction = .40
    
    newd = expand.grid(subj = (seq(1,ns)), 
                       item = (seq(1,ni))) %>%
      mutate(rand1=runif(n()),
             rand2 = runif(n()),
             condcode = ifelse(rand1 > .5, .5, -.5),
             boost = ifelse(rand2 > .5, .5, -.5)) %>%
      sample_n(.60 * n()) %>%
      left_join(subjs) %>%
      left_join(items) %>%
      mutate(fitted = inv.logit(beta.int +
                                  condcode * beta.cond + 
                                  boost * beta.boost + 
                                  condcode * boost * beta.interaction + 
                                  subj.intercept +
                                  item.intercept + 
                                  subj.condcode * condcode + 
                                  item.condcode * condcode + 
                                  subj.boost * boost + 
                                  item.boost * boost + 
                                  subj.interaction * boost * condcode +
                                  item.interaction * boost * condcode),
             respcode = as.integer(runif(n()) < fitted))
    
    l.new = update(lx, newdata=newd, cores=2, chains=2, iter=1000)
    x = posterior_samples(l.new, "^b")

    prime.noboost = (x$b_Intercept + .5 * x$b_condcode - .5 * x$b_boost  - (.5 * .5) * x$`b_condcode:boost`)
    noprime.noboost = (x$b_Intercept - .5 * x$b_condcode - .5 * x$b_boost  + (.5 * .5) * x$`b_condcode:boost`)
    prime.boost = (x$b_Intercept + .5 * x$b_condcode + .5 * x$b_boost  + (.5 * .5) * x$`b_condcode:boost`)
    noprime.boost = (x$b_Intercept - .5 * x$b_condcode + .5 * x$b_boost  - (.5 * .5) * x$`b_condcode:boost`)

    df = tibble(interaction=quantile(x$`b_condcode:boost`, c(.025, .5, .975)),
                boost.prime = quantile(prime.boost - noprime.boost, c(.025, .5, .975)),
                noboost.prime = quantile(prime.noboost - noprime.noboost, c(.025, .5, .975)),
                quantile = c(2.5, 50, 97.5)) %>%
      gather(variable, value, -quantile) %>%
      mutate(iter=i, subj = ns, i = ni)
    powers = rbind(powers, df)
  }
  return(powers)
}

lmer.simulate.pb.diff.subjs = function(ns, ni, niter, lx, lx.between) {
  powers = NULL
  for (i in 1:niter) {
    nsubj = ns
    # intercept:intercept intercept:condcode intercept:boost intercept:interaction
    # condcode:intercept condcode:condcode condcode:boost condcode:interaction
    # boost:intercept boost:condcode boost:boost boost:interaction
    # interaction:intercept interaction:condcode interaction:boost interaction:interaction
    subjs = data.frame(MASS::mvrnorm(n=nsubj, mu=c(0, 0),
                                     Sigma=matrix(c(.6^2, 0, 0, .08^2 ), ncol=2))
                                                 ) %>%
      rename(subj.intercept = X1, subj.condcode = X2
             ) %>%
      mutate(subj = 1:n(),
             boost = ifelse(subj %% 2 == 0, .5, -.5)) # make boost property of subject
    
    nitems = ni
    items = data.frame(MASS::mvrnorm(n=nitems, mu=c(0, 0, 0, 0),
                                     matrix(sapply(c("Intercept", "condcode", "boost", "condcode:boost"),
                                                   function(x) {VarCorr(lx)$item$cov[,,x][,"Estimate"]}), ncol=4))) %>%
      rename(item.intercept = X1, item.condcode = X2,
             item.boost = X3, item.interaction = X4) %>%
      mutate(item = 1:n())
    
    beta.int = -.45
    beta.cond = .40
    beta.boost = 0
    beta.interaction = .40
    
    newd = expand.grid(subj = (seq(1,ns)), 
                       item = (seq(1,ni))) %>%
      mutate(rand1=runif(n()),
             rand2 = runif(n()),
             condcode = ifelse(rand1 > .5, .5, -.5)) %>%
      sample_n(.60 * n()) %>%
      left_join(subjs) %>%
      left_join(items) %>%
      mutate(fitted = inv.logit(beta.int +
                                  condcode * beta.cond + 
                                  boost * beta.boost + 
                                  condcode * boost * beta.interaction + 
                                  subj.intercept +
                                  item.intercept + 
                                  subj.condcode * condcode + 
                                  item.condcode * condcode + 
                                  item.boost * boost + 
                                  item.interaction * boost * condcode),
             respcode = as.integer(runif(n()) < fitted))
    
    l.new = update(lx.between, newdata=newd, cores=2, chains=2, iter=1000)
    x = posterior_samples(l.new, "^b")
    
    prime.noboost = (x$b_Intercept + .5 * x$b_condcode - .5 * x$b_boost  - (.5 * .5) * x$`b_condcode:boost`)
    noprime.noboost = (x$b_Intercept - .5 * x$b_condcode - .5 * x$b_boost  + (.5 * .5) * x$`b_condcode:boost`)
    prime.boost = (x$b_Intercept + .5 * x$b_condcode + .5 * x$b_boost  + (.5 * .5) * x$`b_condcode:boost`)
    noprime.boost = (x$b_Intercept - .5 * x$b_condcode + .5 * x$b_boost  - (.5 * .5) * x$`b_condcode:boost`)
    
    df = tibble(interaction=quantile(x$`b_condcode:boost`, c(.025, .5, .975)),
                boost.prime = quantile(prime.boost - noprime.boost, c(.025, .5, .975)),
                noboost.prime = quantile(prime.noboost - noprime.noboost, c(.025, .5, .975)),
                quantile = c(2.5, 50, 97.5)) %>%
      gather(variable, value, -quantile) %>%
      mutate(iter=i, subj = ns, i = ni)
    powers = rbind(powers, df)
  }
  return(powers)
}


a = NULL

ns = 1000
ni = 32
x = lmer.simulate.pb(ns, ni, 1, l.corley)

for (ns in c(200, 400, 600, 800, 1200)) {
  for (ni in c(48)) {
    a = rbind(a, lmer.simulate.pb(ns, ni, 5, l.corley))
  }
}

b = lmer.simulate.pb.diff.subjs(600, 32, 4, lx, l.corley.between)
# for (ns in c(500, 600)) {
#   for (ni in c(48)) {
#     a = rbind(a, cbind(ns, ni, lmer.simulate.pb(ns, ni, 10, lx)))
#   }
# }

filter(a, variable == "interaction", quantile == 2.5) %>%
  group_by(subj, i) %>%
  summarise(m = mean(value > 0))

filter(a, variable == "boost.prime", quantile == 2.5) %>%
  group_by(subj, i) %>%
  summarise(m = mean(value > 0))

filter(a, variable == "noboost.prime", quantile == 2.5) %>%
  group_by(subj, i) %>%
  summarise(m = mean(value > 0))


filter(a, variable == "noboost.prime", quantile == 50) %>%
  group_by(subj, i) %>%
  summarise(m = mean(value > 0))

filter(b, variable == "noboost.prime", quantile == 2.5) %>%
  group_by(subj, i) %>%
  summarise(m = mean(value > 0))

#lmer.simulate.pb(300, 32, 4)


#############################################
# Scheepers et al. (2003)

d1 = read_csv("~/db/ToUsePrimingPapers/all_data/469.csv") %>% 
  mutate(exp = 1)
d2 = read_csv("~/db/ToUsePrimingPapers/all_data/471.csv") %>% 
  mutate(exp = 2)
d = bind_rows(d1, d2)
d$subj = paste(d$exp, d$subj)
d$item = paste(d$exp, d$item)

d.sum = group_by(d, condcode) %>% summarise(m=mean(respcode),
                                                        effect=logit(m)) 
d.sum  %>% arrange(condcode) %>%
  mutate(x = effect - lag(effect))

l.2 = brm(respcode  ~ condcode  + (condcode  | subj) + (condcode  | item), 
        family="bernoulli",
        data=d)

x = posterior_samples(l.2, "^b")

prime = (x$b_Intercept + .5 * x$b_condcode )
noprime = (x$b_Intercept - .5 * x$b_condcode)

hist(prime - noprime, breaks=50)
mean(prime - noprime)


newd = expand.grid(subj = paste("newsubj", seq(1,100)), 
                   item = paste("newitem", seq(1,24))) %>%
  mutate(n=runif(n()),
         condcode = ifelse(n > .5, .5, -.5))
p <- fitted(l.2,
             sample_new_levels = "gaussian",
             newdata=newd,
             allow_new_levels = T,
             summary = F)
newd = cbind(newd, data.frame(t(p))[, seq(1, 100)])

lmer.simulate = function(ns, ni, niter) {
  powers = NULL
  for (i in 1:niter) {
    nsubj = ns
    subjs = data.frame(mvrnorm(n=nsubj, mu=c(0, 0),
                               Sigma=matrix(c(.88^2, -.38^2, -.38^2, .51^2), ncol=2))) %>%
      rename(subj.intercept = X1, subj.condcode = X2) %>%
      mutate(subj = 1:n())
    
    nitems = ni
    items = data.frame(mvrnorm(n=nitems, mu=c(0, 0),
                               Sigma=matrix(c(2.33^2, -.33^2, -.33^2, 1.03^2), ncol=2))) %>%
      rename(item.intercept = X1, item.condcode = X2) %>%
      mutate(item = 1:n())
    
    beta.int = 1.17
    beta.cond = .33
    newd = expand.grid(subj = (seq(1,ns)), 
                       item = (seq(1,ni))) %>%
      mutate(rand=runif(n()),
             condcode = ifelse(rand > .5, .5, -.5)) %>%
      sample_n(.49 * n()) %>%
      left_join(subjs) %>%
      left_join(items) %>%
      mutate(fitted = inv.logit(beta.int + condcode * beta.cond + 
                      subj.intercept + item.intercept + 
                      subj.condcode * condcode + 
                      item.condcode * condcode),
             respcode = as.integer(runif(n()) < fitted)) # do exclusions
    
    group_by(newd, condcode) %>%
      summarise(m=mean(respcode))
    
    lx = lmer(data=newd,
              respcode ~ condcode + (condcode | subj) + (condcode | item))
    powers = append(NULL, as.numeric(summary(lx)$coef[, "t value"]["condcode"]) > 1.96)
  }
  print(mean(powers))
}

lmer.simulate(200, 48, 30)
################

## Scheepers arithmetic 2011 Experiment 2
# 1 high
# 2 low
d = read_tsv("arithmetic/Scheepers_et_al_2011/Exp2/Scheepers_et_al_2011_Exp2.txt")

exclude = mean(d$PrimeCond %in% c(1, 2) == F | d$TargetAttach %in% c(1, 2) == F | d$PrimeCorrect == F)
print(exclude)


d = filter(d, TargetAttach %in% c(1, 2),
         PrimeCorrect == 1,
         PrimeCond %in% c(1, 2)) %>%
  mutate(condcode = ifelse(PrimeCond == 1, .5, -.5),
         respcode = ifelse(TargetAttach == 1, 1, 0)) %>%
  rename(subj = Subj,
         item = Item)

d.sum = group_by(d, condcode) %>% summarise(m=mean(respcode),
                                            effect=logit(m)) 
d.sum  %>% arrange(condcode) %>%
  mutate(x = effect - lag(effect))

l.3 = brm(respcode  ~ condcode  + (condcode  | subj) + (condcode  | item), 
          family="bernoulli",
          data=d)

x = posterior_samples(l.3, "^b")

prime = (x$b_Intercept + .5 * x$b_condcode )
noprime = (x$b_Intercept - .5 * x$b_condcode)

hist(prime - noprime, breaks=50)
mean(prime - noprime)



