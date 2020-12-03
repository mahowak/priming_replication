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

simulate.df.simple = function(ns, ni, beta1, beta2) {
  nsubj = ns
  # intercept:intercept intercept:condcode intercept:boost intercept:interaction
  # condcode:intercept condcode:condcode condcode:boost condcode:interaction
  # boost:intercept boost:condcode boost:boost boost:interaction
  # interaction:intercept interaction:condcode interaction:boost interaction:interaction
  subjs = tibble(subj.intercept = rnorm(nsubj, 0, 1.5), subj.b1 = rnorm(nsubj, 0, .04),
                 subj.b2 = rnorm(nsubj, 0, .04)) %>%
    mutate(subj = 1:n())
  
  nitems = ni
  items = tibble(item.intercept = rnorm(nitems, 0, 1.5), item.b1 = rnorm(nitems, 0, .04),
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
#     }
#   }
# }
# 
# 
# 

beta1 = .88
beta2 = .29
group_by(d, subj) %>% 
  summarise(m=mean(respcode)) %>%
  ggplot(data=., aes(x=m)) + geom_histogram() + xlim(-0.01, 1.01)

simulate.df.simple(54, 48, beta1, beta2) %>%
  group_by(subj) %>%
  summarise(m=mean(respcode)) %>%
  ggplot(data=., aes(x=m)) + geom_histogram() + xlim(-0.01, 1.01)

group_by(d, item) %>% 
  summarise(m=mean(respcode)) %>%
  ggplot(data=., aes(x=m)) + geom_histogram() + xlim(-0.01, 1.01)


simulate.df.simple(54, 48, beta1, beta2) %>%
  group_by(item) %>%
  summarise(m=mean(respcode)) %>%
  ggplot(data=., aes(x=m)) + geom_histogram() + xlim(-0.01, 1.01)

get_means <- function(d) {group_by(d, condcode, boost) %>%
  summarise(m=mean(respcode))}

simulate.mean = function(beta1, beta2) {
  return(get_means(  simulate.df.simple(54, 48, beta1, beta2)))
}

a = NULL
for (i in 1:100) {
  a = rbind(a, cbind(i, simulate.mean(beta1, beta2)))
}

a = data.frame(a)

d.s = group_by(d, condcode, boost) %>%
  summarise(m=mean(respcode)) %>%
  mutate(cc = ifelse(condcode == -.5, "no prime", "prime"),
         bb = ifelse(boost == .5, "with boost", "no boost"))

a = mutate(a, 
           cc = ifelse(condcode == -.5, "no prime", "prime"),
           bb = ifelse(boost == .5, "with boost", "no boost"))
ggplot(d.s, aes(x=cc, y=m)) + facet_grid(. ~ bb) +
  geom_point(size=3) + 
  ylim(0, 1) + 
  theme_bw() +
  geom_jitter(data=a, aes(x=cc, y=m), alpha=.1, width=.2) + 
  xlab("") + ylab("proportion choosing DO") + 
  theme_bw(18)




#  ggplot(data=., aes(x=m)) + geom_histogram() + xlim(-0.01, 1.01)

simulate.df.simple(1000, 48, beta1, beta2) %>%
  group_by(subj) %>%
  summarise(m=mean(respcode)) %>%
  ggplot(data=., aes(x=m)) + geom_histogram(fill='gray') + 
  geom_histogram(data= group_by(d, subj) %>% 
                   summarise(m=mean(respcode)), aes(x=m), fill= 'blue') + 
  theme_bw() + 
  xlab("proportion subject is DO")


simulate.df.simple(54, 1000, beta1, beta2) %>%
  group_by(item) %>%
  summarise(m=mean(respcode)) %>%
  ggplot(data=., aes(x=m)) + geom_histogram(fill='gray') +
  geom_histogram(data= group_by(d, item) %>% 
                   summarise(m=mean(respcode)), aes(x=m), fill= 'blue') + 
  theme_bw() + xlab("proportion item is DO")


##################
b = NULL
for (i in 1:8) {
b = rbind(b, simulate.df.simple(54, 32, beta1, beta2) %>%
  group_by(subj) %>%
  summarise(m=mean(respcode)) %>%
    mutate(iter=paste("simulation:", as.character(i)))
)
}
b = bind_rows(b, group_by(d, subj) %>% 
                summarise(m=mean(respcode)) %>%
                mutate(iter="real"))
b[is.na(b$iter), "iter"] = "real"
ggplot(b, aes(x=m)) + geom_histogram() + 
  facet_wrap(~iter, ncol=3)
#############
b = NULL
for (i in 1:8) {
  b = rbind(b, simulate.df.simple(54, 32, beta1, beta2) %>%
              group_by(item) %>%
              summarise(m=mean(respcode)) %>%
              mutate(iter=paste("simulation:", as.character(i)))
  )
}
b = bind_rows(b, group_by(d, item) %>% 
                summarise(m=mean(respcode)) %>%
                mutate(iter="real"))
b[is.na(b$iter), "iter"] = "real"
ggplot(b, aes(x=m)) + geom_histogram() + 
  facet_wrap(~iter, ncol=3)

#######################
# subjs, simulate and plot histograms for subj effects
b = NULL
for (i in 1:8) {
  b = rbind(b, simulate.df.simple(54, 32, beta1, beta2) %>%
              group_by(subj, condcode) %>%
              summarise(m=mean(respcode)) %>%
              spread(condcode, m) %>%
              mutate(m = logit(`0.5`) - logit(`-0.5`)) %>%
              mutate(iter=paste("simulation", i),
                     m = ifelse(is.na(m), 0, m),
                     m = ifelse(m > 100, 3, m),
                     m = ifelse(m < -100, -3, m)))
}
b = bind_rows(b, group_by(d, subj, condcode) %>%
                summarise(m=mean(respcode)) %>%
                spread(condcode, m) %>%
                mutate(m = logit(`0.5`) - logit(`-0.5`)) %>%
                mutate(iter="real",
                       m = ifelse(is.na(m), 0, m),
                       m = ifelse(m > 100, 3, m),
                       m = ifelse(m < -100, -3, m)))
b[is.na(b$iter), "iter"] = "real"
ggplot(b, aes(x=m)) + geom_histogram() + 
  facet_wrap(~iter, ncol=3) + 
  xlim(-3, 3)

#######################
# items, simulate and plot histograms for priming effect
b = NULL
for (i in 1:8) {
  b = rbind(b, simulate.df.simple(54, 32, beta1, beta2) %>%
              group_by(item, condcode) %>%
              summarise(m=mean(respcode)) %>%
              spread(condcode, m) %>%
              mutate(m = logit(`0.5`) - logit(`-0.5`)) %>%
              mutate(iter=paste("simulation", i),
                     m = ifelse(is.na(m), 0, m),
                     m = ifelse(m > 100, 3, m),
                     m = ifelse(m < -100, -3, m)))
}
b = bind_rows(b, group_by(d, item, condcode) %>%
                summarise(m=mean(respcode)) %>%
                spread(condcode, m) %>%
                mutate(m = logit(`0.5`) - logit(`-0.5`)) %>%
                mutate(iter="real",
                       m = ifelse(is.na(m), 0, m),
                       m = ifelse(m > 100, 3, m),
                       m = ifelse(m < -100, -3, m)))
b[is.na(b$iter), "iter"] = "real"
ggplot(b, aes(x=m)) + geom_histogram() + 
  facet_wrap(~iter, ncol=3) + 
  xlim(-3, 3)
