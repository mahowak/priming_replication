library(tidyverse)
library(brms)

######### read in coded data
d = read_csv("pilot1_20201230/coded_results_exp2.csv") %>%
  filter(subject != "c1d9b98c6eff7b8cef5b640d712cec44") %>%
  select(-item) %>%
  mutate(type = ifelse(grepl("target", item_type), "target", "prime"),
         item = gsub("prime", "", item_type),
         item = gsub("target", "", item))

primes = filter(d, grepl("high", condition) | grepl("low", condition)) %>%
  mutate(correct = (grepl("high", condition) & code == "H") | (grepl("low", condition) & code == "L")) %>%
  rename(prime.cond = condition) %>%
  select(subject, item, correct, prime.cond)

targets = filter(d, type == "target") %>%
  left_join(primes)

group_by(d, type, condition, item, prime) %>%
  filter(type == "target") %>%
  summarise(mean.do = mean(code == "H"),
            mean.po = mean(code == "L"),
            mean.other = mean(code != "H" & code != "L")) %>%
  arrange(-mean.other)

group_by(targets, type, prime.cond) %>%
  filter(type == "target") %>%
  summarise(mean.ha = mean(code == "H"),
            mean.la = mean(code == "L"),
            mean.other = mean(code != "H" & code != "L")) %>%
  arrange(-mean.other)

d.analyze = filter(targets, correct == T,
                   code == "H" | code == "L")


##############################################
d.analyze = mutate(d.analyze,
                   respcode = ifelse(code == "H", 1, 0),
                   prime = ifelse(grepl("high", prime.cond), .5, -.5))
                   
priors <-c(set_prior("normal(0, .5)", class = "Intercept"),
           set_prior("normal(0, .5)", class = "b"),
           set_prior("normal(0, .05)", class = "sd"),
           set_prior("lkj(2)", class = "L"),
           set_prior("normal(0, .5)", class = "sd", group="subject", coef="Intercept" ),
           set_prior("normal(0, .5)", class = "sd", group="item", coef="Intercept" ))

less.informative.priors <-c(set_prior("normal(0, 2)", class = "Intercept"),
                            set_prior("normal(0, 2)", class = "b"),
                            set_prior("normal(0, 2)", class = "sd"),
                            set_prior("lkj(2)", class = "L"),
                            set_prior("normal(0, 2)", class = "sd", group="subj", coef="Intercept" ),
                            set_prior("normal(0, 2)", class = "sd", group="item", coef="Intercept" ))

priors.null <-c(set_prior("normal(0, .5)", class = "Intercept"),
           set_prior("normal(0, .05)", class = "sd"),
           set_prior("lkj(2)", class = "L"),
           set_prior("normal(0, .5)", class = "sd", group="subject", coef="Intercept" ),
           set_prior("normal(0, .5)", class = "sd", group="item", coef="Intercept" ))

less.informative.priors.null <-c(set_prior("normal(0, 2)", class = "Intercept"),
                            set_prior("normal(0, 2)", class = "sd"),
                            set_prior("lkj(2)", class = "L"),
                            set_prior("normal(0, 2)", class = "sd", group="subj", coef="Intercept" ),
                            set_prior("normal(0, 2)", class = "sd", group="item", coef="Intercept" ))

full.model.exp2 = brm(respcode ~ prime + 
                   (prime | subject) + 
                   (prime | item),
                 prior = priors,
                 data = d.analyze, 
                 family = 'bernoulli',
                 chains=4, cores=4,
                 save_all_pars = T,
                 iter=30000)

null.model.1.exp2 = brm(respcode ~ 1  + 
                     (prime | subject) + 
                     (prime | item),
                   prior = priors.null,
                   data = d.analyze, 
                   family = 'bernoulli',
                   chains=4, cores=4,
                   save_all_pars = T,
                   iter=30000)


bf1.exp2 = bayes_factor(full.model.exp2, null.model.1.exp2)

