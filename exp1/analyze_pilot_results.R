library(tidyverse)
library(brms)

######### read in coded data
d = read_csv("pilot1_20201230/coded_results_exp1.csv") %>%
  filter(subject != "c1d9b98c6eff7b8cef5b640d712cec44")

group_by(d, type, condition) %>%
  summarise(mean.do = mean(code == "D"),
            mean.po = mean(code == "P"),
            mean.other = mean(code != "P" & code != "D"))

primes = filter(d, type == "prime") %>%
  mutate(correct = (grepl("DO", condition) & code == "D") | (grepl("PO", condition) & code == "P")) %>%
  rename(prime.cond = condition) %>%
  select(subject, item, correct, prime.cond)

targets = filter(d, type == "target") %>%
  left_join(primes)

group_by(d, type, condition) %>%
  summarise(mean.do = mean(code == "D"),
            mean.po = mean(code == "P"),
            mean.other = mean(code != "P" & code != "D"))


group_by(d, type, condition, item, prime, target_verb) %>%
  filter(type == "target") %>%
  summarise(mean.do = mean(code == "D"),
            mean.po = mean(code == "P"),
            mean.other = mean(code != "P" & code != "D")) %>%
  arrange(-mean.other) %>%
  write_csv("~/Downloads/other_producing_datives.csv")

group_by(d, type, condition, target_verb) %>%
  filter(type == "target") %>%
  summarise(mean.do = mean(code == "D"),
            mean.po = mean(code == "P"),
            mean.other = mean(code != "P" & code != "D")) %>%
  arrange(-mean.other)



group_by(d, type, condition) %>%
  filter(code == "D" | code == "P") %>%
  summarise(mean.do = mean(code == "D"),
            mean.po = mean(code == "P"),
            mean.other = mean(code != "P" & code != "D"))

d.analyze = filter(targets, correct == T,
                   code == "D" | code == "P")


##############################################
d.analyze = mutate(d.analyze,
                   respcode = ifelse(code == "D", 1, 0),
                   boost = case_when(grepl("same", prime.cond) ~ .5,
                                     TRUE ~ -.5),
                   prime.boost = case_when(prime.cond == "DO-same" ~ .5,
                                           prime.cond == "PO-same" ~ -.5,
                                           TRUE ~ 0),
                   prime.noboost = case_when(prime.cond == "DO-diff" ~ .5,
                                             prime.cond == "PO-diff" ~ -.5,
                                             TRUE ~ 0))

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



full.model = brm(respcode ~ boost + prime.boost + prime.noboost + 
                   (boost + prime.boost + prime.noboost | subject) + 
                   (boost + prime.boost + prime.noboost | item),
                 prior = priors,
                 data = d.analyze, 
                 family = 'bernoulli',
                 chains=4, cores=4,
                 save_all_pars = T,
                 iter=30000)

# probability boost prime is bigger than no boost prime
mean(fixef(full.model, summary = F)[, "prime.boost"] > 
          fixef(full.model, summary = F)[, "prime.noboost"])

null.model.1 = brm(respcode ~ boost + prime.boost  + 
                     (boost + prime.boost + prime.noboost | subject) + 
                     (boost + prime.boost + prime.noboost | item),
                   prior = priors,
                   data = d.analyze, 
                   family = 'bernoulli',
                   chains=4, cores=4,
                   save_all_pars = T,
                   iter=30000)

null.model.2 = brm(respcode ~ boost + prime.boost + prime.noboost + 
                     (boost + prime.boost + prime.noboost | subject) + 
                     (boost + prime.boost + prime.noboost | item),
                   prior = priors,
                   data = d.analyze, 
                   family = 'bernoulli',
                   chains=4, cores=4,
                   save_all_pars = T,
                   iter=30000)


bf1 = bayes_factor(full.model, null.model.1)
bf2 = bayes_factor(full.model, null.model.2)



