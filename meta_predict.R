library(tidyverse)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) {exp(x)/(1+exp(x))}

e = read_csv("data/master_spreadsheet_processed.csv") %>%
  filter(Target.task == "WSC", Construction.type == "dative",
       Language == "English",
       Lag == "0",
       L1L1..L2L1..L1L2..L2L2 == "L1L1") %>%
  mutate(v = 1/(num.data.1 * num.data.items.1 *Dep.primed) + 1/(num.data.2 * num.data.items.2 * Alt.primed) + (1/(num.data.1 * num.data.items.1 * (1- Dep.primed))) + (1/(num.data.2 * num.data.items.2 * (1 - Alt.primed))),
         se = sqrt(v))

e = select(e, ExperimentID, Subcondition, Alt.primed, Dep.primed, Dep.measure) %>%
  mutate(PrimeDO = ifelse(Dep.measure == "DO", Dep.primed, Alt.primed),
         PrimePO = ifelse(Dep.measure == "PO", Dep.primed, Alt.primed)) %>%
  pivot_wider(id_cols = c(ExperimentID, Subcondition),
              names_from = c(Dep.measure), 
              values_from = c(PrimeDO, PrimePO)) %>%
  mutate(PrimeDO_DO = ifelse(is.na(PrimeDO_DO), 1 - PrimeDO_PO, PrimeDO_DO), # handle case where the ratio is just out of 1 to begin with
         PrimePO_DO = ifelse(is.na(PrimePO_DO), 1 - PrimePO_PO, PrimePO_DO),
         PrimeDO_DO.pct = PrimeDO_DO/(PrimeDO_DO + PrimeDO_PO),
         PrimeDO_PO.pct = PrimeDO_PO/(PrimeDO_DO + PrimeDO_PO),
         PrimePO_DO.pct = PrimePO_DO/(PrimePO_PO + PrimePO_DO),
         PrimePO_PO.pct = PrimePO_PO/(PrimePO_PO + PrimePO_DO),
         lor.DO = logit(PrimeDO_DO.pct) - logit(PrimePO_DO.pct),
         lor.PO = logit(PrimePO_PO.pct) - logit(PrimeDO_PO.pct),
         lor = lor.DO) %>%
  select(ExperimentID, lor, lor.DO, lor.PO, PrimeDO_DO.pct,
         PrimeDO_PO.pct, PrimePO_DO.pct, PrimePO_PO.pct) %>%
  right_join(e)

exp1 = filter(e, Dep.measure  == "DO" | (Paper.ID == 62 & (grepl("Shifted", Subcondition) == F)) ,
       Target.task == "WSC",
       Language == "English",
       Construction.type == "dative",
       Lag == "0",
       L1L1..L2L1..L1L2..L2L2 == "L1L1") %>%
  mutate(Lexical.boost. =     # fix mistake in coding meta-analysis
           ifelse(Authors == "Branigan H.P., Pickering M.J., Cleland A.A.",
                  "yes",
                  Lexical.boost.) ) %>%
  select(Paper.ID, Authors, Year, Experiment.name, lor, se, Lexical.boost.,
         StudyID,
         ExperimentID,
         Dep.measure,
         num.data.1,
         num.data.2,
         Subcondition,
         N.unique.items) %>%
  filter(Paper.ID != 58,
         !grepl("spoken", Subcondition)) %>%
  mutate(Experiment.name = ifelse(is.na(Experiment.name), "", Experiment.name),
         Subcondition = ifelse(is.na(Subcondition), "", Subcondition),
         Name = paste(Authors, " (", Year, ") ", Experiment.name, " ", Subcondition, sep=""),
         Name = fct_reorder(Name, -as.numeric(Year)))

# Branigan H.P., Pickering M.J., Cleland A.A. studies whether you can have intervening
# material. We focus on just the no lag condition.

# Cleland A.A., Pickering M.J. compares across modalities, we use only 
# the Experiment 3 one, which is written.

# Kaschak M.P., Loney R.A., Borreggine K.L.: block priming

# Pickering M.J., Branigan H.P., McLean J.F.: compare to baseline

ggplot(exp1, aes(x=Name, y=lor,
                 ymin=lor - 1.96*se, ymax = lor + 1.96*se,
                 colour=Lexical.boost.)) +
  geom_point() + 
  geom_errorbar() +
  coord_flip() + 
  theme_bw(14)
ggsave("exp1_summary.png", width=9, height=4)
  
filter(exp1, Lexical.boost. == "no") %>%
  select(Name, num.data.1, N.unique.items)
exp1$num.data.1



#######################
e = read_csv("~/db/ToUsePrimingPapers/public_data/master_spreadsheet_processed.csv") %>%
  filter(Dep.measure %in% c("HA", "LA"),
         Construction.type == "RC attachment") %>%
  mutate(v = 1/(num.data.1 * num.data.items.1 *Dep.primed) + 1/(num.data.2 * num.data.items.2 * Alt.primed) + (1/(num.data.1 * num.data.items.1 * (1- Dep.primed))) + (1/(num.data.2 * num.data.items.2 * (1 - Alt.primed))),
         se = sqrt(v))

e = select(e, ExperimentID, Subcondition, Alt.primed, Dep.primed, Dep.measure) %>%
  mutate(PrimeHA = ifelse(Dep.measure == "HA", Dep.primed, Alt.primed),
         PrimeLA = ifelse(Dep.measure == "LA", Dep.primed, Alt.primed)) %>%
  pivot_wider(id_cols = c(ExperimentID, Subcondition),
              names_from = c(Dep.measure), 
              values_from = c(PrimeHA, PrimeLA)) %>%
  mutate(PrimeHA_HA = ifelse(is.na(PrimeHA_HA), 1 - PrimeHA_LA, PrimeHA_HA), # handle case where the ratio is just out of 1 to begin with
         PrimeLA_HA = ifelse(is.na(PrimeLA_HA), 1 - PrimeLA_LA, PrimeLA_HA),
         PrimeHA_LA = ifelse(is.na(PrimeHA_LA), 1 - PrimeHA_HA, PrimeHA_LA), # handle case where the ratio is just out of 1 to begin with
         PrimeLA_LA = ifelse(is.na(PrimeLA_LA), 1 - PrimeLA_HA, PrimeLA_LA),
         PrimeHA_HA.pct = PrimeHA_HA/(PrimeHA_HA + PrimeHA_LA),
         PrimeHA_LA.pct = PrimeHA_LA/(PrimeHA_HA + PrimeHA_LA),
         PrimeLA_HA.pct = PrimeLA_HA/(PrimeLA_LA + PrimeLA_HA),
         PrimeLA_LA.pct = PrimeLA_LA/(PrimeLA_LA + PrimeLA_HA),
         lor.HA = logit(PrimeHA_HA.pct) - logit(PrimeLA_HA.pct),
         lor.LA = logit(PrimeLA_LA.pct) - logit(PrimeHA_LA.pct),
         lor = lor.HA) %>%
  select(ExperimentID, lor, lor.HA, lor.LA, PrimeHA_HA.pct,
         PrimeHA_LA.pct, PrimeLA_HA.pct, PrimeLA_LA.pct) %>%
  right_join(e)

exp2 = e %>%
  select(Paper.ID, Authors, Year, Experiment.name, lor, se, Lexical.boost.,
         StudyID,
         ExperimentID,
         Dep.measure, num.data.1, num.data.2,
         Subcondition, N.unique.items) %>%
  filter(!grepl("PP-prime", Subcondition),
         Dep.measure == "HA") %>%
  mutate(Experiment.name = ifelse(is.na(Experiment.name), "", Experiment.name),
         Subcondition = ifelse(is.na(Subcondition), "", Subcondition),
         Name = paste(Authors, " (", Year, ") ", Experiment.name, " ", Subcondition, sep=""),
         Name = fct_reorder(Name, -as.numeric(Year)))

# Branigan H.P., Pickering M.J., Cleland A.A. studies whether you can have intervening
# material. We focus on just the no lag condition.

# Cleland A.A., Pickering M.J. compares across modalities, we use only 
# the Experiment 3 one, which is written.

# Kaschak M.P., Loney R.A., Borreggine K.L.: block priming

# Pickering M.J., Branigan H.P., McLean J.F.: compare to baseline

ggplot(exp2, aes(x=Name, y=lor,
                 ymin=lor - 1.96*se, ymax = lor + 1.96*se,
                 colour=Lexical.boost.)) +
  geom_point() + 
  geom_errorbar() +
  coord_flip() + 
  theme_bw(14)
ggsave("exp2_summary.png", width=9, height=4)

filter(exp2, Lexical.boost. == "no") %>%
  select(Name, num.data.1, N.unique.items)
######


ggplot(exp2, aes(x=num.data.1)) + geom_histogram()
ggplot(exp1, aes(x=num.data.1)) + geom_histogram()

# 
# load("~/db/ToUsePrimingPapers/public_data/current_lmer.rda")
# x = current_lmer
# predict(x, newdata=exp1)
# fixef(x)
# condcode
library(tidyverse)
library(brms)
logit = function(p) {log(p)/log(1-p)}

d = read_csv("data/raw_data_merged_with_master.csv") 
priors = c(set_prior("normal(0, .5)", class = "Intercept"),
                           set_prior("normal(0, .5)", class = "b"),
                           set_prior("normal(0, .5)", class = "sd"),
                           set_prior("lkj(2)", class = "L"))
l.meta <- brm(data=d,
                  family = 'bernoulli',
                  respcode ~ 
                    condcode + 
                    condcode:Lag +
                    condcode:YearNum +
                    condcode:Lexical.boost. +
                    condcode:L1 +
                    (1  + condcode | Construction.type ) + 
                    (1 + condcode|Paper.ID) +
                    (1 + condcode|ExperimentID:Paper.ID) +
                    (1 + condcode|ExperimentID:Paper.ID:subj) +
                    (1 + condcode|ExperimentID:Paper.ID:item) + 
                    (1|Target.task),
              prior = priors,
              cores=4,
              chains=4)

