library(tidyverse)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) {exp(x)/(1+exp(x))}

e = read_csv("data/master_spreadsheet_processed.csv") %>%
  filter(Dep.measure %in% c("HA", "LA"),
         Construction.type == "RC attachment") %>%
  mutate(v = 1/(num.data.1 * num.data.items.1 *Dep.primed) +
           1/(num.data.2 * num.data.items.2 * Alt.primed) +
           (1/(num.data.1 * num.data.items.1 * (1- Dep.primed))) +
           (1/(num.data.2 * num.data.items.2 * (1 - Alt.primed))),
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
         ExpNum = ifelse(Experiment.name == "",
                         0,
                         as.numeric(gsub("Experiment ", "", Experiment.name))),
         Name = paste(Authors, " (", Year, ") ", Experiment.name, " ", Subcondition, sep=""),
         Name = fct_reorder(Name, -as.numeric(ExpNum)),
         Name = fct_reorder(Name, -as.numeric(Year)))


ggplot(exp2, aes(x=Name, y=lor,
                 ymin=lor - 1.96*se, ymax = lor + 1.96*se,
                 colour=Lexical.boost.)) +
  geom_point() + 
  geom_errorbar() +
  coord_flip() + 
  theme_bw(14) + 
  ylab("log odds ratio") + 
  xlab("experiment")
ggsave("pngs/exp2_summary.png", width=9, height=3)