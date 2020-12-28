library(tidyverse)

logit = function(p) {log(p) - log(1-p)}
inv.logit = function(x) {exp(x)/(1+exp(x))}

# read in data, compute standard error followinbg Mahowald et al. (2016)
e = read_csv("data/master_spreadsheet_processed.csv") %>%
  filter(Target.task == "WSC", Construction.type == "dative",
         Language == "English",
         Lag == "0",
         L1L1..L2L1..L1L2..L2L2 == "L1L1") %>%
  mutate(v = 1/(num.data.1 * num.data.items.1 *Dep.primed) +
           1/(num.data.2 * num.data.items.2 * Alt.primed) +
           (1/(num.data.1 * num.data.items.1 * (1- Dep.primed))) +
           (1/(num.data.2 * num.data.items.2 * (1 - Alt.primed))),
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

ggplot(exp1, aes(x=Name, y=lor,
                 ymin=lor - 1.96*se, ymax = lor + 1.96*se,
                 colour=Lexical.boost.)) +
  geom_point() + 
  geom_errorbar() +
  coord_flip() + 
  theme_bw(14)
ggsave("pngs/exp1_summary.png", width=9, height=4)