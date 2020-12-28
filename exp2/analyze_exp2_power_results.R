library(tidyverse)

d = read_csv("exp2/exp_2_power_analysis.csv", col_names=F)

d = mutate(d, repeated = i == lag(i),
           repeated = ifelse(is.na(repeated), FALSE, repeated),
           diff = i - lag(i), 
           nextdiff = lead(diff)) %>%
  filter(nextdiff != 0,
         (bf < 1/6 | bf > 6) | nsubj == 500) %>%
  mutate(bf = ifelse(bf > 100, 100, bf))


ggplot(d, aes(x=bf)) +
  geom_histogram() +
  theme_bw(12) +
  xlab("Bayes Factor")
ggsave("pngs/exp2_bf.png", width=4, height=3)

d.subjsum = group_by(d, nsubj) %>%
  summarise(n=n()) %>%
  mutate(charn = as.character(n))
ggplot(d.subjsum, aes(x=nsubj, y=n, label=charn)) +
  geom_bar(stat="identity") + 
  geom_text(data=d.subjsum, aes(x=nsubj, y=n + 3, label=charn)) + 
  theme_bw(12) + 
  xlab("number of subjects run to get conclusive results") + 
  ylab("count")
ggsave("pngs/exp2_nsubj.png", width=4, height=3)
