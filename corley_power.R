library(tidyverse)

d = read_csv("results_corley_highvar.csv", col_names=F)
names(d) = c("nsubj", "nitems", "i", "bf", "p")

d = mutate(d, repeated = i == lag(i),
           repeated = ifelse(is.na(repeated), FALSE, repeated),
           diff = i - lag(i), 
           nextdiff = lead(diff)) %>%
  filter(nextdiff != 0,
         (bf < 1/6 | bf > 6) | nsubj == 500) %>%
  mutate(bf = ifelse(bf > 100, 100, bf))


ggplot(d, aes(x=bf)) + geom_histogram()

mean(d$bf > 6)
d[d$bf > (1/6) & d$bf < 6, ]

nrow(d)

table(d$nsubj)
