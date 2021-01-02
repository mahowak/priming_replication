library(tidyverse)

read_dat = function(x) {
  read_csv(x,
             col_names = c("time", "subject",
                           "controller", "item",
                           "element", "item_type",
                           "group", "prime",
                           "completion"),
             comment = "#") %>%
  filter(grepl("prime", item_type) | grepl("target", item_type))
}

d = map_df(c("pilot1_20201230/pilot_results_list1.txt",
         "pilot1_20201230/pilot_results_list2.txt",
         "pilot1_20201230/pilot_results_list3.txt",
         "pilot1_20201230/pilot_results_list4.txt"),
       read_dat)

dative_map = read_csv("pilot1_20201230/PB_items_melodie_format.csv") %>%
  rename(prime = sentence)
exp2_map = read_csv("pilot1_20201230/exp2_items.csv") %>%
  rename(prime = sentence)

dative = filter(d, grepl("dat-", item_type)) %>%
  separate(item_type, sep="-", into=c("dat", "item")) %>%
  mutate(type = ifelse(grepl("prime", item), "prime", "target") ,
         item = gsub("prime", "", item),
         item = gsub("target", "", item)) %>%
  left_join(mutate(dative_map, condition = ifelse(type == "target", "target", condition)) %>%
                select(prime, condition, target_verb, other_verb) %>%
              unique()) %>%
  mutate(exp = "exp1")

exp2 = filter(d, !grepl("dat-", item_type)) %>%
  mutate(type = ifelse(grepl("prime", item), "prime", "target") ,
         item = gsub("prime", "", item),
         item = gsub("target", "", item)) %>%
  left_join(mutate(exp2_map, condition = ifelse(type == "target", gsub("-low", "", condition), condition),
                   condition = ifelse(type == "target", gsub("-high", "", condition), condition)) %>%
              select(prime, condition) %>%
              unique()) %>%
  mutate(exp = "exp2")

write_csv(dative, "pilot1_20201230/exp1_pilot_results.csv")
write_csv(exp2, "pilot1_20201230/exp2_pilot_results.csv")