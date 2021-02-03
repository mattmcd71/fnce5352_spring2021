library(ggplot2)
library(MASS)

data("Sitka")
str(Sitka)

ggplot(txhousing, aes(x=volume, y=sales)) +
       geom_point()

ggplot(Sitka, aes(x=Time, y=size)) +
  geom_point(aes(color=treat)) + 
  geom_smooth()

ggplot(Sitka, aes(x=Time, y=size)) +
  geom_point(aes(shape=treat)) + 
  geom_smooth()


#To zoom into small values of y, and clearly see that they exist, coord_cartesian(ylim=c(0.50)) was used. Can you explain this code

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
  
scale_y_log10()
  #coord_cartesian(ylim = c(0, 50))






##############################
library(tidyverse)

col_types <- cols(
  .default = col_double(),
  country = col_character(),
  iso2 = col_character(),
  iso3 = col_character(),
  iso_numeric = col_character(),
  g_whoregion = col_character(),
  new_sn_sexunk04 = col_double(),
  new_sn_sexunk514 = col_double(),
  new_sn_sexunk014 = col_double(),
  new_sn_sexunk15plus = col_double(),
  new_ep_m04 = col_double(),
  new_ep_sexunkageunk = col_double(),
  rdxsurvey_newinc = col_double(),
  rdxsurvey_newinc_rdx = col_double(),
  hiv_ipt_reg_all = col_double(),
  hiv_tbdetect = col_double()
)

who <- read_csv('https://extranet.who.int/tme/generateCSV.asp?ds=notifications', col_types = col_types)
who <- who[,c(1:3, 6, 27:33, 37:43, 47:53, 58:64, 73:79, 84:90)]

who2 <- who %>%
  select(-iso2, -iso3) %>%
  gather(group, cases, -country, -year ) %>%
  mutate(group = str_replace(group, "new_*", ""),
         method = str_extract(group, "[a-z]+"),
         gender = str_sub(str_extract(group, "_[a-z]"), 2, 2),
         age = str_extract(group, "[0-9]+"),
         age = ifelse(str_length(age) > 2,
                      str_c(str_sub(age, 1, -3), str_sub(age, -2, -1), sep = "-"),
                      str_c(age, "+"))) %>%
  group_by(year, gender, age, method) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE)) 

who2 %>%
  ggplot(aes(x = year, y = total_cases, linetype = gender)) +
  geom_line() +
  facet_grid(method ~ age,
             labeller = labeller(.rows = label_both, .cols = label_both)) +
  scale_y_log10() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))