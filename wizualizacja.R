library(ggplot2)
library(dplyr)

dane <- read.csv("./data/ncn_grants.csv")

ggplot(data = dane, mapping = aes(x = panel)) +
  geom_bar()

filter(dane, panel == "ST") %>% 
  ggplot(aes(x = subpanel)) +
  geom_bar()

filter(dane, panel == "ST") %>% 
  group_by(subpanel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, 
             color = subpanel)) +
  geom_point(size = 4)

filter(dane, panel == "ST") %>% 
  group_by(subpanel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, 
             color = subpanel, shape = subpanel)) +
  geom_point(size = 4)

filter(dane, panel == "ST") %>% 
  group_by(subpanel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, 
             color = subpanel, label = subpanel)) +
  geom_label()

library(ggrepel)

group_by(dane, panel, subpanel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, 
             label = subpanel)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel() +
  facet_wrap(~ panel, scales = "free_y")

filter(dane, panel == "ST") %>% 
  ggplot(aes(x = coinvestigators)) +
  geom_histogram() +
  facet_wrap(~ subpanel)

filter(dane, panel == "ST") %>% 
  ggplot(aes(x = coinvestigators)) +
  geom_density() +
  facet_wrap(~ subpanel)

filter(dane, panel == "ST") %>% 
  ggplot(aes(x = subpanel, y = coinvestigators)) +
  geom_boxplot()

filter(dane, panel == "ST") %>% 
  ggplot(aes(x = subpanel, y = budget)) +
  geom_boxplot()

filter(dane, panel == "ST") %>% 
  ggplot(aes(x = subpanel, y = budget)) +
  geom_boxplot() +
  geom_violin(fill = NA)


ggplot(dane, aes(x = subpanel, y = budget)) +
  geom_boxplot(outlier.color = NA) +
  geom_violin(fill = NA) +
  facet_wrap(~ panel, ncol = 1, scales = "free_x")


