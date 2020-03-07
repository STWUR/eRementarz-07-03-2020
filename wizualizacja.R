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

p <- ggplot(dane, aes(x = subpanel, y = budget)) +
  geom_boxplot(outlier.color = NA) +
  geom_violin(fill = NA) +
  facet_wrap(~ panel, ncol = 1, scales = "free_x")

p + ggtitle("Genialny obrazek Michała")

png("nazwa.png", width = 600, height = 1000)
print(p)
dev.off()

# bmp, jpeg

pdf(file = "nazwa.pdf", width = 5, height = 8)
print(p)
dev.off()

# cairo_pdf, cairo_ps, svglite
# ggsave()

ggplot(dane, aes(x = panel, label = panel)) +
  geom_bar() +
  geom_label(stat = "count")


ggplot(dane, aes(x = panel, label = ..count..)) +
  geom_bar() +
  geom_label(stat = "count")

filter(dane, subpanel %in% c("NZ2", "NZ8",
                             "ST1", "ST6")) %>% 
  ggplot(aes(x = panel, fill = subpanel,
             label = ..count..)) +
  geom_bar(position = "dodge")

filter(dane, subpanel %in% c("NZ2", "NZ8",
                             "ST1", "ST6")) %>% 
  ggplot(aes(x = panel, fill = subpanel,
             label = ..count.., color = subpanel)) +
  geom_bar(position = position_dodge(width = 1)) +
  geom_label(stat = "count", 
             position = position_dodge(width = 1),
             fill = "white")
