# Biblioteki
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readr)
# Wczytanie danych
ncn_grants <- read_csv("../../data/ncn_grants.csv")
# Ograniczmy się do jednego popularnego konkursu i dwóch paneli
hs_st = ncn_grants %>%
    filter(type == "OPUS") %>%
    filter(panel %in% c("HS", "ST")) %>%
    mutate(is_HS = panel == "HS") %>%
    mutate(year = year(date))
head(hs_st)
# Podział na zbiór uczący i testowy
set.seed(17)
train_indices = sample(c(TRUE, FALSE), nrow(hs_st), replace = TRUE,
                       prob = c(0.75, 0.25))
hs_st_train = hs_st[train_indices, ]
hs_st_test = hs_st[!train_indices, ]
# Szybka wizualizacja
hs_st_train %>%
    mutate(budget = sqrt(budget), coinvestigators = sqrt(coinvestigators)) %>%
    select(is_HS, year, budget, duration, coinvestigators) %>%
    gather("col", "val", -is_HS, -year) %>%
    ggplot(aes(x = val, fill = is_HS)) +
    geom_histogram() +
    facet_wrap(~col, scales = "free") +
    theme_bw()
# Początkowy model
glm_3var = glm(is_HS ~ budget + duration + coinvestigators,
               data = hs_st_train)
summary(glm_3var)
plot(glm_3var)
residuals(glm_3var)
# hs_st_train[1365, ]
# hs_st_train[-1365, ] %>%
hs_st_train %>%
    mutate(predicted_glm = predict(glm_3var, type = "response")) %>%
    select(predicted_glm, is_HS, budget, duration, coinvestigators) %>%
    gather("col", "val", -is_HS, -predicted_glm) %>%
    ggplot(aes(x = val, y = predicted_glm, color = is_HS)) +
    geom_point() +
    facet_wrap(~col, scales = "free_x")
# Ocena modelu
glm_4var = glm(is_HS ~ budget + duration + coinvestigators+ year,
               data = hs_st_train)
anova(glm_3var, glm_4var, test = "LRT")

prop.table(table(true = hs_st_test$is_HS,
                 predicted = (predict(glm_3var, hs_st_test, type = "response") > 0.5) == hs_st_test$is_HS),
           margin = 1)

mean((predict(glm_3var, hs_st_test, type = "response") > 0.5) == hs_st_test[["is_HS"]])
mean((predict(glm_4var, hs_st_test, type = "response") > 0.5) == hs_st_test[["is_HS"]])

tn_n = function(true, predicted) {
    sum(true & predicted)
    # Dla wektorów typu innego niż lgl: sum(true == "success" & predicted == "success")
}

# library(gam)
# gam_model = gam(is_HS ~ s(budget) + s(duration) + s(coinvestigators),
#                 data = hs_st,
#                 family = binomial)
# plot(gam_model)
# predict(gam_model, type = "response") > 0.5
# prop.table(table(true = hs_st$is_HS,
#                  predicted = (predict(gam_model, type = "response") > 0.5) == hs_st$is_HS),
#            margin = 1)
# mean((predict(gam_model, type = "response") > 0.5) == hs_st$is_HS)
