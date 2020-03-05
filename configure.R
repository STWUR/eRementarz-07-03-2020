install.packages(packages = c("tidyverse", "mgcv", 
                              "gam", "ggbeeswarm", "ggrepel"), 
                 repos = "https://cloud.r-project.org")
gd <- read.csv("https://github.com/STWUR/eRementarz-07-03-2020/raw/master/data/ncn_grants.csv")
write.csv(gd, file = "ncn_grants.csv", row.names = FALSE)
