################################################################################
# Univariate analysis underlying 
# Aswani, Ferse, Stäbler & Chong-Montenegro 2020,
# "Detecting Change in Local Ecological Knowledge: An Application of an Index of
# Taxonomic Distinctness to an Ethnoichthyological Classification in the Solomon
# Islands"
################################################################################

require(tidyverse)
require(ggpubr)
require(nortest)


## 01: Load in data ####

workdir <- 'D://SYNC//01-ZMT//Colleagues//Sebastian_FERSE//LEK-tree-distance'

dat <- readxl::read_xlsx(path = paste0(workdir, '//input//latest_LEK_results.xlsx'),
                         sheet = 'Sheet1')
names(dat) <- tolower(names(dat))
head(dat)

dat <- dat %>%  mutate(gender = as.factor(gender),  # code variables as factors
                       age = as.factor(age),
                       community = as.factor(community)) %>%
  droplevels()  # drop potential empty factor levels


## Create plot ####

  # change the order of the legend for clarity (from east to west, or from more
  # "traditional" to more "monetized" (developed)

  dat$community <- factor(dat$community, levels = c(
    
    "Ilangana",
    "NusaRoviana",
    "Baraulu",
    "NusaHope"
  ))
  
x11()
ggplot(data = dat, aes(x = spp, y = meandist)) +
  geom_point(aes(col = community)) + 
  #geom_smooth(method = 'glm') + 
  theme_minimal() +
  xlab('No. of distinct species ') + 
  ylab('Mean taxonomic distinctness') +
  guides(col = guide_legend(title=""))

require(nortest)
  ad.test(dat$meandist)
  ad.test(dat$spp)  
  ## --> Both not normally distributed
  
  cor(dat$spp, dat$meandist, method = 'spearman')
  cor.test(dat$spp, dat$meandist, method = 'spearman')
  cor.test(dat$spp, dat$meandist, method = 'kendall')
  