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
  

## 04: build models with various transformations of response variable ####
  range(dat$spp)
  hist(dat$spp)  #--> Oh, bimodal!
  ad.test(dat$spp)  #--> hence, not normally distributed
  ad.test(log(dat$spp))  #--> nope
  cvm.test(log(dat$spp))   #--> affirmative, nor distr
  
  # define the model formula:
  my_expl_vars <- paste0("age + gender + community + community:age")
  my_formula <- paste0("spp ~ ", my_expl_vars)
  
  # model raw data:
  model <- lm(data = dat,
              formula = as.formula(my_formula))
  summary(model)
  cvm.test(resid(model))  
  ad.test(resid(model))  #--> resid's nor dist.
  x11()
    par(mfrow = c(2,2))
    plot(model)  #--> looks OK, two and a half outliers
  dev.off()
  
  # Reduce to minimal adequate model ====
  summary.aov(model)
  model1 <- update(model, .~. -age:community) ; summary.aov(model1)
  
  # Check model assumptions:
  ad.test(resid(model1)) ; cvm.test(resid(model1))  # OK
  x11()
    par(mfrow = c(2,2))
    plot(model1)  #--> OK
  dev.off()
  
  summary.aov(model1)
  summary(model1)
  
          
## 0x: Exploratory plot of variables ####
    plotdat <- dat
  plotdat$community <- factor(x = plotdat$community,
                              levels = c("Ilangana", "NusaRoviana", "Baraulu",
                                         "NusaHope"))
    
## 0x: Plot of (almost) significant variables ####
    
    px1 <- ggplot(data = plotdat) +
      geom_boxplot(aes(y = spp, x = age)) + 
      ylab('Number of names') + xlab('Age group')
    px1
    
    px2 <- ggplot(data = plotdat) +
      geom_boxplot(aes(y = spp, x = community)) + 
      ylab('Number of names') + xlab('Community')
    px2
    
    px3 <- ggplot(data = plotdat) +
      geom_boxplot(aes(y = spp, x = gender)) + 
      ylab('Number of names') + xlab('Gender')
    px3
    
    ggpubr::ggarrange(px1, px2, px3, 
                      labels = c("A", "B", "C"),
                      ncol = 3, nrow = 1)
    