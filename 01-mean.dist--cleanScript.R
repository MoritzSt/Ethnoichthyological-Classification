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

  dat <- readxl::read_xlsx(path = paste0(workdir, 
                                         '//input//latest_LEK_results.xlsx'),
                           sheet = 'Sheet1')
  names(dat) <- tolower(names(dat))
  head(dat, n = 2)
  
  # Adjust data lineup    
  dat <- dat %>%  mutate(gender = as.factor(gender),  # code variables as factors
                         age = as.factor(age),
                         community = as.factor(community)) %>%
    droplevels()  # drop potential empty factor levels
  

## 04: build model ####
  range(dat$meandist)
  hist(dat$meandist)
  ad.test(dat$meandist)  #--> not normally distributed
  ad.test(log(dat$meandist))  #--> nor distr
  cvm.test(log(dat$meandist))   #--> affirmative, nor distr
  
  # define the model formula ----
  my_expl_vars <- paste0("age + gender + community + community:age")
  my_formula <- paste0("meandist ~ ", my_expl_vars)
  
  # model raw data ----
  model <- lm(data = dat,
              formula = as.formula(my_formula))
  summary(model)
  cvm.test(resid(model))  
  ad.test(resid(model))  #--> resid's nor dist.
  x11()
    par(mfrow = c(2,2))
    plot(model)  #--> looks OK, two extreme values
                  #   (20: very high: M_56-up_Bar	, 24: very low: F_56-up_Bar)
  dev.off()
  
  
  # Full model not significant, minimize to min ad model ====
  model1 <- update(model, .~. - age:community) ; summary.aov(model1)
  model1 <- update(model1, .~. - age) ; summary.aov(model1)
  model1 <- update(model1, .~. - gender) ; summary.aov(model1)
  
  x11()
    par(mfrow = c(2,2))
    plot(model1)  #--> looks OK, one considerable outlier
  dev.off()
  ad.test(resid(model))
  cvm.test(resid(model))  # --> resids normally distributed
  AIC(model, model1)
  anova(model, model1)  # --> final model does not perform sig worse than full
  
  summary.aov(model1)
  summary(model1)

  
## 0x: Exploratory plot of variables ####
    
     # Define data to plot:
    plotdat <- dat
    # Sort community factor levels acc. distance to market ----
    plotdat$community <- factor(x = plotdat$community,
                            levels = c("Ilangana", "NusaRoviana", "Baraulu",
                                       "NusaHope"))
    
    head(plotdat)
    
    
    p1 <- ggplot(data = plotdat) +
      geom_boxplot(aes(y = meandist, x = community)) +
      xlab('Community') + ylab('Folk taxonomic distinctness')
    p1  
    
    p2 <- ggplot(data = plotdat) +
      geom_violin(aes(y = meandist, x = community)) +
      xlab('Community') + ylab('Folk taxonomic distinctness')
    p2  