##Code for matching analysis
## Daniel M. Auliz Ortiz
##email: dauliz@cieco.unam.mx

######################################################################

library(tidyverse)
library(MatchIt)
library(optmatch)
library(cobalt)





##load fragmentation database
#df_frag<- read.csv("C:/Users/ZBokk/OneDrive - UNIVERSIDAD NACIONAL AUT?NOMA DE M?XICO/Documents/Trabajo de ivestigaci?n VIII/tables_8csv/match_loss_regrowth_frag/repository/df_frag_land.csv")
df_frag<- read.csv("df_frag_land.csv")


#define a categorical variable for treatment (prot=1) and control (prot=0) groups
df_frag<-df_frag %>% 
  mutate(treat= if_else(prot=="reserves", 1, 0))


###Propensity score estimation # el peso de las covariables en ser tratado o no tratado (prot/no_prot)


m_cov_f<-glm(treat~road_ds+cits_ds +sutblty, family = "binomial", data = df_frag)

summary(m_cov_f)




#####################################################################
##Matching 


##Matching forest loss data

mod_match_frag <- matchit(treat ~ road_ds + I(road_ds^2)+ 
                             cits_ds + I(cits_ds^2)+
                             sutblty+ I(sutblty^2),
                           method = "genetic", pop.size = 1000, 
                           wait.generations= 25,max.generations=150,replace = T,
                           data = df_frag)

#summary matched data
sum_frag<-summary(mod_match_frag, standardize=T)


#plotting covariates balance
lplot_frag<-love.plot(mod_match_frag, binary ="std", thresholds = 0.1)


lplot_frag

########## getting matched data

df_match_frag <- match.data(mod_match_frag)



