
##Code for matching analysis
## Daniel M. Auliz Ortiz
##email: dauliz@cieco.unam.mx

######################################################################

library(tidyverse)
library(MatchIt)
library(optmatch)
library(cobalt)

###the general database
df<-read.csv("C:/Users/ZBokk/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/Documents/Trabajo de ivestigación VIII/tables_8csv/match_loss_regrowth_frag/repository/df_loss_regrowth.csv")

#filtering data to compare forest loss in areas covered by forest in 2000 (lc_2000=1)
#in protected and unprotected zones
loss_df<-df %>% 
  filter(lc_2000==1)


#filtering data to compare forest regrowth in areas covered by no forest in 2000 (lc_2000=1)
#in protected and unprotected zones
regrowth_df<-df %>% 
  filter(lc_2000==2)



#define a categorical variable for treatment (prot=1) and control (prot=0) groups
loss_df<-loss_df %>% 
  mutate(treat= if_else(prot=="reserves", 1, 0))

regrowth_df<-regrowth_df %>% 
  mutate(treat= if_else(prot=="reserves", 1, 0))



###Propensity score estimation # 

m_ps<-glm(treat~rod_dst+cts_dst +sutblty, family = "binomial", data = loss_df)

summary(m_ps)



m_ps2<-glm(treat~rod_dst+cts_dst +sutblty, family = "binomial", data = regrowth_df)

summary(m_ps2)



#####################################################################
##Matching 


##Matching forest loss data

mod_match_loss <- matchit(treat ~ rod_dst + I(rod_dst^2)+ 
                            cts_dst + I(cts_dst^2)+
                            sutblty+ I(sutblty^2),
                          method = "nearest", replace = T,data = loss_df)

#summary matched data
sum_loss<-summary(mod_match_loss, standardize=T)


#plotting covariates balance
lplot_loss<-love.plot(mod_match_loss, binary ="std", thresholds = 0.1)

lplot_loss



##Matching forest regrowth data


mod_match_regrowth <- matchit(treat ~ rod_dst + I(rod_dst^2)+ 
                                cts_dst + I(cts_dst^2)+
                                sutblty+ I(sutblty^2),
                              method = "nearest", data = regrowth_df)

#summary matched data
sum_rgr<-summary(mod_match_regrowth, standardize=T)


#plotting covariates balance
lplot_regr<-love.plot(mod_match_regrowth, binary ="std", thresholds = 0.1)


lplot_regr



########## getting matched data


df_match_regr <- match.data(mod_match_regrowth)


df_match_loss <- match.data(mod_match_loss)




