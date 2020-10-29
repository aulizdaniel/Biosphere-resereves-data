###########################################################################################################################

#Author:Daniel M. Auliz Ortiz
#e-mail: dauliz@cieco.unam.mx
#Institution: IIES, UNAM, México

#the current script calculates the diversity of non-farm labor for all Mexican municipalities for the year 2000

###########################################################################################################################

library(tidyverse)
library(hillR)
library(curl)


b<- read_csv(curl("https://raw.githubusercontent.com/aulizdaniel/Biosphere-resereves-data/main/municipalities_employment_data_2000.csv"))



socio<- mutate(b, abs_prof= SPT00) ##population working on profesional activities
socio<- mutate(socio, abs_agro= AGCP00) ##population working on agriculture activities
socio<- mutate(socio, abs_ind= MIN00+IM00+EA00+CON00)  ##population working on industrial activities
socio<- mutate(socio, abs_serv= COM00+TRCO00+SF00+APD00+SCS00+SRH00+SPM00+IMM00+SI00+SN00+SE00+SS00) ##population working on the service sector
socio<-mutate(socio, Pob_sec_prim= (abs_agro/POOC00)*100) #percentage of population working on agriculture activities


economic_act<- select(socio, abs_prof:abs_serv)

names(economic_act)


socio<- mutate(socio, hill1= hill_taxa(economic_act, q=1)) #calculate diversity index (exponetial of shannon index)


summary(socio$hill1)


socio<- mutate(socio, dnfl= (1-(Pob_sec_prim/100 )/hill1)) ###calculate diversity of non-farm labor




















