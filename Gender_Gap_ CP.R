###############################################################################
#                                                                             #
# Script per ottenere geneder gap per cause di morte.                         #
# Dati cause di morte da https://www.causesofdeath.org/                       #
#                                                                             #
# 1-males, 2-females, 3-both sexes                                            #
# Files with age-specific death rates                                         #
# Death rates are calculated per 1,000,000 and published as integer numbers.  #
#                                                                             #
###############################################################################


########################
#                      #
# RATES male           #
#                      #
########################

library(tidyverse)
library(reshape)
library(data.table)

# usa <- read.csv("/Users/flaviolaudenzi/Desktop/USA_m_short_idr.csv",header = T)
usa <- read.csv("USA_m_short_idr.csv",header = T)
head(usa)  #descrizione delle variabili

#costruisco il vettore usa_M il quale filtra il sesso maschile e causa
usa_M <- usa %>% filter(sex==1,cause!=0) # select 1-male
usa_M <- usa_M[,-c(1,3,4,5,25,27,29)]

head(usa_M)  

#metto in ordine il vettore: causa/anno/gruppi di età/tassi mx

usa_M <- melt(usa_M, id.vars =c("cause","year"), measure.vars = c("m0", "m1", "m5", "m10", 
                                                              "m15", "m20", "m25", "m30", 
                                                              "m35", "m40", "m45", "m50", 
                                                              "m55", "m60", "m65", "m70",
                                                              "m75", "m80", "m85", "m90",
                                                              "m95","m100p"))
#setDT(usa_M)

# ricodifica eta: metto variabile age rispetto a mx
usa_M$Age <- NA
usa_M$Age[usa_M$variable=="m0"] <- 0
usa_M$Age[usa_M$variable=="m1"] <- 1
usa_M$Age[usa_M$variable=="m5"] <- 5
usa_M$Age[usa_M$variable=="m10"] <- 10
usa_M$Age[usa_M$variable=="m15"] <- 15
usa_M$Age[usa_M$variable=="m20"] <- 20
usa_M$Age[usa_M$variable=="m25"] <- 25
usa_M$Age[usa_M$variable=="m30"] <- 30
usa_M$Age[usa_M$variable=="m35"] <- 35
usa_M$Age[usa_M$variable=="m40"] <- 40
usa_M$Age[usa_M$variable=="m45"] <- 45
usa_M$Age[usa_M$variable=="m50"] <- 50
usa_M$Age[usa_M$variable=="m55"] <- 55
usa_M$Age[usa_M$variable=="m60"] <- 60
usa_M$Age[usa_M$variable=="m65"] <- 65
usa_M$Age[usa_M$variable=="m70"] <- 70
usa_M$Age[usa_M$variable=="m75"] <- 75
usa_M$Age[usa_M$variable=="m80"] <- 80
usa_M$Age[usa_M$variable=="m85"] <- 85
usa_M$Age[usa_M$variable=="m90"] <- 90
usa_M$Age[usa_M$variable=="m95"] <- 95
usa_M$Age[usa_M$variable=="m100p"] <- 100


usa_M <- as.data.table(usa_M)

head(usa_M)
names(usa_M)[4]<-"mx.tot.by.Caus"  #cambio nome alla colonna, tasso mortalità per quella causa e a una certa età


# Classificazione cause: aggiungo nuova colonna cuase_ rev che vanno da 1 a 7
usa_M$Cause_Rev[usa_M$cause==1] <- 1 # inf
usa_M$Cause_Rev[usa_M$cause==2] <- 2 # neop
usa_M$Cause_Rev[usa_M$cause==3] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==4] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==5] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==6] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==7] <- 3 # CVD
usa_M$Cause_Rev[usa_M$cause==8] <- 3 # CVD
usa_M$Cause_Rev[usa_M$cause==9] <- 3 # CVD
usa_M$Cause_Rev[usa_M$cause==10] <- 4 # RESP
usa_M$Cause_Rev[usa_M$cause==11] <- 4 # RESP
usa_M$Cause_Rev[usa_M$cause==12] <- 5 # DIG
usa_M$Cause_Rev[usa_M$cause==13] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==14] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==15] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==16] <- 6 # ext

usa_M$mx.tot.by.Caus <- usa_M$mx.tot.by.Caus/1000000   #divide coeff per 1 milione
usa_M %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(mx.tot.by.Caus) %>%  sum()
#0.000226

usa_M=usa_M[,.(mx.tot.by.Caus = sum(mx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]
usa_M %>% filter(Cause_Rev==3) #ordino causa in base all'anno
#graifco log del coeff di mortalità in funzione dell'età e per le 7 cause di morte
usa_M %>%filter(year==1979) %>% ggplot(aes(Age,log(mx.tot.by.Caus)))+geom_line()+facet_wrap(~Cause_Rev) 

usa_M$Sex <- "M"
save(usa_M,file="usa_M.RData")  #file finale ordinato

############################################################






rm(list=ls())
########################
#                      #
# RATES female         #
#                      #
########################
#faccio la stessa identica cosa

library(tidyverse)
library(reshape)
library(data.table)

# usa <- read.csv("/Users/flaviolaudenzi/Desktop/USA_m_short_idr.csv",header = T)
usa <- read.csv("USA_m_short_idr.csv",header = T)
usa_F <- usa %>% filter(sex==2,cause!=0) #select 2-female
usa_F <- usa_F[,-c(1,3,4,5,25,27,29)]

usa_F <- melt(usa_F, id.vars =c("cause","year"), measure.vars = c("m0", "m1", "m5", "m10", 
                                                                  "m15", "m20", "m25", "m30", 
                                                                  "m35", "m40", "m45", "m50", 
                                                                  "m55", "m60", "m65", "m70",
                                                                  "m75", "m80", "m85", "m90",
                                                                  "m95","m100p"))

#setDT(usa_M)

usa_F$Age <- NA

# ricodifica eta
usa_F$Age[usa_F$variable=="m0"] <- 0
usa_F$Age[usa_F$variable=="m1"] <- 1
usa_F$Age[usa_F$variable=="m5"] <- 5
usa_F$Age[usa_F$variable=="m10"] <- 10
usa_F$Age[usa_F$variable=="m15"] <- 15
usa_F$Age[usa_F$variable=="m20"] <- 20
usa_F$Age[usa_F$variable=="m25"] <- 25
usa_F$Age[usa_F$variable=="m30"] <- 30
usa_F$Age[usa_F$variable=="m35"] <- 35
usa_F$Age[usa_F$variable=="m40"] <- 40
usa_F$Age[usa_F$variable=="m45"] <- 45
usa_F$Age[usa_F$variable=="m50"] <- 50
usa_F$Age[usa_F$variable=="m55"] <- 55
usa_F$Age[usa_F$variable=="m60"] <- 60
usa_F$Age[usa_F$variable=="m65"] <- 65
usa_F$Age[usa_F$variable=="m70"] <- 70
usa_F$Age[usa_F$variable=="m75"] <- 75
usa_F$Age[usa_F$variable=="m80"] <- 80
usa_F$Age[usa_F$variable=="m85"] <- 85
usa_F$Age[usa_F$variable=="m90"] <- 90
usa_F$Age[usa_F$variable=="m95"] <- 95
usa_F$Age[usa_F$variable=="m100p"] <- 100

usa_F <- as.data.table(usa_F)

names(usa_F)[4]<-"mx.tot.by.Caus"

# Classificazione cause
usa_F$Cause_Rev[usa_F$cause==1] <- 1 # inf
usa_F$Cause_Rev[usa_F$cause==2] <- 2 # neop
usa_F$Cause_Rev[usa_F$cause==3] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==4] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==5] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==6] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==7] <- 3 # CVD
usa_F$Cause_Rev[usa_F$cause==8] <- 3 # CVD
usa_F$Cause_Rev[usa_F$cause==9] <- 3 # CVD
usa_F$Cause_Rev[usa_F$cause==10] <- 4 # RESP
usa_F$Cause_Rev[usa_F$cause==11] <- 4 # RESP
usa_F$Cause_Rev[usa_F$cause==12] <- 5 # DIG
usa_F$Cause_Rev[usa_F$cause==13] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==14] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==15] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==16] <- 6 # ext

usa_F$mx.tot.by.Caus <- usa_F$mx.tot.by.Caus/1000000

usa_F %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(mx.tot.by.Caus) %>%  sum()
#0.000172
usa_F=usa_F[,.(mx.tot.by.Caus = sum(mx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]
usa_F %>% filter(Cause_Rev==3)
usa_F %>%filter(year==1979) %>% ggplot(aes(Age,log(mx.tot.by.Caus)))+geom_line()+facet_wrap(~Cause_Rev)
usa_F$Sex <- "F"

save(usa_F,file="usa_F.RData") 

####################
#
# Uniamo i dati di M & F
#
####################

load("usa_M.RData")


names(usa_F)[4]<-"mx.tot.by.Caus_F"
head(usa_F)

tot <- cbind(usa_F,usa_M)  #unisce vettori

tot$gender_gap <- tot$mx.tot.by.Caus/tot$mx.tot.by.Caus_F  #costruisco colonna GG rapporto tassi di mortalità M/F

tot <- tot[,c(1,2,3,11)]  #causa_morte/age/gg/coorte

tot %>%filter(year==2000) %>% ggplot(aes(Age,(gender_gap)))+geom_line()+facet_wrap(~Cause_Rev,scales = "free")
tot$Cohort <- tot$year-tot$Age
  
tot %>%filter(Age==25) %>% ggplot(aes(Cohort,(gender_gap)))+geom_line()+
  facet_wrap(~Cause_Rev,scales = "free")


save(tot,file="final_data.RData")

###########################################################
# decomporre il gender gap utilizzando il metodo Candecomp-Parafac (CP) all’interno del pacchetto R “ThreeWay” 
# con la funzione “CP”.

# input in forma di array e delle variabili in base alle quali effettuare la decomposizione (cause di morte, età, coorte).

library("ThreeWay")

# etichette 

laba <- unique(tot$Cause_Rev)
labb <- unique(tot$Age)
labc <- unique(tot$Cohort)  

#view(laba) --> cuase di morte: 7 elementi
#view(labb) ---> age a gruppi da 5 anni: 22 elementi
#view(labc) --> coorte dal 1879 al 2018: 140 elementi

# creazione array()

dim3 <- nrow(tot)/(length(laba)*length(labb))
tot2=permnew(tot[,4],7,22,dim3)  #permutazione dell'array 
tot3=permnew(tot2,22,dim3,7)  #permutazione dell'array 
tot4=permnew(tot3,dim3,7,22)  #permutazione dell'array 

tot_cp <- CP(tot4,laba,labb,labc)

tot_cp$fit

save(tot_cp, file="tot_cp.RData")





