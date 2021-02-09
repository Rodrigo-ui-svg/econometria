setwd("D:/ECONOMETRIA CLASE/Logistico/RELIGION")

#Paquetes
library(tidyverse)
library(lme4)
library(readstata13)
library(jtools)

#base de datos
religion <- read.dta13("religiosidad_small.dta")

colnames(religion) #Nombre de las columnas 
table(religion$relatt) # asistencia serv. relig
prop.table(table(religion$relatt))*100

length(unique(religion$COUNTRY)) #60 países en la base de datos 


#Distribucion del ingreso atipica en Turquia 
religion %>% 
  filter(COUNTRY == "Turkey") %>% 
  ggplot(aes(income)) +
  geom_boxplot()

#Se elimina Turquia
religion <- religion %>% 
  filter(COUNTRY != "Turkey")

#
Modelo1 <- glm(relatt ~ 1, data = religion, family = binomial(link = "logit"))
Modelo2 <- glmer(relatt ~ 1+(1|countryn), family = binomial("logit"), 
                 data = religion)
# -2(l1-l2)
-2*(logLik(Modelo1)-logLik(Modelo2))
chinorm



# Modelo nulo
M0 <- glmer(relatt ~ 1 + (1 | countryn), family = binomial("logit"), 
            data = religion) 
summary(M0)
summ(M0)


#modelo completo 
M1 <- glmer(relatt ~1+educ+income+unemp+female+single+divorced+widowed+
              gini+educ_c+unemp_c+divorce_c + (1|countryn), 
            family = binomial("logit"), data = religion)
summary(M1)
summ(M1)
fixef(M1) #fixed effects

# razon de momios 
exp(fixef(M1))








