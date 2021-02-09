setwd("C:/Users/THINKPAD T560S/Documents/ECONOMETRIA CLASE/Scripts del curso/HLM/Datos Multinivel")

# install.packages("lme4")
# install.packages("merTools")
# install.packages("jtools")
# install.packages("lmerTest")
# install.packages(lmtest)
# install.packages("arm")

# Paquetes 
library(lmtest)
library(readstata13)
library(lme4)
library(merTools)
library(jtools) # summary model
library(lmerTest)# Likelihood ratio test
library(dplyr)
library(ggplot2)
library(readxl)
library(arm)

#importamos base de datos
base <-read.dta13("gcse.dta")

#Variable de respuesta : gcse 

#Variables predictoras
  # nivel 1 : lrt, girl 
  # nivel 2 : schgend

str(base) #school, girl, schgend son strings, las cambiamos a factor 

base$school <-as.factor(base$school) 
base$girl <- as.factor(base$girl)
base$schgend <-  as.factor(base$schgend)

# Relacion entre LRT y GSCE 

# Para la escuela 1 n=1 
base %>% 
  filter(school == 1) %>% 
  ggplot(aes(x=lrt, y=gcse)) +
  geom_point() +
  geom_smooth(method = "lm", se=F)

# a forma de ejemplo 
base %>% 
  filter(school %in% c(1,20,25,47,63)) %>% 
  ggplot(aes(x=lrt, y=gcse)) +
  geom_point(aes(col=school)) +
  geom_smooth(aes(col=school), method = "lm", se=F)


# rectas de regresion para todas las escuelas 
base %>% 
  ggplot(aes(x=lrt, y=gcse)) +
  geom_smooth(aes(group=school), method = "lm", se=F, col="steelblue")


# Modelo nulo con intercepto aleatorio 

M0<-lmer(gcse~1+(1|school),REML=FALSE,data=base)
summary(M0)
summ(M0)


#Prueba estadistica para el intercepto aleatorio 

# H0:t = 0 v.s. H1:t > 0

# L = 2 (l1 - l0)

# l1 = modelo con intercepto aleatorio 
# l0 = modelo sin intercepto aleatorio 
M0_ols <- lm(data = base, base$gcse ~1)

# Nuestro estadistico de prueba sigue una distribucion chi-cuadrado con grados 
# de libertad iguales a la diferencia en el numero de parametros libres entre 
# el modelo complejo y el modelo anidado. Con esta informacion, podemos calcular 
# el valor p, y si es menor que nuestro nivel de significancia, rechazamos la hip nula.

l1 <- logLik(M0) # 3 grados de libertad
l0 <- logLik(M0_ols) # 2 grados de libertad
L <- 2*(l1-l0)


# Sacamos el p-value. Comparamos con un estadistico de tablas con grados de libertad 
# df = 3 -2 = 1
p_value <- pchisq(L, df = 1, lower.tail = FALSE)

#Rechazamos hipotesis nula. El modelo con interceptos aleatorios ajusta mejor 


#Likelihood ratio test
rand(M0)

#Coeficiente de correlacion intraclase ICC 
ICC(outcome="gcse",group="school",data=base)


# Intercepto aleatorio con 1 variable predictora 

M1 <-lmer(gcse~1 + lrt + (1|school),REML=FALSE,data=base)
summary(M1)
summ(M1)

# Modelo con intercepto y pendiente aleatoria 
M2 <-lmer(gcse~1 + lrt + (1+lrt|school),REML=FALSE,data=base)
summary(M2)
summ(M2)

rand(M2)
ranova(M2)
# Con lo cual rechazamos Ho, y se concluye que el intercepto y la pendientes son
# aleatorios


## Estimadores de efectos aleatorios 
ranef(M2) #random effects
Estimadores <- ranef(M2) #Se guardan en forma de lista

Interceptos <- Estimadores[["school"]][["(Intercept)"]] # Acceden al vector de interceptos
Pendientes <- Estimadores[["school"]][["lrt"]] #Acceden al vector de pendientes 

x <- data.frame(Interceptos = Interceptos, 
                Pendientes = Pendientes, 
                Escuela = c(1:65))

#distribucion de pendientes e interceptos 
x %>% 
  ggplot(aes(Interceptos)) +
  geom_histogram(binwidth = 3, col="black", fill="firebrick") 

x %>% 
  ggplot(aes(Pendientes)) +
  geom_histogram(binwidth = .07, col="black", fill="firebrick") 

#normalidad de residuales del nivel 1 
residuals(M2)
residuales <- data.frame(Res_n1= residuals(M2))

# grafica
residuales %>% 
  ggplot(aes(Res_n1))+
  geom_histogram(binwidth = 5, col= "black", fill="goldenrod")


#ranking de escuelas por intercepto

# Error estandar 
SE <- se.ranef(M2)
SE <- as.data.frame(SE[["school"]])

#agregamos los errores estandar a la base 
names(SE) <- c("SE_intercepto", "SE_pendiente")
x$SE_intercepto <- SE$SE_intercepto
x$SE_pendiente <- SE$SE_pendiente
x

#Necesitamos calcular ahora intervalos de confianza 
x <- x %>% 
  mutate(int_low_95 = Interceptos - (1.96*SE_intercepto), 
         int_sup_95 = Interceptos + (1.96*SE_intercepto))


#Graficamos 
ranking <-  x %>% 
  arrange(Interceptos) %>% 
  ggplot(aes(x= reorder(Escuela,Interceptos), y=Interceptos)) +
  geom_point(col="steelblue", size=2) +
  geom_errorbar(aes(ymin=int_low_95, ymax=int_sup_95)) +
  theme_minimal(base_size = 12)+
  labs(title = "Ranking de escuelas por interceptos", 
       x="Escuela") 

# install.packages("plotly")
library(plotly)
ggplotly(ranking)

base %>% 
  select(school, student) %>% 
  group_by(school) %>% 
  summarise(n = n()) %>% 
  View()




