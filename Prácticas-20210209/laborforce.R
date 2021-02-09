setwd("D:/ECONOMETRIA CLASE/Logistico")

# install.packages("pROC")

#cargamos paquetes
library(tidyverse)
library(readstata13)
library(lmtest)
library(pROC)

#cargamos base de datos 
labor <- read.dta13("laborforce.dta")
head(labor)

unique(labor$lfp)
unique(labor$agecat)
unique(labor$wc)
unique(labor$hc)

#Hacemos cambios a la base de datos 
labor <- labor %>% 
  mutate(lfp = case_when(lfp == "NotInLF" ~ 0, 
                         lfp == "inLF" ~ 1), 
         agecat = case_when(agecat == "30-39" ~ 1,
                            agecat == "40-49" ~ 2, 
                            agecat == "50-60" ~ 3), 
         wc = case_when(wc == "NoCol" ~ 0, 
                        wc == "College" ~ 1), 
         hc = case_when(hc == "NoCol" ~ 0,
                        hc == "College" ~ 1))


# Modelo logit 
Mlogit <- glm(lfp ~ k5+k618+age+wc+hc+lwg+inc, data = labor, 
              family = binomial(link = "logit"))
summary(Mlogit)


# Modelo probit 
Mprobit <- glm(lfp ~ k5+k618+age+wc+hc+lwg+inc, data = labor, 
              family = binomial(link = "probit"))
summary(Mprobit)


#Modelando variables dicotomicas 
labor$k5_1 <- 0
labor$k5_1 <- replace(labor$k5_1, labor$k5==1, 1)
labor$k5_2 <- 0
labor$k5_2 <- replace(labor$k5_2, labor$k5==2, 1)
labor$k5_3 <- 0
labor$k5_3 <- replace(labor$k5_3, labor$k5==3, 1)


logit2 <- glm(lfp ~ k5_1 + k5_2 + k5_3, data = labor, 
               family = binomial(link = "logit"))
summary(logit2)

#En la tercer categoria de k5 hay 3 observaciones solamente
table(labor$lfp, labor$k5)

# Tambien se puede realizar de la siguiente forma 
class(labor$k5)
labor$k5 <- as.factor(labor$k5)

logit2_ <- glm(lfp ~ k5, data = labor, family = binomial(link = "logit"))
summary(logit2_)


#Variables independientes categoricas
labor$k5 <- as.numeric(labor$k5)
labor$agecat <- as.factor(labor$agecat)
labor$wc <- as.factor(labor$wc)
labor$hc <- as.factor(labor$hc)

logit3 <- glm(lfp ~ k5+ k618+agecat+wc+hc+lwg+inc, 
              data = labor, family = binomial(link = "logit"))
summary(logit3)


# odds ratio 
exp(logit3$coefficients)


#probando diferentes modelos 
M1 <- glm(lfp ~ k5+ k618+agecat+wc+hc+lwg+inc, 
              data = labor, family = binomial(link = "logit"))

M2 <- glm(lfp ~ k618+agecat+wc+hc+lwg+inc, 
          data = labor, family = binomial(link = "logit"))

M3 <- glm(lfp ~ k5+agecat+wc+hc+lwg+inc, 
    data = labor, family = binomial(link = "logit"))

# El efecto de eliminar la variable k5 es significativa al 5%, y eliminar 
# la variable k618 no lo es
lrtest(M1, M2) 
lrtest(M1, M3)




## Matriz de confusion 

## en stata, despues de correr la regresion
## estat class

predicted <-predict(M3, labor, type = "response")
range(predicted)

table(predicted>0.5, labor$lfp)

cat("Correctamente clasificados=",(182+333)/753)
cat("Sensibilidad Pr(+|inLF) = ", 333/(333+95))
cat("Especificidad Pr(-|NotInLF) = ", 182/(182+143))


# ROC (Receiver Operator Characteristic)
#En stata lroc

roc <- roc(labor$lfp, predicted)
plot(roc)
auc(roc) #Area bajo la curva

#Gráfica
roc <- plot.roc(labor$lfp, predicted,
                main="Curva ROC", 
                percent=TRUE,
                print.auc=TRUE) 

plot(ci(roc, of="thresholds", thresholds="best")) 

roc<-plot.roc(labor$lfp, predicted, 
              main="Curva ROC",
              percent=TRUE,
              of="thresholds",
              thresholds="best",
              print.thres="best")




