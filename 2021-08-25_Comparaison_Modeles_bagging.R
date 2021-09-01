rm(list =ls())
options(warn=-1)
setwd("C:/Users/ASUS/Documents/06 noart/030 datascience/Exercice 01/01 data")
market = read.table("market.csv", sep=";", dec=".", header=T)

# ================================================================ #
#                                                                  #
#          N E T T O Y A G E     D E S    D O N N E E S            #
#                                                                  #
#                        M  A  R  K  E  T                          #
#                                                                  #
# ================================================================ #
# --------------------------------------------------------------- #
# num + facteurs                                                  #
# --------------------------------------------------------------- #
market$prod_cost   =as.numeric(market$prod_cost)
market$product_type=as.factor(market$product_type)
market$quality     =as.factor(market$quality)

market$warranty=substr(market$warranty, 1,1) 
market$warranty=as.factor(market$warranty)

hist(market$prod_cost)

# --------------------------------------------------------------- #  
# valeurs manquantes                                              #
# --------------------------------------------------------------- #eigen() decomposition
sapply(market,function(x) sum(is.na(x)))
market$prod_cost[is.na(market$prod_cost)] <- median(market$prod_cost, na.rm=T)
sum(is.na(market))

# --------------------------------------------------------------- #
# Normalité des données non factorielles et non ID                #
# --------------------------------------------------------------- #
str(market)
unclass(market$product_type)
# shapiro test marche sur factors ? non uniqt variables quanti + tests linéaires en général
shapiro.test(market$capacity)
shapiro.test(market$failure_rate)
shapiro.test(market$margin)
shapiro.test(market$price)
shapiro.test(market$prod_cost)
shapiro.test(market$market_share)
# p-value < 2.2e-16 pour toutes les données

# ================================================================ #
#                                                                  #
#      T A B L E S    D' E C H A N T I L L O N N A G E             #
#                                                                  #
#             80%  -----------   20%                               #
#                                                                  #
# ================================================================ #
ratio = 0.2
n = dim(market)[1]
p = dim(market)[2]
ntest = ceiling(n*ratio)
# ------------------------------------------------------------- #
# test20           =  280  =  20% de 1399                       #
# appr80           = 1119  =  80% de 1399                       #
# ------------------------------------------------------------- #
test20 =  sample(1:n, ntest)
appr80 = setdiff(1:n, test20)
# ------------------------------------------------------------- #
#construction de l'échantillon d'apprentissage de 80% avec la table appr80
# ------------------------------------------------------------- #
data_train80 = market[appr80, ]
data_test20  = market[test20, ]
# ------------------------------------------------------------- #
# Xtrain80 et Xtest20  = tout sauf attractivness                #
# Ytrain80 et Ytest20 = attractiveness                          #
# ------------------------------------------------------------- #
Xtrain80  =  data_train80[ ,-11]
Xtest20   =   data_test20[ ,-11]
Ytrain80  =    data_train80[,11]
Ytest20   =     data_test20[,11]

# ===================================================================== #
#                                                                       #
#               F O N C T I O N        R M S L E                        #
#                  Root Mean Squared Log Error                          #
#                                                                       #
# ===================================================================== #
rmsle <- function(valeur_reelle, valeur_predite) {
  res =  sqrt(mean((log(valeur_reelle + 1) - log(valeur_predite + 1))^2))
  return(res)
}
# ===================================================================== #
#                                                                       #
#         M A T R I C E     D E S     M O D E L E S                     #
#             RESULTATS RMSLE SUR CHAQUE MODELE                         #
#                               "modele.SVM1",                          #   
# ===================================================================== #

modeles= c("modele.RN","modele.boosting","modele.bagging","modele.SVM","modele.naif1", "modele.naif2", "modele.UnsurX", "modele.log", "modele.sqrt", "AIC.backward", "AIC.forward", "AIC.step",   "PLS0", "modele.RF")
compar_erreurs = rep( 0, length((modeles)))
names(compar_erreurs)= modeles

# ===================================================================== #
#                                                                       #
#        E X E C U T I O N     D E S     M O D E L E S                  # 
#             + CALCUL DU RMSLE > DANS TABLE COMPAR                     #
#                                                                       #   
# ===================================================================== #

# ------------------------------------------------------------- #
# M O D E L E    N A I F     1    toutes les données            #
# ------------------------------------------------------------- #
formula1 = attractiveness ~ capacity+failure_rate+margin+price+prod_cost+product_type+quality+warranty+market_share
reglin1=lm(formula1,data=data_train80)
data_test20$pred1 =  predict(reglin1, newdata = data_test20)
compar_erreurs["modele.naif1"] <- rmsle(data_test20$attractiveness, data_test20$pred1)
compar_erreurs
# ------------------------------------------------------------- #
# M O D E L E    N A I F     2    données significatives        #
# ------------------------------------------------------------- #
formula2 = attractiveness~capacity+margin+product_type
reglin2=lm(formula2,data=data_train80)
data_test20$pred2 =  predict(reglin2, newdata = data_test20)
compar_erreurs["modele.naif2"] = rmsle(data_test20$attractiveness, data_test20$pred2)
compar_erreurs

# ------------------------------------------------------------- #
#  M O D E L E    U N S U R X                                   #
# ------------------------------------------------------------- #
formula3=attractiveness~ 1/capacity + 1/failure_rate +1/margin+ 1/price+prod_cost+product_type+ quality+warranty+1/market_share
reglin3=lm(formula3,data=data_train80)
data_test20$pred3 =  predict(reglin3, newdata = data_test20)
result = rmsle(data_test20$attractiveness, data_test20$pred3)
compar_erreurs["modele.UnsurX"] = result
compar_erreurs
# ------------------------------------------------------------- #
#  M O D E L E    L O G                                         #
# ------------------------------------------------------------- #
formula4=attractiveness~ log(capacity)+log(failure_rate)+log(margin)+log(price+prod_cost)+product_type+quality+warranty+log(market_share)
reglin4=lm(formula4,data=data_train80)
data_test20$pred4 =  predict(reglin4, newdata = data_test20)
result = rmsle(data_test20$attractiveness, data_test20$pred4)
compar_erreurs["modele.log"] = result
compar_erreurs
# ------------------------------------------------------------- #
#  M O D E L E    S Q R T                                       #
# ------------------------------------------------------------- #
formula5=attractiveness~ sqrt(abs(capacity))+sqrt(abs(failure_rate))+sqrt(abs(margin))+sqrt(abs(price+prod_cost))+product_type+quality+warranty+sqrt(abs(market_share))
reglin5=lm(formula5,data=data_train80)
data_test20$pred5 =  predict(reglin5, newdata = data_test20)
result = rmsle(data_test20$attractiveness, data_test20$pred5)
compar_erreurs["modele.sqrt"] = result
compar_erreurs
# ------------------------------------------------------------- #
#  M O D E L E    A I C . B A C K W A R D                       #
# ------------------------------------------------------------- #
if (!require("MASS")) install.packages("MASS")
library(MASS)
#-----------------
AIC.backward = stepAIC(reglin1, d="backward")
pred.AIC.backward        <- predict(AIC.backward, newdata=data_test20) 
compar_erreurs["AIC.backward"] <- rmsle(pred.AIC.backward,Ytest20)
compar_erreurs
# ------------------------------------------------------------- #
#  M O D E L E    A I C . F O R W A R D                         #
# ------------------------------------------------------------- #
AIC.forward = stepAIC(reglin1, d="forward")
pred.AIC.forward <- predict(AIC.forward, newdata=data_test20) 
compar_erreurs["AIC.forward"] <- rmsle(pred.AIC.forward,Ytest20)
compar_erreurs
# ------------------------------------------------------------- #
#  M O D E L E    S T E P W I S E   B O T H                     #
# ------------------------------------------------------------- #
AIC.stepwise = stepAIC(reglin1, d="both")
pred.AIC.stepwise   <- predict(AIC.stepwise, newdata=data_test20) 
compar_erreurs["AIC.step"] <- rmsle(pred.AIC.stepwise,Ytest20)
compar_erreurs
# ------------------------------------------------------------- #
#  M O D E L E    P L S                                         #
# ------------------------------------------------------------- #
if (!require("pls")) install.packages("pls")
library(pls)
PLS0 = plsr(formula1, data=data_train80, validation = "CV")
#nombre de composantes choisies
ncomp = length((coef(PLS0)))
#calcul des prédictions
pred.PLS0        <- predict(PLS0, newdata=data_test20)
#calcul erreur ds table 
compar_erreurs["PLS0"] <- rmsle(pred.PLS0,Ytest20)

# ------------------------------------------------------------- #
#  M O D E L E    R A N D O M   F O R E S T                     #
# ------------------------------------------------------------- #
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)
# utilisation de formula1 = attractiveness ~ capacity+failure_rate+margin+price+prod_cost+product_type+quality+warranty+market_share
# augmenter drastiquement le nombre d'arbres n'améliore pas significativement le taux d'erreur, mais multipliera le temps de calcul.

modelRF <- randomForest(formula1, data =data_train80, ntree = 5000, na.action = na.omit)
data_test20$pred6 =  predict(modelRF, newdata = data_test20)
compar_erreurs["modele.RF"] = rmsle(data_test20$attractiveness, data_test20$pred6)
compar_erreurs

# ------------------------------------------------------------- #
#  M O D E L E    S V M                      
# noyaux sigmoïde (1/(1+e^-x)), radial, linéaire
# https://scikit-learn.org/stable/modules/svm.html
# https://fr.wikipedia.org/wiki/Hyperparam%C3%A8tre
# https://lrouviere.github.io/TUTO_ML/SVM.html
# ------------------------------------------------------------- #
#===================chargement librairie
if (!require("e1071")) install.packages("e1071")
library(e1071)

#===================optimisation des paramètres SVM
obj = tune.svm(formula1, data = data_train80, gamma = 2^(-10:-1), cost = 2^(2:4))
summary(obj) 
plot(obj)
#===================Modele SVM
modelSVM <- svm(formula1, data=data_train80, scale = TRUE, gamma = 0.4, cost = 8)
data_test20$predSVM1 =  predict(modelSVM1, newdata = data_test20)
compar_erreurs["modele.SVM"] = rmsle(data_test20$attractiveness, data_test20$predSVM1)
compar_erreurs

# ------------------------------------------------------------------------- #
#             M O D E L E    B A G G I N G                                  #
#                 REGRESSION AVEC R                                         #
# modèle d'aggrégation de modèles avec échantillon indépendant aléatoire    # 
# calcule plusieurs prédicteurs et fait la moyenne des résutats             #
# obtenus par chaque échantillon                                            #
# permet de réduire la variance                                             #
# utilise le bootstrap (amorce) comme méthode d'échantillonage avec remise  #
# ------------------------------------------------------------------------- #
#===================chargement librairie
if (!require("ipred")) install.packages("ipred")
library(ipred)
#===================modèle bagging
model_bagging <- bagging(formula1, data=data_train80, scale = TRUE)
data_test20$predbag =  predict(model_bagging, newdata = data_test20)
compar_erreurs["modele.bagging"] = rmsle(data_test20$attractiveness, data_test20$predbag)
compar_erreurs

# ------------------------------------------------------------------------- #
#             M O D E L E    B O O S T I N G                                #
#                 REGRESSION AVEC R                                         #
#  librairy
# https://perso.univ-rennes2.fr/system/files/users/rouviere_l/poly_apprentissage.pdf
#                                                                                     #
# liens saber
# https://www.math.univ-toulouse.fr/~besse/Wikistat/pdf/st-m-app-agreg.pdf
# codes nécessaires pour faire de l'aggrégation de modèles
# https://www.math.univ-toulouse.fr/~besse/Wikistat/pdf/st-scenar-app-ozone-meteoF.pdf
#
# ------------------------------------------------------------------------- #
#===================chargement librairie
if (!require("gbm")) install.packages("gbm")
library(gbm)
#===================modèle bagging
model_boosting <- gbm(formula1, data=data_train80)
data_test20$predboost  =  predict(model_boosting, newdata = data_test20, n.trees=4000, n.minobsinnode = 5, shrinkage=0.01,verbose=TRUE)
compar_erreurs["modele.boosting"] = rmsle(data_test20$attractiveness, data_test20$predboost)
compar_erreurs

# ------------------------------------------------------------------------- #
#             M O D E L E    R E S E A U X   DE N E U R O N E              #
# réseaux de neurones sont trop performants car pbm surapprentissage
# ------------------------------------------------------------------------- #
#===================chargement librairie
if (!require("e1071")) install.packages("e1071")
library(e1071)
library(nnet)
#===================modèle bagging
# model_rn <- 
plot(tune.nnet(formula1,data=data_train80,size=c(2,3,4),
               decay=3,maxit=2,linout=TRUE))
?nnet
# d'après le schéma decay = 3 et maxit = 2 
model_RN <- nnet(formula1, data=data_train80, size=3, decay=3,maxit=2)
data_test20$predrn  =  predict(model_RN, newdata = data_test20)
compar_erreurs["modele.RN"] = rmsle(data_test20$attractiveness, data_test20$predrn)
compar_erreurs


# ------------------------------------------------------------- #
#  R E S U L T A T                                              #
# ------------------------------------------------------------- #
(compar_erreurs_tri  <-  sort(compar_erreurs))