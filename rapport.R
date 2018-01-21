#' ---
#' title: Projet Régression 2017
#' author: Bénédicte <>, Xian YANG <xian.yang@etu.univ-lyon1.fr> 
#' date: 24 déc. 2017
#' output:
#'    html_document:
#'      toc: true
#'      theme: spacelab
#'      highlight: kate
#' ---
#+ echo=FALSE, message=FALSE, results='hide', warning=FALSE
set.seed(11715490)
library(usdm)
library(glmnet)
library(readxl)
library(MASS)
library(Blossom)
library(ggfortify)
library(glmnet)
library(caret)
library(gvlma)
library(qpcR)
library(L1pack)
# library(car)
# library(tidyverse)
#+ echo = FALSE, results = "hide"
dfSansObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/StepwiseSelectionLAD/FW_groupe3.xls")
dfAvecObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/StepwiseSelectionLAD/FW_groupe3_obs.xls")
names(dfSansObs)[1] <- "rowname"
names(dfAvecObs)[1] <- "rowname"
head(dfSansObs)
#' ## 0. Introduction
#' In this article we will present four common feature selection techniques in regression problems with the help of some 
#' chemical reaction data. The data devide into two parts: the first dataset consists of 100 chemical substances with each
#' having 50 numeric measurements, called descriptors. The second one consists of 25 chemical substances with the same
#' measurements, apart from the fact that to each of them an additional numeric mark is given, that we will call response.
#' The response occupies the second column of the second dataset, which looks like below
#+ message=FALSE, warning=FALSE
head(dfAvecObs, n = 5L)
dim(dfSansObs) # dont une colonne est le nom d'observation
dim(dfAvecObs) # dont une colonne est le nom d'observation
#' Ici : faire une stat descriptive sur la variable à expliquer  
#' The objective of this article, is to predict the response using the 50 descriptors of the chemical substances
#' of the second dataset. According to the project requirements we shall use linear regression models as a baseline, while
#' we will try different feature selection methods to avoid overfitting and to keep our model simple to explain. The article 
#' is organized as follows:   
#'   
#' We are interested first in finding a suitable ordinary linear regression model, which can possibly include interactions of second degree, using the
#' substances of the second dataset. But with a number of variables much greater than that of observations, how can we ever
#' run a regression model, even with the goal to result in fewer but persuassive variables? Our answer is to take advantage of
#' the first dataset without response by doing an unsupervised feature selection on it prior to the regression. In the second paragragh of
#' the article, we will apply principally an iterative selection procedure using VIF (Variation Inflation Factor) to the first dataset, 
#' while providing 
#' an alternative called sparse PCA before we discuss shortly its pros and contras. Now that we have fewer descriptors
#' left, we are going to run in parapragh III an OLS regression model with these descriptors and their interactions of second 
#' degree on the second dataset. In order to further avoid overfitting, a stepwise
#' selection of variables with AIC and BIC criteria will needed. We then compare two slightly different linear regression models
#' resulting from both information criteria and assess their goodness of fit. 
#' 
#' Second, we want to try some other feature selection variants than stepwise. In paragraph IV we will run a LAD regression model with 
#' L-1 errors. In the last paragraph, we will add a Lasso and than an Elastic Net penalty term to the OLS regression model
#' to make the final coefficients sparse. 
#'   
#' ## I. Unsupervised stepwise feature selection among main effects using VIF
#+ echo = FALSE
VifEffetsPrincipaux <- vifstep(as.data.frame(dfSansObs[-1]), th = 10)
VifEffetsPrincipaux
variablesReduites <- VifEffetsPrincipaux@results$Variables
# VifEffetsPrincipaux@results
#' 
#' ## II. Supervised stepwise feature selection with interaction terms of degree 2 (3 main effects at most)
#'
#' Cette fonction prend 3 indices en entrée, effectue avec ces 3 variables et 3 interactions 
#' une sélection bidirectionnelle par AIC et 
#' retourne la PRESS du meilleur modèle (du plus petit AIC).
#+ results = "hide"
meilleurModele3Var <- function(i, j, k) {
  modeleComplet <- lm(reponse ~ .^2, 
                      data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables)[c(i, j, k)])])
  meilleurModele <- stepAIC(modeleComplet, direction = "both", trace = FALSE)
  press <- MPV::PRESS(meilleurModele)
  return(press)
}
#' Cette fonction, même qu'avant, retourne en revanche le modèle au lieu de sa PRESS.
#+ results = "hide"
meilleurModele3VarResume <- function(i, j, k) {
  modeleComplet <- lm(reponse ~ .^2, 
                      data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables)[c(i, j, k)])])
  meilleurModele <- stepAIC(modeleComplet, direction = "both", trace = FALSE)
}
#'
#' Mise en place d'un cube pour stocker les AIC 
RSS <- array(1000, dim = rep(length(variablesReduites), times = 3))
#' Parcourir tous les 3-uplets possibles et stocker la PRESS du meilleur modèle de chaque 3-uplet
#+ results = "hide"
lapply(1:(length(variablesReduites) - 2), function(i) {
  lapply((i+1):(length(variablesReduites) - 1), function(j) {
    lapply ((j+1):length(variablesReduites), function(k) RSS[i, j, k] <<- meilleurModele3Var(i, j, k) )
  })
})
minimum <- min(RSS)
#' Minimum global de la PRESS
#+ 
minimum
#' Quel modèle c'est ?
#+
which(minimum == RSS, arr.ind = TRUE)
#' Extraire ce modèle
#+
meill2 <- meilleurModele3VarResume(8, 17, 18)
#' Revoyons son AIC
#+
AIC(meill2)
#' Maintenant, on effectue une analyse sur le modèle de régression sélectionné.  
#' Regardons d'abord les graphes ensemble :
#+ 
autoplot(meill2, which = 1:6) # Outliers : 12 (le pire), 2, 17). High leverage : 24
#' On fait un test pour être sûr des valeurs abberantes :
#+ 
car::outlierTest(meill2) # Effectivement, 12 a le plus grand résidu studentisé.
#' En outres, est-ce que ce modèle passe toutes les hypothèses de la régression linéaire ?
summary(gvlma(meill2)) # Oui. En outre, R carré pas mal.

# Enlever le 12 d'abord ?
nouveau1 <- lm(meill2$terms, data = dfAvecObs[-12, ])
summary(nouveau1)
autoplot(nouveau1, which = 1:6)
car::outlierTest(nouveau1)
summary(gvlma(nouveau1))


nouveau2 <- lm(meill2$terms, data = dfAvecObs[-c(2, 12), ])

summary(nouveau2)
autoplot(nouveau2, which = 1:6)
car::outlierTest(nouveau2)
summary(gvlma(nouveau2)) # Assez bien au niveau graphique.

# nouveau21 <- lm(meill$terms, data = dfAvecObs[-c(3, 12, 16), ])
# summary(nouveau21)
# autoplot(nouveau21)
# car::outlierTest(nouveau21)
# summary(gvlma(nouveau21))

#' Qu'est-ce qu'il se passe si on enlève le point 24 au lieu du 12 au début ?
nouveau3 <- lm(meill2$terms, data = dfAvecObs[-c(12, 2, 17), ])
summary(nouveau3)
autoplot(nouveau3, which = 1:6)
car::outlierTest(nouveau3)
summary(gvlma(nouveau3))

nouveau4 <- lm(meill2$terms, data = dfAvecObs[-c(3, 24), ])
summary(nouveau4)
autoplot(nouveau4, which = 1:6)
car::outlierTest(nouveau4)
summary(gvlma(nouveau4))
# Enlever 24puis 3 peut être une autre option.

#' ## III. Méthode alternative : stepwise directement avec toutes les variables
#' On établit d'abord le modèle de départ et le modèle d'arrivé, entre lesquels on cherchera le modèle optimal. On accepte
#' toutes les variables qui restent après l'opération de recherche de linéarités préliminaire, ainsi que leurs interactions
#' de second degré.
#+ results = "hide"
modeleDepart <- lm(reponse ~ 1, 
                   data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
modeleComplet <- lm(reponse ~ .^2, 
                    data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
#' Maintenant on va efffctuer la sélection des variables stepwise, avec respctivement les critères AIC et BIC. 
#' Sachant que dans un contexte que le nombre de variables est égal au nombre d'observation, la sélection backward n'est 
#' pas possible, on peut alors choisir entre "forward" et "bidirectional". Du coup nous avons 4 combinaisons possible devant nous.  
#'   
#' Forward + AIC
#+ echo = FALSE
forwardAIC <- stepAIC(modeleDepart, direction = "forward", scope = list(upper = modeleComplet), trace = FALSE) 
#' AIC ends at `-Inf`, which means perfect fit. Not good. We don't take into account this result.  
#'   
#' Forward + BIC
#+ echo = FALSE
forwardBIC <- stepAIC(modeleDepart, direction = "forward", scope = list(upper = modeleComplet), k = log(nrow(dfAvecObs)), trace = FALSE) 
#' On arrive à un bon résultat avec trois variables de degré 1 et une interaction de degré 2.  
#'   
#' Bidirectional AIC
#+ echo = FALSE
bidirectionalAIC <- stepAIC(modeleDepart, direction = "both", scope = list(upper = modeleComplet), k = 2, trace = FALSE)
#' On arrive à un bon résultat avec quatre variables de degré 1 et deux interactions de degré 2.  
#'   
#' Bidirectional BIC
#+ echo = FALSE, results = "hide"
bidirectionalBIC <- stepAIC(modeleDepart, direction = "both", scope = list(upper = modeleComplet), k = log(nrow(dfAvecObs)), trace = FALSE)
#' Celui-ci done le même résultat que forward BIC.  
#'   
#' En fin du compte, on garde les deux modèles obtenus respectivement par forward BIC et bidirectional AIC. On peut ensuite
#' étudier leurs qualités de régression en ragardant certains critères.   
#' On peut d'abord regarder leurs R carrés.
#+
summary(forwardBIC)
summary(bidirectionalAIC)
#' Le deuxième modèle possède un R carré de 82% contre 63% chez le premier. De ce point de vu on préfèrerait opter pour le 
#' deuxième modèle avec quatre varibles et 2 interactions.
#' 
#' On peut par exemple comparer leurs scores d'AIC et de BIC :
#+
# class(step2$terms) # c'est la formule.
AIC(forwardBIC, bidirectionalAIC)
BIC(forwardBIC, bidirectionalAIC)
#' dire que le 2e est meilleur que le 1er.  
#' 
#' Both criteria say the second model is better with 2 more degrees. But one can questioning the meaning of comparing these two 
#' by AIC or BIC, since the complete
#' model will always give the best AIC and BIC but it's surely far from the best model, by all means.  
#'   
#' On peut encore chercher et virer les valeurs abberantes dans les modèles en regardant les graphes et en faisant des tests
#+
autoplot(forwardBIC, which = 1:6, label.size = 3)
summary(gvlma(forwardBIC))
car::outlierTest(forwardBIC)
autoplot(bidirectionalAIC, which = 1:6, label.size = 3)
summary(gvlma(bidirectionalAIC))
car::outlierTest(bidirectionalAIC)
#' On voit cette fois-ci une qualité de régression acceptable. Donc le résultat : en enlevant l'observation 24, les variables
#' qu'on garde pour la régression sont
#'
attr(bidirectionalAIC$terms, "term.labels")
#' ## IV. Least absolute deviation regression
#' 
#' Matrice pour stocker les PRESS
#+ echo = FALSE, results = "hide"
RSS2 <- array(1000, dim = rep(length(variablesReduites), times = 3))
#' On parcourt tous les 3-uplets des variables explicatives et construit un modèle de LAD. Puis, en comparant la PRESS
#' de tous ces modèles trouvés, on en prend celui qui a la plus petite PRESS.
#+ echo = FALSE, results = "hide"
lapply(1:(length(variablesReduites) - 2), function(i) {
  lapply((i+1):(length(variablesReduites) - 1), function(j) {
    lapply ((j+1):length(variablesReduites), function(k) {
      ladFit <- L1pack::lad(reponse ~ .^2, data = dfAvecObs[c("reponse", as.character(variablesReduites)[c(i, j, k)])])
      press <- sum(sapply(1:25, function(l) {
           (dfAvecObs[l, "reponse"] - predict(ladFit, data = dfAvecObs[-l, ]))^2
      }))
      RSS2[i, j, k] <<- press
    })
  })
})
#' Cherchons la plus petite PRESS
#+
minimum2 <- min(RSS2)
minimum2
#' Quel modèle c'est ?
#+ echo = FALSE
which(minimum2 == RSS2, arr.ind = TRUE)
#' On étudie ce modèle
#+ echo = FALSE
lad.bestFit <- lad(reponse ~ .^2, data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables)[c(8, 12, 18)])])
summary(lad.bestFit)
oldpar <- par(mfrow = c(1,2))
plot(lad.bestFit, which = 1:2)
par(oldpar)
lad.bestFit2 <- lad(reponse ~ .^2 - descripteur30:descripteur68, data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables)[c(8, 12, 18)])])
summary(lad.bestFit2)
oldpar <- par(mfrow = c(1,2))
plot(lad.bestFit2, which = 1:2)
par(oldpar)

lad.bestFit3 <- lad(reponse ~ .^2 - descripteur30:descripteur68, data = dfAvecObs[-c(5, 4, 12), c("reponse", as.character(VifEffetsPrincipaux@results$Variables)[c(8, 12, 18)])])
summary(lad.bestFit3)
oldpar <- par(mfrow = c(1,2))
plot(lad.bestFit3, which = 1:2)
par(oldpar)

#' -------- FIN ----------