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
# library(car)
# library(tidyverse)
dfSansObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/StepwiseSelectionLAD/FW_groupe3.xls")
dfAvecObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/StepwiseSelectionLAD/FW_groupe3_obs.xls")
names(dfSansObs)[1] <- "rowname"
names(dfAvecObs)[1] <- "rowname"
head(dfSansObs)

dim(dfSansObs)
dim(dfAvecObs)
#' ## 0. Introduction
#' In this article we will present four common feature selection techniques in regression problems with the help of some 
#' chemical reaction data. The data devide into two parts: the first dataset consists of 100 chemical substances with each
#' having 50 numeric measurements, called descriptors. The second one consists of 25 chemical substances with the same
#' measurements, apart from the fact that to each of them an additional numeric mark is given, that we will call response.
#' The response occupies the first column of the second dataset, which looks like below
#+ message=FALSE, warning=FALSE
head(dfAvecObs, n = 5L)
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
#+
VifEffetsPrincipaux <- vifstep(as.data.frame(dfSansObs[-1]), th = 10)
VifEffetsPrincipaux
# VifEffetsPrincipaux@results
#' 
#' ## II. Supervised stepwise feature selection with interaction terms of degree 2
#' On établit d'abord le modèle de départ et le modèle d'arrivé, entre lesquels on cherchera le modèle optimal. On accepte
#' toutes les variables qui restent après l'opération de recherche de linéarités préliminaire, ainsi que leurs interactions
#' de second degré.
#+
modeleDepart <- lm(reponse ~ 1, 
                   data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
modeleComplet <- lm(reponse ~ .^2, 
                    data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
#' Maintenant on va efffctuer la sélection des variables stepwise, avec respctivement les critères AIC et BIC. 
#' Sachant que dans un contexte que le nombre de variables est égal au nombre d'observation, la sélection backward n'est 
#' pas possible, on peut alors choisir entre "forward" et "bidirectional". Du coup nous avons 4 combinaisons possible devant nous.  
#'   
#' Forward + AIC
#+
forwardAIC <- stepAIC(modeleDepart, direction = "forward", scope = list(upper = modeleComplet)) 
#' AIC ends at `-Inf`, which means perfect fit. Not good. We don't take into account this result.  
#'   
#' Forward + BIC
#+
forwardBIC <- stepAIC(modeleDepart, direction = "forward", scope = list(upper = modeleComplet), k = log(nrow(dfAvecObs))) 
#' On arrive à un bon résultat avec trois variables de degré 1 et une interaction de degré 2.  
#'   
#' Bidirectional AIC
#+
bidirectionalAIC <- stepAIC(modeleDepart, direction = "both", scope = list(upper = modeleComplet), k = 2)
#' On arrive à un bon résultat avec quatre variables de degré 1 et deux interactions de degré 2.  
#'   
#' Bidirectional BIC  
bidirectionalBIC <- stepAIC(modeleDepart, direction = "both", scope = list(upper = modeleComplet), k = log(nrow(dfAvecObs)))
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
#' Both criteria say the second model is better with 2 more degrees. But one can questioning the meaning of comparing these two 
#' by AIC or BIC, since the complete
#' model will always give the best AIC and BIC but it's surely far from the best model, by all means.  
#'   
#' On peut encore chercher et virer les valeurs abberantes dans les modèles en regardant d'abord la distance de Cook
#+
oldPar <- par(mfrow = c(2, 3))
autoplot(forwardBIC, which = 1:6, label.size = 3)
autoplot(bidirectionalAIC, which = 1:6, label.size = 3)

#' Tous les deux modèles suggèrent que l'observation 24 est une valeur abberante. On va l'enlever et regarder à nouveau
#' les distances de Cook.
#+ 
nouveauFit1 <- lm(forwardBIC$terms, data = dfAvecObs[-24, ])
nouveauFit2 <- lm(bidirectionalAIC, data = dfAvecObs[-24, ])
summary(nouveauFit1)
summary(nouveauFit2)
#' On voit que le R carré du premier nouveau modèle sans l'observation 24 est inacceptable (50%). En plus, il ne  
#' satisfait pas non plus à l'hypothèse de normalité, comme le montre le graphe suivant : 
#+
autoplot(lm(forwardBIC, data = dfAvecObs[-24, ]),  which = 2, label.size = 3)
#' On ne va considérer que le deuxième nouveau modèle.
#+
autoplot(lm(bidirectionalAIC, data = dfAvecObs[-24, ]),  which = 1:6, label.size = 3)
par(oldPar)
#' On voit cette fois-ci une qualité de régression acceptable. Donc le résultat : en enlevant l'observation 24, les variables
#' qu'on garde pour la régression sont
#+
attr(bidirectionalAIC$terms, "term.labels")


#' ## Least absolute deviation regression
#' On essie d'abord de faire une régression de L1 avec les interaction de degré 2.
#+
library(Blossom)
lad.fit2 <- lad(reponse ~ .^2, data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
# summary(lad.fit2)
data.frame(variables = colnames(lad.fit2@inputData)[coefficients(lad.fit2) != 0], 
          coeff = coefficients(lad.fit2)[coefficients(lad.fit2) != 0])
#' Conclusion : le résultat de la LAD viole la règle du modèle régression, ce qui est un enjeu en particuler dans la sélection 
#' de molécules : si on prend en compte une interaction, on doit aussi prendre en compte les varibles qui la constituent.
#' Du coup le nombre de régresseurs sera trop.   
#'   
#' On essaie ensuite de faire une régression mais qu'avec les variables simples.
#+
lad.fit1 <- lad(reponse ~ ., data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
data.frame(variables = colnames(lad.fit1@inputData)[coefficients(lad.fit1) != 0], 
           coeff = coefficients(lad.fit1)[coefficients(lad.fit1) != 0])
#' Il y a 21 variables qui restent, ce n'est donc pas très intéressant vu le nombre d'observations.
#'
#' ## Lasso and Elastic Net
#' 
#' ## Semi-Supervised Regression
#' 
#' 