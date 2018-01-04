#' ---
#' title: Projet Régression 2017
#' author: Bénédicte <>, Xian YANG <xian.yang@etu.univ-lyon1.fr> 
#' date: 24 déc. 2017
#' output:
#'    html_document:
#'      toc: true
#'      theme: spacelab
#'      highlight: zenburn
#' ---
#+ echo=FALSE, message=FALSE, results='hide', warning=FALSE
set.seed(11715490)
library(usdm)
library(glmnet)
library(readxl)
library(MASS)
# library(tidyverse)
dfSansObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/StepwiseSelectionLAD/FW_groupe3.xls")
dfAvecObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/StepwiseSelectionLAD/FW_groupe3_obs.xls")
names(dfSansObs)[1] <- "rowname"
names(dfAvecObs)[1] <- "rowname"
head(dfSansObs)

dim(dfSansObs)
dim(dfAvecObs)
#' ## I. Introduction
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
#' ## II. Unsupervised stepwise feature selection among main effects using VIF
#' 
VifEffetsPrincipaux <- vifstep(as.data.frame(dfSansObs[-1]), th = 10)
VifEffetsPrincipaux
# VifEffetsPrincipaux@results
#' 
#' ## Supervised stepwise feature selection with interaction terms of degree 2
#' 
modeleComplet <- lm(reponse ~ .^2, 
                    data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
modeleDepart <- lm(reponse ~ 1, 
                   data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])

stepAIC(modeleDepart, direction = "forward", scope = list(upper = modeleComplet)) # AIC ends at -Inf, means perfect fit. Not good.
step2 <- stepAIC(modeleDepart, direction = "forward", scope = list(upper = modeleComplet), k = log(nrow(dfAvecObs))) # Good.
stepAIC(modeleDepart, direction = "both", scope = list(upper = modeleComplet), k = log(nrow(dfAvecObs))) # Same result.
step4 <- stepAIC(modeleDepart, direction = "both", scope = list(upper = modeleComplet), k = 2) # Also good!
# Comare with the upper one: todo
step4$coefficients
class(step2$terms)
AIC(lm(step2$terms, data = dfAvecObs), lm(step4$terms, data = dfAvecObs))
BIC(lm(step2$terms, data = dfAvecObs), lm(step4$terms, data = dfAvecObs))
# Both criteria say the second model is better with 2 more degrees. But the meaning of comparing these two 
# by AIC or BIC is doubtful since the complete
# model will always give the best AIC and BIC but it's surely far from the best model, by all means.
?cooks.distance
cookd <- cooks.distance(lm(step4$terms, data = dfAvecObs))
library(car)
leveragePlots(lm(step4$terms, data = dfAvecObs))
plot(cookd)
plot(lm(step4$terms, data = dfAvecObs))
plot(lm(step4$terms, data = dfAvecObs[-24, ]))
# plot(lm(step4$terms, data = dfAvecObs[-c(24, 9, 17, 12), ]))

cookd2 <- cooks.distance(lm(step2$terms, data = dfAvecObs))
plot(cookd2)
plot(lm(step2$terms, data = dfAvecObs))
plot(lm(step2$terms, data = dfAvecObs[-24, ]))
# plot(lm(step2$terms, data = dfAvecObs[-c(24, 9, 17, 12), ]))

#' ## Least absolute deviation regression preceeded by a PCA
#' For the sake of avoiding over-fitting, how can we benifit from the dataset without response this time, 
#' in order to realize a dimensionality reduction, if we don't wan't to naively filter the variables any more?
#' One possibility is to do a PCA using both datasets, before doing a LAD regression with the scores obtained by 
#' the PCA.
library(Blossom)
lad.fit <- lad(reponse ~ ., data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
#lad.fit2 <- lad(reponse ~ .^2, data = dfAvecObs[c("reponse", as.character(VifEffetsPrincipaux@results$Variables))])
summary(lad.fit)
#coefficients(lda.fit2)[which(coefficients(lda.fit2) != 0)]
# library(nsprcomp)

#' ## Lasso and Elastic Net
#' 
#' ## Semi-Supervised Regression
#' 
#' 