#' ---
#' title: Projet Régression 2017
#' author: Bénédicte <>, Xian YANG <xian.yang@etu.univ-lyon1.fr> 
#' date: 24 déc. 2017
#' output:
#'    html_document:
#'      toc: true
#'      highlight: ##zenburn
#' ---
#+ echo=FALSE, message=FALSE, results='hide'
set.seed(11715490)
library(usdm)
library(glmnet)
library(readxl)
library(MASS)
# library(tidyverse)
dfSansObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/FW_groupe3.xls")
dfAvecObs <- read_excel("C:/Users/ellie/Documents/R/projetRegressionLyon1/FW_groupe3_obs.xls")
names(dfSansObs)[1] <- "rowname"
names(dfAvecObs)[1] <- "rowname"
head(dfSansObs)
head(dfAvecObs)
dim(dfSansObs)
dim(dfAvecObs)
#' ## Foreword
#' 
#' ## Unsupervised stepwise feature selection among main effects using VIF
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