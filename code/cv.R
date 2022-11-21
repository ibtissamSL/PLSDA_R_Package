#' Cross Validation for Partial Least Squares Discriminant Analysis
#'
#' This function performs a k-cross-validation in order to determine the number of components \code{ncomp}
#' to use in \code{plsda.fit} function.
#' @param
#' formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param
#' nfold the number of folds used for cross-validation (k=10 by default).
#' @return
#' \code{ncomp} the number of components that must be used in plsda.fit.
#' \cr
#' \code{PRESS} a vector containing the calculated PRESS for each components.
#' \cr
#' \code{min.PRESS}the minimum value of te vector PRESS that has been calculated.
#' @examples
#' plsda.cv(Species~., data = iris)
#' plsda.cv(Species~.,data=iris, nfold = 50)

plsda.cv<-function(formula,data,nfold=10){

  #Vérification que l'entrée est bien une formule Y~X
  #if(plyr::is.formula(formula)==F){
   # stop("formula must be R formula !")
  #}

  #Récupération des X et Y
  X <- model.matrix(formula, data = data)
  X <- X[,-1] #suppression de l'intercept
  Y <- model.response(model.frame(formula, data = data))

  ncomp <- qr(X)$rank
  #PRESS <- NULL
  R2_scores <- NULL
  for(j in 1:ncomp){

    #press <- NULL
    r2_score <-NULL

    s<-sample(1:nrow(X),nrow(X))
    ns<-trunc(nrow(X)/nfold)
    newX <- X[s,]
    newY <- Y[s]

    for(i in 1:nfold){
      #index du ième échantillon
      idx<-c((1+(i-1)*ns):(ns*(i)))

      #on divise les données test et entraînement

      X.train <- newX[-idx,]
      X.test <- newX[idx,]
      Y.train <- newY[-idx]
      Y.test <- plsda.dummies(newY)
      Y.test <- Y.test[idx,]
      train <- data.frame("Y"=Y.train, X.train)

      #on exécute le modèle sur les données d'appprentissage
      fit<-plsda.fit(Y~., train, ncomp = j)
      #on fait la prédiction sur X.test
      pred <- plsda.predict(fit, X.test, type ="posterior")

      #on calcule le press pour le ième échantillon
      #press[i] <- sum((Y.test-pred)^2)
      R2_scores[i] <- R2(pred, Y.test)

      #au lieu de la press, qui doit etre min pour le meilleur echantillon,
      #on calcule le score R², qui doit être max


    }
    #PRESS[j] <-as.numeric(sum(press))
    R2_scores[j] <-as.numeric()
  }

  #ncomp <- which.min(PRESS)
  #min.PRESS <- PRESS[ncomp]
  max.R2_scores <- R2_scores[ncomp]
  #object=list("ncomp" = ncomp,
   #           "PRESS" = PRESS,
    #          "min.PRESS" = min.PRESS)

  object=list("ncomp" = ncomp,
              "R2_scores" = R2_scores,
              "max.R2_scores" = max.R2_scores)

  class(object)<-"CV"
  return(object)
}
