#Importation des données
install.packages("rjson")
library("rjson")
json_file<-"data.json"
donneesBrutes <- fromJSON(paste(readLines(json_file), collapse=""))
donneesExtraites<-donneesBrutes[["rows"]]
nomDesVariables<-c('day','views','estimatedMinutesWatched','likes','dislikes',
                  'subscribersGained','comments','shares','annotationClickThroughRate',
                  'annotationClickableImpressions','averageViewDuration','averageViewPercentage')


nommageDesDonnees<-function(l,v)
{
  r_l<-list()
  for (i in 1:length(l)) {
    temp<-l[[i]]
    names(temp)<-v
    r_l[[i]]<-temp
    
  }
  return(r_l)
}


donneesNommees<-nommageDesDonnees(donneesExtraites,nomDesVariables)
DonneesChaineYoutube<-data.frame(do.call(rbind,donneesNommees))
DonneesChaineYoutube$day<-1:366

#Debut de l'analyse

#Choix des variables
Y<-unlist(DonneesChaineYoutube$views)#Variable à expliquer : le nombre de vues par jour
#Variables explicatives
X1<-unlist(DonneesChaineYoutube$day)#Jours ecoules
X2<-unlist(DonneesChaineYoutube$shares)#Partages par jour
X3<-unlist(DonneesChaineYoutube$estimatedMinutesWatched)#Minutes regardees par jour
X4<-unlist(DonneesChaineYoutube$likes)#Nombre de likes par jour


#Creation des graphiques
NombreDeVuesSelonLeNombreDeJoursEcoules<-plot(X1,Y,main = "Nombre de vues selon le nombre de jours écoulés",
                                              xlab = "Jours écoulés",ylab = "Vues",pch=4,col="red")
NombreDeVuesSelonLeNombreDePartagesParJour<-plot(X2,Y,main = "Nombre de vues selon le nombre de partages par jour",
                                              xlab = "Partages",ylab = "Vues",pch=4,col="red")
NombreDeVuesSelonLeNombreDeMinutesRegardeesParJour<-plot(X3,Y,main = "Nombre de vues selon le nombre de minutes regardées par jour",
                                                 xlab = "Minutes regardées",ylab = "Vues",pch=4,col="red")
NombreDeVuesSelonLeNombreDeLikesParJour<-plot(X4,Y,main = "Nombre de vues selon le nombre de likes par jour",
                                                         xlab = "Likes",ylab = "Vues",pch=4,col="red")

#Regressions simples
#Regression 1 - Vues selon Jours ecoules.
#Premier modèle : linearisation d'une relation expontielle Y = B0.e^(B1X1) en un modèle du type ln Y = ln B0 + B1X1

#fonction pour retirer les valeurs nulles et infinies dans lnY et les valeurs X1 correspondantes
#car ln 0 est non defini
deleteNaNvalues<-function(y,x1)
{
  newY<-vector(mode="numeric")
  newX1<-vector(mode="numeric")
  indexesToDelete<-sort(c(which(y %in% -Inf), which(y %in% Inf),which(y %in% -0)))
  newY<-y[-indexesToDelete]
  newX1<-x1[-indexesToDelete]
  
  return(list(newX1,newY))
}

#on calcule lnY et puis on l'utilise
lnY<-log(Y)
X1<-deleteNaNvalues(lnY,X1)[[1]]
lnY<-deleteNaNvalues(lnY,X1)[[2]]

varX1<-var(X1)#variance de X1
CX1Y<-cov(lnY,X1) #covariance de X1 et du logarithme de Y

#Calcul des estimateurs
B1estime1<-CX1Y/var(X1)
lnB0estime1<-mean(lnY)-B1estime1*mean(X1)

#Estimation de ln Y, puis de Y
lnYestime1<-lnB0estime1+B1estime1*X1
Yestime1<-exp(lnYestime1)

#Second modèle : linearisation d'une relation puissance Y = B0.X1^(B1) en un modèle du type ln Y = ln B0 + B1.lnX1
#

