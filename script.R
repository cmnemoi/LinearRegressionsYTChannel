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
B0estime1<-exp(lnB0estime1)

#Estimation de ln Y, puis de Y
lnYestime1<-lnB0estime1+B1estime1*X1
Yestime1<-exp(lnYestime1)

#Second modèle : linearisation d'une relation puissance Y = B0.X1^(B1) en un modèle du type ln Y = ln B0 + B1.lnX1
lnX1<-log(X1)

#Calcul dela regression
regressionLnYLnX1<-lm(lnY~lnX1)

#On extrait les estimateurs
lnB0estime2<-regressionLnYLnX1$coefficients[1]
B0estime2<-exp(lnB0estime2)
B1estime2<-regressionLnYLnX1$coefficients[2]

#On calcule une estimation de Y
lnYestime2<-lnB0estime2 + B1estime2*lnX1
Yestime2<-exp(lnYestime2)

#Regression 2 : Vues selon les partages quotidiens
#On cherche un modèle Y = B0 + B1*X2
regressionYX2<-lm(Y~X2)
B0estime3<-regressionYX2$coefficients[1]
B1estime3<-regressionYX2$coefficients[2]

Yestime3<-B0estime3+B1estime3*X2

#Regression 3 : Vues selon le nombre de minutes regardes quotidiennement

regressionYX3<-lm(Y~X3)
B0estime4<-regressionYX3$coefficients[1]
B1estime4<-regressionYX3$coefficients[2]

Yestime4<-B0estime4+B1estime4*X3


#Regression 4 : Vues selon le nombre de likes quotidiens

regressionYX4<-lm(Y~X4)
B0estime5<-regressionYX4$coefficients[1]
B1estime5<-regressionYX4$coefficients[2]

Yestime5<-B0estime5+B1estime5*X4

#Analyse des resultats
#Visualisation graphique et premières conjectures

#Regression 1
#Modèle 1 : exponentiel
X1<-unlist(DonneesChaineYoutube$day)
plot(X1,Y,main = "Nombre de vues selon le nombre de jours écoulés",
     xlab = "Jours écoulés",ylab = "Vues",pch=4,col="blue")
curve(B0estime1*exp(B1estime1*x),col="red")

#Modèle 2 : puissance
curve(B0estime2*x^(B1estime2),col="green")

plot(lnX1,lnY)
curve(2*x,add=TRUE,col="green")

#Regression 2
plot(X2,Y,main = "Nombre de vues selon le nombre de partages par jour",
     xlab = "Partages",ylab = "Vues",pch=4,col="blue")
curve(B0estime3+B1estime3*x,add = TRUE,col="red")

#Regression 3
plot(X3,Y,main = "Nombre de vues selon le nombre de minutes regardées par jour",
     xlab = "Minutes regardées",ylab = "Vues",pch=4,col="blue")
curve(B0estime4+B1estime4*x,add = TRUE,col="red")

#Regression 4
plot(X4,Y,main = "Nombre de vues selon le nombre de likes par jour",
     xlab = "Likes",ylab = "Vues",pch=4,col="blue")
curve(B0estime5+B1estime5*x,add = TRUE,col="red")

#Indicateurs de performance des modelisations
#Coefficients de correlation

#Regression 1 - Modèle exponentiel
rho1X1Y<-cor(lnY,X1)#0.6532197

#Regression 1 - Modèle puissance
rho2X1Y<-cor(lnY,lnX1)#0.50821528

#Regression 2
rhoX2Y<-cor(Y,X2)#0.37886849

#Regression 3
rhoX3Y<-cor(Y,X3)#0.99629185

#Regression 4
rhoX4Y<-cor(Y,X4)#0.91838271


#Calcul des residus

#Regression 1 - Modèle exponentiel
lnresidus1<-lnYestime1-lnY
residus1<-exp(lnresidus1)
r1<-summary(residus1)

#Regression 1 - Modèle puissance
lnresidus2<-lnYestime2-lnY
residus2<-exp(lnresidus2)
r2<-summary(residus2);r2

#Regression 2
residus3<-Y-Yestime3
r3<-summary(residus3);r3

#Regression 3
residus4<-Y-Yestime4
r4<-summary(residus4);r4

#Regression 4
residus5<-Y-Yestime5
r5<-summary(residus5);r5
