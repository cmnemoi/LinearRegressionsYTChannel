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


#Calcul des residus et de la somme de leurs carres

#Regression 1 - Modèle exponentiel
lnresidus1<-lnYestime1-lnY
residus1<-exp(lnresidus1)
r1<-summary(residus1)
sCres1<-sum(residus1^2)#550.98

#Regression 1 - Modèle puissance
lnresidus2<-lnYestime2-lnY
residus2<-exp(lnresidus2)
r2<-summary(residus2);r2
sCres2<-sum(residus2^2)

#Regression 2
residus3<-Y-Yestime3
r3<-summary(residus3);r3
sCres3<-sum(residus3^2)

#Regression 3
residus4<-Y-Yestime4
r4<-summary(residus4);r4
sCres4<-sum(residus4^2)

#Regression 4
residus5<-Y-Yestime5
r5<-summary(residus5);r5
sCres5<-sum(residus5^2)

statistiquesY<-summary(Y);statistiquesY

#Dispersion et coefficient de determination
#Calcul des dispersions

sCtot<-sum((Y-mean(Y))^2)#Dispersion observee de Y

sCreg1<-sum((Yestime1-mean(Y))^2)
sCreg2<-sum((Yestime2-mean(Y))^2)
sCreg3<-sum((Yestime3-mean(Y))^2)
sCreg4<-sum((Yestime4-mean(Y))^2)
sCreg5<-sum((Yestime5-mean(Y))^2)

#Coefficients de determination
R2_1<-sCreg1/sCtot
R2_2<-sCreg2/sCtot
R2_3<-sCreg3/sCtot
R2_4<-sCreg4/sCtot
R2_5<-sCreg5/sCtot

#Tests de signifivativite
#Regression 1 : modèle exponentiel

X<-X1
n<-length(lnY) #nombre d'observations
B0<-B0estime1
B1<-B1estime1
Yestime<-Yestime1
R2<-R2_1

varY<-sum((Yestime-mean(Y))^2)/(n-2) #variance de Y
varX<-var(X)
varB1<-varY/(n*varX)
varB0<-varY/n*(1+mean(X)^2/varX)

#Test de significativite de B1 : on veut tester H0 B1 = 0 contre J1 B1 =/= 0 au seuil 1%
alpha<-1/100
Z_B1<-B1/sqrt(varB1)#0.31259

seuilTheorique<-qt(alpha,n-2,lower.tail = FALSE)#2.32

#Z_B1 < Z_0.01, on ne rejette donc pas H0 au seuil alpha = 1%. B1 est donc nul selon notre test. ce nest pas etonnant en effet

#Test de significativite de B0 : on veut tester H0 B0 = 0 contre J1 B0 =/= 0 au seuil 1%
alpha<-1/100
Z_B0<-B0/sqrt(varB0)#0.423

#Z_B1 < Z_0.01, on ne rejette donc pas H0 au seuil alpha = 1%. B0 est donc nul selon notre test, ce que je trouve etonnant.


#Regression 3
X<-X3
n<-length(lnY) #nombre d'observations
B0<-B0estime4
B1<-B1estime4
Yestime<-Yestime4
R2<-R2_4

varY<-sum((Yestime-mean(Y))^2)/(n-2) #variance de Y
varX<-var(X)
varB1<-varY/(n*varX)
varB0<-varY/n*(1+mean(X)^2/varX)

#Test de significativite de B1 : on veut tester H0 B1 = 0 contre J1 B1 =/= 0 au seuil 1%
alpha<-1/100
Z_B1<-B1/sqrt(varB1)#18.1

seuilTheorique<-qt(alpha,n-2,lower.tail = FALSE)#2.32

#Z_B1 > Z_0.01, on ne rejette donc H0 avec une probabilite de 1% de se tromper
#B1 est donc significatif dans notre modèle selon notre test

#Test de significativite de B0 : on veut tester H0 B0 = 0 contre J1 B0 =/= 0 au seuil 1%
alpha<-1/100
Z_B0<-B0/sqrt(varB0)#0.31

seuilTheorique<-qt(alpha,n-2,lower.tail = FALSE)#2.32

#Z_B1 < Z_0.01, on ne rejette donc pas H0 au seuil alpha = 1%. B0 est donc nul selon notre test, ce qui est rassurant.

#Conclusion partie 1
#minutes regardees predit très bien les vues > augmenter la duree de videos ? Faux, car correlation nest pas causalite. 
#ce sont surement les vues qui entrainent plus de minutes regardes
#jours ecoules nest pas un si bon indicateur, la perseverance seule ne paie pas...
#inciter aux partages nest pas très efficace contrairement à ce qu'on pourrait croire
#likes non testes mais quasiment aussi efficace que les minutes pour predire le nombre de vues, correspond au prejuge bayesien sur lalgo youtube

#Regression multiple
#Construction de la matrice de regression
n<-length(Y)
q<-1+3
ones<-rep(1,n)
X<-matrix(c(ones,X2,X3,X4),ncol=q)

Y<-matrix(Y)

#Resolution de la regression
BestimeM<-solve(t(X)%*%X)%*%t(X)%*%Y
YestimeM<-X%*%BestimeM

#Test global de linearite
alpha<-1/100
Fcalc<-(sum((YestimeM-mean(Y))^2)/q)/(sum((YestimeM-mean(Y))^2)/(n-q-1))#90.25
Ftheorique<-qf(alpha,df1 = q,df2 = n-q--1,lower.tail = FALSE)#3.371206006
#on rejette H0 avec 1% de se tromper : au moins l'un des Bi est non nul.

#Tests de signifivativite des Bi
varY_M<-sum((YestimeM-mean(Y))^2/(n-q-1))#67243.4159639
varcov<-varY_M*solve((t(X)%*%X))#matrice variance-covariance
sigmaBi<-sqrt(diag(varcov))#ecart type estimateurs des Bi

Tcalc<-BestimeM/sigmaBi #statistiques de test
Ttheroique<-qt(alpha,df=n-q-1,lower.tail = FALSE)#2.33675
#on rejette H0 pour B2 uniquement au risque 1%.

#on revient alors à une regression simple avec Y = B2*X3 uniquement, ce qui confirme nos hypothèses de la 1ère partie
#jugeons tout de même en detail la qualite de cette regression multiple :

#coefficient de correlation entre les variables
variables<-data.frame(Y,X2,X3,X4)
corX2X3X4Y<-cor(variables)
#coeff entre variables fortes, l'une doit expliquer les autres, probablement X3

#dispersion et coefficient de determination
sCtot<-sum((Y-mean(Y))^2)#Dispersion observee de Y
sCregM<-sum((YestimeM-mean(Y))^2)#Dispersion restitue par le modèle

#Coefficient de correlation
R2_M<-sCregM/sCtot#0.9929181
#très eleve, ce qui est logique en considerant quil revient à une regression simple

#Calcul des residus
residusM<-c(Y-YestimeM)
summary(residusM)

#Somme des carres des residus
sCresM<-sum(residusM^2)#24274873.1629698

#rappel des statistiques du Y observee
summary(Y)
