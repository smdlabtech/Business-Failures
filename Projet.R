#********************************************************************************
#     Sujet: Nbre de défaillances d'entreprises en Normandie 
#             Secteur Hébergement-Restauration
#********************************************************************************

#Ié) Importations, Manipulations, Nettoyages et Validations des données
file="//calebasse/21713532/Documents/M2_SAAD/Mod_Temporelles/Projet/data_projet/valeurs_trimestrielles.csv"
BD <- read.csv2(file,header = TRUE, sep = ";",stringsAsFactors = FALSE)
colnames(BD)
class(BD)

# #Manipulation des données : Suppression les lignes 2 et 3 et renommer les BD
# colnames(BD)<-c("date","NbreObs","code")  #Renommer les champs des données
# BD
# BD<-BD[-1:-2,,]   #Suppression de lignes 1 et 2

#Vérification de l'importation des données
summary(BD)
head(BD,5)  #debut de série
tail(BD,5)  #fin de série

#Structure et format des données
str(BD) #122 observations pour 3 variables (var: date,NbreObs,code)

#Recherche de Valeurs manquantes (NA)
library(dplyr)
apply(BD, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum})
attach(BD)      #Aucune valeur manquante 
sum(is.na(BD))  #Recherche de valeurs manquantes: Aucune valeurs trouvee

#Transformation des données en séries temporelles et Affichage du graphe
X <- ts(data = NbreObs, start = c(1990, 1), frequency = 4)
X
plot(X, ylab="Nombre de défaillances d'entreprises",col="brown")
#Observons bien une série de type multiplicative

#*******************************************************************************
#II) Analyse et Exploitation des données
#II-1) Détermination de la Série Corrigé des variations saisionniéres
t=1:122
X<-data.frame(cbind(t,X))
modele<-lm(X~t, data=X)
summary(modele)
#R2=0.2955 trés faible. Donc la série est saisonniére

#Calcul des coefs et Ajustement par la droite
v<-modele$coefficients
plot(X$X, type="l", main="Ajustement de la série par la droite de régression", 
     ylab="défaillances")
lines(modele$fitted.values, col="red")

#Calcul des valeurs estimées
Z=modele$fitted.values
Z=ts(Z,start=c(1990,1),frequency=4)
X<-data.frame(X)
X<-select(X,X)
X=ts(X,start=c(1990,1),frequency=4)

#Calcul des coefficients saisonniers
#comme notre modéle est multiplicatif, alors on divsise les valeurs brutes par
#les valeurs estimées.
S=X/Z
cycle(S) #indicatrices des colonnes
s=tapply(S,cycle(S),mean,na.rm=TRUE) #Estimation des moyennes de chaque periodes
#la fonction tapply est appliquée fonction a chaque groupe

#Calcul de la CVS et de la représentation de la CVS
CVS=matrix(1,29,4)
CVS=rbind(CVS,matrix(c(1,1,0,0),1,4)) #Ajout d'une ligne é la matrice
dim(CVS)    #on obtient bien dimension attendue

for (i in 1:30) {for (j in 1 :4) {CVS[i,j]=t(matrix(X,4,30))[i,j]/s[j]}}
CVS=as.vector(t(CVS))
CVS=as.ts(CVS)
CVS=ts(CVS,start=c(1990,1), frequency=4)

#Superposition de la CVS et de la série brute
ts.plot(X,CVS, col = c("black", "red"), lwd = c(1, 2),main=" Répresentation de série brute et de la CVS")
legend(1990, 120, legend=c("Série brute", "CVS"), col=c("black", "red"), lty=1:2, cex=0.8)

#IIIé) Décomposition de la série temporelle
X_decom <- decompose(X, "multiplicative")
plot(X_decom)

# #Saisonnalité :Lissage avec les moyennes mobiles
# MA<-stats::filter(X_decom$seasonal,filter=array(1/10,dim=10), method = c("convolution"),sides=2,circular = FALSE)
# plot(X_decom$seasonal,type='l',main="Représentaion de la saisonnalité")
# lines(MA,col='brown')

#IIIé-1) Autocorrélation de la série
par(mfrow = c(1,1))
res_acf<-acf(X,lag=40)
# res_pacf<-pacf(X)
#On trouve bien que le coef de corrélation vaut 1 au décalage 0 de départ
#Et La décroissance progressive des batons est typique d'une série chronologique 
#contenant une tendance.
#Le pic au trimestre 1 indique bien une variation saisonniére.

#Affichages des valeurs du graphe de corrélation
print(data.frame(res_acf$lag,res_acf$acf)[1:10,])
#Afficher la sortie de ces résultats pour une bonne interprétation 

#IIIé-2) Analyse des Résidus 
plot(ts(X_decom$random[3:120]))
#la série des défaillance est corrigée des variations saisonniéres 
#et la tendance est supprimée. 

#Tracé du corréllogramme
par(mfrow=c(1,2))
acf(X_decom$random[3:120], main="acf des résidus")
pacf(X_decom$random[3:120], main="pacf des résidus")
#Le coef de corrélation vaut 1 au décalage 0.
#Et on ne distingue aucune structure partuliére.

#Test de Normalité des Résidus
shapiro.test(X_decom$random[3:120])
#p-value = 0.1912 > 0.05.
#Donc les résidus sont distribuées suivant une loi normale de paramétres N(0,1)

#Détection des corrélation sur les 4 derniers trimestres
lag.plot(X,lag=12,layout=c(3,4))
#Le nombre de défaillances d'entreprise est fortement corrélé au 4éme trimestre
#Ainsi donc, au lag4 les points sont alignés.
#Donc les nombres de défaillances pour un trimestre est fortement corrélé au méme 
#trimestre de l'année précédente.

#**************************************************************************
#IIIé-3) Calcul des ratios et des valeurs extrémes
#Evolution annuel des moyennes trimestrielles des défaillances (par années)
layout(1:2)
X.annuel <- aggregate(X)/4     
plot(X.annuel,ylab="Nbre de défaillances" ,col="brown")      #Moyenne trimestrielles par année
boxplot(X~cycle(X),ylab="Nbre de défaillances",names=c("T1","T2","T3","T4")) #Boite é moustache de la défaillance par trim

layout(1:1)
#Valeurs extrémes des défaillances
max(aggregate(X))  
min(aggregate(X))  

# Calcul des Ratio trimestriels
X.trim1 <- window(X, start=c(1990, 1), freq=TRUE)
X.trim2 <- window(X, start=c(1990, 2), freq=TRUE)
X.trim3 <- window(X, start=c(1990, 3), freq=TRUE)
X.trim4 <- window(X, start=c(1990, 4), freq=TRUE)

ratio.trim1 <- mean(X.trim1)/mean(X)
ratio.trim2 <- mean(X.trim2)/mean(X)
ratio.trim3 <- mean(X.trim3)/mean(X)
ratio.trim4 <- mean(X.trim4)/mean(X)
ratio.trim1; ratio.trim2; ratio.trim3; ratio.trim4

taux2 = 1-ratio.trim2
taux3 = 1-ratio.trim3  
taux1 = ratio.trim1 - 1
taux4 = ratio.trim4 - 1

#********* Prévision par lissage exponentielle ***********
#Décomposition par lissage de la série corrigée
ts_ajust = stl(CVS,s.window=4,s.degree=0)
plot(ts_ajust,main = "Ajustement de la tendance de la CVS")
tendance=ts_ajust$time.series[,"trend"]
saisonalite=ts_ajust$time.series[,"seasonal"]
residus=ts_ajust$time.series[,"remainder"]

#estimation de la tendance 
lm.tendance=lm(tendance~time(tendance))
plot(tendance)
abline(lm.tendance$coefficients[1],lm.tendance$coefficients[2],col="red")

#Prévision par la Méthode de Holt-Winters: estimation de la tendance
library(forecast)
hw=ets(tendance,model="MMM")
hw.pred=predict(hw,10)  #10 prochains trimestres
hw.pred
plot(hw.pred, xlab="Time",ylab = "Nbr. de défaillances")

#***************** Prévison de la série X ******************
library(forecast)
hwt.pred=hw(X,seasonal="multiplicative")  #prochains trimestres
hwt.pred
plot(hwt.pred, xlab="Time",ylab = "Nbr. de défaillances")
#Au 3éme trimestre de l'année 2020, il était prévu un total de 51.67408~52 
#défaillances d'entreprises dans l'Hebergement et la Restauration.

