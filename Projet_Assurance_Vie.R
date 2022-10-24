#######NETTOYAGE DE LA MEMOIRE ET DEFINITION DU REPERTOIRE DE TRAVAIL

rm(list = ls())
getwd()
setwd("D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/Codes_projet")

########BIBLIOTHEQUES UTILES

library(tidyverse)
library(gplots)
library(ggplot2)
library(cowplot)
library(here)

#######IMPORTATION DES BASES DE DONNEES UTILES POUR LE PROJET

data.death=read.table("d.txt", sep="",header = TRUE)
#as.data.frame(data.death)
data.exposure=read.table("expr.txt", sep="",header = TRUE)
#as.data.frame(data.exposure)
data.lifex=read.table("lexp.txt", sep="",header = TRUE)
#as.data.frame(data.lifex)
data.lifef=read.table("flt.txt",sep="",header = TRUE)
#as.data.frame(data.lifef)
data.lifeh=read.table("mlt.txt",sep="",header = TRUE)
#as.data.frame(data.lifeh)
data.lifet=read.table("blt.txt",sep="",header = TRUE)
#as.data.frame(data.lifet)

######CALCUL DES TAUX DE MORTALITE PAR MAXIMUM DE VRAISSEMBLANCE
deat.ratef=matrix(0:255,nrow = 64,ncol = 4)
deat.rateh=matrix(0:255,nrow = 64,ncol = 4)
deat.ratet=matrix(0:255,nrow = 64,ncol = 4)


Line=0
Start=9214
C=1:64

###TAUX DE MORTALITE PAR AGE POUR LES COHORTES FEMMES ET HOMMES NES EN 1955 ET POUR LE TOTAL

for (k in C) {
  
  deat.ratef[k,2]=(data.death$Female[Start+Line]/data.exposure$Female[Start+Line])
  Variance=deat.ratef[k,2]^2/data.death$Female[Start+Line]
  deat.ratef[k,3]=deat.ratef[k,2]-qnorm(0.95)*sqrt(Variance)
  deat.ratef[k,4]=deat.ratef[k,2]+qnorm(0.95)*sqrt(Variance)
  
  deat.rateh[k,2]=(data.death$Male[Start+Line]/data.exposure$Male[Start+Line])
  Variances=deat.rateh[k,2]^2/data.death$Male[Start+Line]
  deat.rateh[k,3]=deat.rateh[k,2]-qnorm(0.95)*sqrt(Variances)
  deat.rateh[k,4]=deat.rateh[k,2]+qnorm(0.95)*sqrt(Variances)
  
  
  deat.ratet[k,2]=(data.death$Total[Start+Line]/data.exposure$Total[Start+Line])
  Variances=deat.ratet[k,2]^2/data.death$Total[Start+Line]
  deat.ratet[k,3]=deat.ratet[k,2]-qnorm(0.95)*sqrt(Variances)
  deat.ratet[k,4]=deat.ratet[k,2]+qnorm(0.95)*sqrt(Variances)
  Line=Line+112
}
deat.ratef
deat.rateh
deat.ratet

###INTERVALLES DE CONFIANCES 

#FEMMES
plot(deat.ratef[61:64,1],deat.ratef[61:64,2])
plotCI(deat.ratef[61:64,1],deat.ratef[61:64,2],uiw = deat.ratef[61:64,4]-deat.ratef[61:64,2],type="o",pch=16,main="Femme",xlab="?ge",ylab="Taux de mortalit?",barcol="blue",col="black")
#HOMMES
plot(deat.rateh[61:64,1],deat.rateh[61:64,2])
plotCI(deat.rateh[61:64,1],deat.rateh[61:64,2],uiw = deat.rateh[61:64,4]-deat.rateh[61:64,2],type="o",pch=16,main="Homme",xlab="?ge",ylab="Taux de mortalit?",barcol="blue",col="black")
#TOTAL
plot(deat.ratet[61:64,1],deat.ratet[61:64,2])
plotCI(deat.ratet[61:64,1],deat.ratet[61:64,2],uiw = deat.ratet[61:64,4]-deat.ratet[61:64,2],type="o",pch=16,main="Global",xlab="?ge",ylab="Taux de mortalit?",barcol="blue",col="black")

#EXERCICE II
###ESPERANCE DE VIE A LA NAISSANCE (TABLE HMD)

plot(x=data.lifex$Year,y=data.lifex$Female,xlab="Ann?e",ylab="?ge",type="o",pch=".",ylim=c(20,100))
points(x=data.lifex$Year,y=data.lifex$Male,type="o",pch=".",col="blue",ylim=c(0,110))
points(x=data.lifex$Year,y=data.lifex$Total,type="o",pch=".",col="red",ylim=c(0,110))
legend("topleft",legend = c("Femme","Homme","Total"),col = c("black","blue","red"),lty=1)

###ESPERANCE DE VIE A 60 ANS

Tauxf=matrix(data = NA,nrow = 64,ncol = 111)
Step1f=matrix(data = NA,nrow = 64,ncol = 111)
lifexpf=matrix(data = NA,nrow = 64,ncol = 111)
Tauxh=matrix(data = NA,nrow = 64,ncol = 111)
Step1h=matrix(data = NA,nrow = 64,ncol = 111)
lifexph=matrix(data = NA,nrow = 64,ncol = 111)
Tauxt=matrix(data = NA,nrow = 64,ncol = 111)
Step1t=matrix(data = NA,nrow = 64,ncol = 111)
lifexpt=matrix(data = NA,nrow = 64,ncol = 111)

Start=9214

for (i in 0:63) {
  n=0
  for (k in 1:111) {
    Tauxf[i+1,k]=data.death$Female[Start+n]/data.exposure$Female[Start+n]
    expression.1f= (1-exp(-Tauxf[i+1,k]))/Tauxf[i+1,k]
    Step1f[i+1,k]=expression.1f
    Tauxh[i+1,k]=data.death$Male[Start+n]/data.exposure$Male[Start+n]
    expression.1h= (1-exp(-Tauxh[i+1,k]))/Tauxh[i+1,k]
    Step1h[i+1,k]=expression.1h
    Tauxt[i+1,k]=data.death$Total[Start+n]/data.exposure$Total[Start+n]
    expression.1t= (1-exp(-Tauxt[i+1,k]))/Tauxt[i+1,k]
    Step1t[i+1,k]=expression.1t
    n=n+1
  }
  Start=Start+111
}

#TABLEAU DES TAUX DEPUIS 1955 POUR TOUS LES AGES ET TOUTES LES CATEGORIES

Tauxf
Step1f
Tauxh
Step1h
Tauxt
Step1t

i=0
a=0
m=0
g=0
j=0

for (p in 0:63) {
  g=g+1   
  for (n in 0:110) {
    expression.2f=0
    for (h  in seq(1,110-n)) {
      term.1f=1
      for (j in seq(1,h)) {
        if (is.nan(exp(-Tauxf[g,n+j]))){
          a=a+1
        }else{
          term.1f=term.1f*exp(-Tauxf[g,n+j])
        }
      }
      if (is.nan((1-exp(-Tauxf[g,n+h]))*Tauxf[g,n+h]^(-1))){
        i=i+1
      }else{
        term.2f=(1-exp(-Tauxf[g,n+h]))*Tauxf[g,n+h]^(-1)
        expression.2f=expression.2f+term.1f*term.2f
      }
      
    }
    lifexpf[g,n+1]=expression.2f+Step1f[g,n+1]
  }
  
}

i=0
a=0
m=0
g=0
j=0

for (p in 0:63) {
  g=g+1   
  for (n in 0:110) {
    expression.2h=0
    for (h  in seq(1,110-n)) {
      term.1h=1
      for (j in seq(1,h)) {
        if (is.nan(exp(-Tauxh[g,n+j]))){
          a=a+1
        }else{
          term.1h=term.1h*exp(-Tauxh[g,n+j])
        }
      }
      if (is.nan((1-exp(-Tauxh[g,n+h]))*Tauxh[g,n+h]^(-1))){
        i=i+1
      }else{
        term.2h=(1-exp(-Tauxh[g,n+h]))*Tauxh[g,n+h]^(-1)
        expression.2h=expression.2h+term.1h*term.2h
      }
      
    }
    lifexph[g,n+1]=expression.2h+Step1h[g,n+1]
  }
  
}

i=0
a=0
m=0
g=0
j=0

for (p in 0:63) {
  g=g+1   
  for (n in 0:110) {
    expression.2t=0
    for (h  in seq(1,110-n)) {
      term.1t=1
      for (j in seq(1,h)) {
        if (is.nan(exp(-Tauxt[g,n+j]))){
          a=a+1
        }else{
          term.1t=term.1t*exp(-Tauxt[g,n+j])
        }
      }
      if (is.nan((1-exp(-Tauxt[g,n+h]))*Tauxt[g,n+h]^(-1))){
        i=i+1
      }else{
        term.2t=(1-exp(-Tauxt[g,n+h]))*Tauxt[g,n+h]^(-1)
        expression.2t=expression.2t+term.1t*term.2t
      }
      
    }
    lifexpt[g,n+1]=expression.2t+Step1t[g,n+1]
  }
  
}

view(lifexpf)
view(lifexph)
view(lifexpt)
view(data.lifex)



######EXPORTATION ET RE-IMPORTATION DES DONNEES

##write.csv2(lifexpf, "D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/life_female.txt",row.names =FALSE)


##write.csv2(lifexph, "D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/life_male.txt",row.names =FALSE)


##write.csv2(lifexpt, "D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/life_total.txt",row.names =FALSE)

life_female=read.table("D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/life_female.txt",sep="",header = TRUE)
##as.data.frame(life_female)

life_male=read.table("D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/life_male.txt",sep="",header = TRUE)
##as.data.frame(life_male)

life_total=read.table("D:/Bureau/Emploie_de_Temps_ULB/John/Cours/Q2/ACTUF502-Assurance_vie_II/Projet/Base/Fichier txt/life_total.txt",sep="",header = TRUE)
##as.data.frame(life_total)


#GRAPHE ESPERANCE DE VIE A LA NAISSANCE & ESPERANCE DE VIE A 60 ANS

p1f=ggplot(data.lifex[84:147,])+aes(x=Year,y=Female)+geom_line()
p1f
p2f=ggplot(life_female)+aes(x=Year,y=Exp.60)+geom_line()
p2f
plot_grid(p1f,p2f)
p1h=ggplot(data.lifex[84:147,])+aes(x=Year,y=Male)+geom_line()
p1h
p2h=ggplot(life_male)+aes(x=Year,y=Exp.60)+geom_line()
p2h
plot_grid(p1h,p2h)
p1t=ggplot(data.lifex[84:147,])+aes(x=Year,y=Total)+geom_line()
p1t
p2t=ggplot(life_total)+aes(x=Year,y=Exp.60)+geom_line()
p2t
plot_grid(p1t,p2t)




plot(x=data.lifex$Year[84:147],y=data.lifex$Female[84:147],main="Life expectancy at birth",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red")

plot(x=life_female$Year,life_female$Exp.0,main="Life expectancy at 60",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="blue")

plot(x=data.lifex$Year[84:147],y=data.lifex$Female[84:147],main="Life expectancy at birth",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red")

plot(x=life_male$Year,life_male$Exp.60,main="Life expectancy at 60",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="blue")

plot(x=data.lifex$Year[84:147],y=data.lifex$Female[84:147],main="Life expectancy at birth",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red")

plot(x=life_female$Year,life_female$Exp.60,main="Life expectancy at 60",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="blue")

#############QUARTILES 

Data.quartilef=matrix(data = NA,nrow = 64,ncol = 4)
Data.quartileh=matrix(data = NA,nrow = 64,ncol = 4)
Data.quartilet=matrix(data = NA,nrow = 64,ncol = 4)


#FONCTION QUI DETERMINE LE QUANTILE RECHERCHE

Quart=function(data,year,percentage) {
  
  Data_year=subset(data,Year==year)
  Size=Data_year$lx[1]
  i=0
  
  while (Size>(100000*percentage))  {
    i=i+1
    Size=Data_year$lx[i]
  }
  up=100000*percentage-Data_year$lx[i]
  down=Data_year$lx[i-1]-100000*percentage
  center=Data_year$lx[i]-Data_year$lx[i-1]
  if (up<down) {
    result=(i-1)+(up/center)
  }else{result=(i-2)-(down/center)}
  
  return(result)
  
}
##TESTING DE LA FONCTION

Quart(data.lifef,1872,0.5)

####CALCUL DES QUARTILES POUR CHAQUE ANNEE

for (i in 1955:2018) {
  Data.quartilef[i-1954,1]=Quart(data.lifef,i,0.75)
  Data.quartilef[i-1954,2]=Quart(data.lifef,i,0.5)
  Data.quartilef[i-1954,3]=Quart(data.lifef,i,0.25)
  Data.quartilef[i-1954,4]=Data.quartilef[i-1954,3]-Data.quartilef[i-1954,1]
  
  Data.quartileh[i-1954,1]=Quart(data.lifeh,i,0.75)
  Data.quartileh[i-1954,2]=Quart(data.lifeh,i,0.5)
  Data.quartileh[i-1954,3]=Quart(data.lifeh,i,0.25)
  Data.quartileh[i-1954,4]=Data.quartileh[i-1954,3]-Data.quartileh[i-1954,1]
  
  Data.quartilet[i-1954,1]=Quart(data.lifet,i,0.75)
  Data.quartilet[i-1954,2]=Quart(data.lifet,i,0.5)
  Data.quartilet[i-1954,3]=Quart(data.lifet,i,0.25)
  Data.quartilet[i-1954,4]=Data.quartilet[i-1954,3]-Data.quartilet[i-1954,1]
}

###TABLEAU DES QUARTILES

Data.quartilef
as.data.frame(Data.quartilef)
view(Data.quartilef)
Data.quartileh
as.data.frame(Data.quartileh)
view(Data.quartileh)
Data.quartilef
as.data.frame(Data.quartileh)
view(Data.quartilet)

###GRAPHE QUARTILES

plot(1955:2018,Data.quartilef[,2],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="green",ylim = c(50,100))
points(1955:2018,Data.quartilef[,1],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red",ylim = c(50,100))
points(1955:2018,Data.quartilef[,3],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red",ylim = c(50,100))
points(x=data.lifex$Year[84:147],y=data.lifex$Female[84:147],type="o",pch=".",col="blue",ylim = c(50,110))

plot(1955:2018,Data.quartileh[,2],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="green",ylim = c(50,100))
points(1955:2018,Data.quartileh[,1],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red",ylim = c(50,100))
points(1955:2018,Data.quartileh[,3],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red",ylim = c(50,100))
points(x=data.lifex$Year[84:147],y=data.lifex$Male[84:147],type="o",pch=".",col="blue",ylim = c(50,100))

plot(1955:2018,Data.quartilet[,2],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="green",ylim = c(50,100))
points(1955:2018,Data.quartilet[,1],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red",ylim = c(50,100))
points(1955:2018,Data.quartilet[,3],main="Median",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="red",ylim = c(50,100))
points(x=data.lifex$Year[84:147],y=data.lifex$Total[84:147],type="o",pch=".",col="blue",ylim = c(50,100))


###PHENOMENE DE RECTANGULARISATION DEPUIS 1955 POUR LES COHORTES ETUDIES

Start=9214
plot(data.lifef$Age[Start:(Start+110)],data.lifef$lx[Start:(Start+110)],type="o",pch=".",col="green",xlab="?ge",ylab="lx", ylim = c(0,100000))
Start=Start+111
for (m in rep(10,6,by=10)) {
  points(data.lifef$Age[Start:(Start+110)],data.lifef$lx[Start:(Start+110)],type="o",pch=".",col="grey", ylim = c(0,100000))
  Start=Start+111*m
}
points(data.lifef$Age[Start:(Start+110)],data.lifef$lx[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="lx", ylim = c(0,100000))


Start=9214
plot(data.lifeh$Age[Start:(Start+110)],data.lifeh$lx[Start:(Start+110)],type="o",pch=".",col="green",xlab="?ge",ylab="lx", ylim = c(0,100000))
Start=Start+111
for (m in rep(10,6,by=10)) {
  points(data.lifeh$Age[Start:(Start+110)],data.lifeh$lx[Start:(Start+110)],type="o",pch=".",col="grey", ylim = c(0,100000))
  Start=Start+111*m
}
points(data.lifeh$Age[Start:(Start+110)],data.lifeh$lx[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="lx", ylim = c(0,100000))

Start=9214
plot(data.lifet$Age[Start:(Start+110)],data.lifet$lx[Start:(Start+110)],type="o",pch=".",col="green",xlab="?ge",ylab="lx", ylim = c(0,100000))
Start=Start+111
for (m in rep(10,6,by=10)) {
  points(data.lifet$Age[Start:(Start+110)],data.lifet$lx[Start:(Start+110)],type="o",pch=".",col="grey", ylim = c(0,100000))
  Start=Start+111*m
}
points(data.lifet$Age[Start:(Start+110)],data.lifet$lx[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="lx", ylim = c(0,100000))


###PHENOMENE D EXPANSION / EVOLUTION ECART INTERQUARTILE POUR CHAQUE CATEGORIE

plot(1955:2018,Data.quartilef[,4],main="IQR Female",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="green")
plot(1955:2018,Data.quartileh[,4],main="IQR Male",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="blue" )
plot(1955:2018,Data.quartilet[,4],main="IQR Total",xlab="Ann?e",ylab="?ge",type="o",pch=".",col="yellow")

#####EXPANSION 

Start=9214
plot(data.death$Age[Start:(Start+110)],data.death$Female[Start:(Start+110)],type="o",pch=".",col="green",xlab="?ge",ylab="dx")

abline(v=c(Data.quartilef[1,1],Data.quartilef[1,2],Data.quartilef[1,3],lifexpf[1,1]), col=c("blue", "red","grey","green"), lty=c(3,3,3,3), lwd=c(1,2,2,2))

Start=Start+111
for (m in rep(10,6,by=10)) {
  points(data.death$Age[Start:(Start+110)],data.death$Female[Start:(Start+110)],type="o",pch=".",col="grey",xlab="?ge",ylab="dx")
  Start=Start+111*m
}
points(data.death$Age[Start:(Start+110)],data.death$Female[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="dx")
abline(v=c(Data.quartilef[64,1],Data.quartilef[64,2],Data.quartilef[64,3],lifexpf[64,1]), col=c("blue", "red","grey","green"), lty=c(1,2,3,4), lwd=c(1,2,2,2))

plot(data.death$Age[Start:(Start+110)],data.death$Female[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="dx")

abline(v=c(Data.quartilef[64,1],Data.quartilef[64,2],Data.quartilef[64,3],lifexpf[64,1]), col=c("blue", "red","grey","green"), lty=c(1,2,3,4), lwd=c(1,2,2,2))


Start=9214
plot(data.death$Age[Start:(Start+110)],data.death$Male[Start:(Start+110)],type="o",pch=".",col="green",xlab="?ge",ylab="dx")

abline(v=c(Data.quartileh[1,1],Data.quartileh[1,2],Data.quartileh[1,3],lifexph[1,1]), col=c("blue", "red","grey","green"), lty=c(3,3,3,3), lwd=c(1,2,2,2))

Start=Start+111
for (m in rep(10,6,by=10)) {
  points(data.death$Age[Start:(Start+110)],data.death$Male[Start:(Start+110)],type="o",pch=".",col="grey",xlab="?ge",ylab="dx")
  Start=Start+111*m
}
points(data.death$Age[Start:(Start+110)],data.death$Male[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="dx")

abline(v=c(Data.quartileh[64,1],Data.quartileh[64,2],Data.quartileh[64,3],lifexph[64,1]), col=c("blue", "red","grey","green"), lty=c(1,2,3,4), lwd=c(1,2,2,2))

Start=9214
plot(data.death$Age[Start:(Start+110)],data.death$Total[Start:(Start+110)],type="o",pch=".",col="green",xlab="?ge",ylab="dx")

abline(v=c(Data.quartilet[1,1],Data.quartilet[1,2],Data.quartilet[1,3],lifexpf[1,1]), col=c("blue", "red","grey","green"), lty=c(3,3,3,3), lwd=c(1,2,2,2))

Start=Start+111
for (m in rep(10,6,by=10)) {
  points(data.death$Age[Start:(Start+110)],data.death$Total[Start:(Start+110)],type="o",pch=".",col="grey",xlab="?ge",ylab="dx")
  Start=Start+111*m
}
points(data.death$Age[Start:(Start+110)],data.death$Total[Start:(Start+110)],type="o",pch=".",col="black",xlab="?ge",ylab="dx")

abline(v=c(Data.quartilet[64,1],Data.quartilet[64,2],Data.quartilet[64,3],lifexpt[64,1]), col=c("blue", "red","grey","green"), lty=c(1,2,3,4), lwd=c(1,2,2,2))

