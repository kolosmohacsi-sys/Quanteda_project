
library("dplyr")
library("quanteda")
library("quanteda.textmodels")
library("ggplot2")
library("reshape2")
library("stringr")
library(cluster)
library(factoextra)
library(HunMineR)
library("strucchange")

raw<-read.csv("rawcorpus.csv")
clean<-read.csv("cleandata.csv")

#my primary idea for the first analysis is to attempt to make a 2d political compass for the party members

tapply(clean$text,clean$era,function(x)sum(nchar(x)))/tapply(raw$text,raw$era,function(x)sum(nchar(x))) #there is a relatively high conversion for the speeches found in the 1980s

#we decrease our research to this area specifically

clean<-clean[clean$era=="Kései Kádárizmus",]

table(clean$motion) #economic and party policies seem to dominate, we could work out the two axises for them

remove(raw)

economics<-clean[clean$motion=="gazdasági",]
party<-clean[clean$motion=="pártpolitikai",]

#extractions gained
#we wish to create a wordfish modell, for this two transformations are necessiated

economics<-economics[economics$name!="Other",]
party<-party[party$name!="Other",]

#we managed to strip the not relevant speakers
#now we have to unify the cells

econ<-tapply(economics$text,economics$name,function(x)paste(x,collapse=" "))
pol<-tapply(party$text,party$name,function(x)paste(x,collapse=" "))

X1<-data.frame(names(econ),econ)
X2<-data.frame(names(pol),pol)

colnames(X1)<-c("speaker","text")
colnames(X2)<-c("speaker","text")

#create a wordfish compatible dataset

gazdasagtengely <- quanteda.textmodels::textmodel_wordfish(dfm(X1%>%corpus()%>%tokens()), dir = c(2, 1))
politikatengely <- quanteda.textmodels::textmodel_wordfish(dfm(X2%>%corpus()%>%tokens()), dir = c(2, 1))

#create the wordfish models

remove(economics,party,econ,pol,X1,X2)

#remove no longer relevant party

gazkord<- data.frame(nev = gazdasagtengely$x@docvars$speaker,gazdasag = gazdasagtengely$theta)
polkord<- data.frame(nev = politikatengely$x@docvars$speaker,politika = politikatengely$theta)

#extract thetas

compass<-left_join(gazkord,polkord)
compass<-na.omit(compass)
remove(gazdasagtengely, politikatengely, gazkord, polkord)

compass[,2:3]<-apply(compass[,2:3],2,scale)

#create the 3 dimensional compass dataset

ggplot(compass,aes(x=gazdasag,y=politika))+geom_point()

#plot our compass...
#interstingly there seems to be some clustering at negative politics and negative economics

print(compass[compass[,2]<0&compass[,3]<0,1])

#it seems to contain either old politician or hardliners... interesting
#i wish to attempt k-means clustering for more refined classification
#we use the Gap method to determine the optimal amount of clusters

GAPSTAT<-clusGap(compass[,2:3],kmeans, nstart = 25,K.max = 10, B = 50)
fviz_gap_stat(GAPSTAT)

#3 seems to be the ideal cluster amount hmm...

set.seed(69420) #hahaha...

cluster<-kmeans(compass[,2:3],centers=3)#run kmeans
compass$clust<-as.factor(cluster$cluster)#make it proper for ggplot2

ggplot(compass,aes(x=gazdasag,y=politika, label = nev,col=clust))+geom_text()

#so there seems to be three central points, one reformist(ish), one hardliner, and one moderate one?
#lets apply our model back perhaps

compass$clust<-c("hardliner","reformist","moderate")[cluster$cluster]

ggplot(compass,aes(x=gazdasag,y=politika, label = nev,col=clust))+geom_text()

#im very much satisfied with the shown results, lets continue our analysis
#can we envision the power dynamics within the central commitee? Let's see, of course it is going to be SEVERLY limited but i still have some hope

label<-compass[,c(1,4)]

colnames(label)<-c("name","faction")

clean<-left_join(clean,label)

remove(cluster,GAPSTAT,compass,label)
#remove unnecessary elements

#organise the central commitess up once again

clean$nchar<-nchar(clean$text)
clean$faction<-ifelse(is.na(clean$faction),"swing",clean$faction)

dynamics<-clean[,c(3,7,8)]
dynamics<-tapply(dynamics$nchar,paste(dynamics$faction,dynamics$date,sep=" x "),sum)
dynamics<-data.frame(str_split_fixed(names(dynamics)," x ",2),dynamics)

dynamics<-dcast(dynamics,X2~X1)
dynamics[,2:5]<-apply(dynamics[,2:5],2,function(x)ifelse(is.na(x),0,x))
dynamics$total<-apply(dynamics[,2:5],1,sum)
dynamics[,2:5]<-dynamics[,2:5]/dynamics$total
dynamics[,1]<-order(dynamics[,1])%>%as.character()
dynamics<-melt(dynamics[,1:5])
dynamics[,1]<-as.numeric(dynamics[,1])

colnames(dynamics)<-c("t","faction","strength")

ggplot(dynamics,aes(x=t,y=strength,col=faction))+geom_point()+geom_smooth()
ggplot(dynamics,aes(x=t,y=strength,col=faction))+geom_point()+geom_smooth(method="lm")

#interesting dynamics,there is some outlier though, lets try even simpler dates perhaps it will smoothen out somewhat


dynamics<-clean[,c(3,7,8)]
dynamics[,1]<-dynamics[,1]%>%substr(1,4)
dynamics<-tapply(dynamics$nchar,paste(dynamics$faction,dynamics$date,sep=" x "),sum)
dynamics<-data.frame(str_split_fixed(names(dynamics)," x ",2),dynamics)

dynamics<-dcast(dynamics,X2~X1)
dynamics[,2:5]<-apply(dynamics[,2:5],2,function(x)ifelse(is.na(x),0,x))
dynamics$total<-apply(dynamics[,2:5],1,sum)
dynamics[,2:5]<-dynamics[,2:5]/dynamics$total
dynamics<-melt(dynamics[,1:5])
dynamics[,1]<-as.numeric(dynamics[,1])

colnames(dynamics)<-c("t","faction","strength")

ggplot(dynamics,aes(x=t,y=strength,col=faction))+geom_point()+geom_smooth()
ggplot(dynamics,aes(x=t,y=strength,col=faction))+geom_point()+geom_smooth(method="lm")
#i very much like the trends i see
#lets do one final analysis:
remove(dynamics)

clean<-read.csv("cleandata.csv")
#now we remove the filter of eras if it isnt necessary

table(clean$name)
#Kádár Grósz & Rezső have a large amount of speeches, i believe Kádár is Quite an interesting case, as such i will analyze his speeches, also he has the most speeches

KADAR<-clean[clean$name=="Kádár János",]
KADAR<-na.omit(KADAR)
KADAR<-KADAR[,c(3,6)]
KADAR<-dfm(KADAR%>%corpus()%>%tokens())

segedszotar <- dictionary_poltext

erzelmek<-dfm_lookup(KADAR,segedszotar)
nettoerzelem<-erzelmek[,1]%>%as.numeric()-erzelmek[,2]%>%as.numeric()

plot.ts(nettoerzelem)
#interesting... of course these are individual speeches with no control to length and so on, so lets try to corrigate for it somehow

KADAR<-clean[clean$name=="Kádár János",c(3,6)]
KADAR[,1]<-substr(KADAR[,1],1,4)
KADAR<-tapply(KADAR[,2],KADAR$date,function(x)paste(x,collapse=" "))
KADAR<-data.frame(names(KADAR),KADAR)
colnames(KADAR)<-c("t","text")

KADAR$t<-as.numeric(KADAR$t)
KADAR$t<-KADAR$t-min(KADAR$t)+1

KADARDFM<-dfm(KADAR%>%corpus%>%tokens)
erzelmek<-dfm_lookup(KADARDFM,segedszotar)
nettoerzelem<-erzelmek[,1]%>%as.numeric()-erzelmek[,2]%>%as.numeric()
erzelemkoncentarcio<-nettoerzelem/ntoken(tokens(KADAR$text))

plot.ts(erzelemkoncentarcio)
KADAR$sent<-erzelemkoncentarcio
#of course there is some things let to desire in this, but it was worth an attempt, on more try

structuralbreak<-breakpoints(sent~t,data=KADAR,breaks=4)
KADAR$era<-breakfactor(#)
KADAR$t<-KADAR$t-min(KADAR$t)+1956

?breakpoints()

ggplot(data=KADAR,aes(x=t,y=sent,col=era))+geom_point()+geom_smooth(method="lm")

