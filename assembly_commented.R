
{
library("reshape")#package for reshaping long and wide datasets
library("stringr")#for string manipulation
library("stringi")#for further string manipulation
library("dplyr")#for dataset joining and manipulation
library("rvest")#for handling html and extracing html nodes; and texts
library("stringdist")#for calculating levensthein distances between strings
library("pdftools")#for processing / reading in pdf data
library("parallel")#for paralell compuation to increase speed
library("HunMineR")#for mining tools regarding hungarian texts
library("quanteda")#for text mining
library("quanteda.textplots")#for data validation with wordcloud
}

tech<-read.csv("techdata.csv")#read in the technical dataframe

clust<-makeCluster(8)#create PCPU clusters again

text<-parLapply(clust,tech$path,function(x)try(pdftools::pdf_text(x)))#parallel reading of all 190 pdfs not that this creates a lists variable, where a list is a pdfs with a string vector, where every value is a page

#we need to decompose this into data.frame form, while considering into several elements notably: keeping page numbers, document numbers & and being able to find and separate paragraphs where the speakers name are, and where the speech is
#while also eleminating the part where it isn't the speeches yet, but only some introductory infopages

{
textlist<-list()#we create a list

for(I in 1:length(text)){
  textlist[[I]]<-lapply(text[[I]],function(x)str_split_1(x,"\n|(?<=\\:)")%>%data.frame)%>%bind_rows(.id="page")
  names(textlist[[I]])<-c("page","text")
  print(I)
}

#quite a complicated process but i try to break it down, so this cycle first extracts a list, and then proceeds to break the pages along whitespaces and the charachter ":",
#of course due to length asymmetry, this can be only done if a pdfs result is a new dataform with each page being a sublist
#then we unlist it into a data.frame through the bind_rows function, where the first column keep the sublist's number which is equal to the pdfs page, and the second page is a broken up text vector
#then this N x 2 data.frame is saved out into the a list of the endgoal of "textlist"
#this result in 190 something x 2 data.frame in list form
  
textframe<-bind_rows(textlist,.id="pdf")
#we reformulate the list's content into one singular dataframe, where there is a new variable which is indicative of the textlist list number which is synonomus with the pdf document
remove(textlist)#remove the bridging list for memory 
}

{
data<-textframe#create "data" variable to be subject for manipulation
data$text<-str_squish(data$text)#standardize spaces within the texts
data$elv<-gsub(" ","",data$text)#create and elv variable which goal is to separate speaker names, document introductions, and speeches
aux4<-!grepl("/|\\(",data$elv)#collect certain values from this variable
data$elv<-tolower(data$elv)#lower the elv text
data$elv<-stri_trans_general(data$elv,"Latin-ASCII")#normalize the elv text
data$elv<-gsub("[^a-z:]","",data$elv)#retain only the letters in the elv text along with ":"

aux1<-str_sub(data$elv,-8,-2)
aux1<-stringdist(aux1,"elvtars")<3
aux2<-str_sub(data$elv,-10,-2)
aux2<-stringdist(aux2,"elvtarsno")<2
aux3<-str_sub(data$elv,-1,-1)==":"

#we took all of this measures so the logical criteria for what can be considered a speaker
#it needs to end on ":" #and also it need to contain elvtars / elvtarsno within some levensthein distance in the last 7 or 9 charachter
#finally it cannot contain (or / since these indicate comments which dont change the speaker 

data$elv<-aux3*aux4*I(aux1+aux2)%>%as.numeric()
remove(aux1,aux2,aux3,aux4)
data$elv<-c(0,diff(data$elv))
data$elv<-data$elv%>%abs%>%cumsum

#some mathematical manipulation / transformation such that the logical analysis we had is create ordered identifier for each speaker and speech

data[,1:2]<-apply(data[,1:2],2,as.numeric)#we numerize both the page and document number
data[,1]<-tech$pdf[data[,1]]#we substitute the appropriate document title for the numbers

assist<-tapply(data$elv,data$pdf,min)
assist<-matrix(c(names(assist),assist),ncol=2)
assist<-data.frame(assist)
colnames(assist)<-c("pdf","minmax")
data<-left_join(data,assist)
data$minmax<-as.numeric(data$minmax)

data$filter<-I(data$elv-data$minmax)!="0"
data<-data[data$filter,]

#this algorhytm is once again tricky. To eleminate the part before the first speaker we look which is the smallest elv number for each document (it is one smaller than elv number of the first speaker of the document), and omit the text which have equal elv number as their smallest document elv number
#this eleminates the beginning of the documents
remove(assist,I)#remove excess variables
}

{
X<-tapply(data$text,data$elv,function(x)paste(x,collapse=" "))#we paste the text of each elv number into one
X<-matrix(X,ncol=2,byrow = T)#we create n x 2 matrix, due to the nature of elv number in the first column are the speaker in the second are the speeches
X<-data.frame(X)#we data.frame this matrix
colnames(X)<-c("name","text")#give proper column names
X$elv<-1:nrow(X)*2-1#and reformulate the elv number into a normal form so they are odd numbers, with this it is compatible to join the metadata from "textframe" into our new dataframe "X"

comp<-left_join(X,data[,c(1,2,4)]) #left join the metadata
comp$loc<-apply(comp[,5:4],1,function(x)paste(x[1:2],collapse="pagein"))%>%str_squish() #create an unique location identifier which indicates both page number and the respective documents
remove(X)#remove excess variables
}

{
motions<-apply(tech[,c(4:5)],1,function(x)cbind(x[1],x[2]%>%read_html%>%html_nodes(".tr2 tr td")%>%html_text2(preserve_nbsp = T))%>%data.frame)
motions<-motions[sapply(motions,ncol)==2]
motions<-bind_rows(motions)
}#looking at the metadata page we can see that what were discussed in different pages of a document, we wish to extract these information, and save it into a data.frame

{
cont<-cbind(motions$X1,motions$X2%>%str_split_fixed("\n",Inf))
cont<-data.frame(cont)
cont<-cont[,c(1,2,4:ncol(cont))]
colnames(cont)[1:2]<-c("pdf","motion")
cont<-melt(cont,id.vars=c("pdf","motion"))
cont<-cont[,c(1,2,4)]
cont$value<-ifelse(cont$value=="",NA,cont$value)
cont<-na.omit(cont)

cont[,4:5]<-str_split_fixed(cont$value,"-",2)
cont[,4:5]<-apply(cont[,4:5],2,as.numeric)
cont[,5]<-ifelse(is.na(cont[,5]),cont[,4],cont[,5])
cont<-na.omit(cont)
cont<-cont[,c(1,2,4,5)]
}#tricky data manipulation technique; briefly it extracts the lower and upper page bound of the interval for each discussion of each document

{
list<-apply(cont,1,function(x)cbind(x[1],x[2],x[3]:x[4])%>%data.frame)
list<-list%>%bind_rows()
colnames(list)<-c("pdf","motion","page")

list$loc<-apply(list[,c(3,1)],1,function(x)paste(x,collapse="pagein"))
list<-list[,c(4,2)]
list<-unique(list)

#followung this through brute force we create a respective location identifier same to the one we have at "comp" to each number of the interval

list<- list %>% 
  group_by(loc) %>% 
  mutate(
    motion = str_c(motion, collapse = " ; "))
motions<-unique(list)
remove(cont,list)
#there happens to be cases where there is more than one topic on the same location identifier we merge these topics into one, in order to avoid a many-to-many relation when joining databases
}

{
comp<-left_join(comp,motions)#we join the two dataframe

date<-apply(tech[,c(4:5)],1,function(x)cbind(x[1],x[2]%>%read_html%>%html_nodes("h3+ h3")%>%html_text())%>%data.frame)%>%bind_rows()#we extract the dates from the metadata pages
date<-date[,1:2]
colnames(date)<-c("pdf","date")#formulate it into a proper data.frame which is joinable to "comp"

comp<-left_join(comp,date)#join the data with comp
comp$doc_id<-comp$elv/2+0.5#create a preliminary doc_id variable
rawcorpus<-comp[,c(9,8,7,1,2)]#create a rawcorpus by only keeping the doc_id the metadata & the text
}

{
  rawcorpus$era<-rawcorpus$date%>%substr(1,4)%>%as.numeric()#we extract the dates as a numeric year
  rawcorpus$era<-sapply(rawcorpus$era,function(x)sum(x>c(0,1962,1968,1978)))#we categorize the year into 4 era 1956-1962;1963-1968;1969-1978;1979-1989
  rawcorpus$era<-c("Megtorlás","Új Gazdasági Mechanizmus","Érett Kádárizmus","Kései Kádárizmus")[rawcorpus$era]%>%as.factor()#we give names to these categories
  rawcorpus$date<-gsub(".","-",rawcorpus$date,fixed=T)%>%substr(1,10)%>%as.Date()#with this concluded we also transform the dates into the R date form
}

{
  rawcorpus$motion<-ifelse(is.na(rawcorpus$motion),"undefined",rawcorpus$motion)#we give undefined name to the topicless speeches
  motion_tokens<-rawcorpus$motion%>%tokens(remove_punct = T,remove_numbers = T)%>%tokens_remove(data_stopwords_extra)#we create a dfm & a wordcloud to find usable clusters for simplyfing topics
  textplot_wordcloud(motion_tokens%>%dfm)
  themes<-c("szervezeti","belpolitikai","pártpolitikai","egyéni","gazdasági","külpolitikai","egyéb")
  X<-matrix(nrow=nrow(rawcorpus),ncol = 7)
  X[,1]<-grepl("szervezeti|kb|központi bizottság|munkaterv",rawcorpus[,3])
  X[,2]<-grepl("politikai|belpolitika|jog|alkotmány|Bel-|honvédelem|állam|egyház|választás",rawcorpus[,3])
  X[,3]<-grepl("párt|mszmp|mdp|tag|kongresszus",rawcorpus[,3])
  X[,4]<-grepl("személyi",rawcorpus[,3])
  X[,5]<-grepl("gazdaság|ipar|életszínvonal|ötéves terv|hároméves terv|agrár|építés|termelő|költségvetés",rawcorpus[,3])
  X[,6]<-grepl("nemzetközi|kgst|moszkva|varsói szerződés|szkp|szovjet|küldöttség|külpolitika",rawcorpus[,3])
  X[,7]<-rowSums(X[,1:6])==0
  #we used certain frequent and relevant words for giving categories to our speeches, this results in a 7 column logical matrix where each column indicates the presence of that topic
  tema<-apply(X,1,function(x)paste(themes[x],collapse="; "))#we substitute that logical variable to the "theme" string and collapsed the combination of the 7 values into 1
  rawcorpus$motion<-tema#subsitute the motion with these categories
  remove(tema,X,motion_tokens)#remove excess variables
}

{
  name<-gsub(" ","",tolower(rawcorpus$name))#we extract the speaker names
  name<-stri_trans_general(name,"Latin-ASCII")
  name<-gsub("elvtars|elvtarsno|\\:","",name)
  name<-gsub("[^a-z]","",name)
  
  #normalize it lower it; and remove every non a-z charachter from it
  
  tab<-table(name)
  nametype<-names(tab)
  tab
  
  dist<-stringdistmatrix(name,nametype)<3
  dist<-t(t(dist)*as.numeric(tab))
  suggest<-nametype[apply(dist,1,which.max)]
  
  tab2<-table(suggest)
  relevant<-tab2[tab2>20]%>%names()
  relevant<-relevant[!relevant%in%c("tompe","marosan","revesz","kadar")]
  
  allnames<-ifelse(suggest%in%relevant,suggest,"other")
  
  table(allnames)
  
  GOOD<-rawcorpus$name
  GOOD<-gsub("elvtárs|elvtársnő|\\:","",GOOD)%>%str_squish()%>%str_to_title()
  
  tab<-table(allnames,GOOD)%>%data.frame()
  tab<-tab[order(tab$Freq,decreasing=T),]
  tab<-tab[!duplicated(tab[,1]),1:2]
  
  tab<-apply(tab,2,as.character)
  
  tab[,2]<-ifelse(tab[,1]=="other","Other",tab[,2])
  r2tab<-word(tab[,2],1,1)
  r2tab<-gsub("[^a-z]","",tolower(r2tab)%>%stri_trans_general("Latin-ASCII"))
  tab<-rbind(tab,cbind(r2tab,tab[,2]))%>%data.frame()
  tab<-tab[!duplicated(tab[,1]),]
  relevant<-tab[,1]
  
  allnames<-ifelse(suggest%in%relevant,suggest,"other")
  
  data<-left_join(data.frame(allnames),data.frame(tab))
  rawcorpus[,4]<-data[,2]
  remove(data,dist,tab,allnames,GOOD,name,nametype,r1tab,r2tab,relevant,rtab,suggest,tab2,themes)
}#This one involves a very complicated procedure which basically attempts to cluster frequent speaker names by measuring levensthein distances; simplyfing text and reverse engineering it; and by filling up only family name cases with the full name
#validation seems to show that it were effective at finding the major hungarian polticians; those who made +20 speeches

{
  rawcorpus<-rawcorpus[,c(1,6,2,3,4,5)]
  rawcorpus<-rawcorpus[order(rawcorpus$date),]
  rawcorpus$doc_id<-1:nrow(rawcorpus)
}#some reorganisation of the corpus to a more convienient form, arraging into a chronological order, and adjusting the doc_id respectively


write.csv(rawcorpus,"rawcorpus.csv",row.names=F)#write out the raw corpus
