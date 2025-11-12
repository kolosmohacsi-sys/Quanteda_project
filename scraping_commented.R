
{
library("webdriver")#wrapper for a Phantom JS webdriver in R
library("rvest")#for html reading and extracting nodes and text data from it
library("stringr")#for string manipulation
library("parallel")#for parallel programming, a process which increases the speed of repeated semi complex tasks by (for my computer) approx. 8 times
library("dplyr")#for dataset manipulation
}#load the necessary packages

{
JS<-run_phantomjs()#utlisation of a PhantomJS webdriver to let JSON-s load in

S<-Session$new(port = JS$port)#creating a session within the webdriver

#looking at the website the webpage uses it's url for imitating a page turn, so we can extract all pages by just chaning the url

body<-"https://adatbazisokonline.mnl.gov.hu/adatbazis/mszmp-jegyzokonyvek/kereses?term=eyJxIjoiIiwiZnEiOnsiZGJfaWQiOnsiZDZiYWY2NWUwYjI0MGNlMTc3Y2Y3MGRhMTQ2YzhkYzgiOiIyNjQifSwiZmFjZXRfdGVzdHVsZXQiOnsiM2EwYTFkYzZmZGI4YWY3ZWMwOWMwOTdhOGI4NjY3YWIiOiJcIktcdTAwZjZ6cG9udGkgQml6b3R0c1x1MDBlMWdcIiJ9fSwic29ydCI6InJlY19vcmRlciJ9&P="
#set the url body

link<-paste(body,0:10,sep="")#paste it into the desired amount of pages

source<-sapply(link,function(x)S$go(x)$getSource())#extract the html in string form

write.csv(source,"metasource.csv",row.names = T)#write the it out to a csv just in case
remove(JS,S,link,body)#remove the no longer used variables
}#download the source of the metadata

{

html<-lapply(source,read_html)#read the written out into a rvest based form

url1<-lapply(html,function(x)x%>%html_nodes(".resultTile_text a")%>%html_attr("href"))#extract the file's metadata page url
url1<-lapply(url1,function(x)unique(x))%>%unlist()%>%as.character()#unlist the urls, and delete the url duplication (each url is mentioned twice due to the html structure of the webpage)
url1<-str_split_fixed(url1,"\\?",2)[,1]#keep the part of url which is left of the "?" this remove the excess search terms
#note this is only a path, we need add the main body to create a valid url
url1<-paste("https://adatbazisokonline.mnl.gov.hu",url1,sep="")#formulation into a valid url

url2<-lapply(html,function(x)x%>%html_nodes(".resultButton:nth-child(2)")%>%html_attr("href"))#extract the pdfviewver links of the documents into a list
url2<-lapply(url2,function(x)unique(x))%>%unlist()%>%as.character()#unlist the urls, and delete the url duplication (each url is mentioned twice due to the html structure of the webpage)
url2[157]<-"/pdfview2?file=static/documents/mszmp_mdp/HU_MNL_OL_M-KS_288_04_02420-02430.pdf"#157 pdf title is faulty in the website so we manually corrigate it
url2<-gsub("/pdfview2?file=","",url2,fixed=T)#with some simple manipulation we create a non pdfviewer based link of the pdf, making the pdfs downloadable
#note this is only a path, we need add the main body to create a valid url
url2<-paste("https://adatbazisokonline.mnl.gov.hu/",url2,sep="")#formulation into a valid url


tech<-data.frame(url1,url2)#we create a data.frame out of these infomation
tech$path<-gsub("https://adatbazisokonline.mnl.gov.hu/static/documents/mszmp_mdp/","pdfs/",tech$url2)#formulate the a path within the workspace where to download the pdfs (note i choose an unique folder creatively name pdfs)
tech$pdf<-gsub("https://adatbazisokonline.mnl.gov.hu/static/documents/mszmp_mdp/","",tech$url2)#formulate a also a pdf name for later convinence


remove(source,html,url1,url2)#remove excess variables
}#Assemble technical data into a data.frame

{
clust<-makeCluster(8)#create CPU cluster

files<-list.files(path="pdfs")#list the already downloaded files

download<-tech[!tech$pdf%in%files,c(2,3)]#create a "need to download" data.frame from those pdfs which are listed in the technical dataset but aren't in the pdfs folder

parApply(clust,download,1,function(x)download.file(x[1],x[2],mode = "wb"))#with a parellel method we can download 8 files simulatenously

remove(download)#remove the download folder

meta<-sapply(tech$url1,function(x)S$go(x)$getSource())#follwoing this we also download the JSON script affected metadata page's html, into a string
tech$metahtml<-meta#we add this data to our technical data.frame

write.csv(tech,"techdata.csv",row.names=F)#writing out the technical data into a csv

remove(JS,S,clust,files,meta)#remove the no longer used variables
}

