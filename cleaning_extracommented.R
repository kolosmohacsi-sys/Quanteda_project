
library("stringr")#package for string manipulation
library("HunMineR")#i wish to import the stopword dictionary from this packages
library("dplyr")#package for data table manipulation
library("quanteda")#package for text mining and for further data cleaning
library("quanteda.textplots")#we will use wordcloud as a method of validation
library("udpipe")#package providing an alternative for a quicker but more rudimentary lemmatization algorhytm, this is necessary due to the extremely large size of the corpus

humodel<-udpipe_load_model("hungarian-szeged-ud-2.5-191206.udpipe")#we load in the university of szeged model for udpipe

rawcorpus<-read.csv("rawcorpus.csv")#read in the raw corpus, and we transform the metadata in the proper format
rawcorpus$era<-as.factor(rawcorpus$era)#factor for the era
rawcorpus$date<-as.Date(rawcorpus$date)#date for the date
rawcorpus$motion<-as.factor(rawcorpus$motion)#factor for the nature of the motions topic
rawcorpus$name<-as.factor(rawcorpus$name)#factor for the name of the speaker

rawcorpus$text<-gsub("-","ß",rawcorpus$text,fixed=T)#substitute "-" with ß temporarly, due to the fact that "-" gets confused in my regex expressions
rawcorpus$text<-gsub("[^a-záéíóöőúüűß ]"," ",rawcorpus$text%>%tolower)#lower the text and remove everything which isn't space, in the hungarian alphabet or ß
rawcorpus$text<-gsub("ß","-",rawcorpus$text,fixed=T)#restore ß to "-"
rawcorpus$text<-str_squish(rawcorpus$text)#standardize space sizes to 1
rawcorpus$text<-gsub(" -","",rawcorpus$text,fixed=T)#collapse the separators, all 3 possible cases of them
rawcorpus$text<-gsub("- ","",rawcorpus$text,fixed=T)
rawcorpus$text<-gsub("-","",rawcorpus$text,fixed=T)

KB<-rawcorpus#create a new variable which will be the subject of further manipulation

KB_corpus<-corpus(KB)#create a quantenda based corpus out of the data.frame

KB_tokens<-tokens(KB_corpus)%>%tokens_select(min_nchar=4)%>%tokens_remove(data_stopwords_extra)
#two things happen here: 1.) we remove every token which has a length less than 4 charachters, this eleminates the majority of text errors. 2.) secondly we remove stopwords in order to win some computation speed

nchar(KB$text)%>%sum #measure the size of our corpus
KB_DFM<-dfm(KB_tokens)%>%dfm_trim(min_termfreq = 100)#organize our tokens into a DFM, and remove sparse terms in order to win further computation speed. due to the extremely large nature of the corpus a feature appearing less than 100 cases can be considered sparse, although because we lemmatize later, we theoritcally / and perhaps practically compromise some information for this.
relevant<-KB_DFM@Dimnames$features#extract the non-sparse feature names (DFM colnames)

KB_tokens<-KB_tokens%>%tokens_select(relevant)#filter out the sparse features from our tokens (or more accurately keep those which aren't sparse)

textplot_wordcloud(KB_DFM)#analyze the wordcloud, unweighed
textplot_wordcloud(KB_DFM%>%dfm_tfidf)#analyze the wordcloud, weighed
#there is still plenty of "rubbish" or very frequent cases in our data

spec_stopword<-c("központi","bizottság","kb","pb",
                 "elvtárs","elvtársak","tisztelt","kedves",
                 "magyar","szocialista","párt","mszmp",
                 "kommunista",
                 "országos","levéltár",
                 "politikai","személyi","gazdasági","társadalmi","nemzeti","nemzetközi","állami","megyei",
                 "kérdés","napirend")

#from the weighed wordcloud list some of the words, which are either irrelevant when it comes to the MSZMP central commitee, and/or any decision making body, secondly we also remove very vague but frequent topic labels such as economic or political

filter_token<-KB_tokens%>%tokens_remove(spec_stopword)#remove the corpus specific stopwords

filter_DFM<-dfm(filter_token)#write our filtered tokens into a DFM

lemma<-udpipe(filter_DFM@Dimnames$features,humodel)[,10]#find the lemmatized (or more accurately dictionary) form for the features of our DFM

lemma_tokens<-tokens_replace(filter_token,filter_DFM@Dimnames$features,lemma)#replace the tokens with their respective lemmatized form
lemma_DFM<-dfm(lemma_tokens)#write our transformed tokens into a DFM

textplot_wordcloud(lemma_DFM)#analyze the wordcloud, unweighed
textplot_wordcloud(lemma_DFM%>%dfm_tfidf())#analyze the wordcloud, weighed
#there is nothing unexpected or malign effect of lemmatization, as such we can move with our analysis further

lemma2<-sapply(filter_token,paste,collapse=" ")#transform our transformed tokens back into a string form

hist(lemma2%>%nchar%>%log)#analysis indicate that there is a log-normal nature found within the length of the speeches, as such our interval will be multiplicative

KB<-KB[nchar(lemma2)>2000&nchar(lemma2)<20000,]#we subset for speeches between 2000 approx exp (7.5) and 20000 approx exp(10) as our interval, as such we can capture the second cluster of the distribution

KB$text<-lemma2[nchar(lemma2)>2000&nchar(lemma2)<20000,]#for our convinience we transform our dataframe's text variable back into the lemmatized form

table(KB$name)#several relevant hungarian communist politician (for example Pozsgay Imre) has at least 10 or 15 cases which provide plenty of data for analysis
table(KB$era)#the amount of speech for each era is unbalanced
tapply(KB$text,KB$era,function(x)sum(nchar(x)))#the same principle is true when looking at charachter lengths
tapply(KB$text,KB$era,function(x)mean(nchar(x)))#luckily the speeches average length aren't as much influenced by it, as such analysis of different time frame is possible, although for several other reasons it is questionable

#by looking at metadata level descriptatory statistics, it is concludable, that our corpus is sufficent for analysis

remove(stem_token,stem_DFM,spec_stopword,filter_DFM,filter_token,relevant,humodel,lemma,lemma2,I,lemma_DFM,lemma_tokens,lemmalist)#remove excess variables

KB_corpus<-corpus(KB)#recreate corpus
KB_tokens<-tokens(KB_corpus)#tokenizing it again
KB_DFM<-dfm(KB_tokens)#"DFM"-ing it
KB_DFM<-KB_DFM%>%dfm_tfidf()#weighing it once again

textplot_wordcloud(KB_DFM)#we once again analyze the weighed DFM
#there is some stopword material found; first there are some very frequent features which are likely due to cut offs make no sense; Secondly there are some frequent expressions like "szóló", "ülésen" which is so unambigous to a decision making body that it contain no information to our analysis; 
#Thirdly we remove words which whille make a speech or comment more polite like: "érzem" or "szeretnék" but provides no value for our analysis regarding content

KB_tokens<-tokens(KB_corpus)%>%tokens_remove(c("azes","illió","szem","ttsá","ille","litik","term","dolog","érzem","mondom","mondani","gondolom","hiszem","szót","vita","valamit","illeti","szóló","valóban","tulajdonképpen","mintegy","kérdést","ülésen","amire","lehetne","véleményem","tűnik","szeretnék","tartom","tudjuk","tudom","javaslat","ülés"))
#we list these further stopwords and remove them from our tokens

KB_DFM<-dfm(KB_tokens)#recreate the DFM
KB_DFM<-KB_DFM%>%dfm_tfidf()#reweigh it once again
textplot_wordcloud(KB_DFM)#validate the data our with wordclouding, i concluded it is acceptable for analysis
length(KB_DFM@Dimnames$features)#we look how diverse our DFM is: its about 7000 features which even with the sparse term elimination and lemmatization and several other standardizations, can be considered diverse

#there are some things which leaves room for improvement within the corpus i wish to list them in a reverse chronoligcal order to the assembly of the corpus:
#1.) One could argue that dfm_tfidf isn't the best weighing algorhytm for our data for example due to the corpus text size differences, i primarly choose this method due to it's efficacy of emphasizing valuable/unique information while requiring no parameters; but one cloud argue that for example the logarithmic nature of the length of our data requires alternative weighing methodology
#2.) I categorised & eleminated custom stopwords in highly consequentialist way, or in other words: i took into consideration those stopwords which were the most present by my wordcloud validation methodology. while i believe the for the time it is sufficient for data cleaning, i have to agree it's efficacy (due to the method's high bias) is vulnerable to changes of the corpus's composition
#3.) Several compromises regarding precision were made due to the size of the corpus: 
  #a.) sparse terms were eleminated before lemmatization, which leaves room for error, in cases in which a tokens dictionary form wouldn't have been sparse, this problem is due to hungarian langauge's high diversity in the appendages which in their unprocessed form were considered sparse & as such were omitted; The high feature count / and relatively low barrier of 100 cases (compared to the whole corpus's 80M charachter length) to be considered non-sparse, gives confidence though that most of the information were retained 
  #b.) I deviated from the learnt & much more robust lemmatization process of spacy for udpipe's quicker computation speed, this can result in less accurate lemma's within the corpus.
#4.) The elemination of tokens having less than 4 charachters is a rather blunt way to handle the pdf's common text errors. However comparing it to a R compatible spellchecking algorhytm (like hunspell) & evaluating the 2 and 3 letter words rather small potential value find within the corpus (like kb or pb as acronym), gives us confidence that this method is the best both in terms of simplicity, regarding non NLP based information retention & noise reduction.
#5.) The frequent broken words imply a likelihood that non-Hungarian letters were present which were quickly eleminated by our regex expression. This was probably created due to several common OCR reading issues of the website's pdfs (no whitelist of letters; non-hungarian OCR model; low quality pdfs; no pdf preprocessing). This brings the question? Why did i still utilised this rather . The reasons are the following four: 
  #a.) Originally i planned to normalize these letters, however i only managed to find tools where the normalisation process's goal langauge is english not hungarian (found within the package stringi). 
  #b.) The high letter diversity of UTF-8 resulted in a sitatuion were there wasn't sufficent time to create an own normalization algorhytm/process for hungarian.
  #c.) A lot of junk charachters were present for example: pages are show like "- page number -" which would not have been eleminated under the quanteda tokens() function's data processing aspects/additions
  #d.) Looking at "rawer" forms of the text showed that line breaks sometimes weren't followed with the charachter "-", meaning that word breaks due to a new line & and new lines where the word doesnt break sometimes aren't separable by regex methods. With this in mind i choose to favour the latter due to it's bigger relevance & frequency.
#6.) The formulation of metadata, regarding speaker & motion topics are both improvable:
  #a.) Speakers were extracted by the principle of ending on "elvtárs:" or "elvtársnő:" this leaves room for spelling errors / OCR reading problems / . Any further attempt however brings in a too high likelyhood of misidentfication. For example extending the ending with "." systematically bring in the problem of a rather common sentences of: "XY elvtárs: Következő, YZ elvtárs." To be considered as 2 speakers.
  #b.) One could argue that the topics were clustered in an arbitrary constructed dictionary, wnich was formulated through analyzing a wordcloud. While this always leaves room for criticism for the time i believe the it is easier and probably more accurate than a non supervised method of learning (like k-means) built on the motion metadata.
#7.) There were several pdfs which had parsing erros and as i wasn't been able to read them in, in the future i also wish to integrate these pdfs into my corpora, with reparing these sources by downloading with a method in which it doesnt get corrupted or somehow fixing it in post-download.

#However even with these in mind i consider the corpus assembly and cleaning to be quite succesful, especially considering the scope of which this dataset attempts to encompass, and the level of detail it possess

write.csv(KB,"cleandata.csv",row.names = F)#write out the clean dataframe
