---
title: "Session1 : text metric and word annotations"
author: "cb,jcrm,bc,oc"
date: "`r Sys.Date()`"
output: 
  html_document :
    toc: true
    toc_float: true
    toc_depth: 3
---

# Tools

Mainly [`Quanteda`](http://quanteda.io/) and [`udpipe`](https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html)

```{r setup, include=TRUE,echo=TRUE,message=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = TRUE, include=TRUE, message=FALSE, warning=FALSE)
library(tidyverse) # a swiss knife
library(udpipe) # syntaxis and lexical annotations
library(flextable) #for table rendering
library(ggwordcloud) #for plotting
library(cowplot) #for plotting
library(quanteda) #a complete text mining package
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
#devtools::install_github("kbenoit/quanteda.dictionaries") if necessary, uncomment to install
library("quanteda.dictionaries")
library(lubridate) #date processing
library(udpipe) #annotations
library(igraph) #network representations


theme_set(theme_minimal())

t1=Sys.time()
```

# Our Data

User Experience project from DITP (Services public+)[ https://www.modernisation.gouv.fr/actualites/services-publics-plus-de-15-000-experiences-dusagers-partagees-sur-la-plateforme)]

Comments are relative to a life experience (for instance, renew a passport) at a certain time, with a specific administration in a certain place.  

## Sentiment distribution (transformer method)

```{r 01}
#read the file and sample to reduce computation ( look at the end)
#we put the content of the csv file into a dataframe and select only certain columns
df <- read_csv("data_original_12062022_normalise_sentiment_transformers_lemna_words_adj.csv")%>%
  select (id, date_ecrit, titre, description, ressenti, pays,intitule, canaux_typologie_1, transformer_sentiment_score) 

# we treat the data
# the mutate function enables us to create, modif or delete a column 
# here, we modify the description column for our future analysis
# to specify what we want to remove, we use regex
df<-df %>% 
  mutate(description=str_replace_all(description,"\\.(?=[A-Za-z])", ". "), #space after dot
         description=str_replace(description,"_", " "), #remove underscore
         description=str_replace(description,"(\u20AC)", "euro") )#change euro in euro


#%>% sample_n(1000)

head(df, 5) #we display the first 5 rows of the data frame
#we plot the transformer sentiment distribution, as a density and as a cumulative distribution 
g1<-ggplot(df,aes(x=transformer_sentiment_score))+
  geom_density(fill="pink", alpha=.5)

g2<-ggplot(df, aes(x=transformer_sentiment_score)) +
  stat_ecdf(geom = "step",pad = FALSE)+
  xlim(0,1)

#cowplot
#The plot_grid() function provides a simple interface for arranging plots into a grid and adding labels to them.
#https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(
  g1, g2,
  labels = "AUTO"
  )
```


Compare with "smiles"

```{r 02}
#score and sentiment
#here, we use the geom_violin function. It enables us to see the probability density curve of different values.
#hence, this is why the curve for the negative score is wider at the bottom while it is wider at the top for sentiment score

g2<-ggplot(df, aes(y=transformer_sentiment_score, x=ressenti)) +
  geom_violin(fill="pink", alpha=.5)+
  ylim(0,1)+
  geom_smooth()+
  labs(x=NULL, y = "Score transformer")

g2
```

## channels involved
The interaction channels involved in the experience. 

```{r 03}
## multiple choice question recoding
#we use the function at.data.frame because the mutate function only works on an object of class "c('matrix', 'array', 'character')"
#str_split_fixed allows us to create 10 column in which we place each typology separated with comma
#for each column containing characters we remove spaces at the beginning and end of sentences with str_trim
typo<-as.data.frame(str_split_fixed(df$canaux_typologie_1, ",", 10))%>%
  mutate(across(where(is.character), str_trim)) %>% 
  cbind(df$id) %>% #we add the id column of "df" to "typo" (on the right)
  rename(id=11) %>%
           pivot_longer(-id, names_to = "valeur", values_to="variable") %>% # we pivot the typo data frame from column to row grouping by id so there are 10 rows per id with each typology. "Valeur" ranges from V1 to V10 and "variable" is the typology
  filter(variable!="") %>% #we get rid of empty rows with no typologies (not every row has 10 typologies)
  group_by(id, variable) %>% #inutile?
  summarise(valeur=1) %>% #we put "1" to the rows chosen so we can recreate typologies columns with 0 or 1 for each id
  pivot_wider(id,names_from="variable", values_from="valeur")%>%
  replace(is.na(.), 0)


#we can now summarise and compute the mean of each typology
n_c<-nrow(typo) #count the number of rows
n_c
foo<-typo %>% 
  pivot_longer(-id, names_to = "variable", values_to="value") %>% 
  group_by(variable)%>%
  summarise(Penetration=mean(value))

ggplot(foo, aes(x=reorder(variable, Penetration),y=Penetration))+
  geom_bar(stat="identity",fill="firebrick")+
  coord_flip()+
  labs(x="Typology", y="Penetration rate")
```

## Administrations involved

```{r 04}
#typo
foo<-typo%>%
  select(-id) #we select everything except the id column

#service
t<-as.data.frame(table(df$intitule)) %>% #table enables us to create a frequency table of "intitule" easily
  filter(Freq>50) #with a at least 50 occurrences


ggplot(t, aes(x=reorder(Var1, Freq),y=Freq))+
  geom_bar(stat="identity",fill="firebrick")+
  coord_flip()+scale_y_log10()+
  labs(x="Typology", y="Freq")
```

Sentiment score comparison

```{r 05}
#we create a column "n" initialized to 1 and group the data by intitule, computing the mean value of sentiment score
# and count the number of rows per intitule with at least 50 occurrences
#we exclude rows where intitule = NA and sort in descending order of score
foo<-df%>%mutate(n=1)%>%
  group_by(intitule)%>%
  summarise(score=mean(transformer_sentiment_score,na.rm=TRUE),n=sum(n)) %>% 
  filter(n>50)%>%
  filter(!is.na(intitule))%>%
  arrange(desc(score))

#we specify levels by which we want the graph to be plotted
foo$intitule <- factor(foo$intitule, levels = foo$intitule[order(foo$score)])


ggplot(foo, aes(x=intitule, y=score ))+
  geom_bar(stat="identity",fill="firebrick")+
  coord_flip()+
  labs(x="Typology", y="Sentiment score")

```

# Text level analysis 

(stylometry)

## counting

the number of words per review.

```{r 06}
#we count the number of words per description and plot it with a log10 scale because
#min = 24, mean = 557, max = 7879
df$n_words<-str_count(df$description)
ggplot(df,aes(x=n_words))+
  geom_density()+scale_x_log10()+
  labs(x="Number of words")
```

Evolution of the number of reviews, size of reviews, and correlation between.

```{r 07}
#We group the written reviews by year so that we can see trends
#the trend line is created with the geom_smooth function
foo<-df %>%
  group_by(date_ecrit)%>%
  summarise(n=n(),
            size_t=mean(n_words))

ggplot(foo, aes(x=date_ecrit, y=n))+
  geom_point()+
  scale_y_log10()+
  geom_smooth()

ggplot(foo, aes(x=date_ecrit, y=size_t))+
  geom_point()+
  scale_y_log10()+
  geom_smooth()

ggplot(foo, aes(x=n, y=size_t))+
  geom_point()+
  scale_x_log10()+  scale_y_log10()+
  geom_smooth()
```

## Readability

We aggregate on the month, with `lubridate` functions

```{r 08, warning=TRUE}
#we use the readability function of quanteda
#Flesch is a measure of readability (1948) which uses the average length of sentences (number of words) and the average
#number of syllables per word. It goes from 0 to 100, the lower the harder to read (for university graduates)
#see https://quanteda.io/reference/textstat_readability.html for details
readability<-textstat_readability(df$description, 
                                  measure = c("Flesch",
                                              "meanSentenceLength",
                                              "meanWordSyllables")) 
#we create foo by binding the 2nd column of df (date) with columns 2 to 4 of readability (flesch,meanSentenceLength,meanWordSyllables)
foo<-cbind(df[,2],readability[,2:4])
foo$date<-as.POSIXct(foo$date)

#we transform the date format and select only some columns and compute the mean of the 3 measures per date
foo1<-foo %>% 
  dplyr::mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))%>%
  select(-Month, -date_ecrit,-Year)%>%
  group_by(date)%>%
  summarise(Flesch=mean(Flesch, na.rm=TRUE), 
            SentenceLength= mean(meanSentenceLength, na.rm=TRUE),
            WordSyllables= mean(meanWordSyllables, na.rm=TRUE))

#we pivot the data to have 3 rows with score per date
#before we had 2019-03-01   61.96939   22.75313   1.439376 (each score for a different metric per column)
#now we have 2019-03-01 Flesch 61.96939
#            2019-03-01 SentenceLength 22.753125
#            2019-03-01 WordSyllables 1.439376
foo2<-foo1 %>%
  pivot_longer(-date,names_to="Variable", values_to="Score")%>%
  drop_na()

#we use foo2 to plot the scores by group (measure)
ggplot(foo2,aes(x=date, y=Score, group=Variable))+
  geom_line(size=1.2, aes(color=Variable), stat="identity")+
  facet_wrap(vars(Variable), scale="free", ncol=1)+
  labs(title = "Experience readability", x=NULL, y=NULL)
```

## Lexical diversity 

```{r 09}
#we use quanteda's tokens function to decompose the string "description" into several tokens and proceed to compute the lexical diversity using the textstat_lexdiv function with 2 different measures
#we also remove numbers, punctuation, symbols and split words connected with "-" (hyphens)
#see https://quanteda.io/reference/textstat_lexdiv.html for details
lexdiv<-tokens(df$description)%>%
  textstat_lexdiv(df$text, measure = c("CTTR", "Maas"),  log.base = 10,
                  remove_numbers = TRUE,  
                  remove_punct = TRUE,  
                  remove_symbols = TRUE,
                  remove_hyphens = TRUE) 
#we add the result of our lexical diversification to foo, grouping by date and summarizing by mean value of CTTR of Maas
foo<-cbind(df,lexdiv[,2:5])
foo1<-foo %>% mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))%>%
  group_by(date) %>%
  summarise(CTTR=mean(CTTR, na.rm=TRUE), 
            Maas= mean(Maas, na.rm=TRUE)) %>%
  pivot_longer(-date,names_to="Variable", values_to="Score")

#We then plot the lexical diversity over time
ggplot(foo1,aes(x=date, y=Score, group=Variable))+
  geom_line(size=1.2, aes(color=Variable), stat="identity")+
  facet_wrap(vars(Variable), scale="free", ncol=1)+
  labs(title = "Lexical diversity", x=NULL, y=NULL)

foo1<-foo %>% 
  mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))%>%
  group_by(date) %>%
  summarise(CTTR=mean(CTTR, na.rm=TRUE), 
            Maas= mean(Maas, na.rm=TRUE))

#we compute correlation between two methods of lexical diversity and plot the dots with a regression line
cor(foo1$CTTR, foo1$Maas)
ggplot(foo1,aes(x=CTTR, y=Maas))+
  geom_point(size=1.2, aes(color=date), stat="identity")+
  labs(title = "Lexical diversity", x=NULL, y=NULL)+geom_smooth(method="lm")

```

## Sentiment analysis

We use here a very simple function of sentiment analysis with the "syuzhet" package and "NRC" lexicon.
For every sentence, the syuzhet package transforms every word in the sentence found in the lexicon into a numerical value. The total score is then a computation of every numerical value in the sentence. It is limited as it does not take into account amplifiers (very, much, etc.), negations, etc.

More details can be read here:
http://www.digitalhumanities.org/dhq/vol/16/2/000612/000612.html
https://annieswafford.wordpress.com/2015/03/02/syuzhet/


```{r 10, eval=TRUE}
#library(syuzhet) sentiment analysis

#we specify that we want to use the NRC dictionary and the french language
#we concatenate the title and the description by adding "." between the two and we put it all in the phrase variable
#more info here on the syuzhet package here : https://cran.r-project.org/web/packages/syuzhet/syuzhet.pdf
method <- "nrc"
lang <- "french"
phrase<-as.character(paste0(df$titre,". ",df$description))

#we can now extract emotions by using the get_ncr_sentiment function
#we get 8 emotions [1:8] and two polarities (positive and negative [9:10])
emotions <- get_nrc_sentiment(phrase,language = "french")

```

Now we plot the evolution

```{r 11}
#we put only emotions in the emotion variable and only polarities in the polarity variable
emotion<-emotions[,1:8]
polarity<-subset(emotions,select=c(positive, negative))
foo<-cbind(df,polarity)%>%
mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))

#we compute the positive and the negative polarities by the number of words since syuzhet only adds every numerical value of the sentence to compute a score
foo1<-foo %>% 
  mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))%>%
  mutate(positive=positive/n_words, 
         negative=negative/n_words)%>%
  group_by(date) %>%
  summarise(positive=mean(positive, na.rm=TRUE), 
            negative= -mean(negative, na.rm=TRUE),
            valence=positive+negative,
            expressivity=positive-negative) %>%
  pivot_longer(-date,names_to="Variable", values_to="Score")

#we plot the data by grouping by variable and choosing different colors for each
ggplot(foo1,aes(x=date, y=Score, group=Variable))+
  geom_line(size=1.2, aes(color=Variable), stat="identity")+
  labs(title = "Sentiment", x=NULL, y=NULL)+
  scale_colour_manual(values=c("Orange"," Red", "Darkgreen","Grey"))
```

## Dictionnary methods : LIWC

```{r 12}

#library("quanteda.dictionaries")
#we load the liwc french dictionary and select only certain terms in the description
#the liwcalike allows us to classify certain words into certain categories
dict_liwc_french <- dictionary(file = "FrenchLIWCDictionary.dic",
                             format = "LIWC")
test<-liwcalike(df$description,dictionary = dict_liwc_french)%>%
  select(je, vous, il, ils, pronomimp)
foo<-cbind(df,test)


foo1<-foo %>% 
  mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))%>%
  group_by(date) %>%
  summarise(je=mean(je, na.rm=TRUE), 
            vous= mean(vous, na.rm=TRUE),
            il_s=mean(il+ils,na.rm=TRUE),
            pronomimp=mean(pronomimp,na.rm=TRUE)) %>%
  pivot_longer(-date,names_to="Variable", values_to="Score")

ggplot(foo1,aes(x=date, y=Score, group=Variable))+
  geom_line(size=1.2, aes(color=Variable), stat="identity")+
  labs(title = "Sentiment", x=NULL, y=NULL)+
  scale_colour_manual(values=c("Orange"," Red", "Darkgreen","Grey"))



```

## Own-made dictionnary

Just building list of words and give them a weight.

```{r 14}
#we create a custom lexicon (relative to tax administration) that we use to get the sentiment of the descriptions. We all give them equal weight (=1)
#first we plot the custom distrib by score, then we plot the mean score per month
my_text <- df$description
method <- "custom"
custom_lexicon <- data.frame(word=c("impot", "imp�t","impots", "imp�ts", "taxe","taxes", "fisc", "fiscal", "fiscales", " fiscaux", "fiscalit�", "redevance"),
                             value=c(1,1,1,1,1,1,1,1,1,1,1,1))
custom_distrib <- get_sentiment(my_text, method = method, lexicon = custom_lexicon)

custom_distrib<-as.data.frame(custom_distrib)
ggplot(custom_distrib,aes(x=custom_distrib))+geom_histogram()+scale_y_log10()

foo<-cbind(df,custom_distrib)

foo1<-foo %>% 
  mutate(Year=year(date_ecrit), Month=month(date_ecrit), date=my(paste(Month, Year)))%>%
  group_by(date) %>%
  summarise(custom_distrib=mean(custom_distrib, na.rm=TRUE))

ggplot(foo1,aes(x=date, y=custom_distrib))+
  geom_line(size=1.2,stat="identity")+
  labs(title = "Fisc", x=NULL, y=NULL)+
  scale_colour_manual(values=c("Orange"," Red", "Darkgreen","Grey"))


```


# Word level analysis

Annotations are applied at the word level: lexical (stem, lemma, wordnet) and syntactic (SP, syntactic dependencies) annotations.


## Tokens

The very first step. Useful to set the vocabulary

```{r 015}
library(plotly)
#df$text<-str_replace(df$text, "\\w+", "J ") # trouver la solution!!!! pour le '
#we put every description into a corpus. We delete punctuation, numbers and symbols because we don't need them here
#we remove french stopwords: a list of words that don't bring much value to the analysis (for instance: "le", "la", "du", "ce", etc. ).
corpus<-corpus(df,id_field = "id",text_field = "description")
foo<-tokens(corpus,remove_punct = TRUE, remove_symbols=TRUE, remove_numbers=TRUE)%>%
  tokens_remove(stopwords("french"))

head(foo,5)

foo1 <-unlist_tokens(foo) #we create one row per word with doc_id = the number of the sentence
dim(foo1) #gives dimensions of foo1 (numbers of rows and columns)

#we resume foo2 by the number of tokens and create a new column which gives the rank of the token
#then sort foo2 by the rank of the token ("a" is the 1st token, "j'ai" the 2nd, "plus" the 3rd)
foo2<-foo1 %>% 
  group_by(token)%>%
  summarise(n=n())%>%
  mutate(rank=rank(desc(n)))%>%
  arrange(rank)
                     
dim(foo2)

ggplot(foo2, aes(x=rank,y=n))+
  geom_point(alpha=.2)+geom_smooth(method=lm)+
  scale_x_log10()+
  scale_y_log10()+
  labs(title = "Zipf like")

#with cleaning
#dftest <- dfm(foo)%>%dfm_trim(min_termfreq = 3) => same results than dfmat1
#not useful to remove stopwords and punctuation here since we have already done it just above

dfmat1 <- dfm(foo,
              remove = stopwords("french"), remove_punct = TRUE) %>%
   dfm_trim(min_termfreq = 3)

textplot_wordcloud(dfmat1, max_words = 50)

#we only look at a subset of the corpus where the service is the "CAF" (for "Caisse d'allocations familiales")
dfmat2 <- dfm(corpus_subset(corpus, intitule == "CAF"),
              remove = stopwords("french"), remove_punct = TRUE) %>%
   dfm_trim(min_termfreq = 3)

textplot_wordcloud(dfmat2, max_words = 150)

```


## collocation

Collocations are words that often appear closely together in a text. For instance "fast food", "quick shower", "quick meal", "car park", etc. are collocations (https://en.wikipedia.org/wiki/English_collocations for more examples). Co-occurrences are different from collocations because they don't need to be adjacent and form a meaning. 


```{r 16}

#we select words in foo beginning with a capital letter. We use the tokens_select function from quanteda and a regex to identify it 
toks_cap <- tokens_select(foo, 
                               pattern = "^[A-Z]",
                               valuetype = "regex",
                               case_insensitive = FALSE, 
                               padding = TRUE)

#searching for collocations with a minimum of 3 occurrences. We don't put the words in lowercase
tstat_col_cap <- textstat_collocations(toks_cap, min_count = 3, tolower = FALSE) %>%
  arrange(desc(count))

head(tstat_col_cap,15)


toks_comp <- tokens_compound(foo, pattern = tstat_col_cap[tstat_col_cap$lamba > 10], 
                             case_insensitive = FALSE)

head(toks_comp)

```

## Lemmas and POS

```{r 17, eval=FALSE}
#we load the udpipe french model to annotate the descriptions
#library(udpipe)
fr <- udpipe_download_model(language = "french")
udmodel_french <- udpipe_load_model(file = "french-gsd-ud-2.5-191206.udpipe") #22.7MB to download

UD <- udpipe_annotate(udmodel_french, x=df$description, trace =1000) #trace indicates progress every 1000 rows
UD <- as.data.frame(UD)
saveRDS(UD, "UD.rds") #saves the result as a RDS file
#we can also save the result as a csv file :
#write.csv(UD,"UD.csv", fileEncoding = "UTF-8")
```

Let examine the content : lemma per POS.

```{r 18}
#we can open the csv file with the following function:
#read.csv("UD.csv",fileEncoding = "UTF-8")
#UD<-readRDS("UD.rds")
#we plot the number of occurrences of each upos 
foo<-UD%>%
  group_by(upos)%>%
  summarise(n=n())%>%
  #drop_na(upos)%>%
  ggplot(aes(x=reorder(upos,n), y=n))+
  geom_bar(stat = "identity")+coord_flip()

foo

#we plot the lemmas of nouns by the number of occurrences with a minimum of 1000
foo<-UD %>%
  filter(upos=="NOUN") %>%
  group_by(lemma)%>%
  summarise(n=n()) %>%
  filter(n>1000)%>%
  ggplot(aes(x=reorder(lemma,n), y=n))+
  geom_point(size=1.5, fill="blue3")+coord_flip()

foo

#we make a word cloud with both adjectives and verbs (min occurrences of 300)
foo<-UD %>%
  filter(upos=="ADJ" |upos=="VERB") %>%
  group_by(lemma, upos)%>%
  summarise(n=n()) %>%
  filter(n>300)%>%
  ggplot(aes(label = lemma, size = log(n), group=upos)) +
  geom_text_wordcloud(aes(color=upos)) +
  theme_minimal()+
  facet_wrap(vars(upos))

foo
```


## syntaxic dependance

A few `dplyr gymnastics', and that's already the session 2. 

[We follow](https://rpubs.com/pjmurphy/317838) to get some code ideas.

for dataviz `igraph` is the tool.


```{r 19, fig.width=12}
foo1<-UD %>%
  mutate(id=paste0(doc_id,paragraph_id,sentence_id,token_id))%>%
  select(id, lemma)%>%
  rename(noun=lemma)

foo<-UD %>%
  filter(dep_rel=="amod")%>%
  mutate(id=paste0(doc_id,paragraph_id,sentence_id,head_token_id))%>%
  left_join(foo1)%>%
  select(id, lemma, noun)%>%
  rename(adj=lemma)%>%
  group_by(noun,adj)%>%
  summarise(n=n())%>%
  filter(n>50)
#A  Correspondance Analysis solution?
#igraph approach belong to the next lesson 
library(igraph)
g <- graph.data.frame(foo, directed=FALSE)
V(g)$type <- bipartite_mapping(g)$type  ## Add the "type" attribute
V(g)$label.color <- ifelse(V(g)$type, "salmon4", "blue2")
V(g)$fill <- ifelse(V(g)$type, "salmon4", "blue2")
V(g)$shape <-ifelse(V(g)$type, "circle", "square")
plot(g,vertex.label.cex = 0.8)
```

# Notes

Beware to computing time! Best to sample for testing. (come back to the beginning)

```{r 20}
t2=Sys.time()
t<- t2-t1
print(t)
```

We save the annotated dataset in a format *.rds for further usage. 

```{r 021}
df_work<-cbind(df,readability, lexdiv,emotions,test)
write_rds(df_work,"df_work.rds")
```

See you tomorrow and [go to session 2](https://benaventc.github.io/NLP_lecture_PSLWeek/session1.html)

Some exercises before, for training :

* compute the sentiment evolution for one administration of your choice.
* ...


