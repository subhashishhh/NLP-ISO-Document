require(quanteda)
require(readtext)
require(quanteda.textstats)
require(quanteda.textplots)
require(tidyverse)
require(tidytext)
require(pdftools)
require(tm)
require(ggcorrplot)

#create vector of pdf file name
All_Files<- list.files(pattern = "pdf$")
#loads all PDF files
All_opinions <- lapply(All_Files, pdf_text)
#create corpus
document<-Corpus(VectorSource(All_opinions))

#cleaning the text
document<-tm_map(document, content_transformer(tolower))#convert all text to lower
document<-tm_map(document, removeNumbers)#remove numbers from document
document<-tm_map(document, removeWords, stopwords("english"))#remove stopwards
document<-tm_map(document, removePunctuation)
#Finding collocations
social_sentences <- document %>%
  tolower() %>%
  paste0(collapse = " ") %>%
  stringr::str_split(fixed (".")) %>%
  unlist() %>%
  tm::removePunctuation() %>%
  tm::removeNumbers() %>%
  stringr::str_squish()
#create a token object
social_tokens <-tokens(social_sentences, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))
#extract collocations
social_coll <- textstat_collocations(social_tokens, size = 2, min_count = 20)
#create document-feature matrix
social_dfm <-social_sentences %>%
  quanteda::dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  quanteda::dfm_trim(min_termfreq = 10, verbose = FALSE)
#load function for co-occurence calculation
source("http://slcladal.github.io/rscripts/calculateCoocStatistics.R") 

#define term
coocTerm <- "pregnancy"
#calculate co-occurence statistics
coocs <- calculateCoocStatistics(coocTerm, social_dfm, measure="LOGLIK")
#inspect results
coocs[1:20]
coocs[20:50]
#We now reduce the document-feature matrix to contain only the top 20 collocates
redux_dfm <- dfm_select(social_dfm, pattern = c(names(coocs)[10:50], "sdg"))
#transform the document 
tag_fcm <- fcm(redux_dfm)

#create graph
textplot_network(tag_fcm,
                 min_freq = 1,
                 edge_alpha = 0.2,
                 edge_size = 5,
                 edge_color = "purple",
                 vertex_labelsize = 3)
