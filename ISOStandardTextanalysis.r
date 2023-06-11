require(pdftools)
require(tm)
require(tidytext)
require(dplyr)
require(igraph)
require(tidyr)
require(ggraph)
All_Files<-list.files(pattern = "pdf$")#creates vector of pdf files names
All_opinions<-lapply(All_Files, pdf_text)
length(All_opinions)
lapply(All_opinions,length) 
#createing a pdf database
pdfdatabase <- Corpus(URISource(files), readerControl = list(reader = readPDF))
#create your own list of stopwords, it has to be performed on the Corpus
pdfdatabase<- tm_map(pdfdatabase, removeWords, c("ISO", "Healthcare", "City")) 
#remove english stopwords
pdfdatabase <- tm_map(pdfdatabase, removeWords, stopwords("english"))
# Remove numbers
pdfdatabase <- tm_map(pdfdatabase, removeNumbers)
opinions.tdm <- TermDocumentMatrix(pdfdatabase,control = list(removePunctuation = TRUE,
                                                              stopwords = TRUE, tolower = TRUE, 
                                                              stemming = FALSE, removeNumbers = TRUE, 
                                                              bounds = list(global = c(3,Inf)))) 
 
inspect(opinions.tdm[1:10,])
inspect(opinions.tdm[110:151,]) 
findFreqTerms(opinions.tdm, lowfreq = 20, highfreq = Inf)  
ft <- findFreqTerms(opinions.tdm, lowfreq = 20, highfreq = Inf)
as.matrix(opinions.tdm[ft,])
ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE) 
findAssocs(opinions.tdm, terms = "birth", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "childbirth", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "abuse", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "prevention", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "drug", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "substance", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "harmful", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "maternity", corlimit = 0.75)
findAssocs(opinions.tdm, terms = "pregnancy", corlimit = 0.75)
  
inspect(opinions.tdm[c("birth","congenital"),])
inspect(opinions.tdm[c("pregnancy","women", "fertility", "pregnant"),])
inspect(opinions.tdm[c("abuse","penetration", "harm", "exposure", "physiological", "children"),])
inspect(opinions.tdm[c("pregnancy","women"),])
 
