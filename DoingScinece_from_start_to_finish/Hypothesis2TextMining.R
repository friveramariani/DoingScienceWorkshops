#------Questions-------------------------------
## 1) What are the most frequent words among the students's second hypothesis?


#-----Text file--------------------------------
# load packages
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)

# load directory
std_hyp2 <- "C:/Users/Felix/Dropbox/Scientific/Doing_Science_Workshops/1_TextMining/students_hypothesis2"

# create corpus 
std_hyp2_corp <- Corpus(DirSource(std_hyp2))

# inspect files
inspect(std_hyp2_corp)

#------clean txt file---------------------------
# remove punctuations from text files
std_hyp2_nopunt <- tm_map (std_hyp2_corp, removePunctuation)

# remove numbers from text files
std_hyp2_nonum <- tm_map(std_hyp2_nopunt, removeNumbers)

# all lowercase
std_hyp2_lowcase <- tm_map(std_hyp2_nonum, tolower) 

# remove commond words in spanish and english words
std_hyp2_rm_common <- tm_map(std_hyp2_lowcase, removeWords, stopwords("english"))

# remove white spaces
std_hyp2_nowhite <- tm_map(std_hyp2_rm_common, stripWhitespace)  

# text into plain texts
std_hyp2_plain <- tm_map(std_hyp2_nowhite, PlainTextDocument)

# create documents matrices
std_hyp2_mtx <- DocumentTermMatrix(std_hyp2_plain)

# transposed document matrices
std_hyp2_mtx_t <- TermDocumentMatrix(std_hyp2_plain)

#------exploratory analysis-----------------------
# create, order, and examine frequencies 
freq_stdhyp2 <- colSums(as.matrix(std_hyp2_mtx_t))
ord_freq_stdhyp2 <- order(freq_stdhyp2) 
freq_stdhyp2[head(ord_freq_stdhyp2)]

# create, order, and examine frequencies of w/o spare terms, spanish terms
dtms_stdhyp2nospr <- removeSparseTerms(std_hyp2_mtx, 0.50)
freq_stdhyp2_nospr <- colSums(as.matrix(dtms_stdhyp2nospr))
ord_freq_stdhyp2_nospr <- order(freq_stdhyp2_nospr) 
freq_stdhyp2_nospr[head(ord_freq_stdhyp2_nospr)]

# sort frequencies of non-spare terms for spanish from most to least frequent
(stdhyp2_freq_nospr_sort <- sort(colSums(as.matrix(dtms_stdhyp2nospr)), decreasing=TRUE))

#------plot frequencies--------------------------
wf_df_stdhyp2 <- data.frame(word=names(stdhyp2_freq_nospr_sort), freq=stdhyp2_freq_nospr_sort)

plot_std_hyp2 <- ggplot(subset(wf_df_stdhyp2, freq>2), aes(word, freq)) + geom_bar(stat="identity")
plot_std_hyp2 <- plot_std_hyp2 + theme(axis.text.x=element_text(angle=45, hjust=1))

print(plot_std_hyp2)

#-----word cloud--------------------------------
wordcloud(names(freq_stdhyp2_nospr), freq_stdhyp2_nospr, min.freq = 5, 
          colors=brewer.pal(6, "Dark2"))