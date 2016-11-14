#------Questions-------------------------------
## 1) What are the most frequent words among the students' hypothesis?


#-----Text file--------------------------------
# load packages
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)

# load directory
std_hyp <- "C:/Users/Felix/Dropbox/Scientific/Doing_Science_Workshops/1_TextMining/students_hypothesis"

# create corpus 
std_hyp_corp <- Corpus(DirSource(std_hyp))

# inspect files
inspect(std_hyp_corp)

#------clean txt file---------------------------
# remove punctuations from text files
std_hyp_nopunt <- tm_map (std_hyp_corp, removePunctuation)

# remove numbers from text files
std_hyp_nonum <- tm_map(std_hyp_nopunt, removeNumbers)

# all lowercase
std_hyp_lowcase <- tm_map(std_hyp_nonum, tolower) 

# remove commond words in spanish and english words
std_hyp_rm_common <- tm_map(std_hyp_lowcase, removeWords, stopwords("english"))

# remove white spaces
std_hyp_nowhite <- tm_map(std_hyp_rm_common, stripWhitespace)  

# text into plain texts
std_hyp_plain <- tm_map(std_hyp_nowhite, PlainTextDocument)

# create documents matrices
std_hyp_mtx <- DocumentTermMatrix(std_hyp_plain)

# transposed document matrices
std_hyp_mtx_t <- TermDocumentMatrix(std_hyp_plain)

#------exploratory analysis-----------------------
# create, order, and examine frequencies 
freq_stdhyp <- colSums(as.matrix(std_hyp_mtx_t))
ord_freq_stdhyp <- order(freq_stdhyp) 
freq_stdhyp[head(ord_freq_stdhyp)]

# create, order, and examine frequencies of w/o spare terms, spanish terms
dtms_stdhypnospr <- removeSparseTerms(std_hyp_mtx, 0.50)
freq_stdhyp_nospr <- colSums(as.matrix(dtms_stdhypnospr))
ord_freq_stdhyp_nospr <- order(freq_stdhyp_nospr) 
freq_stdhyp_nospr[head(ord_freq_stdhyp_nospr)]

# sort frequencies of non-spare terms for spanish from most to least frequent
(stdhyp_freq_nospr_sort <- sort(colSums(as.matrix(dtms_stdhypnospr)), decreasing=TRUE))

#------plot frequencies--------------------------
wf_df_stdhyp <- data.frame(word=names(stdhyp_freq_nospr_sort), freq=stdhyp_freq_nospr_sort)

plot_std_hyp <- ggplot(subset(wf_df_stdhyp, freq>2), aes(word, freq)) + geom_bar(stat="identity")
plot_std_hyp <- plot_std_hyp + theme(axis.text.x=element_text(angle=45, hjust=1))

print(plot_std_hyp)

#-----word cloud--------------------------------
wordcloud(names(freq_stdhyp_nospr), freq_stdhyp_nospr, min.freq = 5, 
          colors=brewer.pal(6, "Dark2"))