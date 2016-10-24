#------Questions-------------------------------
## 1) What are the most frequent words among the students expectations?


#-----Text file--------------------------------
# load packages
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)

# load directory
std_exp <- "C:/Users/Felix/Dropbox/Scientific/Doing_Science_Workshops/1_TextMining/students_expectations"

# create corpus 
std_exp_corp <- Corpus(DirSource(std_exp))

# inspect files
inspect(std_exp_corp)

#------clean txt file---------------------------
# remove punctuations from text files
std_exp_nopunt <- tm_map (std_exp_corp, removePunctuation)

# remove numbers from text files
std_exp_nonum <- tm_map(std_exp_nopunt, removeNumbers)

# all lowercase
std_exp_lowcase <- tm_map(std_exp_nonum, tolower) 

# remove commond words in spanish and english words
std_exp_rm_common <- tm_map(std_exp_lowcase, removeWords, stopwords("english"))

# remove white spaces
std_exp_nowhite <- tm_map(std_exp_rm_common, stripWhitespace)  

# text into plain texts
std_exp_plain <- tm_map(std_exp_nowhite, PlainTextDocument)

# create documents matrices
std_exp_mtx <- DocumentTermMatrix(std_exp_plain)

# transposed document matrices
std_exp_mtx_t <- TermDocumentMatrix(std_exp_plain)

#------Exploratory analysis-----------------------
# create, order, and examine frequencies 
freq_stdexp <- colSums(as.matrix(std_exp_mtx_t))
ord_freq_stdexp <- order(freq_stdexp) 
freq_stdexp[head(ord_freq_stdexp)]

# create, order, and examine frequencies of w/o spare terms, spanish terms
dtms_stdnospr <- removeSparseTerms(std_exp_mtx, 0.50)
freq_std_nospr <- colSums(as.matrix(dtms_stdnospr))
ord_freq_std_nospr <- order(freq_std_nospr) 
freq_std_nospr[head(ord_freq_std_nospr)]

# sort frequencies of non-spare terms for spanish from most to least frequent
(std_freq_nospr_sort <- sort(colSums(as.matrix(dtms_stdnospr)), decreasing=TRUE))

#------plot frequencies--------------------------
wf_df_std <- data.frame(word=names(std_freq_nospr_sort), freq=std_freq_nospr_sort)

plot_std_exp <- ggplot(subset(wf_df_std, freq>2), aes(word, freq)) + geom_bar(stat="identity")
plot_std_exp <- plot_std_exp + theme(axis.text.x=element_text(angle=45, hjust=1))

print(plot_std_exp)