## Script:      explore.R
## Author:      Allen H. Nugent
## Created:     2016-03-08
## Last edit:   2016-03-31
## Last test:   2016-03-23
##
## Purpose:     Assignment for Course 10 ('data-science-project')
##              
## Notes:
##
##   1. This is a prerequisite to "quiz1.R".
##
## Tasks
##
##   1. Load data using acceptable character encoding
##   2. Clean
##          remove junk words left over from presumed encoding issues
##          remove double quotes
##          filter profanities
##   3.
##
## To Do elsewhere:
##
##   1. Put apostrophes back into contractions:
##          "snt" -> "s\u0092nt"
##   2. Replace backtick with apostrophe:
##
##

setwd("E:/R_data/Course10_Wk1")

rm(list=ls())

# install.packages("R.utils")
library(R.utils)

library(stringr)

# this had to run twice:
# install.packages("dils")

library(ggplot2)
# # install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

# library(igraph)
# library(dils)

# install.packages("readr")
library(readr)

# install.packages("tm")
library(tm)

# install.packages("RWeka")
library(RWeka)

# install.packages("qdap")
library(qdap)

# install.packages("quanteda")
library(quanteda)

source ("lib_NLP.R")

#???????????  what is word2vec() in ???????????????????



#######################################################################
# if not in working dir, load & run manually from OneDrive:
# source("set_paths.R")
# source(paste0(folder.lib, "/iofns.R"))
#######################################################################

folder.dat <- "data/final/en_US"

# DATA CLEANING DECISIONS, ASSUMPTIONS, IMPLICATIONS: 
# 
#    1. Remove apostrophes: avoid trying to discrminate between possession, contractions, single quotes.
#    2. Remove numbers: avoid trying to genericise quantities.
#    3. Remove punctuation: easy enough for user to add punctuation in-line.
#       *** Solution needs to ignore in-line punctuation when recommending!
#    4. Remove "$": no point in trying to infer currency amounts.
#    5. Remove ".": no point in trying to predict end of sentence, URLs, or ellipsis.
# *** 6. TODO: dumb abbreviations ("u", "r")
# 
#
# 
#
# DATA PROCESSING FOR PREDICTION:
# 
#    1. Truncate words with freq = 1 (mostly misspellings & proper nouns).

#===============================================================================

# tm:
#   PlainTextDocument
#   TermDocMatrix
#   Stem...
#   tm_map

# RWeka:
#   NGramTokenizer


# # other cleaning options:
# text_vector = readLines(con, encoding="UTF-8")
# inconv(text_vector, "UTF-8", "ascii", sub = " ")

# # then:
# theCorpus = tm_map(theCorpus, content_transformer(function(x) iconv(x, to="ASCII", sub=" ")))



   

#-------------------------------------------------------------------------------
# Clean the US_news data
#-------------------------------------------------------------------------------

path.us_news.raw <- paste0("data/final/en_US/", "en_US.news.txt")

# number of lines, size (bytes):
n.lines.us_news.raw <- countLines(path.us_news.raw)
# n.lines.us_news.raw <- get.num.lines(path.us_news.raw)  #: NOT 77259
size.us_news.raw <- file.info(path.us_news.raw)$size  # 205811889
maxlength.us_news.raw <- get.maxlinelength(path.us_news.raw)
    
# US_news.raw <- readLines(path.us_news.raw, n = 100L, encoding = "UTF-8")

# this gets truncated at [SUB] chars ("\u001a"):
# US_news.raw <- readLines(path.us_news.raw, encoding = "UTF-8")  #:NFG for [SUB]:, skipNul = TRUE)

dat <- read_lines(path.us_news.raw)
# dat <- read_lines(path.us_news.raw, n_max = 100)
#
# FINDINGS:
#
#  1.   structure = 1 paragraph per input line + excessive white space

dat <- clean_text(dat)  
writeLines(dat, paste0("data/", "en_US.news.clean.txt"))

# rm(list = c("US_news.raw", "dat"))


#-------------------------------------------------------------------------------
# Clean the US_blogs data
#-------------------------------------------------------------------------------

path.us_blogs.raw <- paste0("data/final/en_US/", "en_US.blogs.txt")

# number of lines, size (bytes):
# n.lines.us_blogs.raw <- get.num.lines(path.us_blogs.raw)  #: 899288
n.lines.us_blogs.raw <- countLines(path.us_blogs.raw)
size.us_blogs.raw <- file.info(path.us_blogs.raw)$size  #: 210160014
maxlength.us_blogs.raw <- get.maxlinelength(path.us_blogs.raw)
n.words.us_blogs.raw <- # number of spaces plus number of lines

# dat <- readLines(path.us_blogs.raw, n = 1000L, encoding = "UTF-8")
dat <- read_lines(path.us_blogs.raw)
#
# FINDINGS:
#
#  1.   structure = 1 paragraph per input line + excessive white space
#  2.   funny characters removed by using UTF-8 encoding on input

dat <- clean_text(dat)  
writeLines(dat, paste0("data/", "en_US.blogs.clean.txt"))

# rm(list = c("US_blogs.raw", "dat"))



#-------------------------------------------------------------------------------
# Clean the Twitter data
#-------------------------------------------------------------------------------

path.twitter.raw <- paste0("data/final/en_US/", "en_US.twitter.txt")

# number of lines, size (bytes):
# n.lines.twitter.raw <- get.num.lines(path.twitter.raw)  #: 2360148
n.lines.twitter.raw <- countLines(path.twitter.raw)
size.twitter.raw <- file.info(path.twitter.raw)$size  #: 167105338
maxlength.twitter.raw <- get.maxlinelength(path.twitter.raw)
n.words.twitter.raw <- # number of spaces plus number of lines

#E: twitter.raw <- read.table(path.twitter.raw, header = FALSE, nrows = 1000)

# dat <- readLines(path.twitter.raw, n = 100L)  #: sample data
dat <- readLines(path.twitter.raw, encoding = "UTF-8", skipNul = TRUE)  #: all data
#
# FINDINGS:
#
#  1.   structure = 1 tweet per input line

# TODO:
#
# 1. special treatment for hashtags (e.g. "#anniehall")
# 2. how to handle dumb abbreviations? 
#       e.g. "u" <--> "you", "r" <--> "are", "2" <-?-> "to", "too", "RT" <--> ?
#    - need Twitter abbrev's lookup table
# 3. peculiar grammar requires separate rules engine
# 4. special treatment for bad grammar
# 5. special treatment for poor spelling
# 6. errors due to inebriation (e.g. adjacent keys, missing spaces)
# 7. how to recognise/understand celebs?

# delete hashtags:
dat <- gsub("#\\w+ *", "", dat)

# reverse stupid abbreviations (where unamibiguous):
dat <- gsub(" u ", " you ", dat, fixed = TRUE)
dat <- gsub(" r ", " are ", dat, fixed = TRUE)
dat <- gsub(" ur ", " your ", dat, fixed = TRUE)
dat <- gsub(" c ", " see ", dat, fixed = TRUE)

dat <- clean_text(dat)  
writeLines(dat, paste0("data/", "en_US.twitter.clean.txt"))

# rm(list = c("twitter.raw", "dat"))


#-------------------------------------------------------------------------------
# Summary of raw data
#-------------------------------------------------------------------------------

n.lines.us_news.raw <- 1010243
n.lines.us_blogs.raw <- 899289
n.lines.twitter.raw <- 2360149
file.size.us_news.raw <- 196.3     # MB
file.size.us_blogs.raw <- 200.4
file.size.twitter.raw <- 159.4

cat(  "          \tNews      \tBlogs      \tTwitter \n",
    c("size:    ", "\t", file.size.us_news.raw,  "   \t", file.size.us_blogs.raw,  "   \t", file.size.twitter.raw, "\n"),
    c("lines:   ",  "\t", n.lines.us_news.raw,  "\t", n.lines.us_blogs.raw,  "\t", n.lines.twitter.raw, "\n"))

# paste0("longest:", maxlength.us_news.raw, maxlength.us_blogs.raw, maxlength.twitter.raw, "\n"),
# paste0("words:", n.words.us_news.raw, n.words.us_blogs.raw, n.words.twitter.raw)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Data Processing
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# sampling? load this fraction of total lines:


#-------------------------------------------------------------------------------
# Sample the US_news data
#-------------------------------------------------------------------------------

sampling <- 0.01

# SAMPLING METHOD 1: Read sequentially until sample acquired:

inpath <- "data/en_US.news.clean.txt"
n.lines.us_news.clean <- countLines(inpath)   # get.num.lines(inpath, safe.read = FALSE) 
if (sampling == 1)
{
    n.lines.us_news.in <- -1L
} else
{
    n.lines.us_news.in <- ceiling(sampling * n.lines.us_news.clean)
}
indata <- read_lines(inpath, n_max = n.lines.us_news.in)
writeLines(indata, paste0("data/corpusdir/", "en_US.news.corpus.txt"))


# SAMPLING METHOD 2: Read all, sample randomly in RAM, then write:

inpath <- "data/en_US.news.clean.txt"
n.lines.us_news.clean <- get.num.lines(inpath, safe.read = FALSE) 
n.lines.us_news.in <- ceiling(sampling * n.lines.us_news.clean)
indata <- read_lines(inpath)
indices <- sample(1:n.lines.us_news.clean, n.lines.us_news.in, replace = FALSE)
outdata <- indata[indices]
writeLines(outdata, paste0("data/corpusdir/", "en_US.news.corpus.txt"))


#-------------------------------------------------------------------------------
# Create Term-Document Matrices from the US_news data, run tests
#-------------------------------------------------------------------------------

# explore raw word frequencies ...

dirsource <- DirSource("data/corpusdir")
corpus1 <- Corpus(dirsource, readerControl = list(language = "eng")) #: NOT "lat" !
# summary(corpus1)  #check what went in

# dtm <- DocumentTermMatrix(corpus1) 
# k <- 4
# print(paste0(dtm$dimnames$Terms[k], ": dtm[", dtm$i[k], ",", dtm$j[k], "] = ", dtm$v[k]))

tdm <- TermDocumentMatrix(corpus1) 

m <- inspect(tdm)
word.count <- sort(rowSums(m), decreasing = TRUE) 
word.freq <- word.count / sum(word.count)

tfm.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.df, file = "data/output/US_news.tfm.txt", row.names = FALSE)
num.words <- nrow(tfm.df)

top500 <- tdm.df[1:500, ]
wordcloud(top500$word, top500$count, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
# wordcloud(top500$word, log(top500$count), random.order = FALSE, colors = brewer.pal(8, "Dark2"))
png("data/output/Top500Cloud.png", width = 480, height = 360, units = "px")
wordcloud(top500$word, top500$count, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()


# frequencies of word doublets ...

tdm.2 <- TermDocumentMatrix(corpus1, control = list(tokenize = BigramTokenizer))

m <- inspect(tdm.2)
word.count <- sort(rowSums(m), decreasing = TRUE) 
word.freq <- word.count / sum(word.count)
tfm.2.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.2.df, file = "data/output/US_news.tfm2.txt", row.names = FALSE)
num.doublets <- nrow(tfm.2.df)


# frequencies of word triplets ...

tdm.3 <- TermDocumentMatrix(corpus1, control = list(tokenize = TrigramTokenizer))

m <- inspect(tdm.3)
word.count <- sort(rowSums(m), decreasing = TRUE) 
word.freq <- word.count / sum(word.count)
tfm.3.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.3.df, file = "data/output/US_news.tfm3.txt", row.names = FALSE)
num.triplets <- nrow(tfm.3.df)



#-------------------------------------------------------------------------------
# Sample & TDM the US_blogs data
#-------------------------------------------------------------------------------

# SAMPLING METHOD 2: Read all, sample randomly in RAM, then write:

inpath <- "data/en_US.blogs.clean.txt"
n.lines.us_blogs.clean <- get.num.lines(inpath, safe.read = FALSE) 
n.lines.us_blogs.in <- ceiling(sampling * n.lines.us_blogs.clean)
indata <- read_lines(inpath)
indices <- sample(1:n.lines.us_blogs.clean, n.lines.us_blogs.in, replace = FALSE)
outdata <- indata[indices]
writeLines(outdata, paste0("data/corpusdir/", "en_US.blogs.corpus.txt"))

dirsource <- DirSource("data/corpusdir")
corpus2 <- Corpus(dirsource, readerControl = list(language = "eng")) 
tdm.blogs <- TermDocumentMatrix(corpus2) 
m <- inspect(tdm.blogs)
word.count <- sort(rowSums(m), decreasing = TRUE) 
word.freq <- word.count / sum(word.count)
tfm.blogs.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.blogs.df, file = "data/output/US_blogs.tfm.txt", row.names = FALSE)
n.unique.words.blogs <- nrow(tfm.blogs.df)
n.words.blogs <- sum(tfm.blogs.df$count)


#-------------------------------------------------------------------------------
# Sample & TDM the twitter data
#-------------------------------------------------------------------------------

# SAMPLING METHOD 2: Read all, sample randomly in RAM, then write:

inpath <- "data/en_US.twitter.clean.txt"
n.lines.twitter.clean <- get.num.lines(inpath, safe.read = FALSE) 
n.lines.twitter.in <- ceiling(sampling * n.lines.twitter.clean)
indata <- read_lines(inpath)
indices <- sample(1:n.lines.twitter.clean, n.lines.twitter.in, replace = FALSE)
outdata <- indata[indices]
writeLines(outdata, paste0("data/corpusdir/", "en_twitter.corpus.txt"))

dirsource <- DirSource("data/corpusdir")
corpus2 <- Corpus(dirsource, readerControl = list(language = "eng")) 
tdm.twitter <- TermDocumentMatrix(corpus2) 
m <- inspect(tdm.twitter)
word.count <- sort(rowSums(m), decreasing = TRUE) 
word.freq <- word.count / sum(word.count)
tfm.twitter.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.twitter.df, file = "data/output/twitter.tfm.txt", row.names = FALSE)
n.unique.words.twitter <- nrow(tfm.twitter.df)
n.words.twitter <- sum(tfm.twitter.df$count)


# OPTIONAL ::::::::::::::::::::::::::::::::::::::::::::::::::::

# explore word frequencies with stop words removed ...

tdm <- tm_map(tdm, removeWords, stopwords("english")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 

#  RE-EXECUTE THE PROCESSING CODE BLOCK HERE ! :::::::::::::::::::::::::::



#-------------------------------------------------------------------------------
# evaluate incidence of non-dictionary words versus sample size ...
#-------------------------------------------------------------------------------

inpath <- "data/en_US.news.corpus_s001.txt"
indata1 <- readLines(inpath)
nonwords1 <- sapply(indata1, function(x) {
    length(which_misspelled(x, suggest = FALSE)) })
n.nonwords1 <- sum(nonwords1) #: 16150

inpath <- "data/output/US_news.tdm_s001.txt"
tfm1 <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
n.words.all1 <- sum(tfm1$count) #: ? 29235
f.nonwords1 <- n.nonwords1 / n.words.all1 #: ? 0.05766

inpath <- "data/en_US.news.corpus_s010.txt"
indata2 <- readLines(inpath)
nonwords2 <- sapply(indata2, function(x) {
    length(which_misspelled(x, suggest = FALSE)) })
n.nonwords2 <- sum(nonwords2) #: ?


#-------------------------------------------------------------------------------
# compare counts of non-dictionary words versus corpus source ...
#-------------------------------------------------------------------------------

nonwords.news <- n.nonwords1
n.words.all.news <- n.words.all1
f.nonwords.news <- f.nonwords1


inpath <- "data/en_US.blogs.corpus_r001.txt"
indata <- readLines(inpath)
nonwords.blogs <- sapply(indata, function(x) {
    length(which_misspelled(x, suggest = FALSE)) })
n.nonwords.blogs <- sum(nonwords.blogs)
f.nonwords.blogs <- n.nonwords.blogs / n.words.blogs
    
    
inpath <- "data/en_twitter.corpus_r001.txt"
indata <- readLines(inpath)
nonwords.twitter <- sapply(indata, function(x) {
    length(which_misspelled(x, suggest = FALSE)) })
n.nonwords.twitter <- sum(nonwords.twitter)
f.nonwords.twitter <- n.nonwords.twitter / n.words.twitter


cat("            \t words        \t misspellings \t fraction \n",
    c("US news 10% : \t", n.words.all2, "\t", n.nonwords2, "\t", cround(f.nonwords2, 4), "\n"),
    c("US news 1% : \t ", n.words.all1, "\t ", n.nonwords1, "\t", cround(f.nonwords1, 4), "\n"),
    c("US blogs 1% : \t ", n.words.blogs, "\t ", n.nonwords.blogs, "\t", cround(f.nonwords.blogs, 4), "\n"),
    c("twitter 1% : \t ", n.words.twitter, "\t ", n.nonwords.twitter, "\t", cround(f.nonwords.twitter, 4), "\n"))


#-------------------------------------------------------------------------------
# compare doublet counts between sequential and random samples 
#-------------------------------------------------------------------------------

inpath <- "data/output/US_news.tfm2_r001.txt"
indata.r <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
inpath <- "data/output/US_news.tfm2_s001.txt"
indata.s <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)

indata.r.f2 <- indata.r[indata.r$count > 1, 1]
indata.s.f2 <- indata.s[indata.s$count > 1, 1]
n.doublets.r.f2 <- length(indata.r.f2)
n.doublets.s.f2 <- length(indata.s.f2)
f.missing.s.f2 <- length(setdiff(indata.r.f2, indata.s.f2)) / n.doublets.r.f2  # length(indata.r)
f.missing.r.f2 <- length(setdiff(indata.s.f2, indata.r.f2)) / n.doublets.s.f2  # length(indata.s)

indata.r.f10 <- indata.r[indata.r$count > 9, 1]
indata.s.f10 <- indata.s[indata.s$count > 9, 1]
n.doublets.r.f10 <- length(indata.r.f10)
n.doublets.s.f10 <- length(indata.s.f10)
f.missing.s.f10 <- length(setdiff(indata.r.f10, indata.s.f10)) / n.doublets.r.f2  # length(indata.r)
f.missing.r.f10 <- length(setdiff(indata.s.f10, indata.r.f10)) / n.doublets.s.f2  # length(indata.s)


cat(  "             n > 1    \tmissing    \t n > 9 \tmissing \n",
    c("Random:    ", n.doublets.r.f2, "\t", cround(f.missing.r.f2, 4), 
      "\t", n.doublets.r.f10, "\t", cround(f.missing.r.f10, 4), "\n"),
    c("Sequential:", n.doublets.s.f2, "\t", cround(f.missing.s.f2, 4), 
      "\t", n.doublets.s.f10, "\t", cround(f.missing.s.f10, 4), "\n"))


# evaluate N-gram frequencies versus sample size ...

inpath <- "data/output/US_news.tfm2_s001.txt"
tfm2.001 <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
f2.001 <- tfm2.001$count[1:50000]
inpath <- "data/output/US_news.tfm2_s010.txt"
tfm2.010 <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
f2.010 <- tfm2.001$count[1:50000]

par(mfrow = c(1, 2))
# barplot(log(f2.001), ylab = "log10 (frequency)", xlab = "word index")
#plot(log(f2.001), cex = "0.5")
plot(log(f2.001), type = "l", col = "blue", ylab = "log10 (frequency)", xlab = "word index", main = "2-grams in 1% Sample")
plot(log(f2.010), type = "l", col = "blue", ylab = "log10 (frequency)", xlab = "word index", main = "2-grams in 10% Sample")

inpath <- "data/output/US_news.tfm3_s001.txt"
tfm3.001 <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
f3.001 <- tfm3.001$count[1:50000]
inpath <- "data/output/US_news.tfm3_s010.txt"
tfm3.010 <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
f3.010 <- tfm3.001$count[1:50000]

par(mfrow = c(1, 2))
plot(log(f3.001), type = "l", col = "blue", ylab = "log10 (frequency)", xlab = "word index", main = "3-grams in 1% Sample")
plot(log(f3.010), type = "l", col = "blue", ylab = "log10 (frequency)", xlab = "word index", main = "3-grams in 10% Sample")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Post-submission analyses
#-------------------------------------------------------------------------------


# sampled code ...

library(quanteda)
library(ggplot2)
library(SnowballC)
library(tm)
library(RWeka)
library(ngram)

freqTerms2 <- findFreqTerms(biGramMatrix, lowfreq = 50)
termFrequency2 <- rowSums(as.matrix(biGramMatrix[freqTerms2,]))
termFrequency2 <- data.frame(bigram=names(termFrequency2), frequency=termFrequency2)

h <- ggplot(termFrequency2, aes(x=reorder(bigram, frequency), y=frequency)) +
    geom_bar(stat = "identity", fill="#3399FF", colour="black") +  coord_flip() +
    theme(legend.title=element_blank()) +
    xlab("Bigram") + ylab("Frequency") +
    labs(title = "Bigrams Frequency greater than 50")


news <- suppressWarnings(readLines('./final/en_US/en_US.news.txt', encoding = 'UTF-8',skipNul=TRUE))
newsWords <- sum(sapply(gregexpr("\\S+", news), length))
countLines("./final/en_US/en_US.news.txt")


tm_uniwordfreq %>% 
    filter(freq > 1000) %>%
    ggplot(aes(word,freq)) +
    geom_bar(stat="identity") +
    ggtitle("Unigrams with frequencies > 1000") +
    xlab("Unigrams") + ylab("Frequency") +
    theme(axis.text.x=element_text(angle=45, hjust=1))



#split data into single words

char <- "[\\.|\\,|\\:|\\;|\\?|\\!| |\\"|\\"|\\!|\\(|\\)|\\{|\\}|\\|\\%|\\$|\\&|\\*|\\-|\\\\|\\+|\\#|\\^|\\\"]{1,5}"
all_wrds <- unlist( strsplit(data_txt, char, fixed=FALSE, perl=TRUE) )



#tokenization to 3-grams

all_3mers <- suppressWarnings(
    rbind(
        data.frame(matrix(all_wrds, ncol = 3, byrow = TRUE)),
        data.frame(matrix(all_wrds[-1], ncol = 3, byrow = TRUE)),
        data.frame(matrix(all_wrds[-2], ncol = 3, byrow = TRUE))
    )
)
all_3mers$mrg <- apply(all_3mers, 1, function(x) paste(x, collapse=" "))
all_3mers$mode <- sapply(gregexpr("[[:alpha:]]+", all_3mers$mrg), function(x) sum(x > 0))

distr_3mers <- summary(as.factor(all_3mers[all_3mers$mode == 3,]$mrg))tmp3mers <- melt(distr_3mers[(length(distr_3mers)/2-10):(length(distr_3mers)/2+10)])

ggplot(tmp3mers, aes(x = rownames(tmp3mers), y = value)) + 
    geom_bar(stat = "Identity") + 
    ggtitle("Median 20 three-gram terms") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# "For the prediction I will test some common Natural Language Processing algorithms (Linear MLE Interpolation, Kneser-Ney Smoothing) for the power in terms of speed and accuracy."







#-------------------------------------------------------------------------------
# truncate n-gram tails
#-------------------------------------------------------------------------------

# Nb. Cannot use truncated 1-gram tfm as input to further tfms, because new n-grams
#     would be invented where there are words missing from the original n-grams !

sample <- 0.01
# sample <- 0.1
# sample <- 1

if (sample == 1)
{
    inpath1 <- "data/output/US_news.tfm_s100.txt"
#     inpath2 <- "data/en_US.news.corpus_s100.txt"
    outpath1 <- "data/output/US_news.tfm_s100t6.txt"
    f.cutoff <- 6
}
if (sample == 0.1)
{
    inpath1 <- "data/output/US_news.tfm_s010.txt"
    outpath1 <- "data/output/US_news.tfm_s010t5.txt"   
    f.cutoff <- 5
}    
if (sample == 0.01)
{
    inpath1 <- "data/output/US_news.tfm_s001.txt"
    outpath1 <- "data/output/US_news.tfm_s001t3.txt" 
    f.cutoff <- 3
}    

# this is dumb: only want tfms, so don't try to remove words from corpus !!!!!
# 
# indata1 <- read.table(inpath1, header = TRUE, stringsAsFactors = FALSE)
# tail.words <- indata1$word[indata1$count < f.cutoff]
# keep.words <- indata1$word[indata1$count >= f.cutoff]
# 
# indata2 <- readLines(inpath2)
# 
# # NFG: regex too long:
# # corpus <- Corpus(VectorSource(indata2))
# #E: words.f6 <- tm_map(corpus, removeWords, tail.words)
# 
# outdata1 <- sapply(indata2, function(x) {
#     words <- strsplit(x, " ")
#     paste(words[words %in% keep.words], collapse = " ")})
# 
# # nonwords1 <- sapply(indata1, function(x) {
# #     length(which_misspelled(x, suggest = FALSE)) })
# # n.nonwords1 <- sum(nonwords1) #: 16150
# 
# writeLines(outdata1, paste0("data/", "en_US.news.corpus_st001.txt"))
# 
# I <- 1
# tail.words[I]
# indata2[grep(tail.words[I], indata2, fixed = TRUE)]
# outdata1[grep(tail.words[I], outdata1, fixed = TRUE)]


indata1 <- read.table(inpath1, header = TRUE, stringsAsFactors = FALSE)
outdata1 <- indata1[indata1$count >= f.cutoff, ]
nrow(outdata1)
write.table(outdata1, file = outpath1, row.names = FALSE)

rm(c("indata1", "outdata1"))


# 2-grams: don't truncate for 1% sampling ...

 sample <- 0.1
# sample <- 1

if (sample == 1)
{
    inpath2 <- "data/output/US_news.tfm2_s100.txt"
    outpath2 <- "data/output/US_news.tfm2_s100t?.txt"
    f.cutoff <- 6
}
if (sample == 0.1)
{
    inpath1 <- "data/output/US_news.tfm_s010.txt"
    outpath1 <- "data/output/US_news.tfm_s010t5.txt"   
    f.cutoff <- 5
}    
# if (sample == 0.01)
# {
#     inpath1 <- "data/output/US_news.tfm_s001.txt"
#     outpath1 <- "data/output/US_news.tfm_s001t3.txt" 
#     f.cutoff <- 3
# }    

indata1 <- read.table(inpath1, header = TRUE, stringsAsFactors = FALSE)
outdata1 <- indata1[indata1$count >= f.cutoff, ]
nrow(outdata1)
write.table(outdata1, file = outpath1, row.names = FALSE)


# TEST: create a small, dummy corpus and test sentence termination behaviour !!!!!!
corpus <- Corpus(VectorSource(indata2))




#-------------------------------------------------------------------------------
# split n-gram matrices by words
#-------------------------------------------------------------------------------

# sampling <- 0.01
sampling <- 0.1

inpath2 <- paste0("data/output/US_news.tfm2_s", get.suffix(sampling), ".txt")
outpath2 <- paste0("data/output/US_news.x2_", get.suffix(sampling), ".txt") # "x" denotes split n-gram

indata2 <- read.table(inpath2, header = TRUE, stringsAsFactors = FALSE)
outdata2.words <- str_split(indata2$word, " ")
#X: outdata2 <- data.frame(word1 = outdata2.words[, 1], 
#                        word2 = outdata2.words[, 2])
#X: outdata2 <- data.frame(word1 = outdata2.words[[]][1], 
#                        word2 = outdata2.words[[]][2])
#X: outdata2 <- unlist(outdata2.words)
#X: outdata2 <- as.data.frame(outdata2.words)
outdata2 <- data.frame(matrix(unlist(outdata2.words), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
outdata2$count <- indata2$count
outdata2$freq <- indata2$freq

write.table(outdata2, file = outpath2, row.names = FALSE)

# inpath3 <- paste0("data/output/US_news.tfm3_s", get.suffix(sampling), "t5.txt")
# inpath3 <- paste0("data/output/US_news.tfm3_s", get.suffix(sampling), "t5.txt")
inpath3 <- paste0("data/output/US_news.tfm3_s", get.suffix(sampling), ".txt")
outpath3 <- paste0("data/output/US_news.x3_", get.suffix(sampling), ".txt")

indata3 <- read.table(inpath3, header = TRUE, stringsAsFactors = FALSE)
outdata3.words <- str_split(indata3$word, " ")
outdata3 <- data.frame(matrix(unlist(outdata3.words), ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
outdata3$count <- indata3$count
outdata3$freq <- indata3$freq

write.table(outdata3, file = outpath3, row.names = FALSE)




#=============================================================================== 
# Quiz 3
#===============================================================================


# last.words <- c("case", "of")
# last.words <- c("mean", "the")
# last.words <- c("me", "the")
# last.words <- c("but", "the")
# last.words <- c("at", "the")
# last.words <- c("on", "my")
# last.words <- c("quite", "some")
# last.words <- c("his", "little")
# last.words <- c("during", "the")
last.words <- c("must", "be")

matches <- outdata3[outdata3[, 1] == last.words[1] & outdata3[, 2] == last.words[2], ]
# matches[matches[,3] %in% c("players", "defense", "referees", "crowd"), ]
# matches[matches[,3] %in% c("grocery", "beach", "movies", "mall"), ]
# matches[matches[,3] %in% c("way", "motorcycle", "phone", "horse"), ]
# matches[matches[,3] %in% c("time", "weeks", "years", "thing"), ]
# matches[matches[,3] %in% c("ears", "toes", "eyes", "fingers"), ]
# matches[matches[,3] %in% c("worse", "bad", "hard", "sad"), ]
matches[matches[,3] %in% c("callous", "insane", "insensitive", "asleep"), ]
matches


# experiments ::::::::::::::::::::
# 23/03/16: trying to insert EOL in place of inline period ...

inpath <- paste0("data/final/en_US/", "en_US.news.txt")
indata <- read_lines(inpath, n_max = 10)

# # 24/03/16 ...
myDfm2 <- dfm(indata, ngrams = 3, concatenator = " ", removePunct = FALSE)
t(myDfm2)

corpus <- Corpus(VectorSource(indata))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.3 <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
m <- inspect(tdm.3)
word.count <- sort(rowSums(m), decreasing = TRUE) 
word.freq <- word.count / sum(word.count)
tfm.3.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.3.df, file = "data/output/US_news.tfm3.txt", row.names = FALSE)
num.triplets <- nrow(tfm.3.df)




