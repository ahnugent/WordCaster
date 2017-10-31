## Script:      process.R
## Author:      Allen H. Nugent
## Created:     2016-03-31
## Last edit:   2017-03-11
## Last test:   2016-04-07
##
## Context:     Assignment for Course 10 ('data-science-project')
## Purpose:     Text data processing for prediction model
##              
## Notes:
##
## Tasks:
##
##
##

rm(list=ls())

setwd("C:/R_data/Course10_Wk1")
setwd("E:/R_data/Coursera_JHU_Data_Science/Course10")

# install.packages("R.utils")
library(R.utils)

library(stringr)

# this had to run twice:
# install.packages("dils")

library(ggplot2)
# # install.packages("wordcloud")
# library(wordcloud)
# library(RColorBrewer)

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


# source ("lib_NLP.R")

#???????????  what is word2vec() in ???????????????????



#######################################################################
# if not in working dir, load & run manually from OneDrive:
# source("set_paths.R")
# source(paste0(folder.lib, "/iofns.R"))
#######################################################################

folder.dat <- "data/final/en_US"

# 
#
# DATA PROCESSING FOR PREDICTION:
# 
#    1. Truncate words with freq = 1 (mostly misspellings & proper nouns).

#===============================================================================


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

#dat <- read_lines(paste0("data/", "test_clean.txt"))
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

dat <- clean_text(dat)  

writeLines(dat, paste0("data/", "en_US.twitter.clean.txt"))

# rm(list = c("twitter.raw", "dat"))


#------------------------------------------------------------------------------
# Summary of raw data
#-------------------------------------------------------------------------------

# n.lines.us_news.raw <-  # old: 1010243
# n.lines.us_blogs.raw <- # old: 899289
# n.lines.twitter.raw <- # old: 2360149
# file.size.us_news.raw <- # old: 196.3
# file.size.us_blogs.raw <- # old: 200.4
# file.size.twitter.raw <- # old: 159.4

cat(  "          \tNews      \tBlogs      \tTwitter \n",
    c("size:    ", "\t", file.size.us_news.raw,  "   \t", file.size.us_blogs.raw,  "   \t", file.size.twitter.raw, "\n"),
    c("lines:   ",  "\t", n.lines.us_news.raw,  "\t", n.lines.us_blogs.raw,  "\t", n.lines.twitter.raw, "\n"))

# paste0("longest:", maxlength.us_news.raw, maxlength.us_blogs.raw, maxlength.twitter.raw, "\n"),
# paste0("words:", n.words.us_news.raw, n.words.us_blogs.raw, n.words.twitter.raw)


#------------------------------------------------------------------------------
# Summary of clean data
#-------------------------------------------------------------------------------

n.lines.us_news.clean <- countLines(paste0("data/", "en_US.news.clean.txt"))
n.lines.us_blogs.clean <- countLines(paste0("data/", "en_US.blogs.clean.txt"))
n.lines.twitter.clean <- countLines(paste0("data/", "en_US.twitter.clean.txt"))
# file.size.us_news.clean <- 
# file.size.us_blogs.clean <- 
# file.size.twitter.clean <- 

cat(  "          \tNews      \tBlogs      \tTwitter \n",
      c("size:    ", "\t", file.size.us_news.clean,  "   \t", file.size.us_blogs.clean,  "   \t", file.size.twitter.clean, "\n"),
      c("lines:   ",  "\t", n.lines.us_news.clean,  "\t", n.lines.us_blogs.clean,  "\t", n.lines.twitter.clean, "\n"))

# paste0("longest:", maxlength.us_news.raw, maxlength.us_blogs.raw, maxlength.twitter.raw, "\n"),
# paste0("words:", n.words.us_news.raw, n.words.us_blogs.raw, n.words.twitter.raw)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Data Processing
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# sampling? load this fraction of total lines:


#-------------------------------------------------------------------------------
# Prepare corpora from samples of the cleaned data
#-------------------------------------------------------------------------------

#sampling <- 0.01
#sampling <- 0.1
sampling <- 0.25
#sampling <- 0.5

# SAMPLING METHOD 3: Prepare random sampling matrix, read all, sample in RAM, then write:

# news ::::::::::::::::::::::::::::::::::::::::::::

# prepare randomised sampling sequence:
inpath <- "data/en_US.news.clean.txt"
n.lines.us_news.clean <- countLines(inpath)
n.lines.us_news.in <- ceiling(sampling * n.lines.us_news.clean)
set.seed(61862)
indices.us_news.all <- sample(1:n.lines.us_news.clean, n.lines.us_news.clean, replace = FALSE)
names(indices.us_news.all) <- "indx"
write.table(indices.us_news.all, file = "data/random_indices.us_news.txt", row.names = FALSE)

# read a contiguous sample of random indices as per <sampling> & index of sample (range = 1 : (1 / sampling))
indices.us_news.in <- read.table("data/random_indices.us_news.txt", header = TRUE, stringsAsFactors = FALSE)
sb <- get.sample.bounds(n.lines.us_news.in, 1)  #: take 1st chunk of random indices
indices.us_news.in <- indices.us_news.in[sb[1]:sb[2], ]

# read whole file, save a sample as per random indices obtained above: 
indata <- read_lines(inpath)
outdata <- indata[indices.us_news.in]
writeLines(outdata, paste0("data/corpusdir/", "en_US.news.corpus.txt"))

rm(list = c("indata", "outdata"))
rm(list = c("indices.us_news.in", "indices.us_news.in"))


# blogs :::::::::::::::::::::::::::::::::::::::::::::::

# prepare randomised sampling sequence:
inpath <- "data/en_US.blogs.clean.txt"
n.lines.us_blogs.clean <- countLines(inpath)
n.lines.us_blogs.in <- ceiling(sampling * n.lines.us_blogs.clean)
set.seed(38419)
indices.us_blogs.all <- sample(1:n.lines.us_blogs.clean, n.lines.us_blogs.clean, replace = FALSE)
names(indices.us_blogs.all) <- "indx"
write.table(indices.us_blogs.all, file = "data/random_indices.us_blogs.txt", row.names = FALSE)

# read a contiguous sample of random indices as per <sampling> & index of sample (range = 1 : (1 / sampling))
indices.us_blogs.in <- read.table("data/random_indices.us_blogs.txt", header = TRUE, stringsAsFactors = FALSE)
sb <- get.sample.bounds(n.lines.us_blogs.in, 1)  #: take 1st chunk of random indices
indices.us_blogs.in <- indices.us_blogs.in[sb[1]:sb[2], ]

# read whole file, save a sample as per random indices obtained above: 
indata <- read_lines(inpath)
outdata <- indata[indices.us_blogs.in]
writeLines(outdata, paste0("data/corpusdir/", "en_US.blogs.corpus.txt"))

indata <- read_lines(inpath)
indices <- sample(1:n.lines.us_blogs.clean, n.lines.us_blogs.in, replace = FALSE)
outdata <- indata[indices]
writeLines(outdata, paste0("data/corpusdir/", "en_US.blogs.corpus.txt"))

rm(list = c("indata", "outdata"))
rm(list = c("indices.us_blogs.in", "indices.us_blogs.all"))


# twits :::::::::::::::::::::::::::::::::::::::::::

# prepare randomised sampling sequence:
inpath <- "data/en_US.twitter.clean.txt"
n.lines.us_twitter.clean <- countLines(inpath)
n.lines.us_twitter.in <- ceiling(sampling * n.lines.us_twitter.clean)
set.seed(3578)
indices.us_twitter.all <- sample(1:n.lines.us_twitter.clean, n.lines.us_twitter.clean, replace = FALSE)
names(indices.us_twitter.all) <- "indx"
write.table(indices.us_twitter.all, file = "data/random_indices.us_twitter.txt", row.names = FALSE)

# read a contiguous sample of random indices as per <sampling> & index of sample (range = 1 : (1 / sampling))
indices.us_twitter.in <- read.table("data/random_indices.us_twitter.txt", header = TRUE, stringsAsFactors = FALSE)
sb <- get.sample.bounds(n.lines.us_twitter.in, 1)  #: take 1st chunk of random indices
indices.us_twitter.in <- indices.us_twitter.in[sb[1]:sb[2], ]

# read whole file, save a sample as per random indices obtained above: 
indata <- read_lines(inpath)
outdata <- indata[indices.us_twitter.in]
writeLines(outdata, paste0("data/corpusdir/", "en_US.twitter.corpus.txt"))

rm(list = c("indata", "outdata"))
rm(list = c("indices.us_twitter.in", "indices.us_twitter.all"))


#-------------------------------------------------------------------------------
# Create Term-Document Matrices from the corpora, run tests
#-------------------------------------------------------------------------------

# KLUDGE: must temporarily replace apostrophes with dummy char or fuking tokenizer will split contractions:
files <- dir("data/corpusdir")
for (ifile in 1:NROW(files))
{
    dat <- readLines(paste0("data/corpusdir/", files[ifile]))
    dat <- gsub("'", "#", dat, fixed = TRUE)
    writeLines(dat, paste0("data/corpusdir/", files[ifile]))
}
rm("dat")


# explore n-gram frequencies ...

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# frequencies of word singlets ...
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

dirsource <- DirSource("data/corpusdir")
corpus1 <- Corpus(dirsource, readerControl = list(language = "eng")) #: NOT "lat" !

# METHOD 1: USING tm ::::::::

#NFG: corpus1[[1]]$content <- gsub("'", "#", corpus1[[1]]$content, fixed = TRUE)
#TEST:
#corpus1[[1]]$content[1:100]

# t1 <- system.time( tdm <- TermDocumentMatrix(corpus1) )
# m <- inspect(tdm)
# word.count <- sort(rowSums(m), decreasing = TRUE) 

# METHOD 2: USING quanteda ::::::::

corpus2 <- corpus(corpus1)
rm("corpus1")
t1 <- system.time( dfm1 <- dfm(corpus2, ngrams = 1, verbose = FALSE, concatenator = " ") )
word.count <- word.count <- sort(colSums(dfm1), decreasing = TRUE) 

# EITHER METHOD :::

word.freq <- word.count / sum(word.count)

tfm.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
write.table(tfm.df, file = "data/output/all.tfm1c1X.txt", row.names = FALSE)
num.singlets.c1 <- nrow(tfm.df)

tfm.df2 <- tfm.df[tfm.df$count > 1, ]
tfm.df2$freq <- tfm.df2$count / sum(tfm.df2$count)
num.singlets.c2 <- nrow(tfm.df2)
write.table(tfm.df2, file = "data/output/all.tfm1c2X.txt", row.names = FALSE)

tfm.df2 <- tfm.df[tfm.df$count > 2, ]
tfm.df2$freq <- tfm.df2$count / sum(tfm.df2$count)
num.singlets.c3 <- nrow(tfm.df2)
write.table(tfm.df2, file = "data/output/all.tfm1c3X.txt", row.names = FALSE)

rm(list=c("tfm.df2", "tfm.df", "dfm1", "m", "tdm", "word.count", "word.freq", "corpus2", "corpus1"))
gc()


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# frequencies of word doublets ...
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

dirsource <- DirSource("data/corpusdir")
corpus1 <- Corpus(dirsource, readerControl = list(language = "eng")) #: NOT "lat" !

# METHOD 1: USING tm ::::::::

# t2 <- system.time( tdm.2 <- TermDocumentMatrix(corpus1, control = list(tokenize = BigramTokenizer)) )
# m <- inspect(tdm.2)
# word.count <- sort(rowSums(m), decreasing = TRUE) 

# METHOD 2: USING quanteda ::::::::

corpus2 <- corpus(corpus1)
rm("corpus1")
t2 <- system.time( dfm2 <- dfm(corpus2, ngrams = 2, verbose = FALSE, concatenator = " ") )
word.count <- word.count <- sort(colSums(dfm2), decreasing = TRUE) 

# EITHER METHOD :::

word.freq <- word.count / sum(word.count)

tfm.2.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
tfm.2.df$word <- gsub("#", "'", tfm.2.df$word, fixed = TRUE)
num.doublets.c1 <- nrow(tfm.2.df)
#write.table(tfm.2.df, file = "data/output/test2_tfm.txt", row.names = FALSE)
write.table(tfm.2.df, file = "data/output/all.tfm2c1X.txt", row.names = FALSE)

tfm.2.df2 <- tfm.2.df[tfm.2.df$count > 1, ]
tfm.2.df2$freq <- tfm.2.df2$count / sum(tfm.2.df2$count)
num.doublets.c2 <- nrow(tfm.2.df2) 
write.table(tfm.2.df2, file = "data/output/all.tfm2c2X.txt", row.names = FALSE)

rm(list=c("tfm.2.df2", "tfm2.df", "dfm2", "m", "tdm2", "word.count", "word.freq", "corpus2", "corpus1"))
gc()


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# frequencies of word triplets ...
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

dirsource <- DirSource("data/corpusdir")
corpus1 <- Corpus(dirsource, readerControl = list(language = "eng")) #: NOT "lat" !

# METHOD 1: USING tm ::::::::

# t3 <- system.time( tdm.3 <- TermDocumentMatrix(corpus1, control = list(tokenize = TrigramTokenizer)) )
# m <- inspect(tdm.3)
# word.count <- sort(rowSums(m), decreasing = TRUE) 

# METHOD 2: USING quanteda ::::::::

corpus2 <- corpus(corpus1)
rm("corpus1")
t3 <- system.time( dfm3 <- dfm(corpus2, ngrams = 3, verbose = FALSE, concatenator = " ") )
word.count <- word.count <- sort(colSums(dfm3), decreasing = TRUE) 

# EITHER METHOD :::

word.freq <- word.count / sum(word.count)
tfm.3.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
num.triplets.c1 <- nrow(tfm.3.df)
tfm.3.df$word <- gsub("#", "'", tfm.3.df$word, fixed = TRUE)
write.table(tfm.3.df, file = "data/output/all.tfm3c1X.txt", row.names = FALSE)

tfm.3.df2 <- tfm.3.df[tfm.3.df$count > 1, ]
tfm.3.df2$freq <- tfm.3.df2$count / sum(tfm.3.df2$count)
num.triplets.c2 <- nrow(tfm.3.df2)
write.table(tfm.3.df2, file = "data/output/all.tfm3c2X.txt", row.names = FALSE)

rm(list=c("tfm.3.df2", "tfm.3.df", "dfm3", "m", "tdm3", "word.count", "word.freq", "corpus2", "corpus1"))
gc()

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# frequencies of word quadruplets ...
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

dirsource <- DirSource("data/corpusdir")
corpus1 <- Corpus(dirsource, readerControl = list(language = "eng")) #: NOT "lat" !

# METHOD 1: USING tm ::::::::

# t4 <- system.time( tdm.4 <- TermDocumentMatrix(corpus1, control = list(tokenize = TetragramTokenizer)) )
# m <- inspect(tdm.4)
# word.count <- sort(rowSums(m), decreasing = TRUE) 

# METHOD 2: USING quanteda ::::::::

corpus2 <- corpus(corpus1)
rm("corpus1")
t4 <- system.time( dfm4 <- dfm(corpus2, ngrams = 4, verbose = FALSE, concatenator = " ") )
word.count <- word.count <- sort(colSums(dfm4), decreasing = TRUE) 

# EITHER METHOD :::

word.freq <- word.count / sum(word.count)
tfm.4.df <- data.frame(word = names(word.count), count = word.count, freq = word.freq, stringsAsFactors = FALSE)
num.quadruplets.c1 <- nrow(tfm.4.df)
tfm.4.df$word <- gsub("#", "'", tfm.4.df$word, fixed = TRUE)
# write.table(tfm.4.df, file = "data/output/all.tfm4c1X.txt", row.names = FALSE)

tfm.4.df2 <- tfm.4.df[tfm.4.df$count > 1, ]
tfm.4.df2$freq <- tfm.4.df2$count / sum(tfm.4.df2$count)
num.quadruplets.c2 <- nrow(tfm.4.df2)
write.table(tfm.4.df2, file = "data/output/all.tfm4c2X.txt", row.names = FALSE)

rm(list=c("tfm.4.df2", "tfm.4.df", "dfm4", "m", "tdm4", "word.count", "word.freq", "corpus2", "corpus1"))
gc()


#-------------------------------------------------------------------------------
# incidence of non-dictionary words ?????????????????????????
#-------------------------------------------------------------------------------

inpath <- "data/en_US.news.corpus_s001.txt"
indata1 <- readLines(inpath)
nonwords1 <- sapply(indata1, function(x) {
    length(which_misspelled(x, suggest = FALSE)) })
n.nonwords1 <- sum(nonwords1) #: 16150



#-------------------------------------------------------------------------------
# split n-gram matrices to separate predicted (n-th) word from (n-1)-gram
#-------------------------------------------------------------------------------

infolder <- "data/output"
outfolder <- "data/modeldir"
files <- dir(infolder)

# 2-grams ...

inpath <- paste0(infolder, "/", "all.tfm2c2.txt")
indata <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
outdata.words <- str_split(indata$word, " ")
outdata <- data.frame(matrix(unlist(outdata.words), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
names(outdata) <- c("parent", "child")
outdata$count <- indata$count
outdata$prob <- indata$freq
outpath <- paste0(outfolder, "/", "pm2.txt")
write.table(outdata, file = outpath, row.names = FALSE)

# 3-grams ...

inpath <- paste0(infolder, "/", "all.tfm3c2.txt")
indata <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
outdata.words <- str_split(indata$word, " ")
outdata2 <- data.frame(matrix(unlist(outdata.words), ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
outdata <- data.frame("parent" = paste(outdata2[, 1], outdata2[, 2]), "child" = outdata2[, 3])
outdata$count <- indata$count
outdata$prob <- indata$freq
outpath <- paste0(outfolder, "/", "pm3.txt")
write.table(outdata, file = outpath, row.names = FALSE)

# 4-grams ...

inpath <- paste0(infolder, "/", "all.tfm4c2.txt")
indata <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
outdata.words <- str_split(indata$word, " ")
outdata2 <- data.frame(matrix(unlist(outdata.words), ncol = 4, byrow = TRUE), stringsAsFactors = FALSE)
outdata <- data.frame("parent" = paste(outdata2[, 1], outdata2[, 2], outdata2[, 3]), "child" = outdata2[, 4])
outdata$count <- indata$count
outdata$prob <- indata$freq
outpath <- paste0(outfolder, "/", "pm4.txt")
write.table(outdata, file = outpath, row.names = FALSE)


rm(list = c("outdata2", "outdata", "indata", "outdata.words"))


#--------------------------------------------------------------------------------------
# reform each parent-child n-gram matrix into a list of lists of named members
#--------------------------------------------------------------------------------------

# same as above:
infolder <- "data/modeldir"
outfolder <- infolder

# tested on Fresnel, not Newton:
maxnum.children <- 6

# 2-grams ...

inpath <- paste0(infolder, "/", "pm2.txt")
pcm <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)

lookup2 <- list()
parents <- unique(pcm$parent)
n.parents <- length(parents)
for (i in 1:n.parents) {
    words <- pcm$child[pcm$parent == parents[i]]
    counts <- pcm$count[pcm$parent == parents[i]]
    probs <- pcm$prob[pcm$parent == parents[i]]
    n.children <- min(c(NROW(words), maxnum.children))  #: limit predictions for each parent
    ngram.tail <- list()
    for (k in 1:n.children) {
        ngram.tail[[k]] <- list(word = words[k], count = counts[k], prob = probs[k])
    }
    lookup2[[parents[i]]] <- list(children = ngram.tail)
    if (i %% 1000 == 0) cat(paste0("element ", i, " of ", n.parents, " ... "))
}
cat(paste0("Finished 2-grams."), "\n")

path.pml2 <- paste0(outfolder, "/", "pml2.rda")
save(lookup2, file = path.pml2)

#X: lookup2.in <- load(file = outpath)
#: load(file = path.pml2)


# # TEST###################################
# # 2-grams ...
# 
# inpath <- paste0(infolder, "/", "pm2_S001.txt")
# pcm <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
# 
# lookup2test <- list()
# parents <- unique(pcm$parent)
# n.parents <- length(parents)
# for (i in 1:n.parents) {
#     words <- pcm$child[pcm$parent == parents[i]]
#     counts <- pcm$count[pcm$parent == parents[i]]
#     probs <- pcm$prob[pcm$parent == parents[i]]
#     n.children <- min(c(NROW(words), maxnum.children))  #: limit predictions for each parent
#     ngram.tail <- list()
#     for (k in 1:n.children) {
#         ngram.tail[[k]] <- list(word = words[k], count = counts[k], prob = probs[k])
#     }
#     lookup2test[[parents[i]]] <- list(children = ngram.tail)
# }
# 
# path.pml2test <- paste0(outfolder, "/", "pml2test.rda")
# save(lookup2test, file = path.pml2test)
# 
# #X: lookup2.in <- load(file = outpath)
# load(file = path.pml2test)
# 
# lookup2test[["of"]]$children
# lookup2test[["start"]]$children[[1]]$word
# lookup2test[["start"]]$children[[1]]$prob


# 3-grams ...

inpath <- paste0(infolder, "/", "pm3.txt")
pcm <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)

lookup3 <- list()
parents <- unique(pcm$parent)
n.parents <- length(parents)
system.time( {
for (i in 1:n.parents) {
    words <- pcm$child[pcm$parent == parents[i]]
    counts <- pcm$count[pcm$parent == parents[i]]
    probs <- pcm$prob[pcm$parent == parents[i]]
    n.children <- min(c(NROW(words), maxnum.children))  #: limit predictions for each parent
    ngram.tail <- list()
    for (k in 1:n.children) {
        ngram.tail[[k]] <- list(word = words[k], count = counts[k], prob = probs[k])
    }
    lookup3[[parents[i]]] <- list(children = ngram.tail)
    if (i %% 1000 == 0) cat(paste0("element ", i, " of ", n.parents, " ... ", "\n"))
}
} )
cat(paste0("Finished 3-grams."))

path.pml3 <- paste0(outfolder, "/", "pml3.rda")
save(lookup3, file = path.pml3)

rm("lookup3")


# 4-grams ...

inpath <- paste0(infolder, "/", "pm4.txt")
pcm <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)

lookup4 <- list()
parents <- unique(pcm$parent)
n.parents <- length(parents)
system.time( {
    for (i in 1:n.parents) {
        words <- pcm$child[pcm$parent == parents[i]]
        counts <- pcm$count[pcm$parent == parents[i]]
        probs <- pcm$prob[pcm$parent == parents[i]]
        n.children <- min(c(NROW(words), maxnum.children))  #: limit predictions for each parent
        ngram.tail <- list()
        for (k in 1:n.children) {
            ngram.tail[[k]] <- list(word = words[k], count = counts[k], prob = probs[k])
        }
        lookup4[[parents[i]]] <- list(children = ngram.tail)
        if (i %% 1000 == 0) cat(paste0("element ", i, " of ", n.parents, " ... ", "\n"))
    }
} )
cat(paste0("Finished 4-grams."))

path.pml4 <- paste0(outfolder, "/", "pml4.rda")
save(lookup4, file = path.pml4)


#--------------------------------------------------------------------------------------
# drop the 'prob' columns
#--------------------------------------------------------------------------------------

infolder <- "data/modeldir"
outfolder <- infolder

table1 <- read.table(paste0(infolder, "/", "tfm1.txt"), header = TRUE, stringsAsFactors = FALSE)
table1 <- table1[, 1:2]
write.table(table1, paste0(infolder, "/", "tfm1c.txt"), row.names = FALSE)

dflookup2 <- read.table(paste0(infolder, "/", "pm2.txt"), header = TRUE, stringsAsFactors = FALSE)
dflookup2 <- dflookup2[, 1:3]
write.table(dflookup2, paste0(infolder, "/", "pm2c.txt"), row.names = FALSE)

dflookup3 <- read.table(paste0(infolder, "/", "pm3.txt"), header = TRUE, stringsAsFactors = FALSE)
dflookup3 <- dflookup3[, 1:3]
write.table(dflookup3, paste0(infolder, "/", "pm3c.txt"), row.names = FALSE)

dflookup4 <- read.table(paste0(infolder, "/", "pm4.txt"), header = TRUE, stringsAsFactors = FALSE)
dflookup4 <- dflookup4[, 1:3]
write.table(dflookup4, paste0(infolder, "/", "pm4c.txt"), row.names = FALSE)


#--------------------------------------------------------------------------------------
# save the input data files as R objects
#--------------------------------------------------------------------------------------

infolder <- "data/modeldir"
outfolder <- infolder

table1 <- read.table(paste0(infolder, "/", "tfm1c.txt"), header = TRUE, stringsAsFactors = FALSE)
path.pmd1 <- paste0(outfolder, "/", "pmd1.rda")
save(table1, file = path.pmd1)

dflookup2 <- read.table(paste0(infolder, "/", "pm2c.txt"), header = TRUE, stringsAsFactors = FALSE)
path.pmd2 <- paste0(outfolder, "/", "pmd2.rda")
save(dflookup2, file = path.pmd2)

dflookup3 <- read.table(paste0(infolder, "/", "pm3c.txt"), header = TRUE, stringsAsFactors = FALSE)
path.pmd3 <- paste0(outfolder, "/", "pmd3.rda")
save(dflookup3, file = path.pmd3)

dflookup4 <- read.table(paste0(infolder, "/", "pm4c.txt"), header = TRUE, stringsAsFactors = FALSE)
path.pmd4 <- paste0(outfolder, "/", "pmd4.rda")
save(dflookup4, file = path.pmd4)






#--------------------------------------------------------------------------------------

# TODO:
# 1. recompute probs, normalised over each group of lookup children; encapsulate as fn for reuse


