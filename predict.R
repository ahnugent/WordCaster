## Script:      predict.R
## Author:      Allen H. Nugent
## Created:     2016-04-05
## Last edit:   2016-04-14
## Last test:   2016-04-17
##
## Context:     Assignment for Course 10 ('data-science-project')
## Purpose:     Word prediction tool
##              
## Notes:
##
## Tasks:
##
##
##

rm(list=ls())

setwd("C:/R_data/Course10_Wk1")
setwd("E:/R_data/Course10")

library(stringr)

infolder <- "data/modeldir"
files <- dir(infolder)

maxnum.choices <- 8

# infiles <- files[grepl("tfm2", files, fixed = TRUE)]  #: should only match 1 file
# inpath <- paste0(infolder, "/", infiles[1])
# tfm2 <- read.table(inpath, header = TRUE, stringsAsFactors = FALSE)
# names(tfm2)

# Nb. caller should wait until one whole word entered before calling predictor.



# Load data for METHOD 1 (list-based)
#..........................................

ngram.find <- tolower(ngram.in)

path.table1 <- paste0(infolder, "/", "tfm1.txt")
table1 <- read.table(path.table1, header = TRUE, stringsAsFactors = FALSE)
path.lookup2 <- paste0(infolder, "/", "pml2.rda")
load(path.lookup2)
path.lookup3 <- paste0(infolder, "/", "pml3.rda")
load(path.lookup3)
path.lookup4 <- paste0(infolder, "/", "pml4.rda")
load(path.lookup4)

mem.data <- (object.size(table1) + object.size(lookup2) + object.size(lookup3) + object.size(lookup4)) / 1024^2
mem.data


# Load data for METHOD 2 (data.frame-based)
#................................................

# METHOD 2A - Read data from data frames ...
#
path.table1 <- paste0(infolder, "/", "tfm1.txt")
table1 <- read.table(path.table1, header = TRUE, stringsAsFactors = FALSE)
path.lookup2 <- paste0(infolder, "/", "pm2.txt")
dflookup2 <- read.table(path.lookup2, header = TRUE, stringsAsFactors = FALSE)
path.lookup3 <- paste0(infolder, "/", "pm3.txt")
dflookup3 <- read.table(path.lookup3, header = TRUE, stringsAsFactors = FALSE)
path.lookup4 <- paste0(infolder, "/", "pm4.txt")
dflookup4 <- read.table(path.lookup4, header = TRUE, stringsAsFactors = FALSE)
#
# METHOD 2B - Read data from R objects ...
#

system.time( {
    load(paste0(infolder, "/", "pmd1.rda")) #: table1
    load(paste0(infolder, "/", "pmd2.rda")) #: dflookup2
    load(paste0(infolder, "/", "pmd3.rda")) #: dflookup3
    load(paste0(infolder, "/", "pmd4.rda")) #: dflookup4
} )

mem.data <- (object.size(table1) + object.size(dflookup2) + object.size(dflookup3) + object.size(dflookup4)) / 1024^2
paste0(cround(mem.data, 1), " MB")


# Functions for METHOD 1 
#..........................................


get.predicted.ngrams <- function(predicted) {
    # returns predicted ngrams and attributes as a data frame
    n.children <- length(predicted$children)
    ngrams <- data.frame(term = rep("", n.children), 
                         count = rep(0, n.children), 
                         prob = rep(0, n.children), 
                         stringsAsFactors = FALSE)
    for (i in 1:n.children) {
        ngrams$term[i] <- predicted$children[[i]]$word
        ngrams$count[i] <- predicted$children[[i]]$count
        ngrams$prob[i] <- predicted$children[[i]]$prob
    }
    ngrams <- ngrams[order(-ngrams$prob), ]   # make sure order is correct
    
    return(ngrams)
}


get.choices.l <- function(ngram.in) {    
    n.children <- 0
    ngrams <- NULL
    ngram.len <- get.num.tokens(ngram.find)
    if (ngram.find != "" & ngram.len == 3)   #: predict 4th word in sequence
    {
        print(paste0("Finding '", ngram.find, "' in 4-grams ..."))
        predicted <- lookup4[[ngram.find]]
        if (!is.null(predicted)) {
            ngrams <- get.predicted.ngrams(predicted)
            ngram.find <- ""  #: flag to stop searching
        } else {
            print(paste0("Did not find '", ngram.find, "'"))
            terms <- strsplit(unlist(ngram.find), split = " ")
            ngram.find <- paste(terms[[1]][2], terms[[1]][3])  #: drop 1st word and keep searching
        }
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (ngram.find != "" & ngram.len == 2)   #: predict 3rd word in sequence
    {
        print(paste0("Finding '", ngram.find, "' in 3-grams ..."))
        predicted <- lookup3[[ngram.find]]
        if (!is.null(predicted)) {
            ngrams <- get.predicted.ngrams(predicted)
            ngram.find <- ""  #: flag to stop searching
        } else {
            print(paste0("Did not find '", ngram.find, "'"))
            terms <- strsplit(unlist(ngram.find), split = " ")
            ngram.find <- terms[[1]][2]  #: drop 1st word and keep searching
        }
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (ngram.find != "" & ngram.len == 1)   #: predict 2nd word in sequence
    {
        print(paste0("Finding '", ngram.find, "' in 2-grams ..."))
        predicted <- lookup2[[ngram.find]]
        if (!is.null(predicted)) {
            ngrams <- get.predicted.ngrams(predicted)
            ngram.find <- ""  #: flag to stop searching
        } else {
            print(paste0("Did not find '", ngram.find, "'"))
        }
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (ngram.find != "" & ngram.len == 1)   #: predict sole word
    {
        # TODO: Find a way to prevent ridiculous predictions:
        #    1. can't have stopword after "the" or "a"
        ngrams <- table1[1,]
    }
    
        return(ngrams)
}


# Functions for METHOD 2
#..........................................


get.choices.df <- function(ngram.in, num.choices = maxnum.choices) {    
    
    maxnum.gram <- 3   #: training data only supports prediction from 3-grams down
    
    ngram.in <- trim(ngram.in)  #: remove leading & trailing space
    if (ngram.in == "") { 
        num.to.find <- 0   #: reject empty string
        ngram.find <- ""
    } else {
        num.to.find <- num.choices
        ngram.find <- ngram.in
    }
    n.children <- 0
    ngrams <- NULL
    ngrams.all <- NULL
    ngram.find <- trim.tokens(ngram.find, maxnum.gram)  #: remove excessive leading tokens
    ngram.len <- get.num.tokens(ngram.find)
    if (num.to.find > 0 & ngram.len == 3)   #: predict 4th word in sequence
    {
        print(paste0("Finding '", ngram.find, "' in 4-grams ..."))
        ngrams <- dflookup4[dflookup4$parent == ngram.find, ]
        if (nrow(ngrams) > 0) {
            ngrams <- ngrams[order(ngrams$count, decreasing = TRUE), ]
            n.children <- nrow(ngrams)
            ngrams <- ngrams[1:min(n.children, num.to.find), ]
            ngrams.all <- ngrams
            # can we stop searching?:
            num.to.find <- num.to.find - nrow(ngrams)  
        } else {
            print(paste0("Did not find '", ngram.find, "'"))
        }
        if(num.to.find > 0) {
            terms <- strsplit(unlist(ngram.find), split = " ")
            ngram.find <- paste(terms[[1]][2], terms[[1]][3])  #: drop 1st word and keep searching
        } else {
            ngram.find <- "" 
        }
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (num.to.find > 0 & ngram.len == 2)   #: predict 3rd word in sequence
    {
        print(paste0("Finding '", ngram.find, "' in 3-grams ..."))
        ngrams <- dflookup3[dflookup3$parent == ngram.find, ]
        if (nrow(ngrams) > 0) {
            ngrams <- ngrams[order(ngrams$count, decreasing = TRUE), ]
            n.children <- nrow(ngrams)
            ngrams <- ngrams[!ngrams$child %in% ngrams.all$child, ]  #: remove children captured above
            ngrams <- ngrams[1:min(n.children, num.to.find), ]
            ngrams.all <- rbind(ngrams.all, ngrams)
            # can we stop searching?:
            num.to.find <- num.to.find - nrow(ngrams)  
        } else {
            print(paste0("Did not find '", ngram.find, "'"))
        }
        if(num.to.find > 0) {
            terms <- strsplit(unlist(ngram.find), split = " ")
            ngram.find <- terms[[1]][2]  #: drop 1st word and keep searching
        } else {
            ngram.find <- "" 
        }
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (num.to.find > 0 & ngram.len == 1)   #: predict 2nd word in sequence
    {
        print(paste0("Finding '", ngram.find, "' in 2-grams ..."))
        ngrams <- dflookup2[dflookup2$parent == ngram.find, ]
        if (nrow(ngrams) > 0) {
            ngrams <- ngrams[order(ngrams$count, decreasing = TRUE), ]
            ngrams <- ngrams[!ngrams$child %in% ngrams.all$child, ]  #: remove children captured above
            n.children <- nrow(ngrams)
            ngrams <- ngrams[1:min(n.children, num.to.find), ]
            
            ngrams.all <- rbind(ngrams.all, ngrams)
            # can we stop searching?:
            num.to.find <- num.to.find - nrow(ngrams)  
            if (nrow(ngrams) == num.to.find) { 
                ngram.find <- "" 
            }
        } else {
            print(paste0("Did not find '", ngram.find, "'"))
        }
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (num.to.find > 0 & ngram.len == 1)   #: predict sole word
    {
        # TODO: Find a way to prevent ridiculous predictions:
        #    1. can't have stopword after "the" or "a"
        
        onegrams <- cbind(rep("", num.to.find), table1[1:num.to.find, ])
        names(onegrams) <- names(ngrams)
        if(num.to.find < num.choices) {    
            ngrams.all <- rbind(ngrams.all, onegrams)
        } else {
            ngrams.all <- onegrams
        }
    }
    
    ngrams.all$child[ngrams.all$child == "i"] <- "I"
    ngrams.all$child[ngrams.all$child == "i'm"] <- "I'm"
    ngrams.all$child[ngrams.all$child == "i'll"] <- "I'll"
    ngrams.all$child[ngrams.all$child == "i've"] <- "I've"
    ngrams.all$child[ngrams.all$child == "i'd"] <- "I'd"
    
    return(ngrams.all)
}


#-------------------------------------------------------------------------------
# TESTS
#-------------------------------------------------------------------------------

# Quiz 2:
ngram.in <- "a case of" # beer
ngram.in <- "would mean the" # world
ngram.in <- "make me the" # happiest
ngram.in <- "struggling but the" # defense
ngram.in <- "date at the" # beach? grocery?
ngram.in <- "be on my" # way
ngram.in <- "in quite some" # time
ngram.in <- "with his little" # fingers
ngram.in <- "faith during the" # bad // no ("during the" "bad") in L4
ngram.in <- "you must be" # insane? asleep? <-! "a", "so", "the", "able"

# Quiz 3:
ngram.in <- "live and I'd" # die <-! NULL
ngram.in <- "me about his" # marital <-! NULL
ngram.in <- "arctic monkeys this" # weekend <-! NULL
ngram.in <- "helps reduce your" # stress <-! NULL
ngram.in <- "to take a" # picture <-! "look" is 29/15 more probable !
ngram.in <- "to settle the" # matter <-! "case"
ngram.in <- "groceries in each" # hand <-! NULL
ngram.in <- "bottom to the" # top <-! NULL
ngram.in <- "bruises from playing" # outside <-! NULL
ngram.in <- "of Adam Sandler's" # movies <-! NULL

# other:
ngram.in <- "one of the"
ngram.in <- "said what the" # zark
ngram.in <- "holy" # zark
ngram.in <- "get zarked"
ngram.in <- "you can get"
ngram.in <- "go zark"

# Test back-off:
#L4 YES: "me centers throughout" "the"
#L4 NO: centers throughout" "it"
#L3 YES (3): throughout" "it"
ngram.in <- "no but throughout"#:+"the" #: TEST: back-off to 2-gram: PASS (07/04/16)
ngram.in <- "any fiscal"#:+"conservative"; actual = "a fiscal"+"conservative" #: TEST: back-off to 1-gram: FAIL


# METHOD 1:

ngram.find <- tolower(ngram.in)
get.choices.l(ngram.in)


# METHOD 2:

ngram.find <- tolower(ngram.in)
get.choices.df(ngram.in)


#-------------------------------------------------------------------------------
# Clean-Up
#-------------------------------------------------------------------------------


rm("lookup2", "lookup3", "lookup4")
rm("dflookup4", "dflookup3", "dflookup2", "table1")
