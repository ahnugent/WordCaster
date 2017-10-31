## Script:      server.R
## Folder:      Wordcaster
## Author:      Allen H. Nugent
## Created:     2016-04-10
## Last edit:   2016-04-17
## Last test:   2016-04-17
## Purpose:     Shiny Server file for DS10 Course project
# 

# load libraries and global data, functions ...

source("global.R")



# get input data for models ...

load("data/pmd1.rda") #: table1 (1-grams)
load("data/pmd2.rda") #: dflookup2
load("data/pmd3.rda") #: dflookup3
load("data/pmd4.rda") #: dflookup4


# custom functions ...


cround <- function(x, ndec)   # round and format a number with a non-ridiculous number of decimals
{
    if (ndec == 0)
    {
        fmt1 <- "%d"
    } else 
    {
        fmt1 <- paste0("%.", ndec, "f")
    }
    result <- sprintf(fmt = fmt1, round(x, ndec))
    return(result)
}


trim <- function (x)  # remove leading or trailing whitespace from string
{
    return (gsub("^\\s+|\\s+$", "", x))
}


get.predicts <- function(textin, num.choices = maxnum.choices)  # return predicted words in a 1-row data frame
{
    predicts.init <- matrix(rep("____", maxnum.choices), nrow = 1, ncol = maxnum.choices)
    predicts.init <- as.data.frame(predicts.init, stringsAsFactors = FALSE)
    names(predicts.init) <- as.character(unlist(seq(from = 1, to = maxnum.choices)))
    rownames(predicts.init) <- "?"
    
    predicts <- predicts.init
    textin <- tolower(trim(textin))
    
    if (textin != "")
    {    
        wordChoices <- get.choices.df(textin, num.choices)[, 2]
        
        for (j in 1:num.choices)
        {
#            predicts[1, j] <- paste0(textin, as.character(j))  #: dummy assignment for testing
            predicts[1, j] <- wordChoices[j]
        }
    }
    predicts <- predicts[, 1:num.choices]
    
    return(predicts)
}


get.num.tokens <- function(ngram)  # returns the number of individual tokens (words) within an n-gram
{
    str1 <- gsub(' {2,}',' ', ngram) #: collapse multiple spaces
    str1 <- trim(str1)  #: trim leading & trailing spaces
    num.tokens <- length(strsplit(str1, " ")[[1]])
    return(num.tokens)
}


trim.tokens <- function(ngram, ntokens = 3)  # trims excess tokens from the beginning of the n-gram
{
    str2 <- ngram
    str1 <- gsub(' {2,}',' ', ngram) #: collapse multiple spaces
    str1 <- trim(str1)  #: trim leading & trailing spaces
    vec1 <- strsplit(str1, " ")[[1]]
    num.tokens <- length(vec1)
    if (num.tokens > ntokens) {
        str2 <- paste(tail(vec1, -(num.tokens - ntokens)), collapse = ' ')
    }
    return(str2)
}


get.choices.df <- function(ngram.in, num.choices = maxnum.choices)  # provides the next word from an n-gram
{    
    # Returns num.choices predictions (most probable first), applying back-off as necessary.
    # Usage:  get.choices.df(ngram.in, num.choices)[,2]
    
    maxnum.gram <- 3   #: training data only supports prediction from 3-grams down
    
    ngram.in <- trim(ngram.in)  #: remove leading & trailing space
    if (ngram.in == "") { 
        num.to.find <- 0   #: reject empty string
        ngram.find <- ""
    } else {
        num.to.find <- maxnum.choices
        ngram.find <- ngram.in
    }
    ngram.find <- gsub(".", "", ngram.find, fixed = TRUE) # remove periods (none in lookups!)
    
    n.children <- 0
    ngrams <- NULL
    ngrams.all <- NULL
    ngram.find <- trim.tokens(ngram.find, maxnum.gram)  #: remove excessive leading tokens
    ngram.len <- get.num.tokens(ngram.find)
    if (num.to.find > 0 & ngram.len == 3)   #: predict 4th word in sequence
    {
        ngrams <- dflookup4[dflookup4$parent == ngram.find, ]
        if (nrow(ngrams) > 0) {
            ngrams <- ngrams[order(ngrams$count, decreasing = TRUE), ]
            n.children <- nrow(ngrams)
            ngrams <- ngrams[1:min(n.children, num.to.find), ]
            ngrams.all <- ngrams
            # can we stop searching?:
            num.to.find <- num.to.find - nrow(ngrams)  
        } else {
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
        ngrams <- dflookup3[dflookup3$parent == ngram.find, ]
        if (nrow(ngrams) > 0) {
            ngrams <- ngrams[order(ngrams$count, decreasing = TRUE), ]
            n.children <- nrow(ngrams)
            ngrams <- ngrams[!ngrams$child %in% ngrams.all$child, ]  #: remove children captured above
            ngrams <- ngrams[1:min(n.children, num.to.find), ]
            ngrams.all <- rbind(ngrams.all, ngrams)
            # can we stop searching?:
            num.to.find <- num.to.find - nrow(ngrams)  
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
        } 
    }
    ngram.len <- get.num.tokens(ngram.find)
    if (num.to.find > 0 & ngram.len == 1)   #: predict sole word
    {
        # TODO: Find a way to prevent ridiculous predictions:
        #    1. can't have stopword after "the" or "a"
        
        onegrams <- cbind(rep("", num.to.find), table1[1:num.to.find, ])
        names(onegrams) <- names(ngrams)
        if(num.to.find < maxnum.choices) {    
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
    
    # TODO: implement spnsored predictions (e.g. marketing campaigns), 
    #       using data from a file that is periodically updated;
    #       for now, just hard-coded:
    if(ngram.in == "wordcaster is")   #: pre-fab'd predictions:
    {
        ngrams.all$child[1] <- "fast"
        ngrams.all$child[2] <- "simple"
        ngrams.all$child[3] <- "cool"
        ngrams.all$child[4] <- "fun"
        ngrams.all$child[5] <- "clever"
        ngrams.all$child[6] <- "awesome"
        ngrams.all$child[7] <- "for sale"
        ngrams.all$child[8] <- "having me on"
    }
    
    return(ngrams.all)
}


# server ...


shinyServer(
    function(input, output) {

        predicts <- reactive({
            msg.text <- ""
            get.predicts(input$textin, input$numChoices)
        })

        output$wordcasts <- renderTable({
            predicts()
        })
    }
)



