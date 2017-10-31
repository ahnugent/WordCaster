## Script:      lib_NLP.R
## Author:      Allen H. Nugent
## Created:     2016-03-22
## Last edit:   2016-04-04
## Last test:   2016-04-04
##
## Purpose:     Support routines for Assignment for Course 10 ('data-science-project')


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Data Cleaning
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clean_text <- function(dat)
{
    # replace unicode apostrophe, curly single quotes with ASCII apostrophe:
    dat <- gsub("[\u0092\u275b\u275c]", "\'", dat)
    
    # replace backtick with ASCII apostrophe:
    dat <- gsub("`", "\'", dat)
    
    # replace minus sign with space (hoping to preserve hyphens):
    dat <- gsub(" -", ' ', dat, fixed = TRUE)
    
    # collapse multiple periods:
    dat <- gsub("\\.{2,}", ".", dat)
    
    # replace all sentence/phrase terminators with (period + space):
    dat <- gsub("[!?:;]", '. ', dat)

    # remove all but letters, spaces, apostrophes, hyphens, periods:
    dat <- gsub("[^a-zA-Z\\. \'-]", ' ', dat)

    # lower case (must be after character cull):
    dat <- tolower(dat)
    
    # split lines at end-of-sentence 
    #   preserve "Mr.", "Mrs.", "Dr.", "St.", "Prof.", etc.):
    dat <- gsub("mr.", "mr|", dat, fixed = TRUE)
    dat <- gsub("mrs.", "mrs|", dat, fixed = TRUE)
    dat <- gsub("ms.", "ms|", dat, fixed = TRUE)
    dat <- gsub("dr.", "mr|", dat, fixed = TRUE)
    dat <- gsub("prof.", "prof|", dat, fixed = TRUE)
    dat <- gsub("st.", "st|", dat, fixed = TRUE)

    #: split paragraphs on periods (other sentence boundaries too ambiguous):
    dat <- unlist(str_split(dat, pattern = "\\."))

    # 16/04/16: this doesn't work: tokenizer kills the periods anyway!
    #   (but this is compensated for in the predictive algorithm, so no probs)
    dat <- gsub("mr|", "mr.", dat, fixed = TRUE)
    dat <- gsub("mrs|", "mrs.", dat, fixed = TRUE)
    dat <- gsub("ms|", "ms.", dat, fixed = TRUE)
    dat <- gsub("dr|", "mr.", dat, fixed = TRUE)
    dat <- gsub("prof|", "prof.", dat, fixed = TRUE)
    dat <- gsub("st|", "st.", dat, fixed = TRUE)
    
    # collapse multiple space chars (do this 2nd last!):
    dat <- gsub(" {2,}", " ", dat)
    
    # remove apostrophes used as single-quotes:
    dat <- gsub("^\'", '', dat)
    dat <- gsub("\'$", '', dat)
    dat <- gsub(" '", '', dat)  
    dat <- gsub("' ", '', dat)
    #? dat <- gsub(" \'", '', dat)  
    #? dat <- gsub("\' ", '', dat)
    
    # filter profanities (by replacing with "zark") ...
    
    bad.words <- readLines("list_badwords.txt")
    for (i in 1:length(bad.words))
    {
        if (bad.words[i] != "")
        {        
            #X: dat <- gsub(paste0("^", bad.words[i]), "zark", dat)
            dat <- gsub(bad.words[i], "zark", dat, fixed = TRUE)
        }
    }
    
    # delete empty lines, 1-char lines:
    dat <- dat[nchar(dat) > 1]
    
    # reverse stupid abbreviations (where unamibiguous):
    dat <- gsub(" u ", " you ", dat, fixed = TRUE)
    dat <- gsub(" r ", " are ", dat, fixed = TRUE)
    dat <- gsub(" ur ", " your ", dat, fixed = TRUE)
    dat <- gsub(" c ", " see ", dat, fixed = TRUE)
    
    # 02/04/16: fix contractions with missing apostrophes:
    dat <- gsub(" don t ", " don\'t ", dat, fixed = TRUE)
    dat <- gsub(" it s ", " it\'s ", dat, fixed = TRUE)
    dat <- gsub(" i m ", " i\'m ", dat, fixed = TRUE)
    dat <- gsub(" didn t ", " didn\'t ", dat, fixed = TRUE)
    dat <- gsub(" can t ", " can\'t ", dat, fixed = TRUE)
    dat <- gsub(" i ve ", " iv\'e ", dat, fixed = TRUE)
    dat <- gsub(" haven t ", " haven\'t ", dat, fixed = TRUE)
    dat <- gsub(" doesn t ", " doesn\'t ", dat, fixed = TRUE)
    dat <- gsub(" that s ", " that\'s ", dat, fixed = TRUE)
    dat <- gsub(" wasn t ", " wasn\'t ", dat, fixed = TRUE)
    dat <- gsub(" isn t ", " isn\'t ", dat, fixed = TRUE)
    dat <- gsub(" won t ", " won\'t ", dat, fixed = TRUE)
    dat <- gsub(" i d ", " i\'d ", dat, fixed = TRUE)
    dat <- gsub(" we d ", " we\'d ", dat, fixed = TRUE)
    dat <- gsub(" wouldn t ", " wouldn\'t ", dat, fixed = TRUE)
    
    #::: PROBLEM: need to catch words at sentence start, also !
    dat <- gsub("^don t ", "don\'t ", dat)
    dat <- gsub("^it s ", "it\'s ", dat)
    dat <- gsub("^i m ", "i\'m ", dat)
    dat <- gsub("^didn t ", "didn\'t ", dat)
    dat <- gsub("^can t ", "can\'t ", dat)
    dat <- gsub("^i ve ", "iv\'e ", dat)
    dat <- gsub("^haven t ", "haven\'t ", dat)
    dat <- gsub("^doesn t ", "doesn\'t ", dat)
    dat <- gsub("^that s ", "that\'s ", dat)
    dat <- gsub("^wasn t ", "wasn\'t ", dat)
    dat <- gsub("^isn t ", "isn\'t ", dat)
    dat <- gsub("^won t ", "won\'t ", dat)
    dat <- gsub("^i d ", "i\'d ", dat)
    dat <- gsub("^we d ", "we\'d ", dat)
    dat <- gsub("^wouldn t ", "wouldn\'t ", dat)
    
    return(dat)
}


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

TetragramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Utilities
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get.suffix <- function(sampling) {
    
    if (sampling == 0.01) { suffix <- "001" }
    if (sampling == 0.1) { suffix <- "010" }
    if (sampling == 1) { suffix <- "100" }
    return(suffix)
}


get.sample.bounds <- function(n.lines.in, i.sample = 1) {

    i1 <- 1 + (i.sample - 1) * n.lines.in
    i2 <- i1 + n.lines.in - 1
    return(c(i1, i2))
}


get.num.tokens <- function(ngram) {
    # returns the number of individual tokens (words) within an n-gram
    
    str1 <- gsub(' {2,}',' ', ngram) #: collapse multiple spaces
    str1 <- trim(str1)  #: trim leading & trailing spaces
    num.tokens <- length(strsplit(str1, " ")[[1]])
    return(num.tokens)
}


trim.tokens <- function(ngram, ntokens = 3) {
    # trims excess tokens from the beginning of the n-gram
    
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


