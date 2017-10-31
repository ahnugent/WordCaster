# file name:    NLP_lookup.R
# author:       Allen H. Nugent
# created:      28/03/16
# purpose:      Data model for n-gram lookup for text predction model

tfm <- data.frame(ngram.base = c("a", "a", "a", "b", "c", "c", "d", "d", "d", "d"), 
                  ngram.end = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                  ngram.freq = c(5, 3, 2, 10, 7, 3, 3, 3, 3, 1), 
                    stringsAsFactors = FALSE)


# lookup <- list()
# parents <- unique(tfm$ngram.base)
# n.parents <- length(parents)
# for (i in 1:n.parents) {
# #    j <- length(lookup) + 1
# #E:    lookup[[j]]$parent <- parents[i]
#     ends <- tfm$ngram.end[tfm$ngram.base == parents[i]]
#     freqs <- tfm$ngram.freq[tfm$ngram.base == parents[i]]
# #    n.children <- length(ends)
#     n.children <- NROW(ends)
#     ngram <- list()
#     for (k in 1:n.children) {
# #         m <- length(ngram) + 1
# #         ngram[[m]]$child <- ends[k]
# #         ngram[[m]]$freq <- freqs[k]
#         ngram[[k]] <- c(child = ends[k], freq <- freqs[k])
#     }
# #    lookup[[j]] <- list(parent = parents[i], children = ngram$child, freqs = ngram$freq)
#     j <- length(lookup) + 1
#     lookup[[j]] <- list(parent = parents[i], children = ngram)
# }


lookup <- list()
parents <- unique(tfm$ngram.base)
n.parents <- length(parents)
for (i in 1:n.parents) {
    ends <- tfm$ngram.end[tfm$ngram.base == parents[i]]
    freqs <- tfm$ngram.freq[tfm$ngram.base == parents[i]]
    n.children <- NROW(ends)
    ngram <- list()
    for (k in 1:n.children) {
#        ngram[[k]] <- c(child = ends[k], freq = freqs[k])
#         ngram[[k]][["child"]] <- ends[k]
#         ngram[[k]][["freq"]] <- freqs[k]
        ngram[[k]] <- list(child = ends[k], freq = freqs[k])
    }
    j <- length(lookup) + 1
    lookup[[parents[i]]] <- list(children = ngram)
}



in.parent <- "b"
out.children <- lookup[[in.parent]]$children

in.parent <- "c"
out.children <- lookup[[in.parent]]$children
out.children
str(out.children)
out.children$child
ngram[[1]]
