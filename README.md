# WordCaster
### NLP engine for predicting the next word in a phrase

Allen H Nugent, 2016

#### Training Data

```
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
dest_file <- "coursera-swiftkey.zip"

if (!file.exists(dest_file)) {
    download.file(url, dest_file)
	files <- c("final/en_US/en_US.twitter.txt", "final/en_US/en_US.news.txt","final/en_US/en_US.blogs.txt") 
    unzip(dest_file, files=files, exdir="en_US", overwrite=TRUE, junkpaths=TRUE)  
}
```

#### Online Prediction

https://ahnugent.shinyapps.io/Wordcaster/
