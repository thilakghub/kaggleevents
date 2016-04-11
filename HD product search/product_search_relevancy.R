
#https://www.kaggle.com/c/home-depot-product-search-relevance
#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#stage-the-data
#https://deltadna.com/blog/text-mining-in-r-for-term-frequency/
#http://stackoverflow.com/questions/24703920/r-tm-package-vcorpus-error-in-converting-corpus-to-data-frame
#http://www.joyofdata.de/blog/comparison-of-string-distance-algorithms/
#http://stackoverflow.com/questions/28653867/best-way-to-transpose-data-table
#http://stackoverflow.com/questions/21977353/get-the-min-of-two-columns
#http://stackoverflow.com/questions/10654723/how-to-plot-min-and-max-of-two-columns-per-row-as-a-scatterplot
#http://stackoverflow.com/questions/11134812/how-to-find-the-length-of-a-string-in-r

#cosine similarity:
#https://blog.nishtahir.com/2015/09/19/fuzzy-string-matching-using-cosine-similarity/
#http://stackoverflow.com/questions/1746501/can-someone-give-an-example-of-cosine-similarity-in-a-very-simple-graphical-wa

#http://alias-i.com/lingpipe/demos/tutorial/stringCompare/read-me.html

#openNLP:
#http://stackoverflow.com/questions/4610974/extracting-adjnounadjnounnoun-prepadjnounnoun-from-text-jus
#http://www.martinschweinberger.de/blog/part-of-speech-tagging-with-r/
#java
#http://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/
#http://stackoverflow.com/questions/28764056/could-not-find-function-tagpos

#grepl for finding a string in another
#http://stackoverflow.com/questions/30854434/test-two-columns-of-strings-for-match-row-wise-in-r
#regepxr to locate the string

#issues with corpus
#http://stackoverflow.com/questions/25069798/r-tm-in-mclapplycontentx-fun-all-scheduled-cores-encountered-errors
#PlainTextDocument --- not useful --- http://stackoverflow.com/questions/24771165/r-project-no-applicable-method-for-meta-applied-to-an-object-of-class-charact
#http://stackoverflow.com/questions/18287981/tm-map-has-parallelmclapply-error-in-r-3-0-1-on-mac

#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)

library(data.table)
library(tm)
library(stringdist)
library(reshape)
library(SnowballC)
library(openNLP)
library(NLP)
# library(ggplot2)

setwd("/Users/z001t72/Documents/General/Kaggle/HD product search/")

train <- fread("train.csv")
prodAttr <- fread("attributes.csv")
prodDesc <- fread("product_descriptions.csv")
#str(train)

dataMan <- function(dataVar,numTreat){
  review_source <- VectorSource(dataVar)
  corpus <- Corpus(review_source)
  corpus <- tm_map(corpus, stemDocument, lazy = TRUE)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #stopwords("english")
  corpus <- tm_map(corpus, removeWords, c("na"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  if(numTreat == 1){
    corpus <- tm_map(corpus, removeNumbers)
    # print("fucked the numbers!")
    #corpus <- tm_map(corpus, removeWords, c(" x "))
  }

  dataVar <- data.table(text=unlist(sapply(corpus, `[`, "content")))
  return(dataVar)
}

train$search_term.n <- dataMan(train$search_term,0)
train$product_title.n <- dataMan(train$product_title,0)

train$search_term.n1 <- dataMan(train$search_term,1)
train$product_title.n1 <- dataMan(train$product_title,1)

#train$label <- stringdist(train$search_term, train$product_title, method = "cosine")
#temp <- train[is.na(train$label) == FALSE & is.infinite(train$label) == FALSE]
#print(cor(temp$relevance,temp$label))

#rm(temp)
#osa        -0.04624228
#lv         -0.0462244
#dl         -0.04619295
#hamming    -0.00739031 not applying; mostly Inf
#lcs        -0.09707424
#qgram      -0.03798784
#cosine     -0.1613202
#jaccard    -0.1231206
#jw         -0.1294172

#temp <- prodAttr[grep("Bullet", prodAttr$name), ]
#more than 12 Bullet12 available for less than 10% of instances

temp <- prodAttr[prodAttr$name %in% c("Bullet01", "Bullet02", "Bullet03", "Bullet04", 
                             "Bullet05", "Bullet06", "Bullet07", "Bullet08", 
                             "Bullet09", "Bullet10", "Bullet11", "Bullet12")]

prodAttr.TRbullet <- cast(temp, product_uid ~ name)
rm(temp)

prodAttr.TRbullet$Bullet <- paste(prodAttr.TRbullet$Bullet01, prodAttr.TRbullet$Bullet02,
                                  prodAttr.TRbullet$Bullet03, prodAttr.TRbullet$Bullet04,
                                  prodAttr.TRbullet$Bullet05, prodAttr.TRbullet$Bullet06,
                                  prodAttr.TRbullet$Bullet07, prodAttr.TRbullet$Bullet08,
                                  prodAttr.TRbullet$Bullet09, prodAttr.TRbullet$Bullet10,
                                  prodAttr.TRbullet$Bullet11, prodAttr.TRbullet$Bullet12,
                     sep = " ")
prodAttr.TRbullet <- prodAttr.TRbullet[,c("product_uid","Bullet")]
#prodAttr.TRbullet$Bullet <- dataMan(prodAttr.TRbullet$Bullet)

train <- merge(train, prodAttr.TRbullet, by= c("product_uid"), all.x = TRUE)
train$Bullet.n <- dataMan(train$Bullet,0)
train$Bullet.n1 <- dataMan(train$Bullet,1)

train$RS1 <- stringdist(train$search_term.n, train$product_title.n, method = "cosine")
train$RS2 <- stringdist(train$search_term.n, train$Bullet.n, method = "cosine")
train$RS3 <- stringdist(train$search_term.n1, train$product_title.n1, method = "cosine")
train$RS4 <- stringdist(train$search_term.n1, train$Bullet.n1, method = "cosine")
train <- transform(train, RS = pmin(RS1, RS2, RS3, RS4, na.rm = TRUE))

train <- train[, SP := grepl(gsub(" ","",search_term.n, fixed = TRUE),
                               gsub(" ","",product_title.n, fixed = TRUE)) + 0,
               by = search_term.n]

train$STlen = nchar(train$search_term.n1)

write.csv(train, file = "train_features.csv")


#### random checks
temp <- train[is.na(train$RS) == FALSE & is.infinite(train$RS) == FALSE & train$RS]
temp$STlen <- nchar(temp$search_term.n1)
temp$PTlen <- nchar(temp$product_title)
temp$PTSTlen <- (temp$PTlen - temp$STlen) / temp$PTlen
temp <- temp[temp$PTSTlen > -0.5 & temp$PTSTlen < 0.5]
print(cor(temp$relevance,temp$RS))
#qplot(temp$relevance, temp$RS)
#test <- temp[,.(RSmean=mean.default(RS)), by=.(relevance)]

t <- temp[temp$RS > 0.3 & temp$relevance > 2.5]
t1 <- t[t$SP > 0]

t2 <- temp[temp$SP > 0 & temp$relevance < 2]
#write.csv(t, file = "C:/General/Kaggle/HD product search/check-tp.csv")
