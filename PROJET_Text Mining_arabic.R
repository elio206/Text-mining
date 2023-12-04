## The following should work on Windows - first grab and save your existing locale
print(Sys.getlocale(category = "LC_CTYPE"))
original_ctype <- Sys.getlocale(category = "LC_CTYPE")
## Switch to the appropriate local for the script
Sys.setlocale("LC_CTYPE","arabic")


## Step (1)
# Select the desired text and save as text file with UTF-8 encoding
## Step (2)

# load required packages
library("tm")
library("arabicStemR")
library("wordcloud2")
library("NLP")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
 
## Step (3)
# read text with UTF-8 encoding
# to load the data into a corpus.
# Read the text file from local machine, choose file interactively
#arabic_text <- readLines(file.choose())
arabic_text<-readLines("arabic.txt", encoding="UTF-8")
# have a look !
arabic_text 
arabic_text = arabic_text[arabic_text!=""]
arabic_text
  
## start cleaning
arabic_text<-removePunctuation(arabic_text)
arabic_text<-removeNumbers(arabic_text)
arabic_text<-removeNewlineChars(arabic_text)
arabic_text<-stripWhitespace(arabic_text)


#tranform the text into data frame
#contains 2 columns (doc_id,Arabic_text)
myartxt = data.frame(arabic_text, stringsAsFactors = F)
View(myartxt)
myartxt$doc_id = c(1:nrow(myartxt))

#transform arabic text into french-arabic(par substitution)
#contains 3 columns (doc_id, text, Arabic_text)
myartxt$text = myartxt$arabic_text
myartxt = myartxt[,c(2,3,1)]
myartxt$text = transliterate(myartxt$text)
View(myartxt)

## Step (4) 
# create Arabic text corpus
str(myartxt)
arabic_corpus <- Corpus(DataframeSource(myartxt) )

# verify that the arabic words still the same after doing corpus
myextract<-data.frame(text = sapply(arabic_corpus, as.character), stringsAsFactors = FALSE)
myextract$text 

#transform our text into a matrix 
# Build a term-document matrix
arabic_tdm <- TermDocumentMatrix(arabic_corpus)
# visualize matrix(we can see frequency of each term in each row)
arabic_m <- as.matrix(arabic_tdm)
View(arabic_m )
#to see dimension of the matrix
#to see how many 0 and different than 0
dim(arabic_m)
sum((arabic_m==0))
sum((arabic_m!=0))
## get word freq, sum rows
arabic_v <- sort(rowSums(arabic_m),decreasing=TRUE)
arabic_v
## get word freq, sum columns
arabic_d <- data.frame(word = names(arabic_v),freq=arabic_v)
arabic_d
# Display the top 5 most frequent words
head(arabic_v, 5)
tail(arabic_v, 5)

# Plot the most frequent words
barplot(arabic_d[1:5,]$freq, las = 2, names.arg = arabic_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = arabic_d$word, freq = arabic_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
#Loading required package: grid
library(graph)
library(Rgraphviz)
freq.terms <- findFreqTerms(arabic_tdm, lowfreq = 5)
plot(arabic_tdm, term = freq.terms, corThreshold = 0.1, weighting = F)
##

#creating a vector
output_ar<-cbind(arabic_v)
head(output_ar)
tail(output_ar)
output_ar_df = as.data.frame(output_ar)
output_ar_df$arabic =  row.names(output_ar_df)
for (i in 1:nrow(output_ar_df)){output_ar_df$arabic_trans[i] =  reverse.transliterate(output_ar_df$arabic[i]) }

## step (5)
# write output in arabic
## Now you can write your text out and have it look as you would expect
#using fct "write.table" in order totransform the df into "table data matrix"

#this is the matrix containing 0 & 1 only
write.table(output_ar_df, "arabic_output.txt", quote = FALSE, col.names = FALSE, 
            row.names = T, sep = "\t", fileEncoding = "UTF-8")
# prepare tdm for writing
#this is the matrix containing more than  0 & 1 only
mytdm <- cbind(arabic_m, names(arabic_m))
write.table(mytdm, "termDocMatrix.txt", quote = FALSE, col.names = FALSE, 
            row.names = T, sep = "\t", fileEncoding = "UTF-8")
View(mytdm)







## Step(6) Wordcloud
## before this step we need to transliterate all the text
#function: transliterate it transforms from arabic to french-arabic
transliterate(myartxt$arabic_text)
n<-length(myartxt)

### step(7)
# create wordcloud
#step1: translate all the rabic text
text<-readLines("arabic.txt", encoding="UTF-8")
tttext<-transliterate(text)
#step2: transforming the text into df
myartxt = data.frame(tttext, stringsAsFactors = F)
myartxt$doc_id = c(1:nrow(myartxt))
myartxt$text = myartxt$tttext

myartxt = myartxt[,c(2,3,1)]

#step3:create the corpus without the terms that we don't want
tap.corpus <- Corpus(DataframeSource(myartxt))
tap.corpus<-tm_map(tap.corpus,content_transformer(tolower))
tap.corpus <- tm_map(tap.corpus, removeWords, c("wqd", "ama", "byn", "f7sb", 
                                                "fhw", "،" ))

#step4:transform the data into matrix to see freq of each term
tap.tdm <- TermDocumentMatrix(tap.corpus)
tap.m <- as.matrix(tap.tdm)
tap.v <- sort(rowSums(tap.m),decreasing=TRUE)
tap.d <- data.frame(word = names(tap.v),freq=tap.v)

#step5:visualize the most repeated
wordcloud2(data = tap.d)

## ...and don't forget to switch back
Sys.setlocale("LC_CTYPE", original_ctype)

