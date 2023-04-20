# Install
install.packages("tm")  # for text miningr
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#text <- readLines(file.choose())

filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)


# Load the data as a corpus
docs <- Corpus(VectorSource(text))


inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)
# In this step and onwards, we are exploring the frequency terms along with their distributions in the text document matrix.
# Using the function 'findFreqTerms', we want to find out those words that have occurred atleast 4 times in the speech.
# We will be provided with a list of words using this code.

findAssocs(dtm, terms = "freedom", corlimit = 0.3)
# After finding out those words which have a frequency of atleast 4 in the data set, 
# we want to analyze the association between these words. Using the function 'findAssocs()', 
# we can find out those terms which correlate/associates with the text/word fed into the parameter 'terms'.
# The code above will provide us with the frequent terms which associates with the word 'freedom' in the speech "I have a dream" with a lower correlation value of 0.3.

head(d, 10)


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
# The bar plot has been used here to plot the frequency of the first 10 frequent words.
# The word 'will' has the highest frequency of 17 which the bar plot is unable to signify due to scaling issues in the code.
# The next frequent terms are 'freedom' and 'ring' with the frequency of 13 and 12 respectively.
# We can also analyse that all the top 10 frequent words in the speech of Martin Luther King have alteast a frequency of 7.







