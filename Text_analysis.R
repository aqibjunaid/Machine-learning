install.packages("tm")
install.packages("lsa")
update.packages("tm",  checkBuilt = TRUE)
library(tm)
library(lsa)
# step 1: import and label records
# read zip file into a corpus
# this command is awesome, it makes it so you dont even have to unzip, yt it goes through EVERY interior directory
corp <- Corpus(ZipSource("/Users/aqibjunaid/Downloads/Machine Learning/text.zip",  
                         recursive = T))

#corp <- list.files(path=path1, pattern="*.txt")
# create an array of records labels
label <- c(rep(1, 1000), rep(0, 1000))
#####



# step 2: preprocess
# tokenization
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
# stopwords
install.packages("textclean")
library(textclean)
corp <- str_replace_all (corp, "\\[.*?\\]", "")


corp <- tm_map(corp, removeWords, stopwords("english"))
# stemming
install.packages("SnowballC")
library(SnowballC)
corp <- tm_map(corp, stemDocument)

# step 3: TF-IDF and latent semantic analysis
# compute TF-IDF
tdm <- TermDocumentMatrix(corp)
inspect(tdm)
tfidf <- weightTfIdf(tdm)
inspect(tfidf)
# extract (20 or select 10 and see how that effects our predictive performance) concepts
library(lsa)
# the next line of code is super cpu intensive
lsa.tfidf <- lsa(tfidf, dim = 10)
# convert to data frame
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))

# Step 4: predictive model
# sample 60% training data
training <- sample(c(1:2000), 0.6*2000)
# run logistic model on training
trainData = cbind(label = label[training], words.df[training,])
reg <- glm(label ~ ., data = trainData, family = 'binomial')
# compute accuracy on validation set
validData = cbind(label = label[-training], words.df[-training,])
pred <- predict(reg, newdata = validData, type = "response")

#Step 5
# produce confusion matrix
library(caret)
confusionMatrix(factor(ifelse(pred>0.5, 1, 0)), factor(label[-training]))
#Oh shit 96.88% accuracy in splitting the files based on terms