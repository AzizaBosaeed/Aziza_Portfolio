x <- training_file_KAUST$Tweet_id
y <- training_file_KAUST$sentiment
y_tweets <- training_file_KAUST_tweets$sentiment

#add sentiment of tweets to training_file_KAUST_tweets
count <- 1
for (val in x) {
  if(val %in% y_tweets) {
    training_file_KAUST_tweets$sentiment[y_tweets == val] <- y[count]
    print(count)
    print(val)
    print(y[count])
    }
    
  count = count+1
}
print(count)


#remove links (http elements) 
training_file_KAUST_tweets$Tweet <- gsub("http\\S+\\s*", "", training_file_KAUST_tweets$Tweet)
#Remove hashtags
training_file_KAUST_tweets$Tweet <- gsub("#\\S+", " ", training_file_KAUST_tweets$Tweet)
#Remove mentions
training_file_KAUST_tweets$Tweet <- gsub("@\\S+", " ", training_file_KAUST_tweets$Tweet)
#remove all non Arabic letters
training_file_KAUST_tweets$Tweet <- cleanChars(training_file_KAUST_tweets$Tweet)
training_file_KAUST_tweets$Tweet <- cleanLatinChars(training_file_KAUST_tweets$Tweet)
#remove Arabic numbers
training_file_KAUST_tweets$Tweet <- removeArabicNumbers(training_file_KAUST_tweets$Tweet)
#remove Diacritics (tashkeel)
training_file_KAUST_tweets$Tweet <- removeDiacritics(training_file_KAUST_tweets$Tweet)
#remove farsi numbers
training_file_KAUST_tweets$Tweet <- removeFarsiNumbers(training_file_KAUST_tweets$Tweet)
#remove all numbers
training_file_KAUST_tweets$Tweet <- gsub("[[:digit:]]+", "", training_file_KAUST_tweets$Tweet)
#remove punctuations
training_file_KAUST_tweets$Tweet <- gsub("[[:punct:]]", "", training_file_KAUST_tweets$Tweet)
#Remove extra whitespaces
training_file_KAUST_tweets$Tweet <- gsub(" +"," ", training_file_KAUST_tweets$Tweet)


#replace آ to ا and إ to ا and أ to ا and ئ to ي and ؤ to و and ة to ه
training_file_KAUST_tweets$Tweet <- gsub("أ", "ا", training_file_KAUST_tweets$Tweet)
training_file_KAUST_tweets$Tweet <- gsub("إ", "ا", training_file_KAUST_tweets$Tweet)
training_file_KAUST_tweets$Tweet <- gsub("آ", "ا", training_file_KAUST_tweets$Tweet)
training_file_KAUST_tweets$Tweet <- gsub("ئ", "ي", training_file_KAUST_tweets$Tweet)
training_file_KAUST_tweets$Tweet <- gsub("ؤ", "و",training_file_KAUST_tweets$Tweett)
training_file_KAUST_tweets$Tweet <- gsub("ة", "ه",training_file_KAUST_tweets$Tweett)

#keywords don't reduce it
words2keep <- c("الله", "اللهم", "والله", "لله", "اللهم")

#reduce multi letters to one letter except words2keep
tmp1 <- paste0('(*UCP)\\b(?:',paste(collapse='|',words2keep),')\\b(*SKIP)(*F)|(\\p{L})\\1+')
training_file_KAUST_tweets$Tweet <- gsub(tmp1, '\\1',training_file_KAUST_tweets$Tweet, perl=TRUE)

#Removing StopWords
install.packages("arabicStemR")
library(arabicStemR)

install.packages('tm')
library(tm)

install.packages('sp')
library(bitops)
library(stringi)
library(readr)
library(RColorBrewer)

#stopWords keywords
stopWords <- c( "ابتدا", "اجل", "اجمع", "اخ", "اخذ", "اذ", "اذا", "اف", "اقل", "اكثر", "الا", "التي", "الذي", "الذين", "اللي", "الورا", "الى", "اليك", "ام", "اما", "امام", "امامك", "امين", "ان", "انا", "انت", "انتم", "انتو", "انك", "انما", "اننا", "انه", "انها", "انهم", "انهما", "اني", "اه", "اها", "او", "اوه", "ايضا", "اين", "ايه", "اولا", "اي", "ايا", "اينما", "باتجاه", "بالرغم", "بان", "بانهم", "بجانب", "بحيث", "بدون", "بذلك", "بس", "بسبب", "بضع", "بعد", "بعض", "بفضل", "بك", "بكل", "بل", "بله", "بلى", "بما", "بماذا", "بمن", "بنا", "به", "بها", "بهم", "بي","بين", "بينكم", "بينما", "بينهم", "بينهما", "تبدل", "تجاه", "تحت", "تلقا", "تلك", "تلكم", "ثم", "جعل", "جميع", "حار", "حتى", "حسب", "حول", "حيث", "خلال", "دون", "دونك", "ذا", "ذات", "ذاك", "ذلك", "ذه", "ذي", "ذيك", "راح", "رب", "رجع", "رغم", "سبحان", "سوف", "سوى", "شبه", "صار", "صه", "عاد", "عامه", "عدا", "عسى", "عل", "علق", "على", "عليك", "عليكم", "علينا", "عليه", "عليها", "عما", "عن", "عنا", "عند", "عندما", "عندنا", "عنك", "عنه", "عنها", "عنهم", "عنهما", "غير", "فاذا", "فانا", "فانهم", "فقط", "فكل", "فلان", "فلم","فلما", "فما", "فمن", "فهذا", "فهل", "فهم", "فهو", "فهولا", "فو", "فوق", "في", "فيك", "فيكم", "فيم", "فيما", "فيه", "فيها", "فيهم", "قام", "قبل", "قد", "قط", "قلما", "كاد", "كان", "كانما", "كذا", "كذلك", "كل", "كلا", "كلما", "كم", "كما", "كي", "كيف", "كيفما", "لا", "لان", "لانه", "لانها", "لانهم", "لدى", "لذا", "لذلك", "لست", "لعل", "لعمر", "لقد", "لك", "لكل", "لكم", "لكما", "لكن", "لكنما", "لكنه", "لكنها", "لكي", "لم", "لما", "لماذا", "لمن", "لن", "لنا", "له", "لها", "لهذا", "لهم", "لو", "لولا", "لي", "ليت", "ليس", "ليست", "لين", "ما", "مادام", "ماذا", "مازال","متى", "مثل", "مذ", "مع", "معه", "معها", "معهم", "مكانك", "مما", "ممن", "من", "منا", "منذ", "منك", "منكم", "منهم", "منهما", "مهما", "نحن", "نحو", "نعم", "نفس", "ها", "هاك", "هذا", "هذان", "هذه", "هذولا", "هذي", "هكذا", "هل", "هلا", "هم", "هما", "هن", "هنا", "هناك", "هو", "هولا", "هي", "هيا", "و", "وا", "واذ", "واذا", "وان", "واها", "ورا", "وراك", "وكم", "ولا", "ولسوف", "ولكن", "ولم", "ولو", "وما", "ومن", "وهل", "وهم", "وي", "يا", "وفي", "وهو", "ي", "مافيه", "مافي", "وش", "شي", "ياخي", "هاذا", "وه", "هنا", "ماعدا", "ما عدا", "وقد",
  "وكم", "تراك", "ياخو", "خبر", "مو", "مش")
stopWords <- data.frame(stopWords)
#remove stopwords
training_file_KAUST_tweets$Tweet <-  removeWords(training_file_KAUST_tweets$Tweet, stopWords$stopWords)

#remove any one letter alone
training_file_KAUST_tweets$Tweet <- gsub(" *\\b[ا | ب | ت | ث | ج | ح | خ | د | ذ | ر | ز | س | ش | ص | ض | ط | ظ | ع | غ | ف | ق | ك | ل | م | ن | ه | و | ي | ى | ء]{1}\\b *", " ", training_file_KAUST_tweets$Tweet)
#remove newlines
training_file_KAUST_tweets$Tweet <- gsub("\\n", " ", training_file_KAUST_tweets$Tweet)
#remove tabs
training_file_KAUST_tweets$Tweet <- gsub("\\t", " ", training_file_KAUST_tweets$Tweet)

#reduce spaces to one space
training_file_KAUST_tweets$Tweet <- gsub("([[:space:]])\\1+", "\\1", training_file_KAUST_tweets$Tweet)
#remove begin and end spaces
training_file_KAUST_tweets$Tweet <- gsub("^\\s+|\\s+$", "", training_file_KAUST_tweets$Tweet)

#delete duplicated rows
training_file_KAUST_tweets <- training_file_KAUST_tweets[!duplicated(training_file_KAUST_tweets$Tweet),]

#delete empty rows
training_file_KAUST_tweets <- training_file_KAUST_tweets[!grepl("\u009f", training_file_KAUST_tweets$Tweet),]
training_file_KAUST_tweets[training_file_KAUST_tweets == ""] <- NA

empty_tweet <- c("1182961832371326976",
                "1086398446708449281",
                "1242873107544670210",
                "1226417312464621571",
                "1221881156712456196",
                "1221803670959988736",
                "1221802822548754434",
                "1251837819158507520",
                "1221803639330746368",
                "1221812252170510336",
                "1221803720532549633",
                "1080081440849756160",
                "1080185435652018176",
                "1080425185587556352",
                "1143410482763771904",
                "1221776724226183169",
                "1221778696010391553",
                "1221787084643540994",
                "1221874307053301761",
                "1223395761192341504",
                "1223396345970679810",
                "1250114138065588226",
                "1224586891145760768",
                "1185860473356509184")
for (val in empty_tweet) {
  if(val %in% training_file_KAUST_tweets$Tweet_ID) {
    
    training_file_KAUST_tweets$Tweet[training_file_KAUST_tweets$Tweet_ID == val] <- NA
    
  }
  
}
training_file_KAUST_tweets <- training_file_KAUST_tweets[grepl("", training_file_KAUST_tweets$Tweet),]

#find Na's values			 
colSums(is.na(training_file_KAUST_tweets))

#save dataset after data preprocessing
write.csv(training_file_KAUST_tweets, file = "training_file_KAUST_tweets_after_preprocessing.csv")

#============== text vectorization manually ===============
#convert tweets to columns of one word by excel
#read data from excel file
training_file_KAUST_tweets_after_preprocessing_withOut_duplicate_space <- read_excel("training_file_KAUST_tweets_after_preprocessing_withOut_duplicate_space.xlsx", 
sheet = "training_file_KAUST_tweets_afte", col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
 
#Find Unique Words---

#find all features in this dataset (unique words from all tweets)
mydataSet<- training_file_KAUST_tweets_after_preprocessing_withOut_duplicate_space

tweets_words_vector <- c()

for (row in 1:52613){
  for(word in mydataSet[row,4:28]){
    if(!(word %in% tweets_words_vector)){
      tweets_words_vector<- c(tweets_words_vector, word)
    }    
  }
}

#unique(vector, incomparables = FALSE)
copy_tweets_words_vector <- as.data.frame(tweets_words_vector) 

#save all features (Unique Words) 
write.csv(copy_tweets_words_vector, file = "tweets_words_vector.csv")

#Features Selection---

#initialize array
tweets_words_vector_count <- array(dim =c(2,76959))
# copy tweets_words_vector
tweets_words_vector_count[1,1:76959] <- tweets_words_vector[1:76959]
#fill count row with 0
for(column in 1:76959){
  tweets_words_vector_count[2,column]<- 0
}

#count feature 
for (row in 1:52613){
  for(word in mydataSet[row,4:28]){
    column <- match(word,tweets_words_vector) 
    tweets_words_vector_count[2,column] <- as.numeric(tweets_words_vector_count[2,column])+1
  }
}


#transpose of a matrix
copy_tweets_words_vector_count<- as.data.frame(tweets_words_vector_count[1:2,1:76958]) 
copy_tweets_words_vector_count <- t(copy_tweets_words_vector_count)

#save all features with their counts
write.csv(copy_tweets_words_vector_count, file = "tweets_words_vector_with_count.csv")

#features is selected by excel (2000 features)
#read selected features from excel file
features_selection_2000f <- read_excel("features_selection_2000f.xlsx", sheet = "features_selection_2000f")

#initialize array
dataset <- array(dim =c(52614,2001))
dataset <- as.data.frame(dataset)

#set features name
for(feature in 1:2000){
  dataset[1,(feature + 1)]<- features_selection_2000f[feature,1]
}

#set tweets ID
for(tweetID in 1:52613){
  dataset[(tweetID + 1), 1]<- mydataSet[tweetID,2]
}

# I will work on only 15000 tweets, because the performance of my device is not high. 
#fill features value (1 or 0) to tweets. '1' if the word in the tweet and '0' if not. (only 15000 tweets) 
for(tweet in 1:15000){
  
  if((tweet%%10) == 0){
    print(tweet)
  }
  
  count <- 2
  for(feature in dataset[1,2:2001]){
    
    if(feature %in% mydataSet[tweet,5:29]){
      dataset[(tweet + 1), count]<- 1
      
    }else{
      dataset[(tweet + 1), count]<- 0
      
    }
    
    count <- count +1
  }
}

#save tweets with their features (only 15000 tweets) 
copy_mydataSetTo15000 <- dataset[2:15002,2:2002]
write.csv(copy_mydataSetTo15000, file = "copy_mydataSetTo15000.csv")

for(tweet in 3:15002){
  
  if((tweet%%10) == 0){
    print(tweet)
  }
  
  count <- 0
  for(feature in copy_mydataSetTo15000[tweet,3:2002]){
    
    count <- count + as.numeric(as.character(feature))
  }
  copy_mydataSetTo15000[tweet,2003]<- count
  
}

#save tweets with their features and counting number of features they have
write.csv(copy_mydataSetTo15000[2:15002,2:2003], file = "copy_mydataSet1To15000WithSum.csv")
copy_mydataSet1To15000WithSum <- copy_mydataSetTo15000[2:15002,2:2003]

#delete rows with no feature or NA
for(tweet in 15001:2){
  
  if((tweet%%10) == 0){
    print(tweet)
  }
  
  if(is.na(copy_mydataSet1To15000WithSum[tweet,2002]) || copy_mydataSet1To15000WithSum[tweet,2002] == 0){
    copy_mydataSet1To15000WithSum <- copy_mydataSet1To15000WithSum[-c(tweet),]
  }
  
}

#save tweets which have 1 or more features (12000 tweets)
write.csv(copy_mydataSet1To15000WithSum, file = "copy_mydataSet1To12000WithSum.csv")

#MydataSet---

#add labels to tweets
for(tweet in 2:12589){
  
  if((tweet%%10) == 0){
    print(tweet)
  }
  
  copy_mydataSet1To15000WithSum[tweet,2003]<- mydataSet$sentiment[mydataSet$Tweet_ID == copy_mydataSet1To15000WithSum[tweet,1]]
  
}

#save tweets With sentiment (12000 tweets)
write.csv(copy_mydataSet1To15000WithSum, file = "copy_mydataSet1To12000WithSentiment.csv")

#Training The Model---

# I will work on only 6902 tweets, because I have to work on balanced dataset (No. of 'Neutral' == No. of 'Positive' == No. of 'Negative'). 
#testing and training datasets
#5521 rows
training_dataset <- array(dim =c(5521,2003))
#1381 rows
testing_dataset <- array(dim =c(1381,2003))

training_dataset<- copy_mydataSet1To15000WithSum[1,1:2003]
testing_dataset<- copy_mydataSet1To15000WithSum[1,1:2003]

#initialize varibales
training_count <-1
testing_count <-1

neutral_count <-0 
positive_count <-0
negative_count <-0

neutral_training_count <-0
positive_training_count <-0
negative_training_count <-0


#split data into training and testing 4:1
for(tweet in 2:12589){
  
  if((tweet%%10) == 0){
    print(tweet)
  }
  if(training_count == 5521 && testing_count == 1381 ){
    print("Well done!")
    break
  }else{
    if(neutral_count == 2300 && copy_mydataSet1To15000WithSum[tweet,2003] == "Neutral"){
      print("(neutral tweets) well done!")
      
    }else if(positive_count == 2300 && copy_mydataSet1To15000WithSum[tweet,2003] == "Positive"){
      print("(positive tweets) well done!")
      
    }else if(negative_count == 2300 && copy_mydataSet1To15000WithSum[tweet,2003] == "Negative"){
      print("(negative tweets) well done!")
      
    }else{
      
      if(copy_mydataSet1To15000WithSum[tweet,2003] == "Neutral"){
        
        if((neutral_training_count / 4) == (neutral_count - neutral_training_count)){
          neutral_count <- neutral_count+1
          testing_count <- testing_count+1
          testing_dataset[testing_count,1:2003]<- copy_mydataSet1To15000WithSum[tweet,1:2003]
          
        }else{
          neutral_count <- neutral_count+1
          neutral_training_count <- neutral_training_count+1
          training_count <- training_count+1
          training_dataset[training_count,1:2003]<- copy_mydataSet1To15000WithSum[tweet,1:2003]
          
          
        }
        
      }else if(copy_mydataSet1To15000WithSum[tweet,2003] == "Positive"){
        if((positive_training_count / 4) == (positive_count - positive_training_count)){
          positive_count <- positive_count+1
          testing_count <- testing_count+1
          testing_dataset[testing_count,1:2003]<- copy_mydataSet1To15000WithSum[tweet,1:2003]
          
          
        }else{
          positive_count <- positive_count+1
          positive_training_count <- positive_training_count+1
          training_count <- training_count+1
          training_dataset[training_count,1:2003]<- copy_mydataSet1To15000WithSum[tweet,1:2003]
          
        }
      }else{
        if((negative_training_count / 4) == (negative_count - negative_training_count)){
          negative_count <- negative_count+1
          testing_count <- testing_count+1
          testing_dataset[testing_count,1:2003]<- copy_mydataSet1To15000WithSum[tweet,1:2003]
          
          
        }else{
          negative_count <- negative_count+1
          negative_training_count <- negative_training_count+1
          training_count <- training_count+1
          training_dataset[training_count,1:2003]<- copy_mydataSet1To15000WithSum[tweet,1:2003]
          
        }
      }
      
    }
    
  }
  
}

#save training and testing datasets
write.csv(training_dataset, file = "training_dataset5520.csv")
write.csv(testing_dataset, file = "testing_dataset1380.csv")

#classification using SVM 
install.packages("e1071")
library(e1071)

#building the model on training dataset
x <- training_dataset[1:5520, 2:2001]
y <- as.factor(training_dataset$V2003.1)#1>> positive, 2>> negative, 3>> neutral

model  =  svm(x,y, kernel = "radial", cost = 4, type="C-classification", gamma =0.125, scale = F)
summary(model)

#applying the model on training dataset 
pred_train = predict(model, x)  
table(y, pred_train) 

accuracy_train = sum(diag(table(y,pred_train)))/nrow(training_dataset) 
accuracy_train

#applying the model on testing dataset
x_test <- testing_dataset[1:1380, 2:2001]
y_test <- as.factor(testing_dataset$V2003.1)#1>> positive, 2>> negative, 3>> neutral

pred_test = predict(model, x_test)  
table(y_test, pred_test)

accuracy_test = sum(diag(table(y_test,pred_test)))/nrow(testing_dataset)
accuracy_test
