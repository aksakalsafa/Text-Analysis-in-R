##### GRADED ACTIVITY N3
#Text Analytics
#DEADLINE: 21-01-2022, 23.59

#Your ability to performn a correct C-SAT analysis will be evaluated, as well as your ability to identify valuable insights from text analytics techniques.


##We are going to work with data from itunes so we can both analyse the customer satisfaction regarding some app and zoom into the comments to try to understand why the app has been rating this way.

#The data set contains 2.627 reviews from Zoom app from multiple countries (remember there is a limit of 500 reviews that can be pulled for each country)

#Step 1: load the libraries you will need for now: tidyverse, syuzhet
library(tidyverse)
library(syuzhet)
library(tidytext)
#Step 2: Make sure the date column has the correct type and also transform the column review to lower case.
#Explore the dataframe and mentioned the highlights in at least 5 lines
#Visualizing the numbers of reviews per day, identify the day with most reviews created
# Let's see the dataset at first
View(reviews_zoomapp)
# let's transform the date column to the correct type and the reviews to lower case.
reviews_zoomapp<-reviews_zoomapp%>%
  mutate(review_time=as.Date(review_time))%>%
  mutate(review=str_to_lower(review))
# Dataset info
str(reviews_zoomapp)
# summary of the data
summary(reviews_zoomapp[-1])
# Let's how many days we are working on
max(reviews_zoomapp$review_time)-min(reviews_zoomapp$review_time)
# Ploting the number of reviews per date
ggplot(mapping = aes(x = as.Date(reviews_zoomapp$review_time)))+
  geom_histogram(binwidth = 20,color='blue',fill='black')+
  ggtitle('Number of reviews per day')+
  labs(x='Date',y='Number of reviews')
# Date with the most frequent reviews
cat('The date with most frequent review is : ',tail(names(sort(table(reviews_zoomapp$review_time))),1),' With a number of reviews : ',max(table(reviews_zoomapp$review_time)))
# Comments :
# - After viewing the dataframe as a first step we saw that the review_time column was in datetime format
#   it's better to have transformed to date format which make it more significant for analysis
# - Summary function help us to have an overall idea about the data with ratings from 1 to 5
#   and with an interesting information that Median which represent 50% of the population is 5
#   this give us an idea about the overall satisfaction of our customers but still we can't ignore
#   that the first quantile is 1 which represent an important number bad reviews that can't be ignored.
# - The range of our study is 2224 days which is a large period with 02-12-2021 is the highest number
#   number reviews. This can probably reflect an import issue about the application in this specific.
#   it could be related to an update or an issue in the system that specific day.

#Step 3: Perform a cleaning over the review column to prepare the data to apply "one-token-per-row" format 
#Consider the need to eliminate puntuation marks, numbers, stop words etc 
#Hint: use the filter() and str_detect() functions along with regex and stop_words() 
#You can also use udpipe to make sure you're only working with relevant words like adjectives, verbs, etc.

# Check for Nan values in reviews and rating column
is.nan(reviews_zoomapp$review)%>%table()
is.nan(reviews_zoomapp$rating)%>%table()
# Let's check separately the reviews with punctuation, with digits and with Non ASCII keywords before start cleaning
# deleting non-ASCII words will help eliminate emojies and reviews in other languages such as (Arabic, Russian, Chinese...) keep mostly English words
punctdf <-reviews_zoomapp%>%filter(str_detect(review,"[[:punct:]]"))
numbdf <-reviews_zoomapp%>%filter(str_detect(review,"[[:digit:]]"))
nonasci<-reviews_zoomapp%>%filter(str_detect(review,"[[^\x01-\x7F]]"))
# After reviewing these datasets, I found there can be some valuable information for
# sentiment analysis with digits which could help us for ngrams analysis.

# Let's import the stop_words data and concatenate it into long character separated with \\b and the | or operator
# in order to perform the str_replace with with the given regex ('\\bword1\\b|\\bword2\\b|\\b...')
data(stop_words)
stopwords_regex = paste(stop_words$word, collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

# Let's create two data separate dataset one with digits (excluding some values such 4 stars...)
# the first dataset will be reviews_zoomapp_wdig containing digits and excluding punctuation and non-ASCII words
# the second dataset will be review_zoomapp1 without numbers, punctuation and non-ASCII words.
reviews_zoomapp_wdig<-reviews_zoomapp%>%
  mutate(review=str_replace_all(review, stopwords_regex, ''))%>%
  mutate(review=str_replace_all(review, '[[:punct:]]', ' '))%>%
  mutate(review=str_replace_all(review, '[[^\x01-\x7F]]', ''))%>%
  mutate(review=str_replace_all(review, '[[:digit:]] stars ', ' '))%>%
  mutate(review=str_replace_all(review, '[[:digit:]] star ', ' '))%>%
  mutate(review=str_replace_all(review,'zoom',''))
# Let's check if there's some empty cells after replacing many words and delete them
empty<-reviews_zoomapp_wdig%>%filter(str_detect(review,'^\\s$'))
reviews_zoomapp_wdig<-reviews_zoomapp_wdig%>%filter(!str_detect(review,'^\\s$'))

# Second dataset
reviews_zoomapp1<-reviews_zoomapp%>%
  mutate(review=str_replace_all(review, stopwords_regex, ''))%>%
  mutate(review=str_replace_all(review, '[[:punct:]]', ' '))%>%
  mutate(review=str_replace_all(review, '[[^\x01-\x7F]]', ''))%>%
  mutate(review=str_replace_all(review, '[[:digit:]]', ' '))%>%
  mutate(review=str_replace_all(review,'zoom',''))

reviews_zoomapp1%>%filter(str_detect(review,'^\\s$'))
reviews_zoomapp1<-reviews_zoomapp1%>%filter(!str_detect(review,'^\\s$'))

# Let's check if we are working with relevant words using Udpipe
library(udpipe)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(file= "english-ewt-ud-2.5-191206.udpipe")
reviews_ud <- udpipe_annotate(ud_model, x = reviews_zoomapp1$review, tagger = "default", parser = "none")
reviews_ud <- as.data.frame(reviews_ud)
table(reviews_ud$upos)
# We see that the most dominant upos are Nouns then Adjectives and verbs

#Step 4: apply "one-token-per-row" format making sure the list of words are all relevant 
#User plots to understand if you cleaning process is good enough
# Let's tokenize the dataset get all the monograms
revies_tokens <- reviews_zoomapp1 %>%
  unnest_tokens(word, review, drop = FALSE, token = "words") %>%
  count(word) %>%
  arrange(desc(n))
# Let's tokenize the reviews to get all bigrams for dataset with digits
revies_tokens1 <- reviews_zoomapp_wdig %>%
  unnest_tokens(word, review, drop = FALSE, token = "ngrams",n=2) %>%
  count(word) %>%
  arrange(desc(n))
# Let's tokenize the reviews to get all trigrams for dataset with digits
revies_tokens2 <- reviews_zoomapp_wdig %>%
  unnest_tokens(word, review, drop = FALSE, token = "ngrams",n=3) %>%
  count(word) %>%
  arrange(desc(n))
# Let's check for nan values and clean the dataset
revies_tokens%>%filter(is.na(word))
revies_tokens1%>%filter(is.na(word))
revies_tokens2%>%filter(is.na(word))
revies_tokens1<-revies_tokens1%>%filter(!is.na(word))%>%filter(n>=4)
revies_tokens2<-revies_tokens2%>%filter(!is.na(word))%>%filter(n>=2)

# After eliminating Nan values let's concatenate the three results with the most relevent of bigrams and trigrams
revies_tokens<-rbind(revies_tokens,revies_tokens1,revies_tokens2)

# There some small characters that is gonna appear after tokenizing the dataset let's explore them and see if they are valuable
small_char<-revies_tokens%>%filter(nchar(word)<3)
revies_tokens<-revies_tokens%>%filter(!nchar(word)<3)

#We can use wordcloud package to visualize the result and quickly understand the relevance of the most important words
#install.packages("wordcloud")
library(wordcloud)
wordcloud(
  words = revies_tokens$word, 
  freq = revies_tokens$n, 
  max.words = 1000, #you can define how many words you want to see
  min.freq = 4, #the freq you set will depend on your dataset and the cleaning process you have apply
  scale =c(1.3,0.6), #you can leave this as default or change it to make the words bigger or smaller
  random.order = T, 
  rot.per = 0.3, random.color = T,
  color = brewer.pal(4, "BrBG"))



##Step 5: analyzed the sentiment for each word using the NRC method from SYUZHET package
##What can you tell about the overall sentiment, how many positive and negative expressions do we have?
#You can use prop.table to understand the distribution of each emotion after removing all the rows with all zeros
Sentiment_review <- unlist(revies_tokens)
Sentiment_review_NRC <- get_nrc_sentiment(char_v = Sentiment_review, language = "english")
Sentiment_review_NRC<-as.data.frame(cbind(revies_tokens, Sentiment_review_NRC))
# Delete the words with Zeros
Sentiment_review_NRC <- Sentiment_review_NRC[rowSums(Sentiment_review_NRC[-1])>0, ]
# Explore the results
summary(Sentiment_review_NRC)
# Let's calculate the number of words for each sentiment
colSums(Sentiment_review_NRC[-1])

#let's explore the negative reviews
neg<-Sentiment_review_NRC%>%filter(negative!=0)

prop.table(Sentiment_review_NRC[c("anger", "anticipation", "disgust", "fear",
                             "joy", "sadness", "surprise", "trust")])

# Comments :
# - By calculating the sum of words for each emotion we found that positive reviews with 562
#   dominate negative reviews which represent 388 word with top emotions on trust, joy and anticipation
# - From the negative reviews we see there are some words like 'bug', 'lag', 'error', 'freezing'... which
#   give us an idea that most of the negative reviews are about errors in the application.

##Perform a C-SAT for the Zoom app using the rating column and applying the C-SAT formula properly.
##The rating provided by the users can be predict by the feedback their offer to the developers? 
reviews_CSAT<-length(which(reviews_zoomapp1$rating >= 4))/length(reviews_zoomapp1$rating)
reviews_CSAT
# Comments :
# We have seen that from the sentiment analysis positive reviews dominates with 562 word compared
# negative reviews with 388 which can be translated to around 59% of the words are positive compared to negative one
# This number reflect the satisfaction of the customer because it's closer to C-SAT we have found

#Step 6: Lets performn a topic modelling to understand the main topics from the reviews
#The data frame "Zoom_topicmodel" is not fully cleaned, but is ready to perform the LDA and get relevant insights
library(topicmodels)
#Let's try to identify different topics with in the reviews
set.seed(1234)
Zoom_LDA <- LDA(Zoom_topicmodel, k = 3, control = list(seed = 1234))
Zoom_LDA2 <- LDA(Zoom_topicmodel, k = 4, control = list(seed = 1234))
Zoom_LDA3 <- LDA(Zoom_topicmodel, k = 5, control = list(seed = 1234))


#Transform each outcome into tidy data
Zoom_topics <- tidy(Zoom_LDA, matrix = "beta")
Zoom_topics2 <- tidy(Zoom_LDA2, matrix = "beta")
Zoom_topics3 <- tidy(Zoom_LDA3, matrix = "beta")


#And try to understand top terms of each model
##k=3 model
topics_top_terms <- Zoom_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zoom_topics_wordcloud <- Zoom_topics %>%
  filter(topic == "1") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud$term, 
  freq = Zoom_topics_wordcloud$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

###re-run Zoom_topics_wordcloud and wordcloud for each of the 3 topics inside the first LDA
# Topic 2
Zoom_topics_wordcloud_t2 <- Zoom_topics %>%
  filter(topic == "2") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud_t2$term, 
  freq = Zoom_topics_wordcloud_t2$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Topic 3
Zoom_topics_wordcloud_t3 <- Zoom_topics %>%
  filter(topic == "3") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud_t3$term, 
  freq = Zoom_topics_wordcloud_t3$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))
##k=4 model
topics_top_terms2 <- Zoom_topics2 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zoom_topics_wordcloud2 <- Zoom_topics2 %>%
  filter(topic == "1") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud2$term, 
  freq = Zoom_topics_wordcloud2$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

###re-run Zoom_topics_wordcloud and wordcloud for each of the 4 topics inside the second LDA
# topic 2
Zoom_topics_wordcloud2_t2 <- Zoom_topics2 %>%
  filter(topic == "2") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud2_t2$term, 
  freq = Zoom_topics_wordcloud2_t2$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Topic 3
Zoom_topics_wordcloud2_t3 <- Zoom_topics2 %>%
  filter(topic == "3") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud2_t3$term, 
  freq = Zoom_topics_wordcloud2_t3$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Topic 4
Zoom_topics_wordcloud2_t4 <- Zoom_topics2 %>%
  filter(topic == "4") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud2_t4$term, 
  freq = Zoom_topics_wordcloud2_t4$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))
##k=5 model
topics_top_terms3 <- Zoom_topics3 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


Zoom_topics_wordcloud3 <- Zoom_topics3 %>%
  filter(topic == "1") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud3$term, 
  freq = Zoom_topics_wordcloud3$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

###re-run Zoom_topics_wordcloud and wordcloud for each of the 5 topics inside the last LDA
# Topic 2
Zoom_topics_wordcloud3_t2 <- Zoom_topics3 %>%
  filter(topic == "2") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud3_t2$term, 
  freq = Zoom_topics_wordcloud3_t2$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Topic 3
Zoom_topics_wordcloud3_t3 <- Zoom_topics3 %>%
  filter(topic == "3") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud3_t3$term, 
  freq = Zoom_topics_wordcloud3_t3$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Topic 4
Zoom_topics_wordcloud3_t4 <- Zoom_topics3 %>%
  filter(topic == "4") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud3_t4$term, 
  freq = Zoom_topics_wordcloud3_t4$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Topic 5
Zoom_topics_wordcloud3_t5 <- Zoom_topics3 %>%
  filter(topic == "5") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zoom_topics_wordcloud3_t5$term, 
  freq = Zoom_topics_wordcloud3_t5$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

#Taking information from all the LDA performed, What can you tell about the reviews from the topic perspective? #Comment the highlights in more than 5 lines
# Comment :
# - From hyperparameters perspective we see that the 'alpha' decrease from the model with k=3 to k=5
#   (Cluster 1 29, Cluster 2 27,Cluster 3 24) which means that the distribution of words are likely to be present in each documents as reflected
#   in the wordcloud where we see similar words for each topic. Similarly the 'beta' decrease the same way
#   from k=3 to k=5. To sum up, the higher alpha is means that a specific topic distribution is more likely for each document
#   high beta-values means each topic is more likely to contain a specific word mix. In this case the cluster k=5 has better result than the others
# - From the wordcloud, we have seen that most of the topics are relatively positive except some of them having words like 'fix', 'log', 'sign'...
#   describing other topic related maybe to problems to sign that should be fixed.


#Step 7:
##After all your analysis, what would you recommend to the product owner in order to improve the app? 
# - Based on  C-SAT, 61% normally it's good score for customer satisfaction. However we can't be ignoring
#   that it's close to the the half of the customers.
# - From the sentiment we have learned that most of the negative words are mostly related to errors and bugs
#   that could be related to the application. As for the the topic modeling we have found that in some topics
#   the appearance of the word 'fix' along with 'sign' and 'access', which could reflect a problem in accessing meetings or logins
# - Even with the first wordcloud with the overall reviews we have seen many keywords around email password
# --> What I would recommend to the product owner is to check errors in the process of joining the meeting (Login->attend meeting)
#     and it would be much better make this process more user-friendly. 


#END



