library(rvest)

# Function to extract reviews from a given URL
reviews_airline<- function(url) {
  # Read the HTML page
  page <- read_html(url)
  
  # Extract review information
  title_airline <- page %>% html_nodes(".review-title.span") %>%
    html_text()
  
  text_airline <- page %>% html_nodes(".text_content") %>%
    html_text()
  
  # Ensure both vectors have the same length
  
  # Create a data frame
  airline_reviewsdf<- data.frame(
    Review_Title = title_airline[1:10],
    Text_Review = text_airline[1:10]
  )
  
  return(airline_reviewsdf)
}

# Specify the base URL
airline_url<- "https://www.airlinequality.com/airline-reviews/pegasus-airlines/page/"

# Initialize an empty data frame to store all reviews
all_airline_reviewsdf <- data.frame()

# Loop through pages 1 to 37
for (page_number in 1:37) {
  # Construct the URL for the current page
  url <- paste0(airline_url, page_number, "/")
  
  # Extract reviews from the current page
  airline_page<- reviews_airline(url)
  
  # Combine the reviews with the existing data frame
  all_airline_reviewsdf<- rbind(all_airline_reviewsdf, airline_page)
}

# View the combined data frame
View(all_airline_reviewsdf)

pegasus_airline_review <- all_airline_reviewsdf[1:300, ]
View(pegasus_airline_review)

pegasus_airline_review<-pegasus_airline_review[-1]
View(pegasus_airline_review)

#CLEANING
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(RColorBrewer)
library(stringr)

clean1<-str_replace_all(pegasus_airline_review$Text_Review, "✅ Trip Verified |", "")
head(clean1)
View(clean1)
clean1<-gsub("Not Verified ","",clean1)
clean1<- gsub("\\|", "", clean1)
clean1<-gsub("\r\n", "", clean1)
clean1<-gsub("\r\n\r\n", "", clean1)
head(clean1)
#cleaning the punctuations and digits
clean1=gsub("[[:punct:]]", "", clean1)
clean1=gsub("[[:digit:]]", "", clean1)
head(clean1)
#spaces
clean1<-str_replace_all(clean1, " ", " ")
tail(clean1)

wordCorpus<-Corpus(VectorSource(clean1))
wordCorpus[[1]]$content

wordCorpus<-tm_map(wordCorpus, removePunctuation)
wordCorpus[[1]]$content

wordCorpus<-tm_map(wordCorpus, removeNumbers)

wordCorpus<-tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus[[1]]$content

wordCorpus<-tm_map(wordCorpus, removeWords, stopwords("SMART"))
wordCorpus$content[1:300]

#Manual removing of words
wordCorpus[[1]]$content
wordCorpus<-tm_map(wordCorpus, removeWords, c("unprofessiona","personel","Im","justifible" , "BolPoint", "DONT" , "Pegasuss" ,"damn", "canceled","Tto", "didnt", "Sabiha", "dayvacationphysical", "E" , "st", "Verified", "Review","kg","tix", "cc", "flypgscom", "exellent", "offerd", "moaned", "fromto", "timesand", "cmin"))
head(wordCorpus$content)

#removing whitespaces
wordCorpus<-tm_map(wordCorpus, stripWhitespace)

wordCorpus$content[1:10]

#plotting Wordcloud
set.seed(1234)
wordcloud(words=wordCorpus, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.50, colors = brewer.pal(10, "Set1"))

pega_airlines<-data.frame(text=sapply(wordCorpus, as.character), stringsAsFactors = FALSE)

#Using "syuzhet" package
pega_sentiments<-get_sentiment(pega_airlines$text, method="syuzhet")

pega_finaldf<-cbind(pega_airlines, pega_sentiments)

#encodeSentiments function
encodeSentiment <- function(x) { 
  if(x <= -0.5){ 
    "1) very negative" 
  }else if(x > -0.5 & x < 0){ 
    "2) negative" 
  }else if(x > 0 & x < 0.5){ 
    "4) positive" 
  }else if(x >= 0.5){ 
    "5) very positive" 
  }else { 
    "3) neutral" 
  } 
}  

#positive and negative sentiments
pega_finaldf$pega_Sentiment<-sapply(pega_finaldf$pega_sentiments,encodeSentiment)

#plotting the (posi to nega)
library(dplyr)
count_sentiment<-pega_finaldf%>%
  count(pega_Sentiment)

count_sentiment

ggplot(pega_finaldf, aes(pega_Sentiment, fill=pega_Sentiment))+geom_bar()+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Number of reviews")+ggtitle("Reviews by Sentiment")

#changing columnnames
colnames(pega_finaldf)<-c("Reviews", "Reviews' Score", "Sentiments")
View(pega_finaldf)

