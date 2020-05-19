library(rtweet)
# plotting and pipes - tidyverse!

library(tidyverse)

library(ggplot2)

library(SentimentAnalysis)

library(dplyr)

library(tm)

library(sentimentr)

library(sjPlot)

library(dplyr)


iphone_tweets <- search_tweets(q = "#iphone", n = 1000, type="recent",
                               lang = "en",
                               include_rts = FALSE)


iphone_tweets_cleaned <- gsub("http.*","",  iphone_tweets$text)

iphone_tweets_cleaned <- gsub("https.*","",  iphone_tweets_cleaned)

iphone_tweets_cleaned <- gsub("t.co","",  iphone_tweets_cleaned)

iphone_tweets_cleaned <- gsub("rt","",  iphone_tweets_cleaned)

iphone_tweets_cleaned <- gsub("amp","",  iphone_tweets_cleaned)

iphone_sentiment<-sentiment_by(iphone_tweets_cleaned)

ggplot(as.data.frame(iphone_sentiment$ave_sentiment), aes(iphone_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 50))+
  labs(title = "iphone tweets polarity Histogram")



iphone_portrait_tweets<-iphone_tweets_cleaned[grep("portrait", iphone_tweets_cleaned, ignore.case = TRUE)]

iphone_portrait_sentiment<-sentiment_by(iphone_portrait_tweets)

ggplot(as.data.frame(iphone_portrait_sentiment$ave_sentiment), aes(iphone_portrait_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "iphone portrait sentiment polarity Score")



iphone_android_tweets<-iphone_tweets_cleaned[grep("android", iphone_tweets_cleaned, ignore.case = TRUE)]

iphone_android_sentiment<-sentiment_by(iphone_android_tweets)

ggplot(as.data.frame(iphone_android_sentiment$ave_sentiment), aes(iphone_android_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "iphone vs android sentiment polarity Score")



iphone_mobilesecurity_tweets<-iphone_tweets_cleaned[grep("mobilesecurity", iphone_tweets_cleaned, ignore.case = TRUE)]

iphone_mobilesecurity_sentiment<-sentiment_by(iphone_mobilesecurity_tweets)

ggplot(as.data.frame(iphone_mobilesecurity_sentiment$ave_sentiment), aes(iphone_mobilesecurity_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "iphone mobilesecurity sentiment polarity Score")



iphone11_tweets<-iphone_tweets_cleaned[grep("iPhone11", iphone_tweets_cleaned, ignore.case = TRUE)]

iphone11_sentiment<-sentiment_by(iphone11_tweets)

ggplot(as.data.frame(iphone11_sentiment$ave_sentiment), aes(iphone11_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "iphone11 sentiment polarity Score")


iphone12_tweets<-iphone_tweets_cleaned[grep("iPhone12", iphone_tweets_cleaned, ignore.case = TRUE)]

iphone12_sentiment<-sentiment_by(iphone12_tweets)

ggplot(as.data.frame(iphone12_sentiment$ave_sentiment), aes(iphone12_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "iphone12 sentiment polarity Score")



ipad_tweets<-iphone_tweets_cleaned[grep("ipad", iphone_tweets_cleaned, ignore.case = TRUE)]

ipad_sentiment<-sentiment_by(ipad_tweets)

ggplot(as.data.frame(ipad_sentiment$ave_sentiment), aes(ipad_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "ipad sentiment polarity Score")

iphone_negative_tweets<-c()

count <- 0
for (val in iphone_sentiment$ave_sentiment) 
{
  
  count<-count+1
  if(val < 0)  
  {
    iphone_negative_tweets<-c(iphone_negative_tweets,iphone_tweets_cleaned[count])
  }
}


iphone_negative_sentiment<-sentiment_by(iphone_negative_tweets)

ggplot(as.data.frame(iphone_negative_sentiment$ave_sentiment), aes(iphone_negative_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "Iphone Negative sentiment polarity Score")


iphone_android_negative_tweets<-iphone_negative_tweets[grep("android", iphone_negative_tweets, ignore.case = TRUE)]

iphone_android_negative_sentiment<-sentiment_by(iphone_android_negative_tweets)

ggplot(as.data.frame(iphone_android_negative_sentiment$ave_sentiment), aes(iphone_android_negative_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "Iphone vs Android Negative sentiment polarity Score")


iphone_camera_negative_tweets<-iphone_negative_tweets[grep("camera", iphone_negative_tweets, ignore.case = TRUE)]

iphone_camera_negative_sentiment<-sentiment_by(iphone_camera_negative_tweets)

ggplot(as.data.frame(iphone_camera_negative_sentiment$ave_sentiment), aes(iphone_camera_negative_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "Iphone Camera Negative sentiment polarity Score")



iphone_ios_negative_tweets<-iphone_negative_tweets[grep("ios", iphone_negative_tweets, ignore.case = TRUE)]

iphone_ios_negative_sentiment<-sentiment_by(iphone_ios_negative_tweets)

ggplot(as.data.frame(iphone_ios_negative_sentiment$ave_sentiment), aes(iphone_ios_negative_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "IOS Negative sentiment polarity Score")


iphone_covid19_negative_tweets<-iphone_negative_tweets[grep("Covid19", iphone_negative_tweets, ignore.case = TRUE)]

iphone_covid19_negative_sentiment<-sentiment_by(iphone_covid19_negative_tweets)

ggplot(as.data.frame(iphone_covid19_negative_sentiment$ave_sentiment), aes(iphone_covid19_negative_sentiment$ave_sentiment)) + geom_histogram(bins = 30,col="black",fill="red")+
  scale_x_continuous("Tweet Polarity Score", breaks = seq(-1,1,by = 0.1))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 2))+
  labs(title = "Iphone with Covid19 Negative sentiment polarity Score")




boxplot(iphone_sentiment$ave_sentiment, iphone_portrait_sentiment$ave_sentiment, iphone_android_sentiment$ave_sentiment, iphone_mobilesecurity_sentiment$ave_sentiment,
        iphone11_sentiment$ave_sentiment, iphone12_sentiment$ave_sentiment, ipad_sentiment$ave_sentiment, 
        main = "Multiple boxplots of Iphone, Iphone camera portrait mode, Iphone MobileSecurity, 
        Iphone vs Android, Iphone11,Iphone12, Ipad tweets Sentiments",
        names=c("iphone","iphone_portrait","iphone_android","iphone_mobilesecurity","iphone11","iphone12","ipad"),
        col = c("blue"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE,
        las=2,
        pars=list(par(mar=c(5,10,4,2)))
)


boxplot(iphone_negative_sentiment$ave_sentiment, iphone_android_negative_sentiment$ave_sentiment, 
        iphone_camera_negative_sentiment$ave_sentiment, iphone_ios_negative_sentiment$ave_sentiment,
        iphone_covid19_negative_sentiment$ave_sentiment, 
        main = "Multiple boxplots for Negative sentiments of Iphone, 
        Iphone vs android, Iphone Camera, Ios, Iphone and Covid19",
        names=c("iphone_negative","iphone_android","iphone_camera","iphone_ios","iphone_Covid19"),
        col = c("blue"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE,
        las=2,
        pars=list(par(mar=c(5,10,4,7)))
)


iphone_sentiment<-iphone_sentiment %>% mutate(Sentiment_Interval = cut(iphone_sentiment$ave_sentiment, c(-1,-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_sentiment_df<-data.frame(iphone_sentiment$Sentiment_Interval)

plot_likert(iphone_sentiment_df, reverse.scale = TRUE, title = "Likert plot for iphone sentiment polarity Score Distribution", 
            legend.title="Iphone sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone Sentiment Polarity Score Percentage"))


iphone_portrait_sentiment<-iphone_portrait_sentiment %>% mutate(Sentiment_Interval = cut(iphone_portrait_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_portrait_sentiment_df<-data.frame(iphone_portrait_sentiment$Sentiment_Interval)

plot_likert(iphone_portrait_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone Camera Portrait mode sentiment polarity Score Distribution", 
            legend.title="Iphone Camera Portrait mode sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone Camera Portrait mode sentiment Polarity Score Percentage"))

iphone_android_sentiment<-iphone_android_sentiment %>% mutate(Sentiment_Interval = cut(iphone_android_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_android_sentiment_df<-data.frame(iphone_android_sentiment$Sentiment_Interval)

plot_likert(iphone_android_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone vs Android sentiment polarity Score Distribution", 
            legend.title="Iphone vs Android sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone vs Android Sentiment Polarity Score Percentage"))

iphone_mobilesecurity_sentiment<-iphone_mobilesecurity_sentiment %>% mutate(Sentiment_Interval = cut(iphone_mobilesecurity_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_mobilesecurity_sentiment_df<-data.frame(iphone_mobilesecurity_sentiment$Sentiment_Interval)

plot_likert(iphone_mobilesecurity_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone Mobilesecurity sentiment polarity Score Distribution", 
            legend.title="Iphone Mobilesecurity sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone Mobilesecurity Sentiment Polarity Score Percentage"))

iphone11_sentiment<-iphone11_sentiment %>% mutate(Sentiment_Interval = cut(iphone11_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone11_sentiment_df<-data.frame(iphone11_sentiment$Sentiment_Interval)

plot_likert(iphone11_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone11 sentiment polarity Score Distribution", 
            legend.title="Iphone11 sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone11 Sentiment Polarity Score Percentage"))

iphone12_sentiment<-iphone12_sentiment %>% mutate(Sentiment_Interval = cut(iphone12_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone12_sentiment_df<-data.frame(iphone12_sentiment$Sentiment_Interval)

plot_likert(iphone12_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone12 sentiment polarity Score Distribution", 
            legend.title="Iphone12 sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone12 Sentiment Polarity Score Percentage"))


ipad_sentiment<-ipad_sentiment %>% mutate(Sentiment_Interval = cut(ipad_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


ipad_sentiment_df<-data.frame(ipad_sentiment$Sentiment_Interval)

plot_likert(ipad_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Ipad sentiment polarity Score Distribution", 
            legend.title="Ipad sentiment Polarity Score Intervals", 
            axis.titles=c("","Ipad Sentiment Polarity Score Percentage"))



iphone_android_negative_sentiment<-iphone_android_negative_sentiment %>% mutate(Sentiment_Interval = cut(iphone_android_negative_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_android_negative_sentiment_df<-data.frame(iphone_android_negative_sentiment$Sentiment_Interval)

plot_likert(iphone_android_negative_sentiment_df, reverse.scale = TRUE, title = "Likert plot for iphone vs android negative sentiment polarity Score Distribution", 
            legend.title="Iphone vs android negative sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone vs android negative Sentiment Polarity Score Percentage"))



iphone_camera_negative_sentiment<-iphone_camera_negative_sentiment %>% mutate(Sentiment_Interval = cut(iphone_camera_negative_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_camera_negative_sentiment_df<-data.frame(iphone_camera_negative_sentiment$Sentiment_Interval)

plot_likert(iphone_camera_negative_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone Camera negative sentiment polarity Score Distribution", 
            legend.title="iphone Camera negative sentiment Polarity Score Intervals", 
            axis.titles=c("","iphone Camera negative Sentiment Polarity Score Percentage"))



iphone_ios_negative_sentiment<-iphone_ios_negative_sentiment %>% mutate(Sentiment_Interval = cut(iphone_ios_negative_sentiment$ave_sentiment, c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), include.lowest=TRUE, right=FALSE))


iphone_ios_negative_sentiment_df<-data.frame(iphone_ios_negative_sentiment$Sentiment_Interval)

plot_likert(iphone_ios_negative_sentiment_df,  reverse.scale = TRUE, title = "Likert plot for Iphone ios negative sentiment polarity Score Distribution", 
            legend.title="Iphone ios negative sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone ios negative Sentiment Polarity Score Percentage"))



iphone_covid19_negative_sentiment<-iphone_covid19_negative_sentiment %>% mutate(Sentiment_Interval = cut(iphone_covid19_negative_sentiment$ave_sentiment, c(1, 0.75, 0.5, 0.25, 0, -0.25, -0.5, -0.75, -1)))


iphone_covid19_negative_sentiment_df<-data.frame(iphone_covid19_negative_sentiment$Sentiment_Interval)

plot_likert(iphone_covid19_negative_sentiment_df, reverse.scale = TRUE, title = "Likert plot for Iphone with Covid19 negative sentiment polarity Score Distribution", 
            legend.title="Iphone with Covid19 negative sentiment Polarity Score Intervals", 
            axis.titles=c("","Iphone with Covid19 negative Sentiment Polarity Score Percentage"))

