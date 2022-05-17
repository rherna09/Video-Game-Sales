#######################################
#Rafael Hernandez
#Video Game Analysis
######################################
#Libraries: ggplot2, ggmap, data.table, cowplot, maps, mapproj, lubridate, plotly
#Libraries cntd: hrbrthemes, gganimate, gapminder, babynames, ggthemes, gridExtra, 
#Continued: tidyr, plyr, tm, SnowballC, wordcloud, RColorBrewer, kernlab. 
install.packages("arules")
library(arules)
library(ggplot2)
library(ggmap)
install.packages("data.table")
library(data.table)
install.packages("cowplot")
library(cowplot)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("gganimate")
library(gganimate)
install.packages("gapminder")
library(gapminder)
install.packages("babynames")
library(babynames)
install.packages("ggthemes")
library(ggthemes)
install.packages("e1071")
library(e1071)
install.packages("gridExtra")
library(gridExtra)
library(tidyr)
library(plyr)
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
install.packages("kernlab")
library(kernlab)
library(plotly)

VideoGame<-(vgsales)
str(VideoGame)

VideoGame$Year<-as.Date(VideoGame$Year)

install.packages("lubridate")
library(lubridate)

VideoGame$Year<-as.Date(as.character(VideoGame$Year), format = "%Y")
VideoGame$Year<-year(VideoGame$Year)

#Removing the Rank Column
VideoGame$Rank <- NULL
summary(VideoGame)

#Filtering only the records of interest for this study, removing the records wth Year=nan and records with the eyar above 2016

VideoGame<- VideoGame[VideoGame$Year != "N/A" & VideoGame$Year != "2017" & VideoGame$Year != "2020", ]

VideoGame$Year <- factor(VideoGame$Year)
head(VideoGame, 6)

#Renaming columns and lower casing headings. 
setnames(VideoGame, old = c("Name", "Platform", "Year", "Genre", "Publisher", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"), new = c("name", "platform", "year", "genre", "publisher", "na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales"))
summary(VideoGame)

#Create new data table for frequency and percent.  
freq_year <- data.frame(cbind(Frequency = table(VideoGame$year), Percent = prop.table(table(VideoGame$year)) * 100))
freq_year<- freq_year[order(freq_year$Frequency, decreasing = TRUE), ]
freq_year


freq_name <- data.frame(cbind(Frequency = table(VideoGame$name), Percent = prop.table(table(VideoGame$name))* 100))
freq_name <- head(freq_name[order(freq_name$Frequency, decreasing = T), ], 5)
freq_name


freq_genre <- data.frame(cbind(Frequency = table(VideoGame$genre), Percent = prop.table(table(VideoGame$genre)) * 100))
freq_genre <-freq_genre[order(freq_genre$Frequency, decreasing = T), ]
freq_genre

setnames(freq_year, old = c("Frequency", "Percent"), new = c("frequency", "percent"))
freq_year

setnames(freq_genre, old = c("Frequency", "Percent"), new = c("frequency", "percent"))
freq_genre

setnames(freq_name, old = c("Frequency", "Percent"), new = c("frequency", "percent"))
freq_name

#Frequency Distribution of Genre Graph.  Code provided by Murilao on Kaggle.com

options(repr.plot.width = 14, repr.plot.height = 6)
ggplot(data = freq_genre, mapping = aes(x = frequency, y = row.names(freq_genre))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_genre), color = row.names(freq_genre)), aplha = .7,
           size = 1.1) +
          geom_label(mapping = aes(label = frequency), fill = "#B22222", size = 6, color = "white", fontface = "bold", hjust = .7)+
          ggtitle("Genre Frequency Distribution") + xlab(" ") + ylab("")+theme_ipsum()+coord_flip()+
          theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"), axis.title = element_text(size = 24, hjust = .5, face = "italic"),
                axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
                axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
                axis.text.x = element_text(size = 20, face = "bold", angle = 20),
                axis.text.y = element_text(size = 20, face = "bold"),
                legend.position = "none")

summary(VideoGame)

install.packages("plotly")
library(plotly)

fig<-plot_ly(
  x=c(VideoGame$genre),
  y=c(VideoGame$na_sales),
  name = "Genre Sales", 
  type = "bar"
)
fig

#Code provided by plotly.com
fig1<-plot_ly(VideoGame, x =~genre, y=~na_sales, type = 'bar', name = 'North America Sales')
fig1 <-fig1 %>% add_trace(y = ~eu_sales, name = 'Europe Sales')
fig1 <-fig1 %>% add_trace(y = ~jp_sales, name = 'Japan Sales')
fig1 <-fig1 %>% add_trace(y = ~other_sales, name = 'Other Markets')
fig1 <-fig1 %>% add_trace(y = ~global_sales, name = 'Global Sales')
fig1 <- fig1 %>% layout(yaxis =list(title = 'Sales'), barmode = 'group')
fig1

fig2<-plot_ly(VideoGame, x =~genre, y=~na_sales, type = 'bar', name = 'North America Sales')
fig2 <-fig2 %>% add_trace(y = ~eu_sales, name = 'Europe Sales')
fig2 <- fig2 %>% layout(yaxis =list(title = 'Sales'), barmode = 'group')
fig2

fig3<-plot_ly(VideoGame, x =~genre, y=~na_sales, type = 'bar', name = 'North America Sales')
fig3 <-fig3 %>% add_trace(y = ~jp_sales, name = 'Japan Sales')
fig3 <- fig3 %>% layout(yaxis =list(title = 'Sales'), barmode = 'group')
fig3


fig4<-plot_ly(VideoGame, x =~genre, y=~na_sales, type = 'bar', name = 'North America Sales')
fig4 <-fig4 %>% add_trace(y = ~other_sales, name = 'Other Markets')
fig4 <- fig4 %>% layout(yaxis =list(title = 'Sales'), barmode = 'group')
fig4

############################################################
#Mean Median and Mode
#Median code provided by Murilao via Kaggle.com
median_Df<-data.frame(Median = c(median(VideoGame$na_sales), median(VideoGame$eu_sales),
                                median(VideoGame$jp_sales), median(VideoGame$other_sales), 
                                median(VideoGame$global_sales)))

row.names(median_Df)<- c("na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales")
median_Df

#Mode code provided by Mrilao via Kaggle.com
mode_df<-data.frame(Mode = c(mode(VideoGame$na_sales), mode(VideoGame$eu_sales),
                               mode(VideoGame$jp_sales), mode(VideoGame$other_sales), 
                               mode(VideoGame$global_sales)))

row.names(mode_df)<- c("na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales")
mode_df


#Central Tendencies 
central_tend<-data.frame(means_df, median_Df, mode_df)
central_tend
row.names(central_tend)<-c("Mean", "Median", "Mode")

central_tend<-central_tend[,-3]
central_tend

options(repr.plot.width = 14, repr.plot.height = 6)
a <- ggplot(data = means_df, mapping = aes(x = Mean, y = row.names(means_df))) +
  geom_line(group = 1, size = 1.2, linetype = "dashed", color = "blue") +
  geom_point(size = 5, shape = 21, stroke = 1.5, mapping = aes(fill = row.names(means_df))) +
  theme_minimal() +
  ylab("") +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.position = "none")

b <- ggplot(data = means_df, mapping = aes(x = Mean, y = row.names(means_df))) +
  geom_line(group = 1, size = 1.2, linetype = "dashed", color = "blue") +
  geom_point(size = 5, stroke = 1.5, shape = 21, mapping = aes(fill = row.names(means_df))) +
  theme_minimal() +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(color = "white"),
        legend.text = element_text(size = 12, face = "bold"))

plot_grid(a, b + coord_polar(), nrow = 1, ncol = 2)





#Code provided by plotly
Sales_Means<- data.frame("Categorie"=rownames(means_df), means_df)
data<-Sales_Means[c('Categorie', 'Mean')]
fig5 <- plot_ly(data, labels = ~Categorie, values = ~Mean, type = 'pie')
fig5 <- fig5%>% layout(title = 'Video Game Sales by Market Means',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig5


# NA_Sales #code provided by murilao via Kaggle.com
t_v_name_NA <- aggregate(list(na_sales = VideoGame$na_sales), list(Name = VideoGame$name), sum)
t_v_name_NA <- t_v_name_NA[order(t_v_name_NA$na_sales, decreasing = T), ]
t_v_name_NA

t_v_name_EU<- aggregate(list(eu_sales= VideoGame$eu_sales), list(Name = VideoGame$name), sum)
t_v_name_EU<-t_v_name_EU[order(t_v_name_EU$eu_sales, decreasing = T),]
t_v_name_EU


#Code provided by plotly
fig6<- plot_ly(data= head(t_v_name_NA, 10), x=~Name, y =~na_sales, type = 'bar', name = 'North America Top')
fig6 <- fig6 %>% layout(yaxis =list(title = 'North America Sales in Millions')) 
fig6

fig9<- plot_ly(data=head(t_v_name_EU, 10), x=~Name, y =~eu_sales, type = 'bar', name = 'Europe Top')
fig9<- fig9 %>% layout(yaxis = list(title = 'Europe Sales in Millions'))
fig9

# Global_Sales code provided by murilao via kaggle.com
t_v_name_Global <- aggregate(list(global_sales = VideoGame$global_sales), list(Name = VideoGame$name), sum)
t_v_name_Global <- t_v_name_Global[order(t_v_name_Global$global_sales, decreasing = T), ]
t_v_name_Global

fig7<- plot_ly(data= head(t_v_name_Global, 10), x=~Name, y =~global_sales, type = 'bar', name = 'Global Top 10 Sales')
fig7 <- fig7 %>% layout(yaxis =list(title = 'Global Sales in Millions')) 
fig7



###########################################################
#Measures of dispersion
Sales_Variance <- data.frame(Variance = c(var(VideoGame$na_sales), var(VideoGame$eu_sales), var(VideoGame$jp_sales), var(VideoGame$other_sales), var(VideoGame$global_sales)))
row.names(Sales_Variance) <- c("na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales")
Sales_Variance

Sales_std <- data.frame(std = c(sqrt(var(VideoGame$na_sales)), sqrt(var(VideoGame$eu_sales)), sqrt(var(VideoGame$jp_sales)), sqrt(var(VideoGame$other_sales)), sqrt(var(VideoGame$global_sales))))
row.names(Sales_std) <- c("na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales")
Sales_std4

Sales_Dispersion <- data.frame(DM = Sales_Means$Mean, Variance = Sales_Variance$Variance, std = Sales_std$std)
row.names(Sales_Dispersion) <- c("na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales")
Sales_Dispersion



#################################################################
#Building and plotting linear models
#Plotting to find dependencies
plot(VideoGame$global_sales, VideoGame$na_sales)
plot(VideoGame$global_sales, VideoGame$eu_sales)
plot(VideoGame$global_sales, VideoGame$jp_sales)
plot(VideoGame$global_sales, VideoGame$other_sales)



plot(VideoGame$year, VideoGame$na_sales)
plot(VideoGame$year, VideoGame$eu_sales)
plot(VideoGame$year, VideoGame$jp_sales)
plot(VideoGame$year, VideoGame$other_sales)

#building linear models
model1<- lm(formula=na_sales~global_sales, data=VideoGame)
summary(model1)
plot(VideoGame$global_sales, VideoGame$na_sales)
abline(model1)
NorthAmerica_World<-ggplot(VideoGame, aes(x=global_sales, y=na_sales))+geom_point()+stat_smooth(method="lm", col = "red")
NorthAmerica_World

#adding year to the variable.  There appears to be no correlation between years and video game sales.  There is no slope.  There does appear to be a correlation between global sales and sales in indidividual markets.  How about sales between different markets?
model2<-lm(formula=global_sales~year, data=VideoGame)
model2
plot(VideoGame$year, VideoGame$global_sales)
abline(model2)
Year_Global<-ggplot(VideoGame, aes(x=year, y=global_sales))+geom_point()+stat_smooth(method="lm", col = "red")
Year_Global


#comparing markets
model3<-lm(formula=eu_sales~na_sales, data=VideoGame)
summary(model3)
plot(VideoGame$na_sales, VideoGame$eu_sales)
abline(model3)
NorthAmerica_Europe<-ggplot(VideoGame, aes(x=na_sales, y=eu_sales))+geom_point()+stat_smooth(method="lm", col = "red")
NorthAmerica_Europe

model4<-lm(formula=eu_sales~jp_sales, data=VideoGame)
summary(model4)
plot(VideoGame$eu_sales, VideoGame$jp_sales)
abline(model4)
Europe_Japan<-ggplot(VideoGame, aes(x=jp_sales, y=eu_sales))+geom_point()+stat_smooth(method="lm", col = "red")
Europe_Japan


model5<-lm(formula=other_sales~jp_sales, data=VideoGame)
summary(model5)
plot(VideoGame$jp_sales, VideoGame$other_sales)
abline(model5)
Japan_Other<-ggplot(VideoGame, aes(x=jp_sales, y=other_sales))+geom_point()+stat_smooth(method="lm", col = "red")
Japan_Other

model6<-lm(formula=jp_sales~na_sales, data=VideoGame)
summary(model6)
plot(VideoGame$na_sales, VideoGame$jp_sales)
abline(model6)
NorthAmerica_Japan<-ggplot(VideoGame, aes(x=na_sales, y=jp_sales))+geom_point()+stat_smooth(method="lm", col = "red")
NorthAmerica_Japan


grid.arrange(Year_Global, NorthAmerica_World,NorthAmerica_Europe,Europe_Japan,Japan_Other,NorthAmerica_Japan, ncol=3,nrow=3)

########################################################
#Visualization Word clouds

text<-VideoGame$genre
docs<-Corpus(VectorSource(text))

summary(docs)
docs<-docs%>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs<-tm_map(docs, content_transformer(tolower))
docs<-tm_map(docs, removeWords, stopwords("english"))
View(docs)
summary(docs)


#Step 3 Create a document-term matrix
dtm<- TermDocumentMatrix(docs)
matrix <-as.matrix(dtm)
words<-sort(rowSums(matrix), decreasing = TRUE)
wordsdf<-data.frame(word = names(words), freq = words)

set.seed(1234)
wordcloud(words = wordsdf$word, freq = wordsdf$freq, min.freq = 1, max.words = 1500, random.order = FALSE, rot.per=.35, colors = brewer.pal(8, "Dark2"))



#Platform Word Cloud
text<-VideoGame$genre
docs<-Corpus(VectorSource(text))

summary(docs)
docs<-docs%>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs<-tm_map(docs, content_transformer(tolower))
docs<-tm_map(docs, removeWords, stopwords("english"))
View(docs)
summary(docs)


#Step 3 Create a document-term matrix
text1<-VideoGame$platform
docs1<-Corpus(VectorSource(text1))

summary(docs1)

dtm2<- TermDocumentMatrix(docs1)
matrix2 <-as.matrix(dtm2)
words2<-sort(rowSums(matrix2), decreasing = TRUE)
wordsdf2<-data.frame(word = names(words2), freq = words2)

set.seed(1234)
wordcloud(words = wordsdf2$word, freq = wordsdf2$freq, min.freq = 1, max.words = 300, random.order = FALSE, rot.per=.35, colors = brewer.pal(8, "Dark2"))


#Publisher Word Cloud
text<-VideoGame$genre
docs<-Corpus(VectorSource(text))

summary(docs)
docs<-docs%>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs<-tm_map(docs, content_transformer(tolower))
docs<-tm_map(docs, removeWords, stopwords("english"))
View(docs)
summary(docs)


#Step 3 Create a document-term matrix
text2<-VideoGame$publisher
docs2<-Corpus(VectorSource(text2))

summary(docs2)

dtm3<- TermDocumentMatrix(docs2)
matrix3 <-as.matrix(dtm3)
words3<-sort(rowSums(matrix3), decreasing = TRUE)
wordsdf3<-data.frame(word = names(words3), freq = words3)

set.seed(1234)
wordcloud(words = wordsdf3$word, freq = wordsdf3$freq, min.freq = 1, max.words = 300, random.order = FALSE, rot.per=.35, colors = brewer.pal(8, "Dark2"))


######################################################
#Using support vector Machines to predict if a video game will be a hit or a flop. 
#Creating testing database and focusing on na_sales and eu_sales.  From our lm there is a possibility for a correlation between the two markets.
#Doing predictive analysis on both of these markets. 


Northerndf<-data.frame(VideoGame$na_sales, VideoGame$eu_sales)
#Creating cut off point
randIndex1<-sample(1:dim(Northerndf)[1])
summary(randIndex1)
head(randIndex1)
#creating a 2/3 cut point
cutPoint2_3<-floor(2*dim(Northerndf)[1]/3)
cutPoint2_3
#10882which is about 2/3 of 153
#Creating train data
trainData1<-Northerndf[randIndex1[1:cutPoint2_3],]

summary(trainData1)
#Creating test data
testData1<-Northerndf[randIndex1[(cutPoint2_3+1):dim(Northerndf)[1]],]

summary(testData1)

#Creating hit variable
hit_na_videogame <-c()

for (i in 1:nrow(trainData1)) {
  if (trainData1$VideoGame.na_sales[i] < mean(trainData1$VideoGame.na_sales)){
    #cat(i, "hitVideoGame", "\n")
    trainData1$hit_na_videogame[i] <- 0
  }
  else {
    trainData1$hit_na_videogame[i] <- 1
    #cat(i, "flopVideoGame", "\n")
  }
}

#creating hit variable for Europe

hit_eu_videogame <-c()

for (i in 1:nrow(trainData1)) {
  if (trainData1$VideoGame.eu_sales[i] < mean(trainData1$VideoGame.eu_sales)){
    #cat(i, "hitEUVideoGame", "\n")
    trainData1$hit_eu_videogame[i] <- 0
  }
  else {
    trainData1$hit_eu_videogame[i] <- 1
    #cat(i, "flopEUVideoGame", "\n")
  }
}


#Support Vector Machine Output

trainData1<-trainData1[,-4]
trainData1
PredictHitVideoGame <- ksvm(hit_na_videogame~., data = trainData1, kernel = "rbfdot", kpar = "automatic",C=10, cross = 10, prob.model = TRUE)

PredictHitVideoGame
summary(PredictHitVideoGame)


NAHitPredictor <- predict(PredictHitVideoGame, testData1, type = "votes")
str(NAHitPredictor)
cTable2 <- data.frame(testData1[,1], NAHitPredictor[,1])
colnames(cTable2) <- c("hit","flop")
cTable2
sqrt(mean((cTable2$test-cTable2$Pred)^2))
cTable2$error <- abs(cTable2$hit - cTable2$flop)
HitsvmPlot <- data.frame(cTable2$error, testData1$VideoGame.na_sales, testData1$VideoGame.eu_sales)
colnames(HitsvmPlot) <- c("error","Hit","Flop")
HitsvmPlot
HitVGM<-ggplot(HitsvmPlot, aes(x=Hit,y=Flop)) + geom_point(aes(size=error, color=error))
HitVGM

fig8 <- plot_ly(data = HitsvmPlot, x=HitsvmPlot$Hit, y=HitsvmPlot$Flop, color = HitsvmPlot$error)              
fig8                    

fig8 <- plot_ly(data = HitsvmPlot, x= ~Hit, y= ~Flop, color = ~error)              
fig8
