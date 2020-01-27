#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("tokenizers")
#install.packages("dplyr")
#install.packages("janeaustenr")
#install.packages("tidyverse")
#install.packages("tm")
#install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages ("RColorBrewer")
#install.packages("extrafont")
# install.packages("shiny")
# install.packages("shinydashboard")
#install.packages("scales")
#install.packages("hrbrthemes")
#install.packages("googleVis")
#install.packages("rgdal")
# install.packages("raster")
# install.packages("ggmap")
# install.packages("maps")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("rgeos")
# install.packages("plotly")
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("RColorBrewer")
#install.packages("RCurl")
#install.packages("XML")

#To Load
library(RCurl)
library(RColorBrewer)
library(XML)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(tokenizers)
library(dplyr,warn.conflicts = FALSE)
library(janeaustenr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(extrafont)
library(shiny)
library(shinydashboard)
library(scales)
library(hrbrthemes)
library(googleVis)
library(rgdal)
library(raster)
library(ggmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(plotly)
library(wordcloud)
library(wordcloud2)

#----------------------Set-up of the dataset-----------------------------------#

#Read the dataset
dataset<-read_csv("C:/Users/Lucile/Documents/INSA/ANALYSIS & DATA PROCESSING/DEALING WITH LARGE AMOUNT DATA/wine-reviews/winemag-data_first150k.csv")
dataset$description %>%
  glimpse()
View(dataset)

#Check the duplicate descriptions
dataset<-dataset %>%
  distinct(description, .keep_all = T)
  View(dataset)

#------------------------Analysis of the countries ----------------------------#  
  
#Grouping the countries per regions 
NAmerica <- c('US', 'Canada', 'Mexico')
SAmerica <- c('Chile', 'Argentina', 'Brazil', 'Colombia', 'Uruguay', 'Paraguay', 'Bolivia', 'Peru', 'Venezuala')
Europe <- c('Germany', 'France', 'England', 'Spain', 'Portugal', 'Italy', 'Austria')
dataset$continent <- ifelse(dataset$country %in% NAmerica, 'NA', ifelse(dataset$country %in% SAmerica, 'SA', ifelse(dataset$country %in% Europe, 'EU', 'Other')))
dataset$price_modified <- ifelse(dataset$price > 200, NA, dataset$price)

#An over representation of European and North American
ggplot(dataset,aes(x=points, y=price, color=continent)) +
  geom_point()

#More precisely, most of the reviews are for US wines : 40531
#Repartition of the reviews between countries
reviews_country <- dataset %>%
  group_by(country) %>% 
  drop_na(country) %>%
  summarise(count = n(),AvgPrice = mean(price, na.rm = T), AvgPoint = mean(points, na.rm = T)) %>% 
  arrange(desc(count)) %>%
  head(10) 
  view(reviews_country)
  
  #Mapping - Repartition reviews between the TOP 10 countries - AVERAGE PRICE
ggplot(data.frame(reviews_country),aes(x=AvgPoint, y =AvgPrice, color=country, size=count)) +
  geom_point(alpha=0.7) + 
  scale_size(range=c(1, 25), name="Reviews") +
  labs(x="Average Rating", y="Average Price", title="Distribution of Wine Reviews by Top 10 Countries") +
  theme(plot.title = element_text(size=10,face="bold" ))

#Repartition of the reviews between countries - MEDIAN PRICE
reviews_country_2 <- dataset %>%
  group_by(country) %>% 
  drop_na(country) %>%
  summarise(count = n(),MedianPrice = median(price, na.rm = T), MedianPoint = median(points, na.rm = T)) %>% 
  arrange(desc(count)) %>%
  head(10) 
view(reviews_country_2)
reviews_country = as.character(reviews_country$country)

#Figure 2 in the report - Mapping - Repartition reviews between the TOP 10 countries - MEDIAN PRICE
ggplot(data.frame(reviews_country_2),aes(x=MedianPoint, y =MedianPrice, color=country, size=count)) +
  geom_point(alpha=0.7) + 
  scale_size(range=c(1, 25), name="Reviews") +
  labs(x="Median Rating", y="Median Price", title="Distribution of Wine Reviews by Top 10 Countries")+
  theme(plot.title = element_text(size=10,face="bold" ))


#If we want to compare countries with more than 10000 reviews
#We can take : US (40531), It (14847), FR (14452)
WineRatingFiltered <- dataset %>%
  filter(country %in% c("France", "US","Italy" )) %>%
  group_by(country) %>%
  summarise_at(vars(points), list(points = ~mean(., na.rm=T))) %>%
  arrange(desc(points))%>%
  head(10)

#Histogram - average rating for the top 3 countries in terms of number of reviews
  ggplot(data=WineRatingFiltered, aes(x = reorder(country,-points), y=points))+
    geom_bar(stat="identity", fill="navy")+
    coord_cartesian(ylim=c(80,90))+
    labs (x="Countries",y="Rating", title="Countries by Average Rating")

# => We see that French wines have higher scores than American and Italian wines

#-----------------------Focus on the variety per country----------------#
  
#What kind of varieties are represented in the dataset ?
  variety.count<- dataset %>%
    group_by(variety) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
    View(variety.count)

#Dataset for comparison variety/country/price/rate
VarPrice = dataset %>% 
  filter(country %in% c("France","US", "Italy", "Spain","Portugal"),
         variety %in% c("Chardonnay","Pinot Noir", "Cabernet Sauvignon", 
                        "Red Blend", "Bordeaux-style Red Blend", "Sauvignon Blanc",
                        "Syrah", "Riesling","Merlot","Champagne Blend")) %>% 
     group_by(country,variety) %>%
     summarise(medianPrice = median(price, na.rm = TRUE))

#Figure 5 in the report - Average Varietal prices by country
ggplot(VarPrice, aes(x=variety, y=medianPrice)) + 
  geom_col(position="dodge", aes(fill=country)) + 
  scale_y_continuous(limits=c(0,100)) +
  coord_flip() +
  labs(title = "Average Varietal Prices by Country")
         
#Dataset for average varietal rating by country
   VarRating = dataset%>%
     filter(country %in% c("France","US", "Italy", "Spain","Portugal"),
            variety %in% c("Chardonnay","Pinot Noir", "Cabernet Sauvignon", 
                           "Red Blend", "Bordeaux-style Red Blend", "Sauvignon Blanc",
                           "Syrah", "Riesling","Merlot","Champagne Blend")) %>% 
     group_by(country,variety) %>%
     summarise(mean_rating = mean(points))

#Figure 5 in the report - Average Varietal rating by country
   ggplot(VarRating, aes(x=variety, y=mean_rating)) +
     geom_col(position="dodge", aes(fill=country)) +
     scale_y_continuous(limits=c(80,95), oob = rescale_none)+
     coord_flip() +
     labs(title = "Average Varietal Ratings by Country")

#---------------------------Analysis Price vs Ratings-----------------------------#
   
#Max price per country
MaxPrice_country <- dataset %>%
  select(country, price) %>%
  group_by(country) %>%
  summarise(maxprice = max(price, na.rm = TRUE)) %>%
  arrange(desc(maxprice)) %>%
  head(10)

ggplot(data=MaxPrice_country,aes(x= reorder(country,-maxprice),y=maxprice))+
  geom_bar(stat="identity", fill="navy")+
  coord_cartesian(ylim=c(0,2500))+
  labs (x="Countries",y="Max Price", title="Top 10 Countries by Max Price")

#Average price per country
dataset <- dataset %>% 
  filter(country!="US-France")
AveragePrice_country <- dataset %>%
  select(country, price) %>%
  group_by(country) %>%
  summarise(avgprice = mean(price, na.rm = T)) %>%
head(50)
View(AveragePrice_country)

#Statistical characteristic of numerical columns : point & price
min(dataset$points)
max(dataset$points)
mean(dataset$points)
sd(dataset$points)
quantile(dataset$points, na.rm = TRUE)

#For the prices, need to remove the NA
min(dataset$price,na.rm=TRUE)
max(dataset$price,na.rm=TRUE)
mean(dataset$price,na.rm=TRUE)
sd(dataset$price,na.rm=TRUE)
quantile(dataset$price, na.rm = TRUE)

#Figure 1 in the report - Histogram - distribution of points - histogram
ggplot(dataset, aes(x=points, fill="count")) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="Number of wines", title="Repartion of wine ratings")+
  theme(plot.title = element_text(size=11))

#How many wines have a rating >90?
dataset_rating <- dataset %>%
  filter(points >= "98") %>%
  view(dataset_rating)

# Dataset for relationship between price and points
dataset_filteredPrice <- dataset %>%
  filter (!is.na(price),price <=300,country %in% c("France", "US","Italy" ))
  
# Figure 3 in the report - Correlation between prices and ratings for the 3 countries most represented in the dataset
ggplot(data=dataset_filteredPrice,aes(x=points, y=price)) + 
  geom_point(position="jitter", alpha=1/10) + 
  geom_smooth(method="lm", se=F) +
  geom_count() + 
  facet_wrap(~country)+
  labs(title="Correlation between price and points")

#---------------------Focus on the price and the ratings per varieties-------------------#

#What are the variety of wine most reviewed 
   dataset_wineVariety <- dataset %>%
      select(variety, points, price) %>%
      group_by(variety) %>%
      summarize(count=n()) %>%
      arrange(desc(count))%>%
      filter(count >= 1000) %>%
      head(10) 
      View(dataset_wineVariety)
      
#What are the top variety in terms of rate?
      dataset_varietyRate <- dataset %>%
        select(variety,points,price) %>%
        group_by(variety)%>%
        summarize(varietyRate = median(points, na.rm=T),count=n(),medianPrice=median(price,na.rm=T)) %>%
        arrange(desc(varietyRate)) %>%
        filter(count > 1000)%>%
        head(10)
        view(dataset_varietyRate)
        
#Figure 4 in the report - Use of the package plot_ly to trace the following graphs
#A bar plot with 2 y-axis

x1 <- factor(dataset_varietyRate$variety, levels=unique(dataset_varietyRate$variety)[order(dataset_varietyRate$varietyRate, decreasing=TRUE)])
dataset_varietyRate$varietyRate <- round(dataset_varietyRate$varietyRate,1)
dataset_varietyRate$meanPrice <- round(dataset_varietyRate$medianPrice,1)

p <- plot_ly(dataset_varietyRate)%>%
  add_trace(x = x1, y = ~varietyRate,type='bar', name="Rating") %>%
  add_markers(x = x1, y = ~medianPrice, yaxis = "y2", name="Price", size=200) %>%
layout(title = '',
       xaxis = list(title = "Variety"),
       yaxis = list(side = 'left', title = 'Rating', showgrid = FALSE, zeroline = FALSE, range=c(20,100)),
       yaxis2 = list(side = 'right', overlaying = "y", title = 'Prices', showgrid = FALSE, zeroline = FALSE), range=c(20,70))

#Print the graph
p

#Lollipop graphs to visualize the prices and the ratings
#Finally, we didn't use it in the report and we used a bar chart with 2 y-axis to display the same data.

ggplot(dataset_varietyRate, aes(x=reorder(variety,-varietyRate), y=varietyRate,label=round(varietyRate,1)))+
    geom_point(colour = "#d32f2f",size=14)+
    geom_segment(colour = "#d32f2f", aes(x=variety, 
                     xend=variety, 
                     y=(20), 
                     yend=varietyRate))+
  geom_text(colour='black', font="bold", size=4)+
  labs(title = "Top rating variety") +
theme(axis.text.x=element_text(size=11, angle=40, vjust=1, hjust=1,family="Serif", face = "bold"))+
 theme(plot.title = element_text(size=15,family="Serif",face="bold"),
       axis.title.x = element_text(size=11, face="bold",family="Serif"),
       axis.title.y = element_text(size=11, face="bold",family="Serif")) +
       
       geom_point(colour = "#FFC107",size=14)+
         geom_segment(colour = "#FFC107", aes(x=variety, 
                                              xend=variety, 
                                              y=(5), 
                                              yend=meanPrice))+
         geom_text(colour='black',family="Serif",face="bold",size=4)+
         labs(title = "Mean price of top Ranking Variety") +
         #theme(axis.text.x = element_text(angle=65, vjust=0.8))
         theme(axis.text.x=element_text(size=11, angle=40, vjust=1, hjust=1,family="Serif", face = "bold"))+
         theme(plot.title = element_text(size=15,family="Serif",face="bold"),
               axis.title.x = element_text(size=11, face="bold",family="Serif"),
               axis.title.y = element_text(size=11, face="bold",family="Serif")
        )

ggplot(dataset_varietyRate, aes(x=reorder(variety,-meanPrice), y=meanPrice,label=round(meanPrice,1)))+
  geom_point(colour = "#FFC107",size=14)+
  geom_segment(colour = "#FFC107", aes(x=variety, 
                                       xend=variety, 
                                       y=(5), 
                                       yend=meanPrice))+
  geom_text(colour='black',family="Serif",face="bold",size=4)+
  labs(title = "Mean price of top Ranking Variety") +
  theme(axis.text.x=element_text(size=11, angle=40, vjust=1, hjust=1,family="Serif", face = "bold"))+
  theme(plot.title = element_text(size=15,family="Serif",face="bold"),
        axis.title.x = element_text(size=11, face="bold",family="Serif"),
        axis.title.y = element_text(size=11, face="bold",family="Serif"))


        
#-------------------TEXT ANALYSIS----------------------~#
  
  description.per.word <- dataset$description
  description.per.word <- VectorSource(description.per.word)
  
  #Load the data as a corpus
  descr <-VCorpus(description.per.word) %>%
  descr <-tm_map(descr,removePunctuation) %>%
  descr<-tm_map(descr, content_transformer(tolower)) %>%
  descr<-tm_map(descr,removeNumbers) %>%
  View(descr)
  
  #Remove stop words, white spaces and unifying 
  descr <- tm_map(descr,removeWords,stopwords("en"))
  descr <- tm_map(descr,stripWhitespace)
  descr<- tm_map(descr, stemDocument)
  
  #creating matrix
  descr_mat <- DocumentTermMatrix(descr)
  
  #Remove wome words such as : wine, a, an, it...
  stop_words_list <- c("wine")
  descr_No_WineWords<- tm_map (descr, removeWords, stop_words_list)
  View(descr_No_WineWords)
  
  #Print top words with an occurence >10000
  top_words <-findFreqTerms(descr_mat,lowfreq=20000)
  print(top_words)
  
  #-----------------------Focus on the Portugese wine------------------------#
  
  Pt <- dataset$description[dataset$country=="Portugal"]
  Pt <- Corpus(VectorSource(Pt))
  inspect(Pt)
  
  #Clean Text
  Pt <- tm_map(Pt, content_transformer(tolower))
  Pt <- tm_map(Pt, removeNumbers)
  Pt <- tm_map(Pt, removeWords, stopwords("english"))
  Pt <- tm_map(Pt, removeWords, c("and","wine", "winery", "it", "winemaking", "winemark", "without", "alcohol", "although", "across", "age", "almost", "along", "also", "amount", "alongsid", "anoth", "approach", "around", "back", "background", "basic", "barrel", "big", "bit", "blend", "bottl", "bouquet", "cellar", "continu", "core", "cut", "develop", "display", "end", "extra",  "drink", "drinking", "doesnt", "element", "enough", "featur", "feel", "fill", "find", "first", "final", "finish", "focus", "follow", "food", "forward", "frame", "front", "get", "give", "given", "glass", "grape", "here", "hint", "highlight", "hold", "just", "keep", "lack", "last", "layer", "length", "lift", "littl", "made", "make", "mark", "medium", "mix", "month", "mouth", "much", "name", "need", "new", "next", "nose", "now", "offer", "one", "open", "overal", "pair", "part", "pack", "play", "price", "produc", "provid", "quick", "quit", "rather", "region",  "remain", "result", "reveal", "right", "round", "run", "select", "seem", "set", "show", "soon", "side", "sip", "small", "slight", "somewhat", "start", "suggest", "suppl", "support", "take", "that", "there", "though", "time", "togeth", "top", "toward", "two", "turn", "use", "variety", "vine", "vineyard", "vintag", "way", "weight", "will", "winemak", "wineri", "year", "yet", "<e2><80><93>", "<c3><a8>dr", "<c3><a9>" ,"aroma", "flavor")) 
  Pt <- tm_map(Pt, removePunctuation)
  Pt <- tm_map(Pt, stripWhitespace)
  
  # Build a Text Doc MatriX
  Pt_dtm <- TermDocumentMatrix(Pt)
  m <- as.matrix(Pt_dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  # Figure 6 in the report - Generate the Word cloud
  set.seed(1000)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=400, random.order=FALSE, rot.per=20, 
            colors=brewer.pal(10, "Spectral"))
  
#Figure 6 in the report - creating a plot of 10 top stems
ggplot(data=head(d, 10), aes(x=factor(word,levels = d$word[order(-d$freq)]), y=freq)) + 
  geom_col(fill="#404080", alpha=0.6, position = 'identity') + 
  labs(x="Words", y="Count", title="Top 10 Word Stems in Portugese Wine Reviews", size=30) +
  theme(plot.title = element_text(size=15,face="bold" ))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,3200,400))
   
