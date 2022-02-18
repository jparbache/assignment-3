library('rvest')

#Specifying the url for desired website to be scraped
url <- 'https://www.imdb.com/search/title/?count=100&release_date=2021,2021&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#  Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#  removing '\n'
description_data<-gsub("\n","",description_data)

#Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#  removing mins and converting it to numerical
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)


#  removing \n
genre_data<-gsub("\n","",genre_data)

#  removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#  converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Using CSS selectors to scrape the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#  removing commas
votes_data<-gsub(",","",votes_data)

#  converting votes to numerical
votes_data<-as.numeric(votes_data)

#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#  converting directors data into factors
directors_data<-as.factor(directors_data)

#Using CSS selectors to scrape the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#  converting actors data into factors
actors_data<-as.factor(actors_data)

#Using CSS selectors to scrape the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)


#  removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

for (i in c(38,51,55,65,74,78,84,85,96)){
  
  a<-metascore_data[1:(i-1)]
  
  b<-metascore_data[i:length(metascore_data)]
  
  metascore_data<-append(a,list("NA"))
  
  metascore_data<-append(metascore_data,b)
  
}

#  converting metascore to numerical
metascore_data<-as.numeric(metascore_data)
length(metascore_data)



#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      
                      Description = description_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = rating_data,
                      
                      Metascore = metascore_data, Votes = votes_data,
                      
                      Director = directors_data, Main_Actor = actors_data)


write.csv(movies_df, "C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/assignment-3/movies_df.csv", row.names = FALSE)


#Ploting 2 cool visualizations
library('ggplot2')

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)+
  labs(title = "2021 Feature Films", subtitle = "Count of the runtime of the 100 most popular films", caption =  "Source: Imdb")


ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre)) +
  labs(title = "2021 Feature Films", subtitle = "100 most popular films classified by runtime and rating", caption =  "Source: Imdb")


