---
title: "Data Mining Ass4 Problem 2-3"
author: "Boyun Jang"
date: "May 1, 2020"
output: html_document
---
## Building User profile
```{r cars}
library(lsa)
library(dplyr)
setwd('D:/2020 Spring/Data Mining/Ass4')
set.seed(100)
rating<-read.csv("ratings.csv", header = T)
movie<-read.csv("movies.csv", header = T)
Userid<-20437298%%671
allgenres<-c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
"Film-Noir", "Horror", "IMAX", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
```

```{r}
user_profile <- read.csv("D:/2020 Spring/Data Mining/Ass4/userprofile.csv",header =T,sep = ",")

for(row in 1:nrow(user_profile)){
  genres<-as.list(strsplit(toString(movie[movie$movieId == user_profile[row, ]$X,]$genres),"[|]")[[1]])
  for (i in genres) {
    user_profile[row, i]=1
  }
}
user_profile[is.na(user_profile)] <- 0

for (i in 20){
  print(sum(user_profile[,i]))
  user_profile["AVG",]<-colMeans(user_profile)
}
user_profile
```

#### Ramdomly choose movie samples
```{r}
movie_profile <- read.csv("D:/2020 Spring/Data Mining/Ass4/movieprofile.csv",header =T,sep = ",")
movie_samples <- movie[sample(1:nrow(movie),10),]
cat("User ID ",Userid, "chose the following 10 movies:",movie_samples$movieId)
```

## Building Movie profile

```{r}
m.sample.id<- movie_samples$movieId
movie_profile<-setNames(data.frame(matrix(ncol = 20, nrow = 10)), allgenres)
movie_profile<-data.frame(movieId=m.sample.id,movie_profile)

movie_samples
movie_profile

for(row in 1:nrow(movie_profile)){
  genres<-as.list(strsplit(toString(movie[movie$movieId == movie_profile[row, ]$movieId,]$genres),"[|]")[[1]])
  for (i in genres) {
    movie_profile[row, i]=1
  }
} 

movie_profile[is.na(movie_profile)] <- 0
movie_profile<-select(movie_profile,-c(NA.))
movie_profile
```

## Cosine similarity
```{r}
print("Of these, the following 5 movies are recommended:")

for (i in 1:5) {
  x<-as.numeric(user_profile['AVG',-1])
  y<-as.numeric(movie_profile[movie_profile$movieId == movie_profile[i,'movieId'],])
  similarity <- cosine(x,y)[[1]]

  movieid <- movie_profile[movie_profile$movieId == movie_profile[i,'movieId'],1]
  title <- as.vector(movie[movie$movieId==movieid,"title"])[1]
  cat("Movie:", title, " similarity: ", similarity , "\n")
}

```
