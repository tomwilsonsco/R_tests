library(tidyverse)

iris
newiris<-iris
newiris$Species<-NULL
newiris
kc <-kmeans (newiris, 3)
kc
table(iris$Species,kc$cluster)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)




#Function that takes table in format table(kc$cluster,iris$Species) 
#converts it to dataframe and labels which species each cluster number represents
#Note- input table is table(kc$cluster,iris$Species) this is other way round
#from the table in our initial example code

my_cluster_species <- function(input_table){
  temp_df = as.data.frame.matrix(input_table)
  temp_df$Cluster_Number <- 1:nrow(temp_df)
  temp_df = mutate(temp_df, Cluster_Species = case_when(setosa > versicolor & setosa > virginica ~ 'setosa',
                                      versicolor > setosa & versicolor > virginica ~ 'versicolor',
                                      virginica > setosa & virginica > versicolor ~ 'virginica',
                                      TRUE ~ 'no winner'))
  return(temp_df)
}


#Function to create data frame of cluster prediction
#Against each record in iris dataset compared to its actual label

my_observation_prediction <- function(observations, cluster_labels){
  temp_observations = as.data.frame(observations)
  colnames(temp_observations)<-c('Cluster_Number')
  temp_observations$Observation_Number <- 1:nrow(temp_observations)
  temp_observations = temp_observations %>%
    inner_join(cluster_labels, by='Cluster_Number') %>%
    select(Observation_Number, Cluster_Number, Cluster_Species)
  return(temp_observations)
}




i = 0
while (i < 100){
  kc <- kmeans(newiris, 3)
  temp_tbl = table(kc$cluster,iris$Species)
  temp_cluster_classes = my_cluster_species(temp_tbl)
  temp_cluster_observations = my_observation_prediction(kc$cluster, temp_cluster_classes)
  if (exists ("cluster_classes")) {
   cluster_classes = rbind(cluster_classes, temp_cluster_classes)
  } else {
    cluster_classes = temp_cluster_classes
  }
  if (exists ("cluster_observations")) {
    cluster_observations = rbind(cluster_observations, temp_cluster_observations)
  } else {
    cluster_observations = temp_cluster_observations
  }
  i = i + 1
}

  