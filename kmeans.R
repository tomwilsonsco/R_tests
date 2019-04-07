library(tidyverse)
library(reshape2)

iris
newiris<-iris
newiris$Species<-NULL
newiris
kc <-kmeans (newiris, 3, nstart=25)
kc$tot.withinss
#table(iris$Species, kc$cluster)
#table(kc$cluster,iris$Species)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)



######################################
#Appendix A CHECK HOW MANY RESULT TYPES

#remove result table if already exists
rm(tot_ss_results)

#While loop to run kmeans 10000 times and store tot.withinss each time
i = 0
while (i < 10000) {
  kc <- kmeans(newiris,3, nstart=25)
  if (exists("tot_ss_results")){
   tot_ss_results = rbind(tot_ss_results, kc$tot.withinss)
  }else{
    tot_ss_results = as.data.frame(kc$tot.withinss)
  }
 i = i + 1 
}

colnames(tot_ss_results) <- c("totss")

#Using dplyr to generate a summary count table
initial_result = tot_ss_results %>%
  group_by(totss) %>%
  summarise(COUNT = n()) %>%
  mutate(PERCENTAGE = COUNT/ sum(COUNT) * 100)

######################################
#Appendix B RETURN CORRECT/ INCORRECT CLASSIFICATION OF IRIS
#RECORDS FOR EACH OF THE TWO POSSIBLE K-MEANS RESULTS

iris_cluster_results <- iris
newiris <- iris
newiris$Species <- NULL

#Function to convert cross tab of format table(kc$cluster,iris$Species)
#Into DF to use for condition testing
get_result_df <- function(input_table){
  temp_df = as.data.frame.matrix(input_table)
  temp_df$Cluster_Number <- 1:nrow(temp_df)
  return(temp_df)
}

#Function to check the contents of cross tab result DF
#And confirm if result and order matches 'RESULT A / RESULT B'
check_result_df <- function(input_table){
  df_test = input_table
  if (df_test[1,]$setosa==50 & df_test[2,]$versicolor==48 &df_test[3,]$virginica ==36) {
    return ('RESULT A')
   } else if (df_test[1,]$setosa==17 & 
             df_test[2,]$versicolor==0 &df_test[3,]$virginica ==50){
    return ('RESULT B')
   }else{return ('NO LUCK')}
}

#Run a while loop creating K-means, until
#Result A and Result B sets are returned 
#(with their specific cluster number assignment)
while (!(exists("RESULT_A_SUM") & exists("RESULT_B_SUM"))){
  kc <- kmeans(newiris, 3)
  temp_tbl = table(kc$cluster,iris$Species)
  temp_df = get_result_df(temp_tbl)
  result = check_result_df(temp_df)
  if (result == 'RESULT A'){
    RESULT_A_SUM = temp_df
    RESULT_A_OBS = kc$cluster
  } else if (result == 'RESULT B'){
    RESULT_B_SUM = temp_df
    RESULT_B_OBS = kc$cluster
  }
}

#Set label RESULT A
RESULT_A_SUM = RESULT_A_SUM %>%
  mutate(ASSIGNED_CLASS= case_when(Cluster_Number == 1~'setosa',
                                   Cluster_Number == 2~'versicolor',
                                   Cluster_Number == 3~'virginica'))
#set label RESULT B
RESULT_B_SUM = RESULT_B_SUM %>%
  mutate(ASSIGNED_CLASS= case_when(Cluster_Number == 1~'setosa',
                                   Cluster_Number == 2~'setosa',
                                   Cluster_Number == 3~'not determinable'))

#For the Result A and Result B outputs, create DF of IRIS record and
#Cluster result
RESULT_A_OBS_DF = as.data.frame(RESULT_A_OBS)
colnames(RESULT_A_OBS_DF)<-c('Cluster_Number')
RESULT_A_OBS_DF$Iris_Record_Number <- 1:nrow(RESULT_A_OBS_DF)
RESULT_A_OBS_DF = RESULT_A_OBS_DF %>%
  inner_join(RESULT_A_SUM, by = 'Cluster_Number') %>%
  select(Cluster_Number, Iris_Record_Number, ASSIGNED_CLASS)
RESULT_A_OBS_DF = RESULT_A_OBS_DF %>% rename(RESULT_A_CLUSTER_CLASS = ASSIGNED_CLASS)
RESULT_A_OBS_DF$ACTUAL_SPECIES <- iris$Species
RESULT_A_OBS_DF = RESULT_A_OBS_DF %>%
mutate(RESULT_A_CORRECT_SPECIES = case_when(RESULT_A_CLUSTER_CLASS == ACTUAL_SPECIES ~1,
                                            RESULT_A_CLUSTER_CLASS != ACTUAL_SPECIES ~ 0))

RESULT_B_OBS_DF = as.data.frame(RESULT_B_OBS)
colnames(RESULT_B_OBS_DF)<-c('Cluster_Number')
RESULT_B_OBS_DF$Iris_Record_Number <- 1:nrow(RESULT_B_OBS_DF)
RESULT_B_OBS_DF = RESULT_B_OBS_DF %>%
  inner_join(RESULT_B_SUM, by = 'Cluster_Number') %>%
  select(Cluster_Number, Iris_Record_Number, ASSIGNED_CLASS)
RESULT_B_OBS_DF = RESULT_B_OBS_DF %>% rename(RESULT_B_CLUSTER_CLASS = ASSIGNED_CLASS)
RESULT_B_OBS_DF$ACTUAL_SPECIES <- iris$Species
RESULT_B_OBS_DF = RESULT_B_OBS_DF %>%
  mutate(RESULT_B_CORRECT_SPECIES = case_when(RESULT_B_CLUSTER_CLASS == ACTUAL_SPECIES ~1,
                                                   RESULT_B_CLUSTER_CLASS != ACTUAL_SPECIES ~0))


#Put final Result tables together and calculate overall probability
#of correct classification
RESULT_B_JOIN = select(RESULT_B_OBS_DF, Iris_Record_Number, 
                       RESULT_B_CLUSTER_CLASS, RESULT_B_CORRECT_SPECIES)
FINAL_OBS_DF = RESULT_A_OBS_DF %>%
  select(Iris_Record_Number, ACTUAL_SPECIES, 
         RESULT_A_CLUSTER_CLASS, RESULT_A_CORRECT_SPECIES) %>%
  inner_join(RESULT_B_JOIN, by='Iris_Record_Number') %>%
  select(Iris_Record_Number, ACTUAL_SPECIES, RESULT_A_CLUSTER_CLASS,
         RESULT_B_CLUSTER_CLASS, RESULT_A_CORRECT_SPECIES, RESULT_B_CORRECT_SPECIES) %>%
  mutate(PROBABILITY_OF_CORRECT_CLASS = (RESULT_A_CORRECT_SPECIES * 0.8) + (RESULT_B_CORRECT_SPECIES * 0.2))

write.csv(FINAL_OBS_DF, 'C:/temp/Kmeans_Prob.csv')


#Appendix C graph construction (uses FINAL_OBS_DF from Appendix B)


library(ggplot2)  
iris_graph = iris
iris_graph$Iris_Record_Number = 1:nrow(iris_graph)
obs_join = select(FINAL_OBS_DF, Iris_Record_Number, PROBABILITY_OF_CORRECT_CLASS)
obs_join = rename(obs_join, Probability = PROBABILITY_OF_CORRECT_CLASS)
iris_graph = iris_graph %>%
  inner_join(obs_join, by = 'Iris_Record_Number')
iris_graph = mutate(iris_graph, "ProbShape" =case_when(Probability == 0~'0%',
                                         Probability == 0.8~'80%',
                                         Probability== 1~'100%'))

iris_graph$ProbShape<- factor(iris_graph$ProbShape,levels=c('100%', '80%', '0%'))




graph1 = ggplot(data=iris_graph, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(shape = Species), size=2)+
  geom_point(aes(colour = ProbShape), shape = 22, size = 5)+
  scale_colour_manual(values = c("Green", "Orange", "Red"),name = "Probability\nof correct\ncluster")+
  scale_shape_manual(values=c(1,4,5))+
  labs(title = "Probability of correct k-means cluster classification", x='Sepal Length', y='Sepal Width')+
  theme_bw()

ggsave(filename='graph1.png',plot=graph1, device='png',path='C:/temp',width=8, height=6, dpi=1000)

#Plot 2 just the good fit
iris_graph2 = iris
iris_graph2$Iris_Record_Number = 1:nrow(iris_graph)
obs_join = select(FINAL_OBS_DF, Iris_Record_Number, RESULT_A_CLUSTER_CLASS, RESULT_A_CORRECT_SPECIES)
obs_join = rename(obs_join, Correct = RESULT_A_CORRECT_SPECIES)
obs_join = rename(obs_join, Cluster = RESULT_A_CLUSTER_CLASS)
iris_graph2 = iris_graph2 %>%
inner_join(obs_join, by = 'Iris_Record_Number')
iris_graph2 = mutate(iris_graph2, "Correct_Class" =case_when(Correct == 0~'Incorrect',
                                                             Correct ==1~'OK'))
incorrect_only = filter(iris_graph2, Correct_Class =='Incorrect')

graph2<-ggplot() +
  geom_point(data=iris_graph2, aes(x=Sepal.Length, y=Sepal.Width, colour=factor(Cluster)), size=2)+
  scale_colour_manual(values = c("Black","Purple", "Green"),name = "Cluster Species")+
  geom_point(data=incorrect_only, aes(x=Sepal.Length, y=Sepal.Width, shape = Correct_Class), colour='Red', size = 5)+
  scale_shape_manual(values=c(4), name= "Correct Cluster\nCompared to\nActual Species")+
  labs(title = "Correct and Incorrect Cluster Allocation for Result A, which has 80% Probability\nof being returned by kmeans", x='Sepal Length', y='Sepal Width')+
  theme_bw()

ggsave(filename='graph2.png',plot=graph2, device='png',path='C:/temp',width=8, height=6, dpi=1000)

