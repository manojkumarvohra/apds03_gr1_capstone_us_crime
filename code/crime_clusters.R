######################### LIBS ################################
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(rpart)
library(rpart.plot)
library(data.table)
library(dplyr)
library(sqldf)
library(ggplot2)
library(usmap)
library(ggplot2)
library(maps)
library(statebins)
library(NbClust)
#######################INIT####################################
setwd("C:/Users/mvohra/Documents/APDS/capstone/analysis/clustering/")
###############################################################

############ Cluster Function Definition ######################
prep_cluster_data_for <- function(iYear=0, scale=FALSE){
  
  #iYear=0
  raw<-read.csv("2.Missing_value_treated.csv")
  #View(raw)
  crimeData<-raw[,1:11]
  #View(crimeData)
  crime_data_raw<-crimeData[,-c(2,3)]
  #View(crime_data_raw)
  suffix <- "AGGREGATED"
  if(iYear==0){
    ########## Aggregating across years ##########
    summed_df <- crime_data_raw %>% 
      group_by(State) %>% 
      summarise_all(funs(sum))
    crime_df<-data.frame(summed_df, row.names = 1)
  }
  else{
    suffix <- as.character(iYear)
    ############## filtering for year ############
    crime_data_for_year<-crimeData %>% filter(crimeData$Year==iYear)
    #View(crime_data_for_year)
    crime_data_for_year<-crime_data_for_year[,-c(2,3)]
    crime_df<-data.frame(crime_data_for_year, row.names = 1)
  }
  
  print(paste("Clusterizing data for:", suffix))

  ####################### CLUSTERIZE ##############
  if(scale){
    crime_df <- scale(crime_df)  
  }
  #******** ELBOW METHOD *******#
  plt <- fviz_nbclust(crime_df, kmeans, method = "wss")+
    labs(subtitle = paste("Elbow method",suffix, sep=" "))
  
  clusters <- c(plt$layers[[1]]$data$clusters)
  y <- c(plt$layers[[1]]$data$y)
  
  elbowPoint_info = getElbowPoint(x_values = clusters, y_values = y)
  elb_count <- as.numeric(elbowPoint_info[1])
    
  imFileName <- paste("elbow_plot_",suffix,".png", sep="")
  ggsave(filename=imFileName, plot = plt, width = 7, height = 7, units = "in", limitsize = TRUE)
  
  #******** SILHOUETTE METHOD *******#
  plt <-  fviz_nbclust(crime_df, kmeans, method = "silhouette")+
    labs(subtitle = paste("Silhouette method",suffix," "))
  n_clust <- plt$data
  silhouette_count <- as.numeric(n_clust$clusters[which.max(n_clust$y)])
  
  imFileName <- paste("silhouette_plot_",suffix,".png", sep="")
  ggsave(filename=imFileName, plot = plt, width = 7, height = 7, units = "in", limitsize = TRUE)
  
  #******** GAP STATISTICS METHOD *******#
  set.seed(123)
  plt <- fviz_nbclust(crime_df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
    labs(subtitle = paste("Gap statistic method",suffix,sep=" "))
  gaps_count <- as.numeric(plt$layers[[4]]$data$xintercept)
  
  imFileName <- paste("gap_stat_plot_",suffix,".png", sep="")
  ggsave(filename=imFileName, plot = plt, width = 7, height = 7, units = "in", limitsize = TRUE)
  
  #**************** GET OPTIMUM COUNT *****************#
  print(paste("elb_count:",elb_count))
  print(paste("gaps_count:",gaps_count))
  print(paste("silhouette_count:",silhouette_count))
  cluster_count <- max(elb_count, gaps_count, silhouette_count)
  
  print(paste("Optimal clusters count:", cluster_count))
  #clustering
  crimeClusters<-kmeans(crime_df, centers = cluster_count, nstart = 25)
  #visualization
  clustersPlot <- fviz_cluster(crimeClusters, crime_df, main=paste("Cluster",suffix,sep=" "))
  imFileName <- paste("cluster_",suffix,".png", sep="")
  ggsave(filename=imFileName, plot = clustersPlot, width = 9, height = 9, units = "in", limitsize = TRUE)
  
  #Write cluster summary
  fileConn<-file(paste("cluster_summary_",suffix,".txt", sep=""))
  writeLines(capture.output(crimeClusters), fileConn)
  close(fileConn)
  
  ################ GEO ANALYSIS #################
 
  statesMap <- read.csv("statelatlong.csv")
  #View(statesMap)
  
  clusterDF<-data.frame(crimeClusters$cluster)
  setDT(clusterDF, keep.rownames = TRUE)[]
  clusterDF$rn <- as.factor(clusterDF$rn)
  clusterDF<-data.frame(clusterDF)
  
  clusterDF$City <- clusterDF$rn
  clusterDF$cluster <- clusterDF$crimeClusters.cluster
  clusterDF <-clusterDF[,-c(1,2)]
  
  joinedDF <- sqldf("SELECT lower(City) as region, State, Cluster, Latitude, Longitude 
              FROM clusterDF
              JOIN statesMap USING(City)")
  
  
  us_states <- map_data("state")
  
  us_states_joined <- left_join(us_states, joinedDF)
  
  color <- c("yellowgreen", "yellowgreen", "yellowgreen", "yellowgreen", 
             "gold1", "sienna3", "yellowgreen", "yellowgreen", "sienna3", 
             "gold1", "gold1", "gold1", "sienna3", "sienna3", "gold1", "dodgerblue2", 
             "dodgerblue2", "dodgerblue2", "gold1", "dodgerblue2", "sienna3", 
             "dodgerblue2", "burlywood2", "dodgerblue2", "palevioletred4", 
             "dodgerblue2", "palevioletred4", "dodgerblue2", "palevioletred4", 
             "palevioletred4", "gold1", "palevioletred4", "gold1", "darkorchid4", 
             "gold1", "darkorchid4", "palevioletred4", "green4", "cyan4", 
             "gold1", "palevioletred4", "burlywood2", "green4", "green4", 
             "cyan4", "darkorchid4", "green4", "burlywood2", "palevioletred4", 
             "burlywood2", "green4", "green4", "palevioletred4", "palevioletred4", 
             "palevioletred4", "green4", "burlywood2", "olivedrab2", "green4", 
             "olivedrab2", "darkorchid4", "darkorchid4", "cyan4", "darkorchid4", 
             "cyan4", "cyan4", "olivedrab2", "olivedrab2", "olivedrab2", "cyan4", 
             "cyan4", "olivedrab2", "cyan4", "olivedrab2", "olivedrab2", "cyan4", 
             "cyan4", "olivedrab2", "olivedrab2", "olivedrab2", "cyan4", "cyan4", 
             "cyan4", "cyan4", "orange2", "orangered1", "orangered1", "orangered1", 
             "olivedrab2", "orange2", "olivedrab2", "orangered1", "orange2", 
             "orangered1", "orange2", "orange2", "orangered1", "orangered1", 
             "orangered1", "orange2", "orangered1", "orange2", "orangered1", 
             "olivedrab2", "orangered1", "orangered1", "orangered1", "orangered1", 
             "orange2", "orange2", "orange2", "chartreuse3", "orangered1", 
             "chartreuse3")
  colorFrame <- data.frame(cluster=c(1,2,3), color=c("seagreen4","firebrick1","coral"))
  us_states_joined <- inner_join(us_states_joined, colorFrame, by="cluster")
  #View(us_states_joined)
  centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
  centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]
  statenames<-data.frame(region=levels(as.factor(us_states_joined$region))) 
  centroids<-merge(statenames,centroids,by="region")
   
  p <- ggplot(data = us_states_joined,
              aes(x = long, y = lat,
                  group = group, fill = color))
  
  p <- p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +  with(centroids, 
    annotate(geom="text", x = long, y=lat, label = abb, size = 4,color="white",family="Times")
    )  + ggtitle(paste("Crime_Geo_Heat_Map",suffix, sep=" ")) + scale_fill_identity()
  
  imFileName <- paste("geo_stats_",suffix,".png", sep="")
  ggsave(filename=imFileName, plot = p, width = 9, height = 9, units = "in", limitsize = TRUE)
}

getElbowPoint <- function(x_values, y_values) {
  
  max_x_x <- max(x_values)
  max_x_y <- y_values[which.max(x_values)]
  max_y_y <- max(y_values)
  max_y_x <- x_values[which.max(y_values)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))
  
  # Creating straight line between the max values
  fit <- lm(max_df$y ~ max_df$x)
  
  # Distance from point to line
  distances <- c()
  for(i in 1:length(x_values)) {
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  y_max_dist <- y_values[which.max(distances)]
  
  return(c(x_max_dist, y_max_dist, max(distances)))
}

####MAIN####
#**** START: Prepare Clusters ******#
#prepare aggregated cluster
prep_cluster_data_for(0, FALSE)
#prepare clusters for individual years
years <- c(2014:2018)
for(year in years){
  prep_cluster_data_for(year, FALSE)
}
#**** END:   Prepare Clusters ******#





