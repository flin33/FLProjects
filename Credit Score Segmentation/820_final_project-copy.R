options(stringsAsFactors=FALSE)
options(digits = 3)
suppressPackageStartupMessages(library(tidyverse))
# install.packages("arules")
# install.packages("arulesViz")

## load our libraries
library(arules)
library(arulesViz)
library(readr)
library(tidyr)
library(corrplot)
library(tidyverse)
library(factoextra)
library(skimr)
library(dplyr)
library(purrr)
library(cluster)
library(factoextra)
library(gridExtra)
library(recharts)
library(htmlwidgets)
cc <- read_csv("~/Desktop/CC GENERAL.csv")


set.seed(8206)

##########explore raw dataset 
glimpse(cc)
skimr::skim(cc)
dim(cc)


colnames(cc) = tolower(gsub(" ", "_", colnames(cc)))
##########data clenaing 
cc$BALANCE[cc$BALANCE > 7809.060] <- 7809.060
cc$PURCHASES[cc$PURCHASES > 7413.090] <- 7413.090
cc$ONEOFF_PURCHASES[cc$ONEOFF_PURCHASES > 5572.107] <- 5572.107
cc$INSTALLMENTS_PURCHASES[cc$INSTALLMENTS_PURCHASES > 3124.082] <- 3124.082
cc$CASH_ADVANCE[cc$CASH_ADVANCE > 7270.351] <- 7270.351
cc$CASH_ADVANCE_FREQUENCY[cc$CASH_ADVANCE_FREQUENCY > 0.736] <- 0.736
cc$CASH_ADVANCE_TRX[cc$CASH_ADVANCE_TRX > 23.723] <- 23.723
cc$PURCHASES_TRX[cc$PURCHASES_TRX > 89.283] <- 89.283
cc$CREDIT_LIMIT[cc$CREDIT_LIMIT > 15410.910] <- 15410.910
cc$PAYMENTS[cc$PAYMENTS > 10418.320] <- 10418.320
cc$MINIMUM_PAYMENTS[cc$MINIMUM_PAYMENTS > 7981.557] <- 7981.557

missing_value <- is.na(cc$MINIMUM_PAYMENTS)
cc[missing_value,]$MINIMUM_PAYMENTS <- 0
cc <- na.omit(cc)
skimr::skim(cc)



cc %>% 
  select(-cust_id) %>% 
  cor -> cc_cor

dat <-cc %>% 
  select(-cust_id) 

##########pca
c <- prcomp(dat,center = TRUE, scale = TRUE)
# fviz_screeplot(c, addlables = T, ylim = c(0,100))#3/4
# 
# get_eigenvalue(c) >1 #5- 74.4% cumulative.variance.percent

c_pcs <- predict(c, newdata = dat)
class(c_pcs)
c_pcs <- as.data.frame(c_pcs)

##using 5 conponents
c_pca5 <- c_pcs[,1:5]

##########k-mean using pca(5)
c_scale <- scale(c_pca5)

#eval numer of k using silhouette and wss
# fviz_nbclust(c_scale, kmeans, method = 'wss', k.max = 30)--- takes a  long time to run

# k_wss = function(k) {
#   km = kmeans(c_scale, k, nstart=25, iter=25)
#   kwss = km$tot.withinss
#   return(kwss)
# }
# 
# x <- 1:20
# wass <- map_dbl(x,k_wss)
# plot(x,wass, type ="b")#wss- no useful suggestion
# 
# fviz_nbclust(c_scale, kmeans, method = 'silhouette', k.max = 20)##silhouette suggests 6 clusters
# fviz_nbclust(c_scale, kmeans, method = 'silhouette', k.max = 20)

###exam some options: k of 3/4/5/6
# plot(silhouette(c_scale$cluster, dist=dist(s)))
# pca_k3 <- kmeans(c_scale,5,25,30)
# pca_k4 <- kmeans(c_scale,6,25,30)
# pca_k5 <- kmeans(c_scale,5,25,30)
pca_k6 <- kmeans(c_scale,6,25,30)

# table(pca_k3$cluster)
# table(pca_k4$cluster)
# table(pca_k5$cluster)
# table(pca_k6$cluster)
# 
# pca_k4$tot.withinss
# pca_k5$tot.withinss
# pca_k6$tot.withinss

# fviz_cluster(pca_k4,dat)
# fviz_cluster(pca_k5,dat)
# fviz_cluster(pca_k6,dat)

##***[final decision: 6 clusters]***
##adding back clusters and 5 pca variables to original dataset
cc$cluster <- pca_k6$cluster

final_data <- cbind(cc,c_pca5)
###analyze clusters using final data......
glimpse(final_data)

data_summary<-final_data %>% group_by(cluster) %>% summarise_all(mean) %>% select(-starts_with("PC"),-cust_id)
final_data2<-final_data %>% select(-starts_with("PC"),-cust_id) %>% group_by(cluster)

p <- list()
for (i in names(final_data2)[1:6]) {p[[i]]<-ggplot(
  final_data2,aes_string(x="cluster", y = i, group = "cluster"))+
  geom_jitter(alpha = 0.2, color = "grey")+
  geom_boxplot(aes(color = factor(cluster)),outlier.color = "#ff8c33",outlier.shape = 23)+
  ggtitle(paste("Cluster VS", toupper(i), sep = " " ))
  do.call(grid.arrange,c(p,ncol=2))}

p1 <- list()
for (i in names(final_data2)[7:12]) {p1[[i]]<-ggplot(
  final_data2,aes_string(x="cluster", y = i, group = "cluster"))+
  geom_jitter(alpha = 0.2, color = "grey")+
  geom_boxplot(aes(color = factor(cluster)),outlier.color = "#ff8c33",outlier.shape = 23)+
  ggtitle(paste("Cluster VS", toupper(i), sep = " " ))
  do.call(grid.arrange,c(p1,ncol=2))}


p2 <- list()
for (i in names(final_data2)[13:17]) {p2[[i]]<-ggplot(
  final_data2,aes_string(x="cluster", y = i, group = "cluster"))+
  geom_jitter(alpha = 0.2, color = "grey")+
  geom_boxplot(aes(color = factor(cluster)),outlier.color = "#ff8c33",outlier.shape = 23)+
  ggtitle(paste("Cluster VS", toupper(i), sep = " " ))
do.call(grid.arrange,c(p2,ncol=2))}
p2[6]
final_data2
data_summary



## Radar Map
data_summary<-final_data %>% group_by(cluster) %>% summarise_all(mean) %>% select(-starts_with("PC"),-cust_id)
data_summary
dat <- data.table::melt(data_summary, id.vars='cluster')
names(dat) <- c('cluster', 'indicator', 'Parameter')
knitr::kable(dat)
radar<-echartr(dat, indicator, Parameter, cluster, t=cluster, type='radar',sub='fill') %>%
  setTitle('Customer Characteristics by Cluster') %>%
  setPolar(type='circle',radius = "50%" ) %>% setTheme(width=1100, height=900)
radar
saveWidget(radar, file="~/Desktop/cluster.html")
