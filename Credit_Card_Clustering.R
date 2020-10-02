library(tidyverse)
library(rstudioapi)
library(h2o)
library(glue)
library(highcharter)

library(factoextra)
library(NbClust)
library(cluster)
library(plotly)

data <- read.csv("CC GENERAL.csv")

data %>% glimpse()

data$TENURE <- data$TENURE %>% as_factor()

data$TENURE %>% table() %>% prop.table()

data <- data %>% select(-CUST_ID)

data[!complete.cases(data),] %>% View()

data$MINIMUM_PAYMENTS %>% is.na() %>% length()

data <- data %>% select(-MINIMUM_PAYMENTS)

data[is.na(data$CREDIT_LIMIT), "CREDIT_LIMIT"] <- median(data$CREDIT_LIMIT , na.rm=TRUE)




# PCA with h2o ----
h2o.init()

data.h2o <- data %>% as.h2o()


model_pca <- data.h2o %>% 
  h2o.prcomp(transform = "STANDARDIZE",
             k = 4, seed = 123,
             impute_missing = T,
             max_runtime_secs = 90)

model_pca %>% 
  h2o.predict(data.h2o) %>% 
  as_tibble() %>% 
  add_column(TENURE = data$TENURE) -> pca_pred

model_pca@model$model_summary %>% as.data.frame() -> table
table

table[2,] %>% 
  gather() %>% 
  hchart("bar", hcaes(x = key, y = value)) %>%
  hc_colors(colors = 'blue') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)


df <- pca_pred %>% select(-TENURE) %>% scale()

df %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#7 clusters

df %>% 
  fviz_nbclust(kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
#5 clusters 

df %>% 
  fviz_nbclust(kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")
#1 cluster


# Fitting K-Means to the data ----
set.seed(123)
kmeans <- df %>% kmeans(centers = 7)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 7,
                plotchar = F,
                main = 'Clusters of customers')



