library(readxl)
library(neuralnet)
library(dplyr)

data<-read_excel(path = "datasets/ExchangeUSD (2).xlsx")
data<-data.frame(data)
summary(data)

#turn exchange rates in to a vector
exchange_rate<-as.vector(data$"USD/EUR")
exchange_rate

#normalize the dataset
data_norm<-as.data.frame(scale(data[,3]))
names(data_norm) <- c("rate")
data_norm

#creating lagged datasets
lag_data<-list()

lag_data[[1]]<- data_norm %>%
    mutate(t.1 = lag(rate,1))%>%
    na.omit()
  
lag_data[[2]] <- data_norm %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2)) %>%
  na.omit() 

lag_data[[3]] <- data_norm %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2),
         t.3 = lag(rate, 3)) %>%
  na.omit() 

lag_data[[4]] <- data_norm %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2),
         t.3 = lag(rate, 3),
         t.4 = lag(rate, 4)) %>%
  na.omit()

lag_data

#splitting data for training and testing




