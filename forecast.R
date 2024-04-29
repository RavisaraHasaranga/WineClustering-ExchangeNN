library(readxl)
library(dplyr)

data<-read_excel(path = "datasets/ExchangeUSD (2).xlsx")
data<-data.frame(data[,3])
names(data)<-c("rate")
summary(data)

#turn exchange rates in to a vector
exchange_rate<-as.vector(data$rate)
exchange_rate



#creating lagged datasets
lagged_data<-list()

lag_data[[1]]<- data %>%
    mutate(t.1 = lag(rate,1))%>%
    na.omit()
  
lag_data[[2]] <- data %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2)) %>%
  na.omit() 

lag_data[[3]] <- data %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2),
         t.3 = lag(rate, 3)) %>%
  na.omit() 

lag_data[[4]] <- data %>%
  mutate(t.1 = lag(rate, 1),
         t.2 = lag(rate, 2),
         t.3 = lag(rate, 3),
         t.4 = lag(rate, 4)) %>%
  na.omit()

#normalize each lagged dataset
lag_data_norm<-list()

for (i in 1:length(lag_data)){
  lag_data_norm[[i]]<-scale(lag_data[[i]])
}


# Split data into training and testing sets
lag_train_data <- list()
lag_test_data <- list()

for (i in 1: length(lag_data_norm)){
  lag_train_data[[i]]<-lag_data_norm[[i]][1:400,]
  lag_test_data[[i]]<-lag_data_norm[[i]][401:nrow(lag_data[[i]]),]

}


library(neuralnet)


#defining hidden layers
architectures<-list(c(5),c(7),c(13,9))

#activation functions
activate<-c("logistic","tanh")

#output laters
output_layer<-c(TRUE,FALSE) #true for linear, false for non linear

#function to train data

train<-function(architecture,train_data,test_data,activation,output){
  
  #creating a formula for each lag dataset
  formula <- as.formula(paste("rate ~", paste(colnames(train_data)[-1],
                                              collapse = " + ")))
  
  #training the model
  train<-neuralnet(formula,train_data,act.fct = activation,
                   linear.output = output,learningrate = 0.01)
  
  # Predict on test data
  #predictions <-neuralnet::compute(train, test_data[, -1])$net.result
  
  
}

#list to store neural networks
NN<-list()

# Loop through lags, architectures, activation functions, and output types
###Training for Lag 1
for (j in 1:length(architectures)) { 
  for (k in 1:length(activate)) {
    for (l in 1:length(output_layer)) {
      result <- train(architectures[[j]], lag_train_data[[1]], lag_test_data[[1]], 
                      activate[k], output_layer[l])
      NN[[paste("lag1", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}


###Training for Lag 2
for (j in 1:length(architectures)) { 
  for (k in 1:length(activate)) { 
    for (l in 1:length(output_layer)) { s
      result <- train(architectures[[j]], lag_train_data[[2]], lag_test_data[[2]], 
                      activate[k], output_layer[l])
      NN[[paste("lag2", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}

###Training for Lag 3
for (j in 1:length(architectures)) {
  for (k in 1:length(activate)) { 
    for (l in 1:length(output_layer)) { 
      result <- train(architectures[[j]], lag_train_data[[3]], lag_test_data[[3]], 
                      activate[k], output_layer[l])
      NN[[paste("lag3", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}

###Training for Lag 4
for (j in 1:length(architectures)) { 
  for (k in 1:length(activate)) {
    for (l in 1:length(output_layer)) {
      result <- train(architectures[[j]], lag_train_data[[4]], lag_test_data[[4]], 
                      activate[k], output_layer[l])
      NN[[paste("lag4", "arch", j, "act", k, "output", l)]] <- result
    }
  }
}







