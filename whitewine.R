if (!require(readxl)){
  install.packages("readxl")
}

if (!require(tibble)){
  install.packages("tibble")
}

if (!require(cluster)){
  install.packages("cluster")
}

if (!require(factoextra)){
  install.packages("factoextra")
}

if (!require(ggplot2)){
  install.packages("ggplot2")
}

if(!require(NbClust)){
  install.packages("NbClust")
}

if (!require("MASS")) {
  install.packages("MASS")
}

if (!require("fpc")) {
  install.packages("fpc")
}

if (!require("flexclust")) {
  install.packages("flexclust")
}

if (!require("datasets")) {
  install.packages("datasets")
}

if (!require("tidyverse")) {
  install.packages("tidyverse")
}


library(readxl)

##reading the dataset 
whitewine<-read_excel(path="datasets/Whitewine_v6.xlsx")


# Define a function to replace outliers with NA
remove_outliers <- function(x) {
  lowerq <- quantile(x, probs = 0.25)
  upperq <- quantile(x, probs = 0.75)
  iqr <-IQR(x)
  
  x_cleaned<-ifelse(x<lowerq-1.5*iqr|x>upperq +1.5*iqr,NA,x)
  return(x_cleaned)
}

wine_out_rm<-as.data.frame(lapply(whitewine,remove_outliers))


# Count removed outliers per column (assuming numerical data)
removed_per_col <- apply(wine_out_rm, 2, function(x) sum(is.na(x)))
cat("\nNumber of outliers removed per column:\n")
print(removed_per_col)

wine_cleaned<- na.omit(wine_out_rm) 
boxplot(wine_cleaned,outline=FALSE)

#scaling the dataset
wine_subset <- as.data.frame(scale(wine_cleaned[,1:11]))
boxplot(wine_subset,outline=FALSE)  