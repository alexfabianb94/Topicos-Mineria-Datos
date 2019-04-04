# install.packages(c("fastDummies", "dummies", "dplyr"))

val_to_dummy <- function(x){
  if (sum(x) > 0){
    return(1)
  } else {
    return(0)
  }
  
}

BlackFriday <- read.csv("../data/BlackFriday.csv", sep=";", 
                        colClasses = c("integer","character","factor",
                                       "factor", "factor","factor",
                                       "factor", "integer","factor",
                                       "factor", "factor", "integer"))

str(BlackFriday)
clases <- sapply(BlackFriday,class)


dummy <- dummies::dummy.data.frame(BlackFriday, sep = "_",
                                  names = c("Gender", 
                                            "Age", 
                                            "Occupation",
                                            "City_Category", 
                                            "Stay_In_Current_City_Years",
                                            "Product_Category_1", 
                                            "Product_Category_2",
                                            "Product_Category_3"))

test <- subset(dummy,select = -Product_ID)

aggregate_data <- cbind(aggregate(. ~ User_ID, test[1:ncol(test) - 1], val_to_dummy),
                        Purchase = aggregate(Purchase ~ User_ID, test, sum)$Purchase)

row.names(aggregate_data) <- aggregate_data$User_ID
wss <- numeric(15)

for (k in 1:15){
  wss[k] <- kmeans(aggregate_data[2:ncol(aggregate_data)],centers = k)$tot.withinss
}

plot(1:15,wss,"b")

ncluster <- 3
clusters <- kmeans(aggregate_data[2:ncol(aggregate_data)],centers = ncluster)$cluster
aggregate_data$cluster <- clusters