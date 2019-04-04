# install.packages(c("fastDummies", "dummies", "dplyr"))

val_to_dummy <- function(x){
  if (x > 0){
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


