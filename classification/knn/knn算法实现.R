knn_predict <- function(test_data, train_data, k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[6]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"eu_char"]) == "g"){
        good = good + 1
      }
      else
        bad = bad + 1
    }
    
    # Compares the no. of neighbors with class label good or bad
    if(good > bad){          #if majority of neighbors are good then put "g" in pred vector
      
      pred <- c(pred, "g")
    }
    else if(good < bad){
      #if majority of neighbors are bad then put "b" in pred vector
      pred <- c(pred, "b")
    }
    
  }
  return(pred) #return pred vector
}

#Accuracy Calculation in R

#The accuracy metric calculates the ratio of the number of correctly predicted class labels to the total number of predicted labels.

accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,6] == test_data[i,7]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}


#############################
K = 5
predictions <- knn_predict(test.df, train.df, K) #calling knn_predict()
 
test.df[,7] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df))
