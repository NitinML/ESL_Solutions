   #####  Function for Implementing kNN in R  #####
   
##### Function which takes number of neighborhood, training and test datasets as arguments #####
   
   kNN <- function(k,training,test)
   { 
     # Keep Response Column Separately
     test_response <- test[,1]
     train_response <- train[,1]
     
     # Create datasets without response columns
     test_knn <- as.matrix(test[,-1])
     train_knn <- as.matrix(train[,-1])
     
     # Create empty matrix to store eucledian distance between test instance and training point 
     # and first column as index
     euc_dist <- matrix(,nrow = nrow(train_knn),ncol = nrow(test_knn)+1)
     
     # Assign index to euc_dist to identify points
     euc_dist[,1] <- c(1:nrow(train_knn))
     
     # Create empty matrix to store predicted value
     val <- vector(,nrow(test_knn))
     
     i = 1
     for (i in 1:nrow(test_knn)){
       test_instance <- test_knn[i,]
       rep_instance <- rep(test_instance,times = nrow(train_knn))
       matrix_instance <- t(matrix(rep_instance, nrow = ncol(train_knn),ncol = nrow(train_knn)))
       diff_2 <- (train_k - matrix_instance)^2
       sum_col <- colSums(t(diff_2))
       euc_dist[,2] <- sum_col
       dist_sort <- euc_dist[order(euc_dist[,2]),]
       closest_pt <- dist_sort[c(1:k),1]
       response_instance <- train_response[closest_pt]
       uniq_response <- unique(train_response)
       response1 <- sum(ifelse(response_instance == uniq_response[1],1,0))
       response2 <- sum(ifelse(response_instance == uniq_response[2],1,0))
       if (response1 > response2) {
         val[i] <- uniq_response[1]
       } else {
         val[i] <- uniq_response[2]
       }
     }
     
     predictions <- cbind(test_response,val)
     correct <- sum(ifelse(predictions[,1] == predictions[,2], 1, 0))
     error <- nrow(test) - correct
     
     error_frac <- error/nrow(test)
     return(error_frac)
   }
   
   