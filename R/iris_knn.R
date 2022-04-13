#' Runs kNN model for iris data
#'
#'
#'
#'
#' @export
#'
#'
#'

iris_knn <- function(k){
  set.seed(1998)
  
  test_ind <- sample(nrow(iris), 0.267*nrow(iris))
  test <- iris[test_ind,]
  train <- iris[-test_ind,]
  
  iris_knn <- knn3(as.factor(Species)~., data = train, k = k)
  
  y <- predict(iris_knn, test, type = "class")
  
  tibble(test, "Classification" = y)
}



