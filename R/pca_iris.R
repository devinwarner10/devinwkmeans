#' Runs the Principal Component tab 
#'
#'
#'
#'
#'
#' @export
#'
#'


pca_iris <- function(){
  x <- prcomp(iris[,-5])$x
  pc1 <- x[,1]
  pc2 <- x[,2]
  pc3 <- x[,3]
  pc4 <- x[,4]
  tibble("PC1" = pc1, "PC2" = pc2,"PC3" = pc3,"PC4" = pc4,"Species" = iris$Species)
}

