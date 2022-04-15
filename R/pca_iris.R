#' Runs the Principal Component tab 
#'
#'
#'
#'
#'
#' @export
#'
#'


pca_iris <- function(vars = 1){
  pr <- prcomp(iris[,-5])
  if(vars == 1){
    x <- pr$x
    pc1 <- x[,1]
    pc2 <- x[,2]
    pc3 <- x[,3]
    pc4 <- x[,4]
    tibble("PC1" = pc1, "PC2" = pc2,"PC3" = pc3,"PC4" = pc4,"Species" = iris$Species)
  }else if(vars == -1){
    x <- round(summary(pr)["importance"]$importance,3)
    tibble("Metric" = c("Standard Deviation", "Proportion of Variance", "Cumulative Proportion"),
           "PC1" = x[,1],"PC2" = x[,2],"PC3" = x[,3],"PC4" = x[,4])
  }else{
    y <- tibble(names(iris[,-5]), pr$rotation[,1], 
                pr$rotation[,2], pr$rotation[,3], pr$rotation[,4])
    tibble("Variable" = y[,1], "PC1" = y[,2], "PC2" = y[,3], "PC3" = y[,4], "PC4" = y[,5])
  }
}

