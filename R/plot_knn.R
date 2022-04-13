#' Creates plot for kNN
#'
#'
#'
#'
#' @export
#'


plot_knn <- function(k, xcol, ycol){
 data <- as.data.frame(iris_knn(k))
 data %>%
   ggplot(aes(x = data[,xcol], y = data[,ycol], color = Species,shape = Classification)) +
   geom_point() +
   scale_color_manual(values = c("blue3", "violetred2", "springgreen3")) +
   xlab(xcol) + 
   ylab(ycol)
}


