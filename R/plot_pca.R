#' Plots Iris principle components
#'
#'
#'
#'
#'
#' @export
#'
#'


plot_pca <- function(xcol, ycol){
  data <- as.data.frame(pca_iris())
  data %>%
    ggplot(aes(x = data[,xcol], y = data[,ycol], color = Species)) +
    geom_point() +
    scale_color_manual(values = c("blue3", "violetred2", "springgreen3")) +
    xlab(xcol) + 
    ylab(ycol)
}