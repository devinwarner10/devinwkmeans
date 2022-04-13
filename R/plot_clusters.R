#' Plots two-dimensional clusters
#'
#' Takes two column vectors with their associated clusters and plots them
#' on a standard dot plot. Also plots the centers of each cluster.
#'
#' @param cols The columns to be plotted
#' @param kmeans Result of the kmeans algorithm
#'
#' @return plot
#' 
#' @export
#'

plot_clusters <- function(data, xcol, ycol, kmeans){
    dat <- tibble("x1" = data[,xcol], "x2" = data[,ycol],
                  "Species" = data$Species , "Cluster" = as.factor(kmeans$cluster))
    ggplot() + 
      geom_point(data = dat,
                 mapping = aes(x = x1, y = x2,
                               col = Species,shape = Cluster)) + 
      xlab(xcol) +
      ylab(ycol) + 
      geom_point(mapping = aes_string(x = kmeans$centers[,xcol], 
                                      y = kmeans$centers[,ycol]),
                 color = "black", size = 6, shape = "cross") +
      scale_color_manual(values = c("blue3", "violetred2", "springgreen3"))
}


# 
# 