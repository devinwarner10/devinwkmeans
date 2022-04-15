#'
#'
#'
#'
#'
#'

umap_iris <- umap(iris[,1:4])

ui <- fluidPage(
  
  # Put a titlePanel here
  titlePanel("k-means clustering"),
  
  navbarPage("Menu",
      tabPanel("K-Means",
    
        sidebarLayout(
          # Sidebar. Put your inputs inside the sidebarPanel
          sidebarPanel(
            checkboxGroupInput('vars', 'Clustering Variables', names(iris[,-5]),
                               selected = names(iris[,c(1,2)])),
            selectInput('xcol', 'X Variable', names(iris[,-5])),
            selectInput('ycol', 'Y Variable', names(iris[,-5]), selected=names(iris)[[2]]),
            numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)
          ),
          
          # Main panel. put your output plot here
          mainPanel(
            plotOutput('plot1')
          )
        )
    ),
    
    tabPanel("kNN",
          sidebarPanel(
            selectInput('xcol2', 'X Variable', names(iris[,-5])),
            selectInput('ycol2', 'Y Variable', names(iris[,-5]), selected=names(iris)[[2]]),
            numericInput('k', "k-Neighbors", 3, min = 1, max = 9)
          ),
             
          mainPanel(
            plotOutput('plot2')
          )
    
    ),
    
    tabPanel("Dimension Reduction",
        navbarPage("Method",
          tabPanel("Pricnciple Component",
             sidebarPanel(
               selectInput('xcol3', 'X Variable', names(pca_iris()[,-5])),
               selectInput('ycol3', 'Y Variable', names(pca_iris()[,-5]), 
                           selected = names(pca_iris()[,-5])[[2]])
             ),
             
             mainPanel(
               plotOutput('plot3')
             ),
             
             fluidRow(
               DTOutput('table1')
             ),
             fluidRow(
               DTOutput('table2')
             )
          ),
          
          tabPanel("UMAP",
                plotOutput('plot4')
          )
        )
             
    )
  )
)

server <- function(input, output, session) {
  
  selectedData <- reactive({
    iris[, c(input$vars)]
  })
  
  clusters <- reactive({
    create_clusters(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot_clusters(iris, input$xcol, input$ycol, clusters())
  })
  
  output$plot2 <- renderPlot({
    plot_knn(input$k, input$xcol2, input$ycol2)
  })
  
  output$plot3 <- renderPlot({
    plot_pca(input$xcol3, input$ycol3)
  })
  
  output$plot4 <- renderPlot({
    data.frame(umap_iris$layout, Species=iris$Species) %>%
      ggplot(aes(X1,X2, fill = Species))+
      geom_point(cex=3, pch=21) +
      coord_fixed(ratio = 1)
  })
  
  output$table1 <- renderDT(
    pca_iris(0),
    options = list(dom = '')
  )
  
  output$table2 <- renderDT(
    pca_iris(-1),
    options = list(dom = '')
  )
}

shinyApp(ui = ui, server = server)

# palette(c("#999999", "#E69F00", "#56B4E9", "#009E73", 
# "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))