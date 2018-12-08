library(shiny)
library(erhcv)
library(nCopula)
library(ggplot2)
library(gridExtra)
library(data.tree)
library(DiagrammeRsvg)
library(DiagrammeR)
library(rsvg)

# See above for the definitions of ui and server

ui <- navbarPage(title = "Page",
                 tabPanel("Graphs",
                          fluidPage(
                            
                            # App title ----
                            titlePanel("Clustering validation"),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                
                                # Input: Slider for the number of bins ----
                                textInput(inputId = "str",
                                          label = "Structure",
                                          value = "GEO(0.5, 1:2, list(GEO(0.1, 3:4, list(GAMMA(0.1, 5:6, NULL),                                              GAMMA(0.3, 7:8, NULL))),                           GAMMA(0.2, 9:10, NULL)))"),
                                
                                # Input: set.seed
                                textInput(inputId = "seed",
                                          label = "set.seed",
                                          value = "2018"),
                                
                                # Input: Sample size
                                textInput(inputId = "n",
                                          label = "Sample size",
                                          value = "1000"),
                                
                                # Input: Simplification parameter
                                textInput(inputId = "a",
                                          label = "Simplification parameter",
                                          value = "0.95"),
                                textInput(inputId = "height",
                                          label = "Height",
                                          value = "600"),
                                textInput(inputId = "width",
                                          label = "Width",
                                          value = "850"),
                                
                                # Input: Type of tree
                                radioButtons(inputId = "type",
                                             label = "type",
                                             choices = list("hclust", "validated")),
                                
                                submitButton(text = "Apply Changes", icon = NULL, width = NULL)
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                imageOutput("Plot")
                                
                              )
                            )
                          )),
                 tabPanel("Structure",
                          verbatimTextOutput("console")))

server <- function(input, output) {
  output$console <- renderPrint({
    eval(parse(text = paste("struc <- ", input$str, sep = "")))
    print(struc)
  })
  output$Plot <- renderImage({
    
    eval(parse(text = paste("struc <- ", input$str, sep = "")))
    eval(parse(text = paste("set.seed(", input$seed, ")", sep = "")))
    U.. <- rCompCop(eval(parse(text = input$n)), struc)
    
    dd <- dist(cor(U.., method = "sp"), method = "maximum")
    fit <- hclust(dd, method = "average")
    tree_hclust <- hclust2tree(fit)
    
    tree_valid <- VerifyTree(U..,
                             alpha = eval(parse(text = input$a)))$Tree
    
    e1 <- new.env()
    e1$O <- Node$new("(O)")
    
    Update_tree <- function(tree, k = "O"){
      if (length(tree) > 1){
        for (i in 1:length(tree)){
          if (length(tree[[i]]) > 1){
            expr0 <- strsplit(k, "")[[1]]
            expr <- paste("(", paste(c(expr0, i), collapse = ","), ")", sep = "")
            expr2 <- paste(paste("e1$", k, i, sep = ""),
                           " <- e1$",
                           k,
                           "$AddChild('", expr, "')",
                           sep = "")
            eval(parse(text = expr2))
            print(paste(k, i, sep = ""))
            Update_tree(tree[[i]], k = paste(k, i, sep = ""))
          }
          else{
            expr2 <- paste(paste("e1$", k, i, "l", sep = ""),
                           " <- e1$",
                           k,
                           "$AddChild('", tree[[i]], "')",
                           sep = "")
            eval(parse(text = expr2))
          }
        }
      }
    }
    
    if (input$type == "hclust")
      Update_tree(tree_hclust)
    else
      Update_tree(tree_valid)
    
    export_graph(ToDiagrammeRGraph(e1$O), "export.png",
                 width = eval(parse(text = input$width)),
                 height = eval(parse(text = input$height)))
    
    list(src = "export.png",
         contentType = 'image/png',
         width = eval(parse(text = input$width)),
         height = eval(parse(text = input$height)),
         alt = "This is alternate text")
    
  }, deleteFile = TRUE)
  
  # output$console <- renderPrint({
  #   eval(parse(text = paste("struc <- ", input$str, sep = "")))
  #   return(print(struc))
  #   # You could also use grep("Warning", values[["log"]]) to get warning messages and use shinyBS package
  #   # to create alert message
  # })
  
}

shinyApp(ui = ui, server = server)
