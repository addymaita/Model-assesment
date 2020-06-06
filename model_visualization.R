library(shiny)
library(shinydashboard)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
require(scales)
library(plotly)
library(Metrics)


#load model ¿mejor con load? mirarlo mañana
modelo_1 <- readRDS("model_example.rda")
modelo_2 <- readRDS("model_example_2.rda")
modelo_3 <- readRDS("model_example_3.rda")



ui <- dashboardPage(
  dashboardHeader(title = "Model Assesment"),
  dashboardSidebar(
    fileInput("file1", "Model 1 predictions",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("file2", "Model 2 predictions",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("file3", "Real values",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr()
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      box(plotOutput("plot1"),
          box(uiOutput("value1"))
          ),
      
      box(plotOutput("plot2"),
          box(uiOutput("value2"))
          )
      
    )
  )
)

server <- function(input, output) {

  observe({
    req(input$file3)
    input_file3 <- input$file3
    pred_3 <- read.csv(input_file3$datapath)
    req(input$file1)
    input_file1 <- input$file1
    pred_1 <- read.csv(input_file1$datapath)
    
    req(input$file2)
    input_file2 <- input$file2
    pred_2 <- read.csv(input_file2$datapath)
    
    predicted_df_1 <- data.frame(pred = pred_1$x, real=pred_3$x, res= pred_3$x - pred_1$x)
    predicted_df_2 <- data.frame(pred = pred_2$x, real=pred_3$x, res= pred_3$x - pred_2$x)
    

    ##Model 1 plot 
    output$plot1 <- renderPlot({
      
      plot_model_1 <- ggplot(data = predicted_df_1, aes(x = real, y = pred)) +
        geom_smooth(method = "lm", se = FALSE, color = "#93ACB5") +
        geom_point(aes(colour = res)) +
        theme(legend.position="right") + 
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(color = "Residuals") +
        xlab("Real value") +
        ylab("Predicted value") +
        ggtitle("Model 1")
      
      plot_model_1 + theme(
        plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="#353535", size=14, face="bold"),
        axis.title.y = element_text(color="#353535", size=14, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")
      ) + scale_colour_gradient2(
        low = muted("navyblue"),
        mid = "darkorange1",
        high = muted("navyblue"),
        midpoint = 0,
        space = "Lab",
        na.value = NA,
        guide = "colourbar",
        aesthetics = "colour",
        labels = comma
      ) 
      
    })
    

      
    output$value1 <- renderUI({
      
      HTML(paste0("Baseline mean: ", round(mean(predicted_df_1$real)), br(),
                  "Baseline Median: ", round(median(predicted_df_1$real)), br(),
                 "MAPE: ", round(Metrics::mape(predicted_df_1$real, predicted_df_1$pred), digits = 3), "%", br(),
                 "RMSE: ", round(Metrics::rmse(predicted_df_1$real, predicted_df_1$pred)), br(),
                 "MSE: ", round(Metrics::mse(predicted_df_1$real, predicted_df_1$pred)), br(),
                 "MAE: ", round(Metrics::mae(predicted_df_1$real, predicted_df_1$pred)))
               )
    })
    
    ##Model 2m plot
    output$plot2 <- renderPlot({
      
      plot_model_2 <- ggplot(data = predicted_df_2, aes(x = real, y = pred)) +
        geom_smooth(method = "lm", se = FALSE) +
        geom_point(aes(colour = res)) +
        theme(legend.position="right") + 
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(color = "Residuals") +
        xlab("Real value") +
        ylab("Predicted value") +
        ggtitle("Model 2")
      
      plot_model_2 + theme(
        plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="#353535", size=14, face="bold"),
        axis.title.y = element_text(color="#353535", size=14, face="bold")
      ) + scale_colour_gradient2(
        low = muted("navyblue"),
        mid = "darkorange1",
        high = muted("navyblue"),
        midpoint = 0,
        space = "Lab",
        na.value = NA,
        guide = "colourbar",
        aesthetics = "colour",
        labels = comma
      ) 

    })
    output$value2 <- renderUI({
      
      HTML(paste0("Baseline mean: ", round(mean(predicted_df_2$real)), br(),
                  "Baseline Median: ", round(median(predicted_df_2$real)), br(),
                  "MAPE: ", round(Metrics::mape(predicted_df_2$real, predicted_df_2$pred), digits = 3), "%", br(),
                  "RMSE: ", round(Metrics::rmse(predicted_df_2$real, predicted_df_2$pred)), br(),
                  "MSE: ", round(Metrics::mse(predicted_df_2$real, predicted_df_2$pred)), br(),
                  "MAE: ", round(Metrics::mae(predicted_df_2$real, predicted_df_2$pred)))
      )
    })
    
  })
  
  
  
  

  
}

shinyApp(ui, server)
