library(shiny)
library(shinydashboard)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
require(scales)
library(plotly)
library(Metrics)
library(precrec)
library(data.table)
library(yardstick)
source("functions.R")

ui <- dashboardPage(
    dashboardHeader(title = "Classification Models"),
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

        # First model dashboard
        fluidRow(
            column(12,
                box(title = "Model 1", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = "100%",
                    collapsible = TRUE,
                    fluidRow(
                        # A static valueBox
                        valueBoxOutput("accuracy"),
                        
                        # Dynamic valueBoxes
                        valueBoxOutput("AUC")
                    ),
                    column(4,dataTableOutput("details_mod_1")),
                       column(4,
                              plotOutput("plot1")
                       ),
                       column(4, 
                              tabBox(width = "100%",
                                     tabPanel("ROC",
                                         plotOutput("roc_1")   
                                     )
                                  ,
                                  tabPanel("PRC",
                                      plotOutput("prc_1")
                                  ) 
                              )
                       )
                     )
            )
        ),
        
        ## second model dashboard
        fluidRow(
            column(12,
                   box(title = "Model 2", 
                       status = "warning", 
                       solidHeader = TRUE, 
                       width = "100%",
                       collapsible = TRUE,
                       fluidRow(
                           # A static valueBox
                           valueBoxOutput("accuracy_2"),
                           
                           # Dynamic valueBoxes
                           valueBoxOutput("AUC_2")
                       ),
                       column(4,dataTableOutput("details_mod_2")),
                       column(4,
                              plotOutput("plot2")
                       ),
                       column(4, 
                              tabBox(width = "100%",
                                     tabPanel("ROC",
                                              plotOutput("roc_2")   
                                     )
                                     ,
                                     tabPanel("PRC",
                                              plotOutput("prc_2")
                                     ) 
                              )
                              
                       )
                       
                   )
            )
        )
        ###
        
    )
)

server <- function(input, output) {
    
    observe({
        cols <- c("pred", "real")
        
        req(input$file3)
        input_file3 <- input$file3
        real <- read.csv(input_file3$datapath)
        req(input$file1)
        input_file1 <- input$file1
        pred_1 <- read.csv(input_file1$datapath)
        
        req(input$file2)
        input_file2 <- input$file2
        pred_2 <- read.csv(input_file2$datapath)
        
        predicted_df_1 <- data.frame(pred = pred_1$x, real=real$x)
        predicted_df_2 <- data.frame(pred = pred_2$x, real=real$x)
        
        predicted_df_1[,cols] <- data.frame(apply(predicted_df_1[cols], 2, as.factor))
        predicted_df_2[,cols] <- data.frame(apply(predicted_df_2[cols], 2, as.factor))
        
        output$accuracy <- renderValueBox({
            valueBox(
                paste0(round(100*(Metrics::accuracy(real$x, pred_1$x)),2), "%"), "Accuracy",icon= icon("balance-scale"),
                color = "purple"
            )
        })
        output$AUC <- renderValueBox({
            valueBox(
                paste0(round(Metrics::auc(real$x, pred_1$x),2)), "AUC", icon= icon("balance-scale"),
                color = "blue"
            )
        })
        
        ##Model 1 confusion matrix
        output$plot1 <- renderPlot({
            cm <- conf_mat(predicted_df_1, real, pred)
            draw_confusion_matrix(cm)

            
        })
        
        ##Model 1 ROC and PRC
        
        output$roc_1 <- renderPlot({
            roc(pred_1$x, real$x)
        })
        
        output$prc_1 <- renderPlot({
            prc(pred_1$x, real$x)
        })
        
        ##Model 1etails metrics 
        output$details_mod_1 <- renderDataTable({
            cm <- confusionMatrix(data = predicted_df_1$pred, reference = predicted_df_1$real)
            metrics_details(cm, predicted_df_1$pred, predicted_df_1$real)
        })
        #############MODEL 2###################3
        output$accuracy_2 <- renderValueBox({
            valueBox(
                paste0(round(100*(Metrics::accuracy(real$x, pred_2$x)),2), "%"), "Accuracy",icon= icon("balance-scale"),
                color = "purple"
            )
        })
        output$AUC_2 <- renderValueBox({
            valueBox(
                paste0(round(Metrics::auc(real$x, pred_2$x),2)), "AUC", icon= icon("balance-scale"),
                color = "blue"
            )
        })
        ##Model 2 confusion matrix
        output$plot2 <- renderPlot({
            cm <- conf_mat(predicted_df_2, real, pred)
            draw_confusion_matrix(cm)
        })
        
        ##Model 1 ROC and PRC
        output$roc_2 <- renderPlot({
            roc(pred_2$x, real$x)
        })
        
        output$prc_2 <- renderPlot({
            prc(pred_2$x, real$x)
        })
        
        
       #Model 2 details metrics
        output$details_mod_2 <- renderDataTable({
            cm <- confusionMatrix(data = predicted_df_2$pred, reference = predicted_df_2$real)
            metrics_details(cm, predicted_df_2$pred, predicted_df_2$real)
        })
    })
    
    
    
    
    
    
}

shinyApp(ui, server)
