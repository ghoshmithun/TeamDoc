library(shiny);library(plotly);library(caret);library(e1071);library(dplyr);library(shinydashboard)

# Just update the first two rows here, giving the target and predicted score variables of your dataset
# Rest all computations are automatic.
# Target <- validation.data$purchase_in_target_window
# Prediction <- validation.data$mediannnpred
Target <- as.factor(ifelse(runif(100) > 0.5,1,0))
Prediction <-runif(100)

DF <- data.frame(cbind(Target,Prediction))
DF <- DF %>% arrange(desc(Prediction)) %>% mutate(RowNumber = row_number(),PercRank = percent_rank(desc(RowNumber)))
Accuracy <- rep(NA,99)
Sensitivity <- rep(NA,99)
Specificity <- rep(NA,99)
F1score <- rep(NA,99)
Lift <- rep(NA,99)
Gain <- rep(NA,99)
SubsetPositiveResponders <- rep(NA,99)
SubsetTotalResponders <- rep(NA,99)
AvgResponse <- mean(DF$Target)
SumPositiveResponders <- sum(DF$Target)
CumPositiveResponders <- 0
CumTotalResponders <- 0
for(i in 1:99) {
    Class <- ifelse(Prediction > (i*0.01),1,0)
    levels(Class) <- c(0,1)
    confusionmatrix <- confusionMatrix(data = Class,reference = Target,positive = levels(as.factor(Target))[2])
    Accuracy[i] <- confusionmatrix$overall[[1]]*100
    Sensitivity[i] <- confusionmatrix$byClass[[1]]
    Specificity[i] <- confusionmatrix$byClass[[2]]
    F1score[i] <- confusionmatrix$byClass[[7]]
    Subset <- DF  %>% filter(PercRank > (1-(i*0.01)),PercRank <= (1-((i-1)*0.01)))
    SubsetPositiveResponders[i] <- sum(Subset$Target)
    SubsetTotalResponders[i] <- nrow(Subset)
    CumTotalResponders <- CumTotalResponders + SubsetTotalResponders[i]
    CumPositiveResponders <- CumPositiveResponders + SubsetPositiveResponders[i]
    Lift[i] <- CumPositiveResponders / (AvgResponse*CumTotalResponders)
    Gain[i] <- CumPositiveResponders*100 / SumPositiveResponders
}
plots <- data.frame(cbind(Accuracy,Sensitivity,Specificity,F1score,Lift,Gain))
plots$Cutoff <- seq(0.01,0.99,by=0.01)


ui <- fluidPage(
        tabsetPanel(
        tabPanel(title = "Validation Metrics",
            sliderInput(inputId = "cutoff",label = "Select the cutoff point : ",value = 0.5,min=0.01,max=0.99,step = 0.01),
                verbatimTextOutput(outputId = "classificationtable"),
                verbatimTextOutput(outputId = "validationmetrics")),
        tabPanel(title = "Cutoff Selection Plots",
            box(plotlyOutput(outputId = "Accuracy")) ,
            box(plotlyOutput(outputId = "F1score")),
            box(plotlyOutput(outputId = "sensplusspec"))),
        tabPanel(title = "ROC Curve",
            plotlyOutput(outputId = "ROC",height = "500px",width = "500px")),
        tabPanel(title = "Lift Chart",
            plotlyOutput(outputId = "Liftchart",height = "500px",width = "500px")),
        tabPanel(title = "Gainchart",
            plotlyOutput(outputId = "Gainchart",height = "500px",width = "500px"))
))

server <- function(input,output) {
  
    PredictionClass <- reactive({Pred <- ifelse(Prediction > input$cutoff,1,0) })
    confmat <- reactive({ confusionMatrix(data = PredictionClass() , reference = Target , positive = levels(Target)[2] )   })
    output$classificationtable <- renderPrint({ as.table(confmat()  )})
    sensplusspec <- reactive({ SensPlusSpec <- confmat()$byClass[1] + confmat()$byClass[2] })
    output$validationmetrics <- renderPrint({ c(confmat()$byClass[1],confmat()$byClass[2],confmat()$byClass[3],confmat()$byClass[4],confmat()$byClass[5],confmat()$byClass[6],confmat()$byClass[7],sensplusspec(),confmat()$overall[1])})
   
    # Accuracy plot
    output$Accuracy <- renderPlotly({ ggplotly(ggplot(plots,aes(Cutoff,Accuracy)) + geom_line(color="darkblue") + ggtitle(label = "Overall Accuracy Plot") )  })
    
    # F1score plot
    output$F1score <- renderPlotly({ ggplotly(ggplot(plots,aes(Cutoff,F1score)) + geom_line(color="darkblue") + ggtitle(label = "F1 Score Plot") )   })
    
    # Sensitivity Plus Specificity
    output$sensplusspec <- renderPlotly({ ggplotly(ggplot(plots,aes(Cutoff,Sensitivity + Specificity)) + geom_line(color="darkblue") + ggtitle(label = "Sensitivity Plus Specificity Plot") )  })
  
    
    # ROC Curve
    rocplot <- ggplotly(   ggplot(plots,aes(1-Specificity,Sensitivity))  + geom_line(color="darkblue") + geom_abline(slope = 1,intercept = 0,color="red") + ggtitle(label = "ROC Curve") )
    output$ROC <- renderPlotly({ rocplot})
    
    # Lift Chart
    liftplot <- ggplotly(  ggplot(plots,aes(Cutoff*100,Lift)) + geom_line(color = "darkblue") + geom_hline(yintercept = 1,color="red") + ggtitle(label = "Lift Chart") + xlab(label = "Population Percentile") + ylab(label = "Cumulative Lift") ) 
    output$Liftchart <- renderPlotly({ liftplot })
    
    # Gain Chart
     gainplot <- ggplotly(  ggplot(plots,aes(Cutoff*100,Gain)) + geom_line(color = "darkblue") + geom_hline(yintercept = 0,color="red") + ggtitle(label = "Cumulative Gain Chart") + xlab(label = "Population Percentile") + ylab(label = "Cumulative Gain") )
     output$Gainchart <- renderPlotly({ gainplot })
}

shinyApp(ui,server)
