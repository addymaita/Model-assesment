draw_confusion_matrix <- function(cm) {
  autoplot(cm, type = "heatmap") +
    scale_fill_gradient(low="#D6EAF8",high = "#2E86C1", trans = "log")+
    theme(text=element_text(size=15))

}  

metrics_details <- function(cm, pred, test){
  mcc <- round(mltools::mcc(pred, test),3)
  auc <- round(Metrics::auc(pred, test),3)
  my.options <- list(autoWidth = FALSE,
                     searching = FALSE,
                     ordering = FALSE,
                     lengthChange = FALSE,
                     lengthMenu = FALSE,
                     pageLength = FALSE,
                     paging = FALSE,
                     info = FALSE)
  DT = data.table(
    metrics =  c(names(cm$overall[1]),names(cm$overall[2]),names(cm$byClass[1]),names(cm$byClass[2]),names(cm$byClass[5]), names(cm$byClass[6]),names(cm$byClass[7]), "MCC", "AUC"),
    value = c(round(100*(as.numeric(cm$overall[1])),3),round(as.numeric(cm$overall[2]), 3),round(as.numeric(cm$byClass[1]),3),round(as.numeric(cm$byClass[2]),3),round(as.numeric(cm$byClass[5]),3),round(as.numeric(cm$byClass[6]),3),round(as.numeric(cm$byClass[7]),3), mcc, auc)
  )
  
  
  DT
}

roc <- function(pred, test){
  precrec_obj <- evalmod(scores = pred, labels = test)
  plot <- autoplot(precrec_obj, curvetype = c("ROC")) + theme(text=element_text(size=15)) +
  ggtitle("ROC Curve")
  plot
    
}

prc <- function(pred, test){
  precrec_obj <- evalmod(scores = pred, labels = test)
  plot <- autoplot(precrec_obj, curvetype = c("PRC")) + theme(text=element_text(size=15)) +
  ggtitle("PRC Curve")
  plot
}