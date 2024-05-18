##########################################################
#
# Mestrado em Engenharia Informática e de Computadores
# MDLE
# 
# Material de apoio
#
#########################################################

require(caret)
require(e1071)

mdle.printConfusionMatrix<-function(sp.pred, method)
{
  
  pred<-sp.pred %>% select('labels', 'prediction')  %>% collect
  pred$prediction<-round(pred$prediction)
  
  cfxmat<-confusionMatrix(table(as.vector(pred$labels),as.vector(pred$prediction)),mode = "everything",positive = "1")
  cat(noquote(paste("Confusion Matrix and Statistics:",method,"\n")))
  print(cfxmat$table) 
  
  cat(noquote(paste("False Positive Rate :",format(round(1-cfxmat$byClass[[3]], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("Accuracy            :",format(round(cfxmat$overall[[1]], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("Kappa               :",format(round(cfxmat$overall[[2]], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("Pos Pred Value      :",format(round(cfxmat$byClass[[3]], 3), nsmall = 3),"\n" )))
  cat(noquote(paste("Neg Pred Value      :",format(round(cfxmat$byClass[[4]], 3), nsmall = 3) )))
  
  
}

mdle.predict<-function(model,test)
{
  return (ml_predict(model,test))
}


oversample_class <- function(df, target_class, target_size) {
  # Filter the target class
  class_df <- df %>% filter(conditions == target_class)
  
  # Calculate the number of repetitions needed
  current_size <- class_df %>% summarise(count = n()) %>% collect() %>% pull(count)
  reps <- ceiling(target_size / current_size)
  
  # Repeat the samples
  oversampled_df <- class_df %>%
    sdf_sample(fraction = target_size/current_size, replace = TRUE)
  
  return(oversampled_df)
}