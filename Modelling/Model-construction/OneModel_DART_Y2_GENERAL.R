# method = "DART"

library(gmodels) 
library(caret) 
library(broom) 
library(pROC) 
library(dplyr)
library(ggplot2)
library(rsample) 
library(xgboost) 
library(e1071)
library(plyr)
library(forcats)
library(foreach)
library(doParallel)
library(Rmisc)

# Retrieve data
Dane_paramodelos <- read.csv("Dane_paramodelos.csv", header = T, sep = ",", dec = ".", stringsAsFactors = TRUE )
Dane_paramodelos <- Dane_paramodelos %>% select(-c(1,2, 17, 16, 13, 14))
Dane_paramodelos$Y_modelo2 <- fct_collapse(Dane_paramodelos$Y_modelo2, "Desocupado.Informal"="Desocupado/Informal" )

# Create tags to identify each departamento according to DANE encoding system
Departamentos <- c("Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas",
                   "Caqueta", "Cauca", "Cesar", "Cordoba", "Cundinamarca", "Choco", 
                   "Huila", "La Guajira", "Magdalena", "Meta", "Nariño", "Norte de Santander",
                   "Quindio", "Risaralda", "Santander", "Sucre", "Tolima", "Valle")
Depto <- c(5, 8,11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76 )

# Create a matrix to store performance metrics
metricas <- matrix(0,1,17)
colnames(metricas) <- c("Accuracy", "Balanced accuracy", "Sensitivity", "Specificity", "Kappa", "AUC", "Threshold", "F_measure", "Nrounds", "Max_depth", 
                        "Eta", "Gamma", "Colsample_bytree", "Min_child_weight", "Subsample", "Rate drop",
                        "Skip drop")

#Create a matrix to store Log-Loss metrics
logloss <- matrix(0,1,2)
colnames(logloss) <- c("LogLoss_No-Calibration", "LogLoss_-Calibration")
rownames(logloss) <- "Modelo general"

#Steps to enable parallel computing with 3 processor cores
cores <- detectCores()
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)



pdf(file = "Model_plots_DART_Y2_GENERAL.pdf")

  
  # Create training set, cross-validation set and test set for each departamento
  set.seed(1)
  set_rows <- sample(nrow(Dane_paramodelos),nrow(Dane_paramodelos)*0.85, replace=FALSE, prob=NULL )
  data_train <- Dane_paramodelos[set_rows,]
  data_test <- Dane_paramodelos[-(set_rows),]
  
  set.seed(1)
  set_rows <- sample(nrow(data_train),nrow(data_train)*0.90, replace=FALSE, prob=NULL )
  data_train <- data_train[set_rows,]
  data_cv <- data_train[-(set_rows),]
  
  factor <- data_train$Factor.de.expansion
  data_train <- data_train%>%select(-12)
  data_test <- data_test%>%select(-12)
  data_cv <- data_cv%>%select(-12)
  
  
  # function to calculate log-loss metric
  LogLoss<-function(act, pred)
  {
    eps = 1e-15;
    nr = length(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(length(act))
    return(ll);
  }
  

  #### Train each model
  trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 4, classProbs = T,
                            search = "grid", summaryFunction = twoClassSummary)
  
  set.seed(1)
  model <- train(Y_modelo2~.,
                 data = data_train,
                 method = "xgbDART",
                 metric = "ROC",
                 trControl = trControl)
  
  summary(model)
  save(model, file =  "One_model_XGB-DART.RData")
  saveRDS(model, file =  "One_model_XGB-DART2.rds")
  graficas <- plot(varImp(model), top=20)
  
  #Predict in cross validated dataset
  prob_cv <-predict(model, data_cv, type = "prob")[,2]
  write.csv(prob_cv, file=paste0("vector_calibration_set_GENERAL.csv"))
  
  # train a log model 
  dataframe<-data.frame(prob_cv, data_cv$Y_modelo2)
  colnames(dataframe)<-c("x","y")
  
  # training a logistic regression model on the cross validation dataset
  model_log<-glm(y~x,data = dataframe, family = binomial)
  
  # Predicting on the test dataset without Platt Scaling and calculate log-loss
  prob<-as.data.frame(predict(model,newdata = data_test,type = "prob"))[,2]
  write.csv(prob, file=paste0("vector_test_set_GENERAL.csv"))
  
  data_test. <- data_test
  data_test.$Y_modelo2 <- as.double(fct_collapse(data_test$Y_modelo2, "1"="Ocupado", "0"="Desocupado.Informal"))
  data_test.$Y_modelo2 <- data_test.$Y_modelo2-1
  
  (logloss_sin_Platt <- LogLoss(data_test.$Y_modelo2, prob))
  
  # Predicting on the test dataset using Platt Scaling
  dataframe1<-data.frame(prob)
  colnames(dataframe1)<-c("x")
  prob_calibrated<-predict(model_log,dataframe1,type="response")
  
  (logloss_con_Platt <- LogLoss(data_test.$Y_modelo2, prob_calibrated))
  
  #Perfomance of predictions
  ROC_modelo <- roc(data_test$Y_modelo2, prob, auc = T, ci = T)
  cRoc<- plot.roc(ROC_modelo, main=paste0("Modelo_Y2_RANGER_ModeloUnico", legacy.axes = T, print.thres = "best", print.auc = TRUE,
                                          auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
                                          col = 4, grid = TRUE))
  
  Rmisc::multiplot(cRoc, graficas, cols = 1)
  
  
  rfThresh_best <- coords(ROC_modelo, x = "best", best.method = "closest.topleft", transpose = T)
  pred <- ifelse(prob >= rfThresh_best[1], "Ocupado", "Desocupado.Informal")
  pred<- factor(pred, levels = c("Desocupado.Informal", "Ocupado"))
  conf <- caret::confusionMatrix(pred, data_test$Y_modelo2, positive="Ocupado", mode="everything")
  
  
  
  #Save performance metrics
  metricas[1,1] <- conf$overall[1]
  metricas[1,2] <- conf$byClass[11]
  metricas[1,3] <- conf$byClass[1]
  metricas[1,4] <- conf$byClass[2]
  metricas[1,5] <- conf$overall[2]
  metricas[1,6] <- auc(ROC_modelo)
  metricas[1,7] <- rfThresh_best[1]
  metricas[1,8] <- conf$byClass[7]
  metricas[1,9] <- model$bestTune$nrounds
  metricas[1,10] <- model$bestTune$max_depth
  metricas[1,11] <- model$bestTune$eta
  metricas[1,12] <- model$bestTune$gamma
  metricas[1,13] <- model$bestTune$colsample_bytree
  metricas[1,14] <- model$bestTune$min_child_weight
  metricas[1,15] <- model$bestTune$subsample
  metricas[1,16] <- model$bestTune$rate_drop
  metricas[1,17] <- model$bestTune$skip_drop
  logloss[1,1] <- logloss_sin_Platt
  logloss[1,2] <- logloss_con_Platt  

  # Write performance metrics matrix and Log-Loss matrix
  write.csv(metricas, file="DART_Y2_GENERAL.csv")
  write.csv(logloss, file="Logloss_DART_Y2_GENERAL.csv")
  multiplot(graficas, cols=1)


dev.off()













