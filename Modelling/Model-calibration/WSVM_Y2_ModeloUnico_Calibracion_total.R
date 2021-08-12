library(gmodels) 
library(caret) 
library(broom) 
library(pROC) 
library(dplyr)
library(ggplot2)
library(rsample) 
library(gridExtra)
library(ranger)
library(forcats)
library(doParallel)
library(Rmisc)
library(Hmisc)

Dane_paramodelos <- read.csv("Dane_paramodelos.csv", header = T, sep = ",", dec = ".", stringsAsFactors = T)
Dane_paramodelos <- Dane_paramodelos %>% select(-c(1,2,17, 16, 13, 14,18))

Dane_paramodelos$Y_modelo2 <- fct_collapse(Dane_paramodelos$Y_modelo2, "Desocupado.Informal"="Desocupado/Informal" )

set.seed(1)
set_rows <- sample(nrow(Dane_paramodelos),nrow(Dane_paramodelos)*0.85, replace=FALSE, prob=NULL )
data_train <- Dane_paramodelos[set_rows,]
data_test <- Dane_paramodelos[-(set_rows),]

set.seed(1)
set_rows <- sample(nrow(data_train),nrow(data_train)*0.90, replace=FALSE, prob=NULL )
data_train <- data_train[set_rows,]
data_cv <- data_train[-(set_rows),]


prob_cv <- read.csv("vector_calibration_set_GENERAL.csv", header = T, dec = ".")
tabla <- cbind(prob_cv$x,data_cv$Y_modelo2)

dataframe <- tabla
colnames(dataframe)<-c("x","y")
dataframe <- as.data.frame(dataframe)
dataframe$y <- ifelse(dataframe$y==1,0,1)
model_log_general<-glm(y~x,data = dataframe, family = binomial)
saveRDS(model_log_general, file= "model.log_WSVM_Y2_General.rds")


#------------------------------------ 10 BINS

pdf(file = "UnicoModelo_CalPlot_WSVM_Y2.pdf")


prob_test <- read.csv("vector_test_set_GENERAL.csv", header = T, dec = ".")
dataframe1<-data.frame(prob_test$x)
colnames(dataframe1)<-c("x")
prob_calibrated<-predict(model_log_general,dataframe1,type="response")

base_total_test <- cbind(data_test$Y_modelo2, prob_calibrated, prob_test$x)

colnames(base_total_test) <- c("y", "Platt.scaling", "Without.calibration")
dataframe <- base_total_test
dataframe <- as_tibble(dataframe)
dataframe$y <- ifelse(dataframe$y==2, 0, 1) #Ocupado igual a 0
dataframe$y <- as.factor(dataframe$y)


platt_plot <- caret::calibration(y~Platt.scaling + Without.calibration, data=dataframe, cuts = 10)
platt_plot$data
plotTheme <- simpleTheme(col=c("firebrick3", "slategray"),lty=c("solid", "dotted"))
plot(platt_plot, type = "b", par.settings=plotTheme, auto.key = list(columns = 2,
                                                                     lines = FALSE,
                                                                     points = TRUE),
     xlim=c(0,100),
     xlab="Bin Midpoint (%)",
     ylab="Observed fraction (%)",
     col=c("firebrick3", "slategray"), 
     lty=c("solid", "dotted"),
     main=paste("Calibration plot general"))

ggplot(platt_plot, aes(color=model)) + geom_line(aes(linetype=platt_plot$data$calibModelVar)) + 
      scale_x_continuous(limits = c(0, 100)) + theme_minimal() + scale_color_manual(name="Models", values = c("firebrick3", "slategray")) + 
      ggtitle("Platt Scaling Calibration Plot") + scale_linetype(name="Models") +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, face="bold", color = "black"))


#------------------------------------ 5 BINS


platt_plot <- caret::calibration(y~Platt.scaling + Without.calibration, data=dataframe, cuts = 5)
platt_plot$data
plotTheme <- simpleTheme(col=c("firebrick3", "slategray"),lty=c("solid", "dotted"))
plot(platt_plot, type = "b", par.settings=plotTheme, auto.key = list(columns = 2,
                                                                     lines = FALSE,
                                                                     points = TRUE),
     xlim=c(0,100),
     xlab="Bin Midpoint (%)",
     ylab="Observed fraction (%)",
     col=c("firebrick3", "slategray"), 
     lty=c("solid", "dotted"),
     main=paste("Calibration plot general"))

ggplot(platt_plot, aes(color=model)) + geom_line(aes(linetype=platt_plot$data$calibModelVar)) + 
      scale_x_continuous(limits = c(0, 100)) + theme_minimal() + scale_color_manual(name="Models", values = c("firebrick3", "slategray")) + 
      ggtitle("Platt Scaling Calibration Plot") + scale_linetype(name="Models") +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, face="bold", color = "black"))


#------------------------------------ Hist od¿f probabilities

dataframe2 <- as.data.frame(prob_calibrated)
colnames(dataframe2) <- c("x")
hist_dept_platt <- ggplot(dataframe2, aes(x=x, fill=x)) + geom_histogram(bins=50, fill="slategray") + 
      theme_minimal() + 
      ggtitle(paste0("prob. dist. Platt Scaling general")) + 
      xlab("Probability") + 
      ylab("Frequency") + 
      theme(plot.title = element_text(face="bold", hjust = 0.5))

dataframe3 <- as.data.frame(prob_test$x)
colnames(dataframe3) <- c("x")
hist_dept <- ggplot(dataframe3, aes(x=x, fill=x)) + geom_histogram(bins=30, fill="slategray") + 
      theme_minimal() + 
      ggtitle("prob. dist. general") + 
      xlab("Probability") + 
      ylab("Frequency") + 
      theme(plot.title = element_text(face="bold", hjust = 0.5))


grid.arrange(hist_dept, hist_dept_platt, nrow=1)


dev.off()
   




