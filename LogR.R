rm(list=ls())

setwd("C:/Users/VIVEK KUMAR/Desktop/Project")


# Data preprocessing
student_mat <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/dataset/student_mat.csv"))
student_por <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/dataset/student_por.csv"))
dataset = merge(student_mat,student_por, all=TRUE)

set.seed(10)
# Create Training Data
dataset$high <- ifelse(dataset$G3>=12,1,0)
dataset$high <- as.factor(dataset$high)
dataset<- dataset[,c(1:32,34)]

dataset$schoolsup = factor(dataset$schoolsup,
                           levels= c('yes', 'no'),
                           labels= c('1', '0') )
dataset$famsup = factor(dataset$famsup,
                        levels = c('yes', 'no'),
                        labels = c('1', '0') ) 
dataset$paid = factor(dataset$paid,
                      levels= c('yes', 'no'),
                      labels= c('1', '0') )

dataset$activities = factor(dataset$activities,
                            levels= c('yes', 'no'),
                            labels= c('1', '0') )
dataset$nursery = factor(dataset$nursery,
                         levels= c('yes', 'no'),
                         labels= c('1', '0') )

dataset$higher = factor(dataset$higher,
                        levels= c('yes', 'no'),
                        labels= c('1', '0') )

dataset$internet = factor(dataset$internet,
                          levels= c('yes', 'no'),
                          labels= c('1', '0') )

dataset$romantic = factor(dataset$romantic,
                          levels= c('yes', 'no'),
                          labels= c('1', '0') )


#split Data
dataset_train <- sample(1:nrow(dataset), 0.9*nrow(dataset))
training_data <- dataset[dataset_train, ]


test <- -dataset_train
testing_data <- dataset[test,]

# Build Logit Models and Predict
logis_mod <- glm(high~.,data=training_data,family = binomial)
summary(logis_mod)

#Checking Accuracy Levels
predict_lgR <- predict(logis_mod,newdata=testing_data,type="response")
predict_lgR <- round(predict_lgR)
mean(predict_lgR==testing_data$high)

