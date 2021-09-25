rm(list=ls())

setwd("C:/Users/VIVEK KUMAR/Desktop/Project")

# Data preprocessing
student_mat <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/Dataset/student_mat.csv"))
student_por <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/Dataset/student_por.csv"))
dataset = merge(student_mat,student_por, all=TRUE)

# no null variables present in the dataset

# Encoding categorical data 
dataset$school = factor(dataset$school,
                        levels = c('GP', 'MS'),
                        labels = c(1,2) )

dataset$sex = factor(dataset$sex,
                     levels= c('M', 'F'),
                     labels= c('1', '2') )

dataset$address = factor(dataset$address,
                         levels = c('U', 'R'),
                         labels = c(1,2) )

dataset$famsize = factor(dataset$famsize,
                         levels= c('LE3', 'GT3'),
                         labels= c('1', '2') )


dataset$Pstatus = factor(dataset$Pstatus,
                         levels= c('A', 'T'),
                         labels= c('1', '2') )

dataset$Mjob = factor(dataset$Mjob,
                      levels= c('at_home', 'health', 'other', 'services', 'teacher'),
                      labels= c('1', '2', '3', '4', '5') )

dataset$Fjob = factor(dataset$Fjob,
                      levels= c('at_home', 'health', 'other', 'services', 'teacher'),
                      labels= c('1', '2', '3', '4', '5') )

dataset$reason = factor(dataset$reason,
                        levels= c('course', 'home','other','reputation'),
                        labels= c('1', '2','3','4') )

dataset$guardian = factor(dataset$guardian,
                          levels= c('father', 'mother','other'),
                          labels= c('1', '2','3') )


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
dataset$G3 = factor(dataset$G3,
                    levels= c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                    labels= c('Fail','Fail','Fail','Fail','Fail','Fail','Fail','Fail','Fail','Pass','Pass','Pass','Pass','Pass','Pass','Pass','Pass','Pass','Pass','Pass','Pass'))


#Splitting into training set and test set
#install.packages('caTools')  
library(caTools)
set.seed(123)
split = sample.split(dataset$G3, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE) 


#Implementing kNN 
#install.packages("class")
library(class)
y_pred = knn(train = training_set[, c(2, 3, 15, 16, 25, 26, 29, 30)],
             test= test_set[, c(2, 3, 15, 16, 25, 26, 29, 30)],
             cl = training_set[, 31],
             k = 5)


y_pred_step = knn(train = training_set[, c(2, 3, 5, 7, 9, 14, 15, 16, 17, 23, 25, 26, 30)],
                  test= test_set[, c(2, 3, 5, 7, 9, 14, 15, 16, 17, 23, 25, 26, 30)],
                  cl = training_set[, 31],
                  k = 5)

#Checking Accuracy Levels
cm = table(test_set[,31], y_pred)
cm_step = table(test_set[, 31], y_pred_step)

round(cm)
mean(cm)
