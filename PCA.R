rm(list=ls())

setwd("C:/Users/VIVEK KUMAR/Desktop/Project")


# Data preprocessing
student_mat <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/Dataset/student_mat.csv"))
student_por <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/Dataset/student_por.csv"))
dataset = merge(student_mat,student_por, all=TRUE)


#install.packages("dummies")
library(dummies)

student = data.frame(dataset)

student_new = dummy.data.frame(student, sep = ".")

student_new[-57] = scale(student_new[-57])


#Splitting into training set and test set
#install.packages('caTools')  
library(caTools)
set.seed(123)
split = sample.split(student_new$G3, SplitRatio = 0.8)
training_set = subset(student_new, split == TRUE)
test_set = subset(student_new, split == FALSE) 



# PCA 
library(caret)
library(e1071)


prin_comp = prcomp(training_set[-57], scale. = T)
names(prin_comp)
biplot(prin_comp, scale = 0)
std_dev = prin_comp$sdev
pr_var= std_dev^2
pr_var[1:20]
prop_varex = pr_var/sum(pr_var)
prop_varex[1:20]
#scree plot
plot(prop_varex, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

prin_comp

summary(prin_comp)


#creating new training and test set

training_set_pca = predict(prin_comp, training_set)
training_set_pca = data.frame(training_set_pca)
training_set_pca$G3 = training_set$G3


test_set_pca = predict(prin_comp, test_set)
test_set_pca = data.frame(test_set_pca) 
test_set_pca$G3 = test_set$G3
test_set_pca

training_set_pca$G3 <- ifelse(training_set_pca$G3>=1,1,0)

#Applying logistic regression
classifier = glm(formula = G3 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
                 family = binomial, 
                 data = training_set_pca)

prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-57])
prob_pred

y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

test_set_pca$G3 <- ifelse(test_set_pca$G3>=1,1,0)

predict_log <- predict(classifier,newdata=test_set_pca,type="response")
predict_log <- round(predict_log)
mean(predict_log==test_set_pca$G3)

