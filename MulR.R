rm(list=ls())

setwd("C:/Users/VIVEK KUMAR/Desktop/Project")


# Data preprocessing


student_mat <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/student/student_mat.csv"))
student_por <- read.csv(file.choose("C:/Users/VIVEK KUMAR/Desktop/NCI/Project/student/student_por.csv"))
student = merge(student_mat,student_por, all=TRUE)


#checking null values in dataset
is.null(student)
# no null variables present in the student

# Encoding categorical data 
student$school = factor(student$school,
                        levels = c('GP', 'MS'),
                        labels = c(1,2) )

student$sex = factor(student$sex,
                     levels= c('M', 'F'),
                     labels= c('1', '2') )

student$address = factor(student$address,
                         levels = c('U', 'R'),
                         labels = c(1,2) )

student$famsize = factor(student$famsize,
                         levels= c('LE3', 'GT3'),
                         labels= c('1', '2') )


student$Pstatus = factor(student$Pstatus,
                         levels= c('A', 'T'),
                         labels= c('1', '2') )

student$Mjob = factor(student$Mjob,
                      levels= c('at_home', 'health', 'other', 'services', 'teacher'),
                      labels= c('1', '2', '3', '4', '5') )

student$Fjob = factor(student$Fjob,
                      levels= c('at_home', 'health', 'other', 'services', 'teacher'),
                      labels= c('1', '2', '3', '4', '5') )

student$reason = factor(student$reason,
                        levels= c('course', 'home','other','reputation'),
                        labels= c('1', '2','3','4') )

student$guardian = factor(student$guardian,
                          levels= c('father', 'mother','other'),
                          labels= c('1', '2','3') )


student$schoolsup = factor(student$schoolsup,
                           levels= c('yes', 'no'),
                           labels= c('1', '0') )

student$famsup = factor(student$famsup,
                        levels = c('yes', 'no'),
                        labels = c('1', '0') ) 


student$paid = factor(student$paid,
                      levels= c('yes', 'no'),
                      labels= c('1', '0') )

student$activities = factor(student$activities,
                            levels= c('yes', 'no'),
                            labels= c('1', '0') )

student$nursery = factor(student$nursery,
                         levels= c('yes', 'no'),
                         labels= c('1', '0') )

student$higher = factor(student$higher,
                        levels= c('yes', 'no'),
                        labels= c('1', '0') )

student$internet = factor(student$internet,
                          levels= c('yes', 'no'),
                          labels= c('1', '0') )

student$romantic = factor(student$romantic,
                          levels= c('yes', 'no'),
                          labels= c('1', '0') )


#student Splitting 
#install.packages('caTools')  
library(caTools)
set.seed(123)
split = sample.split(student$G3, SplitRatio = 0.8)
train = subset(student, split == TRUE)
test = subset(student, split == FALSE) 

#Fitting Multiple Regression Model
MulR = lm(formula = G3 ~ ., data = train)
summary(MulR)

#Predicting test results
y_pred  = predict(MulR, newdata = test)


plot(MulR)
y_pred

#Backward & Forward Elimination
Elimination = lm(formula = G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob
              + reason + guardian + traveltime + studytime + failures + schoolsup + famsup + paid 
              + activities + nursery + higher + internet + romantic + famrel + freetime + goout
              + Dalc + Walc + health + absences, data = student )
summary(Elimination)                                 

back = step(object = Elimination, direction = "backward")

ford = step(object = Elimination, direction = "forward")

