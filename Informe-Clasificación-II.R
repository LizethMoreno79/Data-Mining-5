library(readxl)
library(tidyverse)
library(magrittr)
library(e1071)


Data <- read_excel("Base/base.xlsx")
#View(Data)

Data <- Data %>% mutate(impago=factor(impago),
                        educ=factor(educ,ordered = T,labels = c(1,2,3,4,5)),
                        sexo = factor(sexo))
#view(Data)

df <- Data %>% dplyr::filter((is.na(Data$impago)))# Datos para predecir
Data <- Data %>% dplyr::filter(!(is.na(Data$impago)))# Datos sin NA

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <-  1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(Data, 0.8, train = TRUE)
data_test <- create_train_test(Data, 0.8, train = FALSE)

m <- naiveBayes(impago ~ ., data = Data)
## alternativamente:
m <- naiveBayes(Data[,-5], Data[,5])
m

table(predict(m, Data), Data[,5])




library(e1071)
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternativamente:
m <- naiveBayes(iris[,-5], iris[,5])
m



