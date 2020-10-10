library(tidyverse)
library(GGally)
library(shiny)
library(infotheo)

theme_set(theme_light())


heart<- read_csv("heart_cleveland.csv")

glimpse(heart)


Y <-  pull(heart, condition)
factors_names <-  c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
cont_names <- names(heart)[!names(heart) %in% c(factors_names, "condition")]
X <-  heart[,-14]

summary(factor(Y))

# Logistic Regression ----------------------------------------------------------

glm = glm(condition ~., data = heart)%>%
  summary() %>% .$coefficients

names(which(glm[,4] < 0.1))

library(tidymodels)
logistic_reg() %&gt;%
  set_engine("glm") %&gt;%
  set_mode("classification") %&gt;%
  translate()




X_scaled = apply(X, 2, function(col) { (col-mean(col))/sd(col)})  
dataset = X_scaled

# Functions --------------------------------------------------------------------

# Cross validation sampling
prepare_folds <- function(data, colX, colY, k_fold = 10){

  require(dismo)

  # Lista com comprimento de K
  X.folds = rep(list(NA), k_fold)
  Y.folds = rep(list(NA), k_fold)

  classes = unique(data[, colY])
  # Armazenar observações pertencentes ao k-ésimo fold
  for (i in seq_along(classes)) {

    ids = which(data[, colY] == classes[i])
    foldId = kfold(ids, k = k_fold)

    for (j in seq_along(1:k_fold)) {
      if (any(is.na(X.folds[[j]]))) {
        X.folds[[j]] = as.matrix(data[ids[which(foldId == j)], seq_along(colX)])
        Y.folds[[j]] = as.vector(data[ids[which(foldId == j)], colY])
      } else {
        X.folds[[j]] =
          rbind(X.folds[[j]],
                as.matrix(data[ids[which(foldId == j)], seq_along(colX)]))
        Y.folds[[j]] = c(Y.folds[[j]],
                         as.vector(data[ids[which(foldId == j)], colY]))
      }
    }
  }

  return(list(X.folds = X.folds, Y.folds = Y.folds))
}

# Implement knn
run_k_knn <- function(X.folds, Y.folds, k_knn = 3){

  k_fold = length(folds$X.folds)
  acc_train = acc_test = c()

  for (i in 1:k_fold) {
    X.train = NULL
    Y.train = c()

    # Treinar com todos os folds exceto o fold i
    for (j in setdiff(1:k_fold, i)) {
      X.train = rbind(X.train, X.folds[[j]])
      Y.train = c(Y.train, Y.folds[[j]])
    }

    correct_train = 0
    for (j in 1:nrow(X.train)) {
      x = X.train[j,]
      y = knn(query = x, k = k_knn, X = X.train, Y = Y.train)$max.prob.class
      if (y == Y.train[j]) {
        correct_train = correct_train + 1
      }
    }
    acc_train[i] = correct_train / nrow(X.train)

    # Testar com o fold = i
    correct_test = 0
    for (j in 1:nrow(X.folds[[i]])) {
      x = X.folds[[i]][j, ]
      y = knn(query = x, k = k_knn, X = X.train, Y = Y.train)$max.prob.class
      if (y == Y.folds[[i]][j]) {
        correct_test = correct_test + 1
      }
    }
    acc_test[i] = correct_test / nrow(X.folds[[i]])
  }

  
  return(acc = list(
    acc_train = acc_train, 
    acc_test = acc_test,
    mean_acc_train = mean(acc_train),
    sd_acc_train = sd(acc_train),
    mean_acc_test = mean(acc_test),
    sd_acc_test = sd(acc_test)
    )
  )
}

# knn algoritm
knn <- function(query, k, X, Y) {
  E = apply(X, 1, function(row) { sqrt(sum((row - query)^2)) })
  row.ids = sort.list(E,dec=F)[1:k]
  classes = unique(Y)
  count = rep(0, length(classes))
  i = 1
  for (class in classes) {
    count[i] = sum(class == Y[row.ids])
    i = i + 1
  }
  ret = list()
  ret$classes = classes
  ret$count = count
  ret$max.prob.class = classes[which.max(count)]
  return (ret)
}



# 10-Fold Cross Validation -----------------------------------------------------

data = cbind(X_scaled, Y)

folds = prepare_folds(data = data, colX = 1:13, colY = 14, k_fold = 10)
tab_10fold = matrix(0, 10, 4)
for(i in 1:10){
    res = run_k_knn(X.folds = folds$X.folds, Y.folds = folds$Y.folds, k_knn = i) 
    tab_10fold[i,] = unlist(res[3:6])
    cat(i, "-nn =", tab_10fold[i,], "\n")
}
rownames(tab_10fold) = paste0(1:10, "-nn")
colnames(tab_10fold) = c("Precisão média (treino)", "Desvio padrão (treino)", "Precisão média (teste)", "Desvio padrão (teste)")
tab_10fold

