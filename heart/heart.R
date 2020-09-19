library(tidyverse)
library(readr)
library(GGally)
library(rlang)
library(shiny)
library(infotheo)

theme_set(theme_light())


heart<- read_csv("heart_cleveland.csv")

glimpse(heart)


Y <-  pull(heart, condition)
factors_names <-  c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
cont_names <- names(heart)[!names(heart) %in% c(factors_names, "condition")]
X <-  heart[,-14]



# Continous variables -----------------------------------------------------

cor(heart[cont_names]) %>%
  round(.,2)


ggpairs(data = heart[cont_names], mapping = aes(color = as.factor(heart$condition))) +
  labs(title = "Correlação entre as variáveis contínuas")


map(2:5, 
     ~ ggplot(heart[cont_names], aes(x = heart[cont_names][[1]], y = heart[cont_names][[.x]])) +
      geom_point(aes(color = as.factor(heart$condition)), size = 2) +
      theme(legend.position = "none") +
      labs(
        title = paste("Correlação =", round(cor(heart[cont_names][[1]], heart[cont_names][[.x]]), 2)),
        x = cont_names[1],
        y = cont_names[[.x]]
      )
)



# Categorical variables ---------------------------------------------------

summary(heart %>%
  mutate_at(factors_names, ~factor(.)) %>%
    select_if(is.factor)
    )


map2(heart[factors_names], factors_names,
    ~ ggplot() + geom_bar(aes(x = as.factor(Y), fill = as.factor(.x))) +
      labs(title = .y, x = "Condition", y = "", fill = .y)
)

map2(heart[factors_names], factors_names,
     ~ ggplot(heart) +
       geom_bar(aes(x = as.factor(.x), fill = as.factor(heart$condition))) +
       labs(title = .y,
            x = "",
            y = "") +
       theme(legend.position = "none")
)


map_dfc(heart[factors_names],
     ~ mutinformation(.x, Y)
)

cont_discretized <- discretize(X[cont_names], nbins=3)

map_dfc(cont_discretized[cont_names],
        ~ mutinformation(.x, Y)
)



# PCA ---------------------------------------------------------------------

#factor_new = heart[factors_names] + rnorm(0, sd = 0.001, n = nrow(heart))

#cov(heart[factors_names])
# X = heart[-14]
# scaled = apply(X, 2, function(col) { (col-mean(col))/sd(col)})
# 
# scaled.cov = cov(scaled)
# E = eigen(scaled.cov)
# 
# 
# print(sum(E$values))
# 
# scores = scaled %*% E$vectors
# plot(scores, col = as.factor(Y), pch=16)
# 
# # Posso usar da relevância doa auto-valores para escolher quais PCs
# # são os mais importantes
# plot(cumsum(E$values / sum(E$values)))
# r = cumsum(E$values / sum(E$values))
# id = which(r >= 0.9)[1]
# 
# X.reduzida = scores[,1:id]
# plot(X.reduzida, col= as.factor(Y), pch =16)
# 
# colnames(E$vectors) = paste0("PC", 1:length(colnames(X)))
# rownames(E$vectors) = names(X)
# 
# df= as.data.frame.matrix(E$vectors)
# cor(df)
# 
# apply(normalized, 2, range)


# KNN ---------------------------------------------------------------------

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

test_k_knn <- function(X.folds, Y.folds, k_knn = 3){
  
  k_fold = length(folds$X.folds)
  acc = c()
  
  for (i in 1:k_fold) {
    X.train = NULL
    Y.train = c()
    
    # Treinar com todos os folds exceto o fold i
    for (j in setdiff(1:k_fold, i)) {
      X.train = rbind(X.train, X.folds[[j]])
      Y.train = c(Y.train, Y.folds[[j]])
    }
    
    # Testar com o fold = i
    correct = 0
    for (j in 1:nrow(X.folds[[i]])) {
      x = X.folds[[i]][j, ]
      y = knn(query = x, k = k_knn, X = X.train, Y = Y.train)$max.prob.class
      if (y == Y.folds[[i]][j]) {
        correct = correct + 1
      }
    }
    acc[i] = correct / nrow(X.folds[[i]])
  }
  return(acc)
}

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

# Hold-out ----------------------------------------------------------------

X_scaled = scale(X)
train.size = 0.8

# Conjunto de Treinamento
ids = sample(1:nrow(X), size = ceiling(train.size*nrow(X_scaled)))  # Sortear
X.train = X_scaled[ids,]	
Y.train = Y[ids]	

# Conjunto de Teste
X.test = X_scaled[-ids,]
Y.test = Y[-ids]

tab_hold_out = matrix(0, nrow = 10, ncol = 1)
for(k in 1:10){
  cat("Running for k =",k,"\n")
  acc = 0 
  for (i in 1:nrow(X.test)){
    pred = knn(query = X.test[i,], k = k, X = X.train, Y = Y.train)
    if (pred$max.prob.class == Y.test[i]){
      acc = acc + 1
    }
    taxa.acerto = acc/nrow(X.test)
  }
    tab_hold_out[k, 1] = taxa.acerto
} 


colnames(tab_hold_out) = "Taxa de acerto"
rownames(tab_hold_out) = paste0(1:10, "-nn")

# K-Fold Cross Validation -------------------------------------------------


data = cbind(as.matrix(X_scaled),Y)

table = matrix(0, 6, 10)
for(i in 5:10){
  
  cat("Running for k =", i, "folds\n")
  folds = prepare_folds(data = data, colX = 1:13, colY = 14, k_fold = 5)
  
  res = c()
  for(j in 1:10){
    res[j] = test_k_knn(X.folds = folds$X.folds, Y.folds = folds$Y.folds, k_knn = j) %>%
      mean()
    cat("k_knn =", j, res[j], "\n")
    table[i-4,j] = res[j]
  }
  
}

tab_kfold = t(table)
colnames(tab_kfold) = paste(5:10, "folds")
rownames(tab_kfold) = paste0(1:10, "-nn")














