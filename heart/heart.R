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

# Continous variables -----------------------------------------------------

# cor(heart[cont_names]) %>%
#   round(.,2)


# ggpairs(data = heart[cont_names], mapping = aes(color = as.factor(heart$condition))) +
#   labs(title = "Correlação entre as variáveis contínuas")


mean = list()
for (i in 1:5){
  mean[[i]] = heart[, c(cont_names[i], "condition")] %>%
    group_by(condition) %>%
    summarise(mean = mean(!!sym(cont_names[i])))
}
mean = set_names(mean, cont_names)


p1 = list()

for(i in 1:5){
    
    p1[[i]] =
      ggplot(heart[cont_names])  +
      geom_density(aes(x = heart[cont_names][[i]], fill = factor(heart$condition, labels=c("Negativo","Positivo"))), alpha = 0.7) +
      scale_x_continuous(breaks = unlist(mean[[i]][2]), label = round(unlist(mean[[i]][2]), 2)) +
      geom_segment(aes(x = unlist(mean[[i]][2])[1], y = -Inf, xend = unlist(mean[[i]][2])[1], yend = Inf),
                   color = 2, linetype = 'dashed', size = 1) +
      geom_segment(aes(x = unlist(mean[[i]][2])[2], y = -Inf, xend = unlist(mean[[i]][2])[2], yend = Inf),
                   color = 4, linetype = 'dashed', size = 1) +
      labs(title = paste("Distribuição de", cont_names[i]),
           x = "",
           y = "",
           fill = "Doença") 
    
      
}

p2 = map(2:5, 
     ~ ggplot(heart[cont_names], aes(x = heart[cont_names][[1]], y = heart[cont_names][[.x]])) +
      geom_point(aes(color = as.factor(heart$condition)), size = 2) +
      theme(legend.position = "none") +
      labs(
        title = paste("Correlação =", round(cor(heart[cont_names][[1]], heart[cont_names][[.x]]), 2)),
        x = cont_names[1],
        y = cont_names[[.x]]
      )
)



#Categorical variables ---------------------------------------------------

summary(heart %>%
  mutate_at(factors_names, ~factor(.)) %>%
    select_if(is.factor)
    )  #----> nenhum fator possui nivel unico


p3 = map(factors_names, 
  ~heart %>%
    group_by(condition) %>%
    count(eval(as.name(.x))) %>%
    rename(.x = `eval(as.name(.x))`) %>%
    ggplot(aes(x = factor(condition), y = n, fill = factor(.x), label = n)) + 
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), colour = "white") +
    labs(title = paste(.x, "em Doença"), x = "Doença", y = "", fill = .x)
)



p4 = map(factors_names,
         ~ heart %>%
            group_by(eval(as.name(.x))) %>%
            count(condition) %>%
            rename(.x = `eval(as.name(.x))`) %>%
            mutate(.x = factor(.x), condition = factor(condition)) %>%
            ggplot(aes(x = .x, y = n, fill = condition, label = n)) + 
            geom_bar(stat = "identity") +
            geom_text(size = 3, position = position_stack(vjust = 0.5), colour = "white") +
            labs(title = paste("Doença em", .x),
                 x = .x,
                 y = "",
                 fill = "Doença")
)

p5 = c(p3, p4)
  

entropy = map_dfc(heart[factors_names],
     ~ mutinformation(.x, Y)
)

cont_discretized <- discretize(X[cont_names], nbins=3)

map_dfc(cont_discretized[cont_names],
        ~ mutinformation(.x, Y)
)


glm = glm(condition ~., data = heart)%>%
  summary() %>% .$coefficients

names(which(glm[,4] < 0.1))

# library(corrr)
# heart[,-14] %>%
#   correlate() %>%
#   rearrange() %>%
#   shave() %>%
#   rplot(shape = 15, colours = c("darkred", "white", "darkblue")) 



# PCA ---------------------------------------------------------------------

#factor_new = heart[factors_names] + rnorm(0, sd = 0.001, n = nrow(heart))

X_scaled = apply(X, 2, function(col) { (col-mean(col))/sd(col)})  # X_scaled = scale(X) 

scaled.cov = cov(X_scaled)
E = eigen(scaled.cov)
colnames(E$vectors) = paste0("PC", 1:length(colnames(X)))
rownames(E$vectors) = names(X)

scores = X_scaled %*% E$vectors
plot(scores, col = as.factor(Y), pch=16)

r = cumsum(E$values / sum(E$values))
plot(r, pch =16, xlab = "PC", ylab = "% de variância explicada")
id = which(r >= 0.9)[1]
abline(v = id, h = 0.9, col = 2)

X.reduzida = scores[,1:id]

## dúvidas:
cor(E$vectors) %>% round(., 2)
cor(scores) %>% round(., 2)  # loadings são ortogonais

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

dataset = X_scaled
dataset = X.reduzida
train.size = 0.8

# Conjunto de Treinamento
ids = sample(1:nrow(dataset), size = ceiling(train.size*nrow(dataset)))  # Sortear
X.train = dataset[ids,]
Y.train = Y[ids]

# Conjunto de Teste
X.test = dataset[-ids,]
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

tab_hold_out
colnames(tab_hold_out) = "Taxa de acerto"
rownames(tab_hold_out) = paste0(1:10, "-nn")

# K-Fold Cross Validation -------------------------------------------------

data = cbind(X_scaled, Y)
data = cbind(X.reduzida,Y) #PCA

table = matrix(0, 6, 10)
for(i in 5:10){

  cat("Running for k =", i, "folds\n")
  folds = prepare_folds(data = data, colX = 1:10, colY = 11, k_fold = i)

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

max(tab_kfold)












