# Continous variables -----------------------------------------------------

# cor(heart[cont_names]) %>%
#   round(.,2)

# ggpairs(data = heart[cont_names], mapping = aes(color = as.factor(heart$condition))) +
#   labs(title = "Correlação entre as variáveis contínuas")



# PCA ---------------------------------------------------------------------

# factor_new = heart[factors_names] + rnorm(0, sd = 0.001, n = nrow(heart))

# X_scaled = apply(X, 2, function(col) { (col-mean(col))/sd(col)})  # X_scaled = scale(X)

# scaled.cov = cov(X_scaled)
# E = eigen(scaled.cov)
# colnames(E$vectors) = paste0("PC", 1:length(colnames(X)))
# rownames(E$vectors) = names(X)

# scores = X_scaled %*% E$vectors
# plot(scores, col = as.factor(Y), pch=16)

# r = cumsum(E$values / sum(E$values))
# plot(r, pch =16, xlab = "PC", ylab = "% de variância explicada")
# id = which(r >= 0.9)[1]
# abline(v = id, h = 0.9, col = 2)

# X.reduzida = scores[,1:id]

# ## dúvidas:
# cor(E$vectors) %>% round(., 2)
# cor(scores) %>% round(., 2)  # loadings são ortogonais

# dataset = X_scaled
# dataset = X.reduzida

# Hold-out ---------------------------------------------------------------------

#   train.size = 0.8

#   # Conjunto de Treinamento
#   ids = sample(1:nrow(dataset), size = ceiling(train.size*nrow(dataset)))  # Sortear
#   X.train = dataset[ids,]
#   Y.train = Y[ids]

#   # Conjunto de Teste
#   X.test = dataset[-ids,]
#   Y.test = Y[-ids]

#   tab_hold_out = matrix(0, nrow = 10, ncol = 1)
#   for(k in 1:10){
#     cat("Running for k =",k,"\n")
#     acc = 0
#     for (i in 1:nrow(X.test)){
#       pred = knn(query = X.test[i,], k = k, X = X.train, Y = Y.train)
#       if (pred$max.prob.class == Y.test[i]){
#         acc = acc + 1
#       }
#       taxa.acerto = acc/nrow(X.test)
#     }
#       tab_hold_out[k, 1] = taxa.acerto
#   }

#   tab_hold_out
#   colnames(tab_hold_out) = "Taxa de acerto"
#   rownames(tab_hold_out) = paste0(1:10, "-nn")

# K-Fold Cross Validation:  Testando 5-folds to 10-folds------------------------

# data = cbind(X_scaled, Y)
# data = cbind(X.reduzida,Y) #PCA

# table = matrix(0, 6, 10)
# for(i in 5:10){
#   cat("Running for k =", i, "folds\n")
#   folds = prepare_folds(data = data, colX = 1:13, colY = 14, k_fold = i)
#   res = c()
#   for(j in 1:10){
#     res[j] = run_k_knn(X.folds = folds$X.folds, Y.folds = folds$Y.folds, k_knn = j)$mean_acc_test
#     cat("k_knn =", j, res[j], "\n")
#     table[i-4,j] = res[j]
#   }
# }

# tab_kfold = t(table)
# colnames(tab_kfold) = paste(5:10, "folds")
# rownames(tab_kfold) = paste0(1:10, "-nn")
# tab_kfold