X_scaled = apply(X, 2, function(col) { (col-mean(col))/sd(col)})  
X_scaled_dummy = X %>% mutate_at(vars(cont_names), ~(. - mean(.))/sd(.))
data_scaled = cbind(X_scaled, Y)
data_scaled_dummy = cbind(X_scaled_dummy, Y)

# 10-Fold Cross Validation -----------------------------------------------------

# ggplot(data = data.frame(data_scaled), aes(x = ldaPred$x, fill = factor(Y))) +
#   geom_histogram(aes(y = stat(density)), color = "white", bins = 50) +
#   scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
#   facet_grid(factor(Y, labels = c("Negativo", "Positivo"))~ . ) 

# Functions --------------------------------------------------------------------

# Cross validation sampling ----------------------------------------------------
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

# Implement knn ----------------------------------------------------------------
run_k_knn <- function(X.folds, Y.folds, k_knn = 3){

  k_fold = length(X.folds)
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

# knn algoritm -----------------------------------------------------------------
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

# Implement Logistic Regression ------------------------------------------------
run_logito <- function(X.folds, Y.folds, threshold = 0.5){
  
  k_fold = length(X.folds)
  acc_train = acc_test = c()
  df = NULL

  for (i in 1:k_fold) {
    X.train = NULL
    Y.train = c()

    # Treinar com todos os folds exceto o fold i
    for (j in setdiff(1:k_fold, i)) {
      X.train = rbind(X.train, X.folds[[j]]) 
      Y.train = c(Y.train, Y.folds[[j]])
    }

    dados_train = data.frame(X.train, condition = Y.train) 
    
    fit = glm(condition ~., family = binomial, data = dados_train) 
    yhat_train = ifelse( fitted(fit)  > threshold, 1, 0)
    acc_train[i] = mean(yhat_train == dados_train[, "condition"])
   
    # Testar com o fold = i
    dados_test = data.frame(X.folds[[i]], condition = Y.folds[[i]])
    fitted_test = predict(fit, newdata = dados_test, type="response")
    yhat_test = ifelse( fitted_test  > threshold, 1, 0)
    acc_test[i] = mean(yhat_test == dados_test[,"condition"])   

    temp = data.frame(
        truth = factor(Y.folds[[i]]), Class1 = 1-fitted_test, Class2 = fitted_test, 
        predicted = factor(yhat_test), fold = i
    )
    
    df = rbind(df, temp)
    
  }

  return(
    list(
    fit = fit,
    model = summary(fit)$coefficients,
    acc_train = acc_train, 
    acc_test = acc_test,
    mean_acc_train = mean(acc_train),
    sd_acc_train = sd(acc_train),
    mean_acc_test = mean(acc_test),
    sd_acc_test = sd(acc_test),
    df = df
    )
  )
}

# Implent Linear Discriminant Analysis -----------------------------------------
run_lda <- function(X.folds, Y.folds, threshold = 0.5){
  
  k_fold = length(X.folds)
  acc_train = acc_test = c()
  df = NULL

  for (i in 1:k_fold) {
    X.train = NULL
    Y.train = c()    

    # Treinar com todos os folds exceto o fold i
    for (j in setdiff(1:k_fold, i)) {
      X.train = rbind(X.train, X.folds[[j]]) 
      Y.train = c(Y.train, Y.folds[[j]])
    }

    dados_train = data.frame(X.train, condition = Y.train)
    
    fit = lda(condition ~., data = dados_train) 
    fitted <- predict(fit)
    yhat_train = ifelse(fitted$posterior[,2] > threshold, 1, 0) %>% as.numeric()
    acc_train[i] = mean(yhat_train == dados_train[, "condition"])
   
    # Testar com o fold = i
    dados_test = data.frame(X.folds[[i]], condition = Y.folds[[i]])
    fitted_test = predict(fit, newdata = dados_test)
    yhat_test = ifelse( fitted_test$posterior[,2]  > threshold, 1, 0)
    acc_test[i] = mean(yhat_test == dados_test[,"condition"])   

    temp = data.frame(
        truth = factor(Y.folds[[i]]), Class1 = fitted_test$posterior[,1], Class2 = fitted_test$posterior[,2], 
        predicted = factor(fitted_test$class), fold = i
    )
    
    df = rbind(df, temp)

  }

  return(
    list(
    fit = fit,
    df = df,
    acc_train = acc_train, 
    acc_test = acc_test,
    mean_acc_train = mean(acc_train),
    sd_acc_train = sd(acc_train),
    mean_acc_test = mean(acc_test),
    sd_acc_test = sd(acc_test)
    )
  )
}

# 10-Fold Cross Validation -----------------------------------------------------
run_CV <- function(data, categorize = FALSE){    

    folds = prepare_folds(data = data, colX = 1:13, colY = 14, k_fold = 10)
    X.folds = folds$X.folds
    Y.folds = folds$Y.folds

    tab_10fold = matrix(0, 12, 4)
    rownames(tab_10fold) = c(paste0(1:10, "-nn"), "Regressão Logística", "LDA")
    colnames(tab_10fold) = c("Precisão média (treino)", "Desvio padrão (treino)", "Precisão média (teste)", "Desvio padrão (teste)")

    for(i in 1:10){
        res = run_k_knn(X.folds = X.folds, Y.folds = Y.folds, k_knn = i)     
        tab_10fold[i,] = unlist(res[3:6])
        cat(i, "-nn =", tab_10fold[i,], "\n")
    }

    if(categorize) X.folds = map(X.folds, ~data.frame(.) %>% mutate_at(factors_names, ~factor(.)))
    
    logito_res = run_logito(X.folds, Y.folds, threshold = 0.5)  
    lda_res = run_lda(X.folds, Y.folds, threshold = 0.5)  
    
    tab_10fold["Regressão Logística",] = unlist(logito_res[5:8]) %>% as.numeric() 
    tab_10fold["LDA",] = unlist(lda_res[5:8]) %>% as.numeric() 


    dat = as.data.frame(tab_10fold) 

    plot = ggplot(dat, aes(x = 1:12)) + 
            geom_line(aes(y = dat[,1], color="Treino"), size = 1) +
            geom_line(aes(y = dat[,3], color="Teste"), size = 1) +
            labs(x = "", y = "Precisão média", color = "") +
            scale_color_manual(values = c(
                'Treino' = 'blue',
                'Teste' = 'orange')) + 
            scale_x_continuous(breaks=1:12, labels= rownames(dat))  +
            theme(axis.text.x=element_text(angle = 45, hjust = 1))

    return(list(
        tab = tab_10fold,
        plot = plot
    ))
}

# Hold-one-out -----------------------------------------------------------------
run_HoldOut <- function(data, categorize = FALSE, prop = 0.7, threshold = 0.5) {
  
  split = initial_split(data, prop = prop, strata = "Y")
  # Conjunto de Treinamento
  train_data <- training(split)
  test_data <- testing(split) 
  
  yhat_train_knn = c()
  for (i in 1:nrow(train_data)){
    x = as.matrix(train_data[i,-14])   
    yhat_train_knn[i] = knn(query = x, k = 9, X = as.matrix(train_data[,-14]), Y = as.matrix(train_data[,14]))$max.prob.class
  }  

  if(categorize) train_data = train_data %>% mutate_at(factors_names, ~factor(.))
  fit = glm(Y ~., family = binomial, data = train_data) 
  yhat_train_glm = ifelse( fitted(fit)  > threshold, 1, 0)
  lda = lda(Y ~., data = train_data) 
  yhat_train_lda = ifelse( predict(lda)$posterior[,2] > threshold, 1, 0)
  train = data.frame(truth = train_data[,"Y"], predicted_knn= yhat_train_knn, predicted_glm = yhat_train_glm, predicted_lda = yhat_train_lda)
  
  yhat_test_knn = c()
  prob_knn = list()
  for (i in 1:nrow(test_data)) {
    x = as.matrix(test_data[i,-14])   
    res = knn(query = x, k = 9, X = as.matrix(test_data[,-14]), Y = as.matrix(test_data[,14]))
    prob_knn[[i]] = res$count/9 
    yhat_test_knn[i] = res$max.prob.class
  }
  
  if(categorize) test_data = test_data %>% mutate_at(factors_names, ~factor(.))
  Class1_glm = predict(fit,test_data[,-14], type = "response") 
  yhat_test_glm = ifelse( predict(fit,test_data[,-14], type = "response")  > threshold, 1, 0)
  lda_res = predict(lda, newdata = test_data)
  yhat_test_lda = ifelse( predict(lda, newdata = test_data)$posterior[,2] > threshold, 1, 0)
  test = data.frame(truth = test_data[,"Y"], Class1_knn = map_dbl(prob_knn, ~.[1]), Class2_knn = map_dbl(prob_knn, ~.[2]), predicted_knn= yhat_test_knn, 
                    Class1_glm = 1 - Class1_glm, Class2_glm = Class1_glm, predicted_glm = yhat_test_glm,
                    Class1_lda = lda_res$posterior[,1], Class2_lda = lda_res$posterior[,2], predicted_lda = yhat_test_lda)

  return(
    list(
      train = train,
      test = test
    )
  )

}

# Compute ROC and other metrics ------------------------------------------------
run_ROC <- function(dataframe, titleforPlot = NULL, method){

  dataframe = dataframe %>% mutate_at(vars("truth", contains("predicted")), as.factor)  
  
  colClass = paste0("Class1_", method)
  aux = dataframe%>%
    roc_auc(truth, colClass) 
  auc = as.numeric(aux[,3]) 

  roc = dataframe %>%
    roc_curve(truth, colClass) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(size = 1.2, color = "blue") +
    geom_abline(lty = 3) +
    coord_equal() +
    labs(x = "Taxa de Falsos Positivos", y = "Taxa de Verdadeiros Positivos", title = paste("Curva ROC", titleforPlot)) +
    annotate("text", x = 0.90, y = 0, label = paste("AUC =", round(auc, 4)), colour = "orange", size = 8)

  aux1 = dataframe %>%
    select_at(vars("truth", ends_with(method)))

  var = sym(paste0("predicted_", method))
  confMat = aux1 %>%
        conf_mat(truth, var)
  confMat_plot = autoplot(confMat, type = "heatmap") +
   ggplot2::scale_fill_gradient(low = "white", high = "blue") +
   labs(x = "Observado", y = "Predito")

  metric = aux1 %>%
    metrics(truth, var)  

  return(list(
    auc = auc,
    roc = roc,
    confMat = confMat,
    confMat_plot = confMat_plot,
    metric = metric
  ))
}

# Compute accuracy and kappa ---------------------------------------------------
run_Metric <- function(dataframe, method){
    # dataframe is a data frame with two columns: truth and predicted value named "predicted_XX" ends with method
    # returns a data frame contains two metrics: accuracy and kappa coefficient
    dataframe = dataframe %>% mutate_if(is.numeric, as.factor)  
    var = sym(paste0("predicted_", method))
    return(
        dataframe %>%
        metrics(truth, var)
    )
}

# Module function to render performance results --------------------------------
performanceUI <-  function(id) {
  ns <- NS(id)

  tagList(
    plotOutput(ns("roc_curve")) %>% withSpinner(),
    plotOutput(ns("confusion_matrix")) %>% withSpinner(),
    h3("Treino"),
    DTOutput(ns("metric_train")) %>% withSpinner(),
    h3("Teste"),
    DTOutput(ns("metric_test")) %>% withSpinner()
  )
  
}

performanceServer <- function(id, results, method, titleforROC) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    out<- reactive({
      return(
        run_ROC(
          dataframe = results()$test, 
          method = method, 
          titleforPlot = titleforROC
        )
      )
    })
    output$roc_curve <- renderPlot({
      out()$roc
    })

    output$confusion_matrix <- renderPlot({
      out()$confMat_plot
    })

    output$metric_train <- renderDT({
      datatable(
        out()$metric[,c(1,3)],
        rownames = FALSE, colnames = c("métrica", "valor estimado"),
        options = list(dom = "t",
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      ) %>%
      formatRound(2, 4) 

    })

    output$metric_test <- renderDT({
      datatable(
        run_Metric(dataframe = results()$train, method = method)[,c(1,3)],
        rownames = FALSE, colnames = c("métrica", "valor estimado"),
        options = list(dom = "t",
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      ) %>%
      formatRound(2, 4)

    })

  })
}