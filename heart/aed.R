theme_set(theme_light())
theme_update(
  plot.title = element_text(size=16, face="bold"),
  axis.text.x = element_text(size=14),
  axis.text.y = element_text(size=14))


heart<- read_csv("heart_cleveland.csv")

glimpse(heart)

factors_names <-  c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")  
cont_names <- names(heart)[!names(heart) %in% c(factors_names, "condition")]

X <-  heart[,-14]
Y <-  pull(heart, condition)

summary(factor(Y))

# Rename factors levels
tidiedData <- heart %>%
  mutate(
    sex = ifelse(sex == 0, "feminino", "masculino"),
    cp = case_when(
      cp == 0 ~ "comum",
      cp == 1 ~ "incomum",
      cp == 2 ~ "não anginar",
      cp == 3 ~ "assintomático"
    ),
    fbs = ifelse(fbs == 0, "menor igual", "maior"),
    restecg = case_when(
      restecg == 0 ~ "normal",
      restecg == 1 ~ "anormal",
      restecg == 2 ~ "provável caso"
    ),
    exang = ifelse(exang == 0, "não ocorreu", "ocorreu"),
    slope = case_when(
      slope == 0 ~ "crescente",
      slope == 1 ~ "plano",
      slope == 2 ~ "decrescente"
    ),
    thal = case_when(
      thal == 0 ~ "normal",
      thal == 1 ~ "defeito fixo",
      thal == 2 ~ "defeito reversível"
    ),
    condition = ifelse(condition == 0, "negativo", "positivo"),
    ca = as.integer(ca)
  ) %>%
  mutate_if(is.character, as.factor) 

glimpse(tidiedData)

factors_names <-  c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
cont_names <- names(heart)[!names(heart) %in% c(factors_names, "condition")]




# Continous variables -----------------------------------------------------

# cor(heart[cont_names]) %>%
#   round(.,2)


# ggpairs(data = heart[cont_names], mapping = aes(color = as.factor(heart$condition))) +
#   labs(title = "Correlação entre as variáveis contínuas")


resumo = list()
for (i in 1:5){
  resumo[[i]] = heart[, c(cont_names[i], "condition")] %>%
    group_by(condition) %>%
    summarise(
             mean = mean(!!sym(cont_names[i])),
             median = median(!!sym(cont_names[i])),
             quantiles = quantile(!!sym(cont_names[i]), c(0.25, 0.75)) 
             # quant_3 = quantile(!!sym(cont_names[i]), 0.75)
              
    )
}
resumo = set_names(resumo, cont_names)

p1 = list()
pValue = c()

for(i in 1:5){
    
    p1[[i]] =
      ggplot(tidiedData[cont_names])  +
      geom_density(aes(x = tidiedData[cont_names][[i]], fill = factor(tidiedData$condition, labels=c("Negativo","Positivo"))), alpha = 0.7) +
      scale_x_continuous(breaks = unlist(resumo[[i]][2]), label = round(unlist(resumo[[i]][2]), 2)) +
      geom_segment(aes(x = unlist(resumo[[i]][2])[1], y = -Inf, xend = unlist(resumo[[i]][2])[1], yend = Inf),
                   color = 2, linetype = 'dashed', size = 1) +
      geom_segment(aes(x = unlist(resumo[[i]][2])[2], y = -Inf, xend = unlist(resumo[[i]][2])[2], yend = Inf),
                   color = 4, linetype = 'dashed', size = 1) +
      labs(title = paste("Distribuição de", cont_names[i]),
           x = "",
           y = "",
           fill = "Doença") 
    
    x = pull(heart, cont_names[i])
    pValue[i] = wilcox.test(x ~ tidiedData$condition)$p.value  # Teste nao parametrico igualdade de mediana    
      
}

names(pValue) = cont_names
pValue < 0.05

map2(tidiedData[cont_names], cont_names,
    ~ggplot(tidiedData[c(cont_names, "condition")], aes(x = condition, y = .x, fill = condition))  +
      geom_boxplot(show.legend = FALSE) +       
      labs(x = "", y = .y)
)

p2 = map(2:5, 
     ~ ggplot(tidiedData, aes(x = tidiedData[cont_names][[1]], y = tidiedData[cont_names][[.x]])) +
      geom_point(aes(color = condition), size = 2) +
      theme(legend.position = "none") +
      labs(
        title = paste("Correlação =", round(cor(tidiedData[cont_names][[1]], tidiedData[cont_names][[.x]]), 2)),
        x = cont_names[1],
        y = cont_names[[.x]]
      )
) 

#Categorical variables ---------------------------------------------------

summary(heart[factors_names]) #----> nenhum fator possui nivel unico
 

p3 = map(factors_names, 
  ~ heart %>%
    group_by(condition) %>%
    count(eval(as.name(.x))) %>%
    rename(.x = `eval(as.name(.x))`) %>%
    ggplot(aes(x = factor(condition), y = n, fill = factor(.x), label = n)) + 
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), colour = "white") +
    labs(title = paste(.x, "em Doença"), x = "Doença", y = "", fill = .x)
)

p3 = map(factors_names, 
  ~ tidiedData %>%
    group_by(condition) %>%
    count(eval(as.name(.x))) %>%
    rename(.x = `eval(as.name(.x))`) %>%
    ggplot(aes(x = condition, y = n, fill = factor(.x), label = n)) + 
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), colour = "white") +
    labs(title = paste(.x, "em Doença"), x = "Doença", y = "", fill = .x)
)

p4 = map(factors_names,
         ~ tidiedData %>%
            group_by(eval(as.name(.x))) %>%
            count(condition) %>%
            rename(.x = `eval(as.name(.x))`) %>%
            mutate(.x = .x, condition = condition) %>%
            ggplot(aes(x = .x, y = n, fill = condition, label = n)) + 
            geom_bar(stat = "identity") +
            geom_text(size = 3, position = position_stack(vjust = 0.5), colour = "white") +
            labs(title = paste("Doença em", .x),
                 x = .x,
                 y = "",
                 fill = "Doença")
)

p5 = c(p3, p4)  

entropy = map_dfc(tidiedData[factors_names],
     ~ mutinformation(.x, tidiedData$condition)
)
