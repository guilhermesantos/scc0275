theme_set(theme_light())
theme_update(
  plot.title = element_text(size=16, face="bold"),
  axis.text.x = element_text(size=14),
  axis.text.y = element_text(size=14))


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
      ggplot(heart[cont_names])  +
      geom_density(aes(x = heart[cont_names][[i]], fill = factor(heart$condition, labels=c("Negativo","Positivo"))), alpha = 0.7) +
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
    pValue[i] = wilcox.test(x ~ Y)$p.value  # Teste nao parametrico igualdade de mediana    
      
}

names(pValue) = cont_names
pValue < 0.05

## Escolher um: Histogram (densidade) ou boxplot
map2(heart[cont_names], cont_names,
    ~ggplot(heart[cont_names], aes(x = factor(Y, labels=c("Negativo","Positivo")), y = .x, fill = factor(Y, labels=c("Negativo","Positivo"))))  +
      geom_boxplot() +       
      labs(x = "", y = .y, fill = "Doença")
)

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