pack <- c(
  "shiny", 
  "shinydashboard", 
  "shinydashboardPlus", 
  "shinyWidgets", 
  "shinyBS",
  "shinycssloaders", 
  "DT",
  "tidyverse",
  "tidymodels",
  "MASS",
  "dismo",
  "infotheo",
  "GGally",
  "glmnet",
  "skimr",
  "randomForest"
 )

lapply(pack, FUN = function(pack){do.call("library", list(pack)) })
options(spinner.type = 4, spinner.size = 0.7)
source('aed.R', encoding = 'UTF-8')
source('heart.R', encoding = 'UTF-8')

ui <- fluidPage(
  shinyBS:::shinyBSDep,
  
  navbarPage("SCC0275",
             theme = shinythemes::shinytheme("cerulean"),
             tabPanel("Sobre", icon = icon("home"),
                      navlistPanel(
                        widths = c(2, 10),
                        "Introdução",
                        tabPanel("Integrantes",
                                 br(), br(),
                                 fluidRow(class = "text-center",
                                 widgetUserBox(
                                   title = "Guilherme Milan Santos",
                                   subtitle = "No USP: 9012966",
                                   type = NULL,
                                   src = "",
                                   color = "aqua-active",
                                   collapsible = FALSE,
                                   closable = FALSE,
                                   width = 4,
                                   "guilherme.milan.santos@usp.br"
                                 ),
                                 widgetUserBox(
                                   title = "Rafael Marques",
                                   subtitle = "No USP: 9846045",
                                   type = NULL,
                                   src = "",
                                   color = "aqua-active",
                                   collapsible = FALSE,
                                   closable = FALSE,
                                   width = 4,
                                   "rafael.polakiewicz@usp.br",
                                   footer = "The footer here!"
                                 ),
                                 widgetUserBox(
                                   title = "Teh Led Red",
                                   subtitle = "No USP: 10368927",
                                   type = NULL,
                                   src = "teh.jpg",
                                   color = "aqua-active",
                                   collapsible = FALSE,
                                   closable = FALSE,
                                   width = 4,
                                   "tehledred@usp.br", br(),
                                  footer = "4º ano do curso de Estatística"
                                 )
                                 )
                        ),
                        tabPanel("Projeto",
                                 h3("Projeto"),
                                 p("Este trabalho faz parte da disciplina da SCC0275 Introdução à Ciência de Dados oferecida pelo Instituto de Ciências Matemáticas e de Computação da USP São Carlos, as aulas foram
                                 ministradas pelo Professor Rodrigo Mello no segundo semestre de 2020."),
                                 p("Foi escolhido o conjunto de dados da doença cardíarca proveniente do", 
                                    a("Kaggle", href = "https://www.kaggle.com/ronitf/heart-disease-uci"), 
                                  "no qual tem o objetivo de construir um modelo classificador que identifica se um indíviduo é portador ou não de doença cardíarca a partir de informações dos fatores relevantes.
                                   A equipe faz exploração de algumas técnicas de modelagem comunmente aplicadas ao problema classificador e tenta otimizar os desempenhos dos algoritmos refinando parâmetros envolvidos.    
                                  "
                                 )
                        ),
                        tabPanel("Dataset",
                                h3("Descrição de variáveis"),
                                 p(strong("age:"), "idade do paciente;"),
                                 p(strong("sex:"),
                                   "gênero do paciente.",
                                   span("0: feminino, 1: masculino.", style = "color:red")
                                 ),
                                 p(strong("cp (chest pain):"), "tipo de dor no peito reportada pelo indivíduo.", 
                                        span("0: angina comum, 1: angina incomum, 2: dor não anginar, 3: assintomático.", style = "color:red"),
                                        a("Angina", href = "https://drauziovarella.uol.com.br/doencas-e-sintomas/angina/"),
                                        "é a dor resultante do estreitamento das artérias que conduzem ao coração, 
                                        que levam ao baixo abastecimento do coração com oxigênio e nutrientes. 
                                        Costumam indicar obstrução ou contrações involuntárias das artérias coronárias."
                                 ),
                                 p(strong("trestbps (resting blood pressure):"),
                                   "pressão sanguínea durante repouso em mmHg. Alta pressão sanguínea durante períodos prolongados provocam o estreitamento e eventual ruptura ou vazamento dos vasos sanguíneos, podendo aumentar o risco de doenças cardiovasculares."
                                   
                                 ),
                                 p(strong("chol (cholesterol):"), 
                                   "nível de LDL (", em("low density lipoprotein"), ", ou \"colesterol ruim\") no sangue. O LDL provoca o estreitamento das artérias, constituindo portanto um possível preditor de doenças cardíacas."
                                 ),
                                 p(strong("fbs (fasting blood sugar):"),
                                   "variável binária que indica se o nível de açúcar no sangue do paciente é maior que 120 miligramas por decilitro de sangue.",
                                   span("0 :não é maior, 1: é maior.", style = "color:red"), "Esta variável pode indicar a má resposta do organismo à produção de",
                                   a("insulina", href = "http://www.saude.gov.br/saude-de-a-z/diabetes"), 
                                   "o que pode levar a problemas como pressão alta e, consequentemente, a problemas cardiovasculares."
                                 ),
                                 p(strong("restecg (resting electrocardiogram):"),
                                   "resultado do exame de eletrocardiograma.",
                                   span("0: normal, 1: anormalidade no sinal ST-T, 2: provável caso de hipertrofia ventricular esquerda.", style = "color:red"), "A condição de hipertrofia ventricular esquerda indica o aumento da espessura da parede ventrículo esquerdo, câmara responsável por bombear o sangue pelo resto do corpo. Isto costuma ocorrer em resposta a fatores externos, como aumento da pressão sanguínea ou presença de doenças cardiovasculares."
                                 ),
                                 p(strong("thalach:"), "máxima frequência cardíaca observada. O estudo pode inferir se frequências cardíacas mais altas podem estar associadas à ocorrência de doenças cardíacas."
                                 ),
                                 p(strong("exang:"),
                                   "se a realização de exercícios físicos induziu angina.", 
                                   span("0: não ocorreu, 1: ocorreu.", style = "color:red")
                                 ),
                                 p(strong("oldpeak (peak exercise ST segment):"),
                                   "indica a duração da depressão do segmento ST do sinal obtido pelo exame de eletrocardiograma. Uma depressão ou inclinação decrescente do segmento ST pode indicar a presença de doenças cardíacas associadas à redução da circulação sanguínea, como taquicardias."
                                 ),
                                 p(strong("slope:"),
                                   "A inclinação do segmento ST do eletrocardiograma durante a realização de execícios físicos intensos.",
                                   span("0: crescente, 1: plano, 2: decrescente.", style = "color:red")
                                 ),
                                 p(strong("ca:"),
                                   "Número de vasos sanguíneos colorizados por fluoroscopia.",
                                   a("Exames de fluoroscopia", href = "https://www.healthcareimaging.com.au/angiography-healthcare-imaging-services.html"),
                                   "estão relacionados com a medição da densidade dos vasos sanguíneos. Isto os torna potenciais indicadores de problemas relacionados à pressão sanguínea e, portanto, de doenças cardiovasculares."
                                 ),
                                 p(strong("thal:"),
                                   "variável categórica significando o status quanto à doença denominada talassemia, que provoca a redução da quantidade de hemoglobina em circulação no sangue.",
                                   span("0: normal, 1: defeito fixo, 2: defeito reversível.", style = "color:red")
                                 ),
                                 br(),                                
                                h3("Conjunto de Dados"),
                                 DTOutput("dados"),
                                br()                       
                        )
                      )
             ),
             tabPanel("Análise Exploratória",
                  tabsetPanel(
                      tabPanel(
                        "Variáveis Categóricas",
                        br(),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              h4("Gráfico de barra"),
                              selectInput("fator", 
                                          label = "Selecione o Fator",
                                          choices = factors_names
                              ),
                              checkboxInput("stack", "Empilhar", value = FALSE),
                              br(),
                              h4("Tabela de contingência"),
                              tableOutput("crossTable")                              
                            ),
                            mainPanel(
                              width = 9,
                              plotOutput("plot_3", height = 650) %>% withSpinner,
                              br(),
                              textOutput("text_plot_3")                            
                            )
                          )
                        )
                        # fluidRow(
                        #   align = "center",
                        #   br(),
                        #   br(),
                        #   h4("Mutual information entre variável e rótulo (indicador de doença)"),
                        #   DTOutput("entropy")
                        # )
                      ),
                      tabPanel(
                        "Variáveis Contínuas",
                        br(),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              h4("Distribuição"),
                              selectInput(
                                "var", 
                                label = "Selecione a variável",
                                choices = cont_names
                              ),
                              radioButtons(
                                "plotType", 
                                label = "Tipo de gráfico",
                                inline = FALSE, 
                                choices = c("Boxplot" = 1, "Densidade estimada" = 2, "Histograma" = 3), 
                                selected = 1
                              ),
                              conditionalPanel(
                                condition = "input.plotType != 1",
                                radioButtons(
                                  "marca_media", 
                                  label = "Medida de centralização",
                                  inline = FALSE, 
                                  choices = c("Média" = 1, "Mediana" = 2), 
                                  selected = 1
                                )
                              )                           
                            ),
                            mainPanel(
                              width = 9,
                              plotOutput("plot_1", height = 650) %>% withSpinner,
                              textOutput("text_plot_1")
                            )
                          )
                        ),
                        hr(),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              h4("Correlação"),
                              selectInput(
                                "x", 
                                label = "Variável no eixo X",
                                choices = cont_names
                              ),
                              selectInput(
                                "y", 
                                label = "Variável no eixo Y",
                                choices = cont_names, 
                                selected = cont_names[2]
                              ),
                              checkboxInput("facet", "Separar em classe", value = FALSE)
                            ),
                            mainPanel(
                              width = 9,
                              plotOutput("plot_2", height = 650) %>% withSpinner,
                              br(), br()
                              
                            )
                          )
                          
                          
                        )                        
                      ) 
                  )                    
             ),
             tabPanel("Técnicas empregadas",  
                fluidRow(
                  p("A seguir aplicamos algumas técnicas diferentes de modelagem nos dados e estudamos a precisão média que cada técnica alcança por meio da validação cruzada de método", em("10-fold."), 
                  "Compara-se os resultados da modelagem com dois tipos de pré-processamento de dados - o primeiro recebe o tratamento onde todas as variáveis explicativas foram normalizadas e 
                  o outro tem as variáveis contínuas normalizados e as variáveis categóricas foram tratadas como variáveis", em("dummy.")),
                  p("Obs:", span("Leva-se em média 3 minutos para implementar todos os algoritmos.", style = "color:red")),
                  column(
                    6, 
                    h3("1º Pre-processamento"),
                    DTOutput("acc_scaled") %>% withSpinner(),
                    br(),
                    plotOutput("acc_scaled_p") %>% withSpinner()
                  ),
                  column(
                    6, 
                    h3("2º Pre-processamento"),
                    DTOutput("acc_scaled_dummy") %>% withSpinner(),
                    br(),
                    plotOutput("acc_scaled_dummy_p") %>% withSpinner()
                  )
                )                    
             ),
             tabPanel("Comparação",
              p("Comparemos os resultados das técnicas de modelagem por meio da curva de ROC, matriz de confusão e as métricas acurácia - acurácia e coeficiente kappa. 
                As modelagens foram realizadas atráves da amostragem", em("Hold-one-out.")),
              fluidRow(
                  sidebarLayout(
                      sidebarPanel(
                          width = 12,
                          h4("Opções"),
                          fluidRow(
                            column(3, 
                              selectInput("tipo_preprocessamento", "Tipo de pré-processamento", choices = c(1:2), width = "100%")
                            ),
                            column(3, 
                              numericInput("split", "Tamanho de conjunto de treino", min = 0.5, max = 0.9, step = 0.05, value = 0.7, width = "100%")
                            )
                            # numericInput("threshold", "Threshold", value = 0.5, step = 0.1)      
                          )                        
                      ),
                      mainPanel(
                          width = 12,
                          fluidRow(
                            column(3, performanceUI("knn")),
                            column(3, performanceUI("glm")),
                            column(3, performanceUI("lda")),
                            column(3, performanceUI("rf"))
                          )
                                             
                      )
                  )
              )
             ),
             tabPanel("Modelo Preditivo",
              p("Pretendo aplicar elastic net via glmnet, mostrar modelo glm por facilidade de interpretação.")
             )             
  )
)


server<- function(input, output, session) {

# Comparação -------------------------------------------------------------------

  res_Compare <- reactive({
    if(input$tipo_preprocessamento == 1){
      res = run_HoldOut(data = as.data.frame.matrix(data_scaled), prop = input$split)#, threshold = input$threshold)
    }else{
      res = run_HoldOut(data = data_scaled_dummy, categorize = TRUE, prop = input$split)#, threshold = input$threshold)
    }

    return(res)
  })

  performanceServer("knn", res_Compare, method = "knn", titleforROC = "9-nn")
  performanceServer("glm", res_Compare, method = "glm", titleforROC = "Regressão Logística")
  performanceServer("lda", res_Compare, method = "lda", titleforROC = "LDA")
  performanceServer("rf", res_Compare, method = "rf", titleforROC = "Random Forest")
  

# Técnicas empregadas ----------------------------------------------------------
  acc_results <- reactive({
    scaled <- run_CV(data_scaled)
    scaled_dummy <- run_CV(data_scaled_dummy, categorize = TRUE)
    return(list(scaled = scaled, scaled_dummy = scaled_dummy))
  })

  output$acc_scaled <- renderDT({
    datatable(
      round(acc_results()$scaled$tab, 4), 
      options = list(
        dom = "t",  pageLength = 15, autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })

  output$acc_scaled_p <- renderPlot({
    acc_results()$scaled$plot
  })

  output$acc_scaled_dummy <- renderDT({
    datatable(
      round(acc_results()$scaled_dummy$tab, 4), 
      options = list(
        dom = "t",  pageLength = 15, autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })

  output$acc_scaled_dummy_p <- renderPlot({
    acc_results()$scaled_dummy$plot
  })

# Dataset ----------------------------------------------------------------------
  output$dados <- renderDT({
    dados = heart %>% mutate_at(c(factors_names, "condition"), ~factor(.))
    datatable(
      dados, rownames = FALSE, filter = "top",
      options = list(dom = "tip", scrollX = TRUE, autoWidth = TRUE,
      columnDefs = list(list(width = '80px', className = 'dt-center', targets = "_all"))
      )
    )
  })

# Variáveis Contínuas ----------------------------------------------------------
  output$plot_1 <- renderPlot({
    var = pull(heart, input$var)
    mean = unlist(resumo[[input$var]][2])
    median = unlist(resumo[[input$var]][3])
    if(input$marca_media == 1) m = mean else m = median
    
    # Gráfico de boxplot 
    if(input$plotType == 1){
      
      ggplot(heart[cont_names], 
             aes(x = factor(Y, labels=c("Negativo","Positivo")), y = var, 
                 fill = factor(Y, labels=c("Negativo","Positivo"))))  +
        geom_boxplot() +
        scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
        labs(x = "", y = input$var, fill = "Doença") 
  
    # Gráfico de Densidade estimada  
    }else if(input$plotType == 2){      
      
      ggplot(heart, aes(x = var, fill = factor(condition, labels=c("Negativo","Positivo")))) +
        geom_density (alpha = 0.7, color = "white") +
        scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
        scale_x_continuous(breaks = m, label = round(m, 2)) +
        geom_segment(aes(x = m[1], y = -Inf, xend = m[1], yend = Inf),
                     color = "blue", linetype = 'dashed', size = 1.2) +
        geom_segment(aes(x = m[3], y = -Inf, xend = m[3], yend = Inf),
                     color = "orange", linetype = 'dashed', size = 1.2) +
        labs(title = paste("Distribuição de", input$var),
             x = "",
             y = "",
             fill = "Doença") 

    # Histograma -----  
    }else{
      
      ggplot(heart[cont_names], 
             aes(x = var, 
                 fill = factor(Y, labels=c("Negativo","Positivo"))))  +
        geom_histogram(alpha = 0.75, color = 'white') +
        scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
        geom_segment(aes(x = m[1], y = -Inf, xend = m[1], yend = Inf),
                     color = "blue", linetype = 'dashed', size = 1.2) +
        geom_segment(aes(x = m[3], y = -Inf, xend = m[3], yend = Inf),
                     color = "orange", linetype = 'dashed', size = 1.2) +
        labs(x = input$var, y = "", fill = "Doença") 
      
    }
  })
  
  output$text_plot_1 <- renderText({
    mean = unlist(resumo[[input$var]]["mean"])
    median = unlist(resumo[[input$var]]["median"])
    q =  unlist(resumo[[input$var]]["quantiles"])
    if(input$var == "age"){
      return(
        paste(
        "Com relação à idade, há maior número de pacientes doentes entre aproximadamente",
        q[3], "e", q[4], "anos. E o mediano da idade do grupo de pacientes doentes é maior."
        )
      )
    }else if (input$var == "cp") {
       return(
         "Curiosamente, pacientes assintomáticos quanto a dor peitoral demonstraram maior proporção de doentes do que os que apresentaram dores."
       )
    }else if (input$var == "exang") {
       return(
         "Considerando a realização de exercícios, no entanto, os pacientes que tiveram dores eram, em sua maioria, portadores de doenças cardíacas."
       )
    }else if (input$var == "ca") {
       return(
         "Quanto maior o número de vasos colorizados no exame, maior a proporção de pacientes portadores de doenças."
       )
    }
    
  })  
  
  # Diagrama de dispersão -----
  output$plot_2 <- renderPlot({
      varx = input$x
      vary = input$y
      x = pull(heart, varx)
      y = pull(heart, vary)
      # facet = input$facet_fator      
      
      plot = ggplot(heart, aes(x = x, y = y, color = as.factor(condition), group = as.factor(condition)))+
        geom_point(size = 3) +
        theme(legend.position = "none") +
        scale_colour_manual(values=c("#0073C2FF", "#EFC000FF")) 
        # facet_wrap(~get(facet))
      
      # Separar por classe
      if(!input$facet){
        return(plot +
                 geom_smooth(method = "lm", se = FALSE) +
                  labs(title = paste("Coeficiente de Pearson =", round(cor(x, y), 2), ",", 
                                      "Coeficiente de Spearman =", round(cor(x, y, method = "spearman"), 2)),
                      x = varx, y = vary) 
        )
      
      # Geral
      }else{
        return(plot +
                 facet_grid(. ~ factor(condition, labels = c("Negativo", "Positivo"))) +
                 geom_smooth(method = "lm", se = FALSE) +
                 labs(x = varx, y = vary)
        )
      }       
         
  })
  
# Variáveis Categóricas --------------------------------------------------------
  
  # Tabela cruzada
  output$crossTable <- renderTable({
    fator = pull(heart, input$fator)
    tab = table(fator, Y) %>% as.data.frame.matrix()  
    colnames(tab) = c("Negativo", "Positivo")  
    tab[, "Soma"] = rowSums(tab) %>% as.integer()
    tab["Soma",] = colSums(tab)%>% as.integer()     
    return(tab)    
  }, align = "c", rownames = TRUE)

  # Gráfico de barra ----
  output$plot_3 <- renderPlot({
    fator = input$fator
    
    tit = switch(fator,
      "sex" = "gênero",
      "cp" = "tipo de dor no peito", 
      "fbs" = "nível de açúcar no sangue", 
      "restecg" = "resultado do eletrocardiograma", 
      "exang" = "ocorrência de angina em exercício", 
      "slope" = "inclinação do segmento ST durante exercício", 
      "ca" = "número de vasos sanguíneos colorizados por fluoroscopia", 
      "thal" = "talassemia"
    )
    p = heart %>%
      group_by(eval(as.name(fator))) %>%
      count(condition) %>%
      rename(fator = `eval(as.name(fator))`) %>%
      mutate(fator = factor(fator), condition = factor(condition, labels = c("Negativo", "Positivo"))) %>%
      ggplot(aes(x = fator, y = n, fill = condition, label = n)) + 
      scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
      labs( 
        title = paste("Número de pacientes doentes por", tit),
        x = fator,
        y = "",
        fill = "Doença") 
    
    if(input$stack){
      return(
        p + geom_bar(stat = "identity") +
        geom_text(size = 5, position = position_stack(vjust = 0.5), colour = "black") 
      )
    }else{
      return(
        p + geom_bar(stat = "identity", position = "dodge") +
        geom_text(size = 5, position = position_dodge(width = 1), vjust = -0.5, colour = "black")  
      )
    }     
      
  })

  output$text_plot_3 <- renderText({
    fator = input$fator
    if(fator == "sex")
      return(
       "Note-se que uma proporção maior de homens (1) apresenta doença diante do total de pacientes daquele gênero, 
        enquanto no sexo feminino (0) há mais pacientes saudáveis do que doentes."
      )      
    if(fator == "cp")
      return(
        "Curiosamente, pacientes assintomáticos (3) quanto a dor peitoral demonstraram maior proporção de doentes do que os que apresentaram dores (0, 1 e 2)."
      )
    if(fator == "restecg")
      return(
        "Percebe-se que há uma proporção maior de doentes em grupos que apresentaram anormalidade (1 e 2) no exame de eletrocardiograma em comparação do grupo normal (0)."
      )
    if(fator == "exang")
      return(
        "Foi observado para grupo que ocorreu angina quando praticar exercício (1) número maior de casos positivos."
      )
    if(fator == "slope")
      return(
        "Existe maior proporção dos pacientes doentes no grupo que tem nível plano no segmento ST (1) do eletrocardiograma durante a realização de execícios físicos intensos."
      )
    if(fator == "ca")
      return(
        "É notável que quanto maior o número de vasos colorizados no exame fluoroscopico, maior a proporção de pacientes portadores de doenças."
      )
    if(fator == "thal")
    return(
      "Os grupos que sofrem do distúrbio sanguíneo (1 e 2) têm manifestados mais que 60% dos casos positivos em comparação do grupo normal (0). "
    )
  })

  # output$entropy <- renderDT({
  #   datatable(
  #     round(entropy, 4), option = list(dom = "t"), rownames = FALSE
  #   )    
  # })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

shinyApp(ui, server)


