library(markdown)
library(shiny)
pack <- c(
  "shiny", 
  "shinydashboard", 
  "shinydashboardPlus", 
  "shinyWidgets", 
  "shinyBS",
  "shinycssloaders", 
  "DT",
  "tidyverse"
 )

lapply(pack, FUN = function(pack){do.call("library", list(pack)) })

source('aed.R', encoding = 'UTF-8')

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
                                   "Some text here!",
                                   footer = "The footer here!"
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
                                   "Some text here!",
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
                                   "tehledred@usp.br"
                                   # footer = "The footer here!"
                                 )
                                 )
                        ),
                        tabPanel("Projeto",
                                 h3("This is the second panel")
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
                                   span("1: crescente, 2: plano, 3: decrescente.", style = "color:red")
                                 ),
                                 p(strong("ca:"),
                                   "Número de vasos sanguíneos colorizados por fluoroscopia.",
                                   a("Exames de fluoroscopia", href = "https://www.healthcareimaging.com.au/angiography-healthcare-imaging-services.html"),
                                   "estão relacionados com a medição da densidade dos vasos sanguíneos. Isto os torna potenciais indicadores de problemas relacionados à pressão sanguínea e, portanto, de doenças cardiovasculares."
                                 ),
                                 p(strong("thal:"),
                                   "variável categórica significando o status quanto à doença denominada talassemia, que provoca a redução da quantidade de hemoglobina em circulação no sangue.",
                                   span("3: normal, 6: defeito fixo, 7: defeito reversível.", style = "color:red")
                                 )
                        
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
                              checkboxInput("stack", "Stack", value = FALSE)
                              
                            ),
                            mainPanel(
                              width = 9,
                              plotOutput("plot_3") %>% withSpinner,
                              textOutput("text_plot_3")                            
                            )
                          )
                        ),
                        fluidRow(
                          align = "center",
                          br(),
                          br(),
                          h4("Mutual information entre variável e rótulo (indicador de doença)"),
                          DTOutput("entropy")
                        )
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
                              plotOutput("plot_1") %>% withSpinner,
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
                              plotOutput("plot_2") %>% withSpinner
                            )
                          )
                          
                          
                        )                        
                      ) 
                  )                    
             ),
             tabPanel("Técnicas empregadas",                      
                navlistPanel(
                  "Classificador",
                  tabPanel("KNN",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              title = "Configurações"
                              
                              
                            ),
                            mainPanel(
                              
                              width = 9,
                            )
                            
                          )
                  ),
                  tabPanel("Random Forest"),
                  tabPanel("Regressão logística",
                          h3("This is the second panel")
                  ),
                  tabPanel("LDA",
                          h3("This is the third panel")
                  ),
                  tabPanel("SVM"),
                  widths = c(2, 10)
                )
             ),
             tabPanel("Comparação")
             )             
)









server<- function(input, output, session) {
 
  output$plot_1 <- renderPlot({
    var = pull(heart, input$var)
    mean = unlist(resumo[[input$var]][2])
    median = unlist(resumo[[input$var]][3])
    if(input$marca_media == 1) m = mean else m = median
    
    if(input$plotType == 1){
      
      ggplot(heart[cont_names], 
             aes(x = factor(Y, labels=c("Negativo","Positivo")), y = var, 
                 fill = factor(Y, labels=c("Negativo","Positivo"))))  +
        geom_boxplot() +
        scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
        labs(x = "", y = input$var, fill = "Doença") 
  
      
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
        q[3], "e", q[4], "anos. E o mediano é maior que pacientes saudáveis."
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

  
  
  output$plot_2 <- 
    renderPlot({
      varx = input$x
      vary = input$y
      x = pull(heart, varx)
      y = pull(heart, vary)
      
      
      plot = ggplot(heart, aes(x = x, y = y)) +
        geom_point(aes(color = as.factor(condition)), size = 3) +
        theme(legend.position = "none") +
        scale_color_manual(values=c("#0073C2FF", "#EFC000FF")) 
      
      if(!input$facet){
        return(plot +
                 geom_smooth(method = "lm", se = FALSE, color = "grey") +
                  labs(title = paste("Coeficiente de Pearson =", round(cor(x, y), 2), ",", 
                                      "Coeficiente de Spearman =", round(cor(x, y, method = "spearman"), 2)),
                      x = varx, y = vary) 
        )
      }else{
        return(plot +
                 facet_grid(. ~ factor(condition, labels = c("Negativo", "Positivo"))) +
                 geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
                 labs(x = varx, y = vary)
        )
      }
        
         
    })
  
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
      mutate(fator = factor(fator), condition = factor(condition)) %>%
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
    if(fator == "sex"){
      return(
       "Note-se que uma proporção maior de homens apresenta doença diante do total de pacientes daquele gênero, 
        enquanto no sexo feminino há mais pacientes saudáveis do que doentes."
      )
      
    }
    
  })

  output$entropy <- renderDT({
    datatable(
      round(entropy, 4), option = list(dom = "t"), rownames = FALSE
    )
    
  })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

shinyApp(ui, server)


