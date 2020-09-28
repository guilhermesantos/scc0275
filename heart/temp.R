vars = c("age", "sex", "cp (chest pain)", "tresbps (resting blood pressure)", "chol (cholesterol)",
         "fbs (fasting blood sugar)", "restecg (resting electrocardiogram)", "thalach", "exang", "oldpeak", "slope", "ca", "thal")

desc = c("idade do paciente",
         "gênero do paciente.",
         "tipo de dor no peito reportada pelo indivíduo.",
         "pressão sanguínea durante repouso em mmHg",
         "nível de LDL (low density lipoprotein, ou 'colesterol ruim') no sangue.",
         "variável binária que indica se o nível de açúcar no sangue do paciente é maior que 120 miligramas por decilitro de sangue.",
         "resultado do exame de eletrocardiograma.",
         "máxima frequência cardíaca observada.",
         "se a realização de exercícios físicos induziu angina.",
         "indica a duração da depressão do segmento ST do sinal obtido pelo exame de eletrocardiograma.",
         "A inclinação do segmento ST do eletrocardiograma durante a realização de execícios físicos intensos.",
         "Número de vasos sanguíneos colorizados por fluoroscopia.",
         "variável categórica significando o status quanto à doença denominada talassemia, que provoca a redução da quantidade de hemoglobina em circulação no sangue."
)


val = c(
  "inteiro positivo",
  "0: feminino, 1: masculino",
  "0: angina comum, 1: angina incomum, 2: dor não anginar, 3: assintomático.",
  "inteiro positivo",
  "inteiro positivo",
  "0: < 120, 1: > 120",
  "real positivo",
  "0: normal, 1: anormalidade no sinal ST-T, 2: provável caso de hipertrofia ventricular esquerda",
  "0: não ocorreu, 1: ocorreu",
  "1: crescente, 2: plano, 3: decrescente",
  "inteiro positivo",
  "inteiro positivo",
  "3: normal, 6: defeito fixo, 7: defeito reversível"
)

data.frame("Variável" = vars,
           "Descrição" = desc,
           "Possíveis valores" = val)
