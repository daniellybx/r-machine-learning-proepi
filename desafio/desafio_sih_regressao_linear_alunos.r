# ==============================================================================
# DESAFIO SIH - 10 PERGUNTAS (SCRIPT ALUNOS)
# Base: SIH | Modelo univariado: VAL_TOT ~ VAL_SH
# ==============================================================================

# Importe os dados "dados_processados_datasus/sih_modelagem.RData" e crie um modelo de regressão linear 
# com o valor total (variável dependente) em função do valor do serviço hospitalar (variável preditora) 
# para responder as questões abaixo.

# Responda o formulário em https://forms.gle/wArNJsj9JWYXwGzj7

library(tidyverse)

load("dados_processados_datasus/sih_modelagem.RData")
if (!exists("df_sih_modelagem")) stop("Objeto df_sih_modelagem nao encontrado.")

# PERGUNTA 1 (Aula 1)
# Pergunta: Qual o total de registros carregados na base SIH?
# Use a a função de identificação de dimensão de um data.frame para identificar o total de registros carregados na base SIH.
# Resolva aqui:

# PERGUNTA 2 (Aula 2)
# Pergunta: Quantos NAs existem em VAL_SH?
# Use a verificacao de ausentes da Aula 2 para contar quantos NAs existem em VAL_SH.
# Resolva aqui:

# PERGUNTA 3 (Aula 3)
# Pergunta: Qual e a media de VAL_SH?
# Use o calculo de media da Aula 3 para obter a media de VAL_SH.
# Resolva aqui:

# PERGUNTA 4 (Aula 3)
# Pergunta: Qual e a media de VAL_TOT?
# Use o calculo de media da Aula 3 para obter a media de VAL_TOT.
# Resolva aqui:

# PERGUNTA 5 (Aula 3)
# Pergunta: Qual e a correlacao de Pearson entre VAL_TOT e VAL_SH?
# Use a correlacao de Pearson da Aula 3 para medir a associacao entre VAL_TOT e VAL_SH.
# Resolva aqui:

# PERGUNTA 6 (Aulas 4 e 5)
# Pergunta: Qual o p-valor do coeficiente de VAL_SH no modelo linear?
# Use a regressao linear da Aula 5 e leitura inferencial da Aula 4 para extrair o p-valor de VAL_SH.
# Resolva aqui:

# PERGUNTA 7 (Aula 5)
# Pergunta: Qual equacao estimada esta correta?
# Use a regressao linear da Aula 5 para montar a equacao estimada (intercepto e inclinacao).
# Resolva aqui:

# PERGUNTA 8 (Aula 5)
# Pergunta: Qual e o valor de R2 do modelo?
# Use o summary da Aula 5 para obter o R2 do modelo univariado VAL_TOT ~ VAL_SH.
# Resolva aqui:

# PERGUNTA 9 (Aulas 5 e 6)
# Pergunta: Qual e o RMSE do modelo?
# Use residuos da Aula 5 e metrica de desempenho da Aula 6 para calcular RMSE.
# Resolva aqui:

# PERGUNTA 10 (Aulas 4 e 5)
# Pergunta: Qual e o intervalo de confianca de 95% do coeficiente de VAL_SH?
# Use o ajuste de regressao da Aula 5 e a logica inferencial da Aula 4 para
# extrair o intervalo de confianca de 95% do coeficiente de VAL_SH.
# Resolva aqui:
