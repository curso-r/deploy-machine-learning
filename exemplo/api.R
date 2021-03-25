library(tidyverse)
library(plumber)
library(tidymodels)

# Lê o modelo final que acabamos de treinar

modelo_final <- readRDS(file = "D:/deploy-machine-learning/exemplo/modelos_ajustados/modelo_final.rds")

# Lê as rotinas de pré-processamento

pre_processamento <- readRDS(file = "D:/deploy-machine-learning/exemplo/modelos_ajustados/preprocessamento.rds")

#' Aplica modelo
#' Descobre a espécie de uma planta em função de médidas de uma das sépalas e das pétalas
#'@param Sepal.Width Largura da sépala
#'@param Sepal.Length Comprimento da sépala
#'@param Petal.Width Largura da pétala
#'@param Petal.Length Comprimento da pétala
#'@response .pred_class Classe prevista pelo modelo
#'@post /gera_previsao 
gera_previsao <- function(Sepal.Width, Sepal.Length, Petal.Width, Petal.Length) {
  
  dados <- tibble(
    Sepal.Width,
    Sepal.Length,
    Petal.Width,
    Petal.Length
  ) %>% 
    mutate_all(as.numeric)
  
  # Executa a previsão
  prediction <- predict(
    modelo_final,
    #bake(pre_processamento, dados) é o resultado de aplicar a nossa receita aos dados em questão
    new_data = bake(pre_processamento, dados))
  
  dplyr::bind_cols(dados, prediction)
  
}