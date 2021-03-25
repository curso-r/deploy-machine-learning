library(readr)
library(tidymodels)

iris <- read_csv2("treinamento/dados_de_treino.csv")

View(iris)

preprocessamento <- recipe(
  Species ~ .,
  # formula do nosso modelo. à esquerda do ~ temos a variável
  # resposta e à direita as nossas covariáveis. O "." indica que
  # tudo que não é "Species" vai ser usado para prever
  data = iris) %>% 
  # iris é o nosso banco de dados de flores
  step_normalize(all_numeric()) %>%
  # esse passo vai deixar todas as variáveis númericas
  # dos dados sem escala
  step_medianimpute(all_numeric())
  # esse passo vai inserir a mediana das colunas caso encontra
  #alguma informação faltante

saveRDS(prep(preprocessamento, iris), "modelos_ajustados/preprocessamento.rds")

# define o modelo que vamos usar
modelo_knn <- nearest_neighbor(neighbors = tune()) %>% 
  # tune() avisa que o número de vizinhos pode ser escolhido por validação cruzada depois
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  translate()

set.seed(11071995)

reamostra_vizinhos <- workflow() %>% 
  add_recipe(preprocessamento) %>% 
  add_model(modelo_knn) %>% 
  tune_grid(resamples = mc_cv(iris, .3),
            grid = 60,
            metrics = metric_set(
                yardstick::accuracy))

# qual é o k que devemos usar?

reamostra_vizinhos %>% 
  autoplot() +
  # firulas para o gráfico ficar bonito:
  theme_minimal(25) + 
  geom_line(size = 2, color = 'royalblue') +
  labs(x = "Número de vizinhos", y = "Performance do modelo\n(Acurácia)") +
  scale_y_continuous(labels = scales::percent, limits = c(0.9, .95)) +
  scale_x_continuous(breaks = c(1:15)) +
  geom_vline(xintercept = 6, linetype = 'dashed', color = 'red', size = 2) +
  geom_point(aes(x = 6, y = 0.9346), color = 'red', size = 7) + 
  annotate(x = 9.5, y = .937, geom = "text", label = "Melhor numero de vizinhos = 6", 
           size =6, color = 'red')
  
# salvando o último modelo

modelo_final <- workflow() %>% 
  add_recipe(preprocessamento) %>% 
  add_model(
    finalize_model(
      modelo_knn, 
      select_best(
        reamostra_vizinhos, 
        "accuracy"
      )
      # nesse passo a gente escolhe o melhor modelo!
    )
  ) %>% 
  fit(iris)

print(modelo_final)

# salvando para que a gente consiga usar no R
saveRDS(modelo_final, "modelos_ajustados/modelo_final.rds")
