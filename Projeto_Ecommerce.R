library(tidyverse)
library(lubridate)
library(scales)
setwd("~/Desktop/Projeto_Ecommerce/dataset_ecmm")

################################################################
#             Limpeza e Tratamento dos Dados 
################################################################

# baixando os dados separadamente e juntando em um completo
df_clientes <- read_csv("olist_customers_dataset.csv")
df_pedidos  <- read_csv("olist_orders_dataset.csv")
df_itens    <- read_csv("olist_order_items_dataset.csv")
df_reviews  <- read_csv("olist_order_reviews_dataset.csv")

data_completo <- df_pedidos %>%
  left_join(df_clientes, by = "customer_id") %>%
  left_join(df_itens, by = "order_id") %>%
  left_join(df_reviews, by = "order_id") 

rm(df_clientes, df_pedidos, df_itens, df_reviews)


# tratando as principais variáveis
data_limpo <- data_completo %>%
  mutate( 
    data_compra = ymd_hms(order_purchase_timestamp),
    data_entrega = ymd_hms(order_delivered_customer_date),
    data_estimada = ymd(order_estimated_delivery_date)) %>%
  mutate(
    preco = as.numeric(price),
    frete = as.numeric(freight_value),
    gasto_total = preco + frete, 
    prop_frete = round((frete/gasto_total) * 100 , 2),
    diferenca_dias = as.integer(difftime(data_entrega, data_estimada, units = "days")),
    status_entrega = case_when(
      is.na(diferenca_dias) ~ "Não Entregue",
      diferenca_dias < 0 ~ "Adiantado",
      diferenca_dias == 0 ~ "Prazo Exato",
      diferenca_dias > 0 ~ "Atrasado"),
    dias_atraso = ifelse(is.na(diferenca_dias) | diferenca_dias <= 0, 0, diferenca_dias),
    escreveu_comentario = ifelse(is.na(review_comment_message) | review_comment_message == "", 0, 1),
    estado = as.factor(customer_state)) %>%
  select(
    order_id, 
    customer_unique_id, 
    data_compra,
    estado,
    gasto_total,
    preco,
    frete,
    prop_frete,
    dias_atraso,
    diferenca_dias,
    status_entrega,
    review_score,
    escreveu_comentario)

# agrupando todos os itens dos pedidos (há linhas com o mesmo pedidos apenas com itens diferentes)
data_limpo <- data_limpo %>%
  group_by(order_id, customer_unique_id, data_compra, estado) %>%
  summarise(
    itens_no_pedido = n(),                               
    preco_total = sum(preco, na.rm = TRUE),              
    frete_total = sum(frete, na.rm = TRUE),              
    gasto_total = sum(gasto_total, na.rm = TRUE), 
    
    diferenca_dias = first(diferenca_dias),
    status_entrega = first(status_entrega),
    dias_atraso = max(dias_atraso, na.rm = TRUE), 
    
    review_score = first(review_score),                  
    escreveu_comentario = max(escreveu_comentario, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(prop_frete = round((frete_total / gasto_total) * 100, 2))


################################################################
#             Análise Exploratória de Dados
################################################################

###### Dor do Cliente (Atrasos e Avaliações) ###### 

# grfc de barras do status de entrega 
cores_status <- c(
  "Adiantado" = "#1A9850",       
  "Prazo Exato" = "#377EB8",  
  "Atrasado" = "#D73027")

stats_status <- data_limpo %>%
  filter(!is.na(status_entrega), status_entrega != "Não Entregue") %>%
  count(status_entrega) %>%
  mutate(
    proporcao = n / sum(n),
    status_entrega = factor(status_entrega, levels = c("Adiantado", "Prazo Exato", "Atrasado")))

grafico_barras_status <- ggplot(stats_status, aes(x = status_entrega, y = n, fill = status_entrega)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = paste0(format(n, big.mark = ".", scientific = FALSE), "\n(", scales::percent(proporcao, accuracy = 0.1, decimal.mark = ","), ")")), 
            vjust = -0.3, size = 3.5, fontface = "bold", color = "grey20", lineheight = 0.9) +
  scale_fill_manual(values = cores_status, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Status das Entregas",
    subtitle = "Distribuição dos cenários de entrega",
    x = "Status da Entrega",
    y = "Quantidade de Pedidos"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 0.5),
    panel.grid.major.x = element_blank()) # obs: 2965 de não entregues, onde havia NA na data de entrega ou na data real de entrega


# histograma dos atrasos (linha com o P90)
stats_atraso <- data_limpo %>%
  filter(dias_atraso > 0) %>%
  summarise(
    mediana = median(dias_atraso, na.rm = TRUE),
    p90 = quantile(dias_atraso, 0.90, na.rm = TRUE),
    maximo = max(dias_atraso, na.rm = TRUE))

grafico_hist_atrasos <- data_limpo %>%
  filter(dias_atraso > 0) %>%
  ggplot(aes(x = dias_atraso)) +
  geom_histogram(binwidth = 2, fill = "red", color = "white", alpha = 0.85) +
  geom_vline(xintercept = stats_atraso$mediana, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = stats_atraso$mediana + 1.5, y = 1650, 
           label = paste("Mediana:", stats_atraso$mediana, "dias"), 
           color = "black", angle = 90, vjust = -0.5, size = 3.7, fontface = "bold") +
  geom_vline(xintercept = stats_atraso$p90, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = stats_atraso$p90 + 1.5, y = 1700, 
           label = paste("P90:", stats_atraso$p90, "dias"), 
           color = "black", angle = 90, vjust = -0.5, size = 3.7, fontface = "bold") +
  theme_minimal(base_size = 14) +
  coord_cartesian(xlim = c(0, stats_atraso$p90 * 1.5)) + 
  labs(
    title = "Atrasos Logísticos",
    subtitle = "Distribuição de dias de atraso",
    x = "Dias de Atraso",
    y = "Quantidade de Pedidos",
    caption = "Nota: Outliers extremos omitidos") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", size = 12),
    panel.grid.minor = element_blank()) # obs: 281 outliers removidos  


# grfc de barras das avaliações 
cores_satisfacao <- c(
  "1" = "#D73027",  
  "2" = "#FC8D59",  
  "3" = "#FEE08B", 
  "4" = "#A6D96A", 
  "5" = "#1A9850")

stats_avaliacoes <- data_limpo %>%
  filter(!is.na(review_score)) %>%
  count(review_score) %>%
  mutate(proporcao = n / sum(n))

grafico_barras_notas <- ggplot(stats_avaliacoes, aes(x = as.factor(review_score), y = n, fill = as.factor(review_score))) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = paste0(format(n, big.mark = ".", scientific = FALSE), "\n(", scales::percent(proporcao, accuracy = 0.1), ")")), 
            vjust = -0.3, size = 3, fontface = "bold", color = "grey20", lineheight = 0.9) +
  scale_fill_manual(values = cores_satisfacao, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Satisfação Geral ",
    subtitle = "Distribuição das notas atribuídas aos pedidos",
    x = "Nota da Avaliação",
    y = "Quantidade de Pedidos") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 0.5),
    panel.grid.major.x = element_blank()) # obs: 768 não avaliaram o pedido, sendo NAs no banco 


# Ridgeline Plot dos atrasos x avaliação
library(ggridges)

grafico_ridgeline_atraso <- data_limpo %>%
  filter(dias_atraso > 0, !is.na(review_score)) %>% 
  ggplot(aes(x = dias_atraso, y = as.factor(review_score), fill = as.factor(review_score))) +
  geom_density_ridges(alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  scale_fill_manual(values = cores_satisfacao, guide = "none") +  
  scale_x_continuous(limits = c(0, NA)) +
  coord_cartesian(xlim = c(0, quantile(data_limpo$dias_atraso[data_limpo$dias_atraso > 0], 0.90, na.rm=TRUE) * 1.5)) + 
  theme_ridges(font_size = 13, grid = TRUE) +
  labs(
    title = "Impacto do Atraso na Satisfação",
    subtitle = "Distribuição dos dias de atraso para cada categoria de avaliação",
    x = "Dias de Atraso",
    y = "Nota da Avaliação") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 0.5))


# coeficiente de corr atrasos x avaliações (spearman)
corr_atrasos_reviews <- data_limpo %>%
  filter(dias_atraso > 0, !is.na(review_score))

teste_spearman <- cor.test(
  x = corr_atrasos_reviews$dias_atraso, 
  y = corr_atrasos_reviews$review_score, 
  method = "spearman",
  exact = FALSE)
print(teste_spearman)


# grfc de barras empilhados para comentarios x avaliações [proporção de comentários (0 ou 1) dentro de cada nota (1 a 5)]
stats_engajamento <- data_limpo %>%
  filter(!is.na(review_score), !is.na(escreveu_comentario)) %>%
  mutate(
    comportamento = case_when(
      escreveu_comentario == 1 ~ "Escreveram Comentário",
      escreveu_comentario == 0 ~ "Não Escreveram Comentário"),
    comportamento = factor(comportamento, levels = c("Escreveram Comentário", "Não Escreveram Comentário"))) %>%
  count(review_score, comportamento) %>%
  group_by(review_score) %>%
  mutate(proporcao = n / sum(n)) %>%
  ungroup()

cores_engajamento <- c(
  "Escreveram Comentário" = "#2C7FB8",       
  "Não Escreveram Comentário" = "darkgray")

grafico_comentarios <- ggplot(stats_engajamento, aes(x = as.factor(review_score), y = proporcao, fill = comportamento)) +
  geom_col(position = "fill", width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(proporcao, accuracy = 0.1, decimal.mark = ",")), 
            position = position_stack(vjust = 0.5), 
            size = 4, fontface = "bold", 
            color = ifelse(stats_engajamento$comportamento == "Escreveram Comentário", "white", "black")) +
  scale_fill_manual(values = cores_engajamento) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comentários justificando a Avaliação",
    subtitle = "Proporção de comentários para cada categoria de avaliação",
    x = "Nota da Avaliação",
    y = "Proporção de Pedidos",
    fill = "") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "top")  # obs: usamos a porporção pois a maioria esmagadora das notas é 5, 
                              #      então distorceria a análise para mostrar o volume de vendas ao inves do comportamento de quem avalia 


###### Geografia & Logística ###### 
library(geobr)
library(sf)
library(ggrepel)
mapa_brasil_regioes <- read_region(year = 2020, showProgress = FALSE)

stats_pedidos_regioes <- data_limpo %>%
  mutate(
    regiao = case_when(
      estado %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      estado %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      estado %in% c("DF", "GO", "MT", "MS") ~ "Centro Oeste",
      estado %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      estado %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ "Outro")) %>%
  count(regiao, name = "total_pedidos")

stats_atraso_regioes <- data_limpo %>%
  filter(dias_atraso > 0) %>%
  mutate(
    regiao = case_when(
      estado %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      estado %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      estado %in% c("DF", "GO", "MT", "MS") ~ "Centro Oeste",
      estado %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      estado %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ "Outro")) %>%
  group_by(regiao) %>%
  summarise(mediana_atraso = median(dias_atraso, na.rm = TRUE)) 


# mapa dos pedidos por região (volume dos pedidos)
mapa_regioes_pedidos <- mapa_brasil_regioes %>%
  left_join(stats_pedidos_regioes, by = c("name_region" = "regiao"))

grafico_mapa_pedidos <- ggplot(data = mapa_regioes_pedidos) +
  geom_sf(aes(fill = total_pedidos), color = "white", size = 0.5) +
  geom_sf_text(
    aes(
      label = paste0(name_region, "\n", format(total_pedidos, big.mark = ".", scientific = FALSE)),
      color = ifelse(name_region == "Sudeste", "white", "grey20")),
      size = 4, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient(low = "#C6DBEF", high = "#08306B") +
  theme_void() +
  labs(
    title = "Volume de Vendas",
    subtitle = "Concentração dos pedidos por macro-região") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none")


# mapa dos dias atrasados por região (mediana dos atrasos)
mapa_regioes_atrasos <- mapa_brasil_regioes %>%
  left_join(stats_atraso_regioes, by = c("name_region" = "regiao"))

grafico_mapa_atrasos <- ggplot(data = mapa_regioes_atrasos) +
  geom_sf(aes(fill = mediana_atraso), color = "white", size = 0.5) +
  geom_sf_text(aes(label = paste0(name_region, "\n", mediana_atraso, " dias")), 
               size = 4, fontface = "bold", color = "grey20") +
  scale_fill_gradient(low = "#FCBBA1", high = "#D73027") +
  theme_void() +
  labs(
    title = "Severidade do Atraso",
    subtitle = "Mediana de dias de espera para pedidos atrasados") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none")


# mapa da proporção do frete no pedido por região (mediana da prop_frete)
mapa_regioes_frete <- mapa_brasil_regioes %>%
  left_join(stats_frete_regioes, by = c("name_region" = "regiao"))

grafico_mapa_frete <- ggplot(data = mapa_regioes_frete) +
  geom_sf(aes(fill = mediana_prop_frete), color = "white", linewidth = 0.5) +
  geom_sf_text(
    aes(
      label = paste0(name_region, "\n", round(mediana_prop_frete, 1), "%"),
      color = ifelse(name_region %in% c("Norte", "Nordeste"), "white", "grey20")), 
    size = 4, 
    fontface = "bold") +
  scale_fill_gradient(low = "#C6DBEF", high = "#08306B") +
  scale_color_identity() + 
  theme_void() +
  labs(
    title = "O Peso do Frete na Compra",
    subtitle = "Mediana da proporção do frete em relação ao valor total da compra") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(hjust = 0.5, color = "#666666", margin = margin(b = 15)),
    legend.position = "none")


###### O Custo da Distância ###### 

# mapa da proporção do frete no pedido por região (mediana da prop_frete)
stats_frete_regioes <- data_limpo %>%
  drop_na(prop_frete) %>% 
  mutate(
    regiao = case_when(
      estado %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      estado %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      estado %in% c("DF", "GO", "MT", "MS") ~ "Centro Oeste",
      estado %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      estado %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ "Outro")) %>%
  group_by(regiao) %>%
  summarise(mediana_prop_frete = median(prop_frete, na.rm = TRUE)) 

mapa_regioes_frete <- mapa_brasil_regioes %>%
  left_join(stats_frete_regioes, by = c("name_region" = "regiao"))

grafico_mapa_frete <- ggplot(data = mapa_regioes_frete) +
  geom_sf(aes(fill = mediana_prop_frete), color = "white", linewidth = 0.5) +
  geom_sf_text(
    aes(
      label = paste0(name_region, "\n", round(mediana_prop_frete, 1), "%"),
      color = ifelse(name_region %in% c("Norte", "Nordeste"), "white", "grey20")), 
    size = 4, 
    fontface = "bold") +
  scale_fill_gradient(low = "#C6DBEF", high = "#08306B") +
  scale_color_identity() + 
  theme_void() +
  labs(
    title = "O Peso do Frete na Compra",
    subtitle = "Mediana da proporção do frete em relação ao valor total da compra") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(hjust = 0.5, color = "#666666", margin = margin(b = 15)),
    legend.position = "none")


# Ridgeline da proporção do frete x avaliação
grafico_ridgeline_frete <- data_limpo %>%
  drop_na(prop_frete, review_score) %>% 
  ggplot(aes(x = prop_frete, y = as.factor(review_score), fill = as.factor(review_score))) +
  geom_density_ridges(alpha = 0.8, scale = 1.5, rel_min_height = 0.01, color = "black") +
  scale_fill_manual(values = cores_satisfacao, guide = "none") +  
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    expand = c(0, 0)) +
  coord_cartesian(
    xlim = c(0, quantile(data_limpo$prop_frete, 0.95, na.rm = TRUE)),
    clip = "on") +  
  coord_cartesian(xlim = c(0, quantile(data_limpo$prop_frete, 0.95, na.rm = TRUE))) + 
  theme_ridges(font_size = 13, grid = TRUE) +
  labs(
    title = "Impacto do Frete na Satisfação",
    subtitle = "Distribuição da proporção do frete para cada categoria de avaliação",
    x = "Proporção do Frete no Valor Total (%)",
    y = "Nota da Avaliação") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 0.5, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 0.5, face = "bold"))


###### Análise de Recorrência ###### 
data_recorrencia  <- data_limpo %>%
  mutate(data_compra_dia = as_date(data_compra)) %>%
  distinct(customer_unique_id, data_compra_dia, .keep_all = TRUE) %>%
  arrange(customer_unique_id, data_compra_dia) %>%
  group_by(customer_unique_id) %>%
  summarise(
    frequencia_dias = n(),
    data_primeira_compra = first(data_compra),
    data_segunda_compra = nth(data_compra, 2), 
    primeira_nota = first(review_score),       
    estado = first(estado),                    
    valor_total_historico = sum(gasto_total),
    .groups = "drop") %>%
  mutate(
    dias_ate_segunda_compra = as.numeric(difftime(as_date(data_segunda_compra), as_date(data_primeira_compra), units = "days")),
    perfil_cliente = ifelse(frequencia_dias > 1, "Recorrente", "Único"),
    regiao = case_when(
      estado %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      estado %in% c("PR", "SC", "RS") ~ "Sul",
      estado %in% c("BA", "PE", "CE", "MA", "PB", "RN", "AL", "PI", "SE") ~ "Nordeste",
      estado %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      estado %in% c("AM", "PA", "AC", "RR", "RO", "AP", "TO") ~ "Norte",
      TRUE ~ "Desconhecido"))

# grfc de barras da proporção de "Clientes Únicos" vs. "Clientes Recorrentes"


# mapa de calor da proporção de "Clientes Únicos" vs. "Clientes Recorrentes" por região 


# tempo de recompra após a 2 vez 


# grfc de barras empilhadas da Nota da 1ª Compra com o perfil [proporção de recorrencia (0 ou 1) dentro de cada nota (1 a 5)]




################################################################
#             Aa
################################################################




###### RASCUNHO ###### 

limite_grafico <- stats_atraso$p90 * 1.5

analise_outliers <- data_limpo %>%
  filter(dias_atraso > 0) %>%
  summarise(
    total_atrasados = n(),
    outliers_removidos = sum(dias_atraso > limite_grafico),
    pct_removidos = round((outliers_removidos / total_atrasados) * 100, 2),
    pior_atraso_historico = max(dias_atraso)
  )

print(analise_outliers)



