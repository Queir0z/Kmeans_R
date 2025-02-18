library(openxlsx)
library(ggplot2)
library(dplyr)

# Carregar os dados
dados <- read.xlsx(file.choose())
colnames(dados)
# Converter "Segmento" para fator se for categórico
dados$Segmento <- as.factor(dados$DR_Unidade)

# Selecionar apenas variáveis numéricas para o k-means
dados_data <- dados %>% select_if(is.numeric)

# Padronizar os dados
dados_scaled <- na.omit(scale(dados_data))

# Aplicar k-means
set.seed(123)
k <- 3
kmeans_result <- kmeans(dados_scaled, centers = k)

# Adicionar os clusters ao conjunto de dados original
dados$Cluster <- as.factor(kmeans_result$cluster)

library(scales)

# Ajustar nomes dos clusters
dados <- dados %>%
  mutate(Cluster = recode(Cluster, 
                          `1` = "Oferta Moderada", 
                          `2` = "Baixa Oferta", 
                          `3` = "Prioridade"))

# Criar o gráfico
grafico <- ggplot(dados, aes(x = Segmento, y = Matrículas_Totais, color = Cluster, fill = Cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = Segmento), vjust = -1, hjust = 0.5, show.legend = FALSE) +
  labs(title = "Clusters das Unidades Operativas",
       x = "Unidades",
       y = "Matrículas Totais") +
  #xlim(0, 450) + 
  #  ylim(0, 12000) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) + 
  scale_color_manual(values = c("#393987", "#9C4829", "#4C9723")) +  
  scale_fill_manual(values = c("#393987", "#9C4829", "#4C9723"))

# Exibir o gráfico
print(grafico)

# Salvar imagem
ggsave(plot = grafico, 
       filename = "cluster_unidades.jpeg", 
       path = "C:/Users/rafael.queiroz/Servico Nacional de Aprendizagem Comercial/GerProspecAvalEducacional - 01 - Administrativo/06 - Demandas/01.Internas/01.Dir Ed Profissional/1.Prospecção/2025/02 - fev/11_02_2025-Documento_MT",
       width = 18,  # Aumentando a largura para evitar cortes
       height = 12, 
       units = "cm", 
       dpi = 300)
