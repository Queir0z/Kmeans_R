
# Passo 1: Carregar os dados
data(iris)
head(iris)
library(openxlsx)
#dados<-read.xlsx("C:/Users/rafael.queiroz/Servico Nacional de Aprendizagem Comercial/GerProspecAvalEducacional - 02 - Producao Educacional/18 - Portal Prospeccao Senac/11 - Relatório de Perfomance/Dados para K-means.xlsx")
dados<-read.xlsx(file.choose())
# Passo 2: Preparar os dados
# Remover a coluna de espécies, pois queremos apenas as variáveis numéricas
#dados$taxa_Aprovação=dados$taxa_Aprovação*100
dados_data <- dados[, -1]

# Padronizar os dados
dados_scaled <-  na.omit(scale(dados_data))

# Passo 3: Aplicar k-means
# Definir o número de clusters (k)
k <- 3

# Aplicar k-means
set.seed(123) # Definir a semente para reprodutibilidade
kmeans_result <- kmeans(dados_scaled, centers = k)

# Visualizar os resultados
print(kmeans_result)

# Adicionar os clusters ao conjunto de dados original
dados$Cluster <- as.factor(kmeans_result$cluster)



# Passo 4: Visualizar os resultados
# Usar a biblioteca ggplot2 para visualização
library(ggplot2)
library(openxlsx)
library(dplyr)
library(scales)
library(stringr)
# Certifique-se de carregar essa biblioteca
# Gráfico de dispersão com clusters


dados <- dados %>%
  mutate(Cluster = recode(Cluster,
                          `1` = "Alta Performance",
                          `3` = "Performance moderada",
                          `2` = "Baixa Performance"))


grafico<-ggplot(dados, aes(x = taxa_Aprovação, y = Matrículas_Aprovadas, color = Cluster, fill = Cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = str_wrap(Nome_Curso, width = 15)), vjust = -1, hjust = 0.5, show.legend = FALSE) +
  labs(title = "Performance do Top 10 Matricúlas Aprovadas no Comerceial (exceto Aprendizagem)",
       x = "Taxa de Aprovação",
       y = "Matrículas Aprovadas") +
  #xlim(0, 450) + 
  # ylim(0, 5000) +
  theme_minimal() +
  scale_y_continuous(
    labels = label_comma(big.mark = ".", decimal.mark = ","),  
    limits = c(0, max(dados$Matrículas_Aprovadas) * 2)  # Aumenta espaço para os rótulos
  ) +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) + # Define o separador de milhares
  scale_color_manual(values = c("#4C9723", "#9C4829","#393987")) +  # Definir as cores manualmente
  scale_fill_manual(values = c("#4C9723", "#9C4829","#393987")) 


print(grafico)

colnames(dados)
ggsave(plot = grafico ,filename = "top_10_APZ_comercial_gabriel.jpeg", path = "C:/Users/rafael.queiroz/Servico Nacional de Aprendizagem Comercial/GerProspecAvalEducacional - 01 - Administrativo/06 - Demandas/01.Internas/01.Dir Ed Profissional/1.Prospecção/2025/02 - fev/14_02_2025_k-means_top",
       width = 10, height = 10)

