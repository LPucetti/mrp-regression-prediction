load("C:/Users/Leo Nogueira/Documents/MestradoCienciadosDados/S2/MRP/Projeto/MoviesDB.RData")
attach(MoviesDB)
View(MoviesDB)

str(MoviesDB)

#grupo 3

library(ggplot2)

# tenho que usar uma base de dados aleatória (salvar essa nova lista de dados)
# com 100 observações menos 3 aleatórias. 

set.seed(123)  # Definir uma semente para reprodutibilidade
sample_indices <- sample(1:nrow(MoviesDB), 97)

# Criar a amostra aleatória
SMovies <- MoviesDB[sample_indices, ]

# Verificar a amostra
View(SMovies)
str(SMovies)
summary(SMovies)

attach(SMovies)

# Se filme for bom vai receber a classificação 1, se for ruim vai receber
# classificação 0 

SMovies["evaluation"] = ifelse(popularity >= 50, 1,0)

str(SMovies)

# Criação de um modelo logístico com base em duas variáveis númericas 
# selecionadas ao acaso, no caso "revenue" e "vote_average"

Ev = SMovies$evaluation
Rv = SMovies$revenue
Va = SMovies$vote_average

LogMod=glm(Ev ~ Rv + Va, family=binomial, data=SMovies)

summary(LogMod)

coefficients(LogMod)

# A função logistica da previsão de popularidade de um filme em função 
# do seu rendimento e da sua média de avaliação 

# Ev^= (P(ST=1))^ = 1/ (1 + exp(-(-12.50092619 + 0.011*Rv + 1.411*Va)))

# Aplicando o teste de hipótese z considerando 
#H0 beta-i = 0 || H1: betai !=0 

# temos que ambas as variáveis possuem valores menores que 0,05, logo há
# evidência estatística para rejeitarmos a hipótese nula, logo podemos concluir
# que ambas as variáveis são não lineares, portanto todas as variaveis 
# influenciam a probabilidade de um filme ser popular ou não

coefficients(LogMod)

exp(coefficients(LogMod))

# Intercepto: Quando revenue e vote_average são zero, os log-odds de um filme
# ser popular são -12.50092619.

# Revenue: A cada aumento de 1 unidade em revenue, a probabilidade de um 
# filme ser popular aumenta em cerca de 1.11%.

# Vote Average: A cada aumento de 1 unidade em vote_average, a probabilidade 
# de um filme ser popular aumenta em cerca de 310.1%.


ProbApr_Prev = LogMod$fitted.values


# Gráfico 1: Comparativo entre resultados previstos e variável "revenue"
plot(Rv, ProbApr_Prev, main = "Probabilidade de Popularidade vs. Revenue", 
     xlab = "Revenue", 
     ylab = "Probabilidade de Popularidade",
     col = "black",   # Cor dos pontos
     pch = 16)       # Símbolo dos pontos
abline(h = 0.5, col = "#374bfa")  # Linha horizontal em y = 0.5
points(Rv[ProbApr_Prev >= 0.5], 
       ProbApr_Prev[ProbApr_Prev >= 0.5],
       col = "#115852", 
       pch = 16)  
points(Rv[ProbApr_Prev < 0.5],
       ProbApr_Prev[ProbApr_Prev < 0.5],
       col = "#f44336",
       pch = 16)
legend("bottomright", 
       legend = c("Aprovado", "Reprovado", "Limiar de Decisão"), 
       col = c("#115852", "#f44336", "#374bfa"),
       pch = c(16, 16, NA),
       lty = c(NA, NA, 1))  # Remover a caixa ao #f44336or da legenda
mtext("A cada 1 aumento em revenue, a probabilidade aumenta em 1.11%", side = 3, line = 0.5, cex = 0.8)

# Gráfico 2: Comparativo entre resultados previstos e variável "vote_average"
plot(Va, ProbApr_Prev, main = "Probabilidade de Popularidade vs. Média de Avaliações", 
     xlab = "Média de Avaliações",
     ylab = "Probabilidade de Popularidade",
     col = "black",   # Cor dos pontos
     pch = 16)       # Símbolo dos pontos
abline(h = 0.5, col = "#374bfa")  # Linha horizontal em y = 0.5
points(Va[ProbApr_Prev >= 0.5], 
       ProbApr_Prev[ProbApr_Prev >= 0.5], 
       col = "#115852", 
       pch = 16)
points(Va[ProbApr_Prev < 0.5], 
       ProbApr_Prev[ProbApr_Prev < 0.5], 
       col = "#f44336", 
       pch = 16)  
legend("topleft", 
       legend = c("Aprovado", "Reprovado", "Limiar de Decisão"), 
       col = c("#115852", "#f44336", "#374bfa"),
       pch = c(16, 16, NA),
       lty = c(NA, NA, 1))  # Remover a caixa ao #f44336or da legenda
mtext("A cada 1 aumento em vote_average, a probabilidade aumenta em 310.1%", 
      side = 3, line = 0.5, cex = 0.8)

# Criação do modelo nulo para comparação

Null_LogMod = glm(Ev ~ 1, family = binomial, data = SMovies)
summary(Null_LogMod)

table(Ev)
# 20/97

# tem-se que a equação do modelo nulo é dada por:
# pi^ = 1/ (1 + exp(-(-1.348))) = 0.2061975

# teste ao racio de verosimilhança
#H0: beta_RM = beta_ND = beta_Dcom = 0 || h1 Algum beta != 0 

library(lmtest)
lrtest(LogMod, Null_LogMod)

# pvalue = 1.11e-10 < alfa -> rejeitar H0, ie, conclui-se que algum parâmetro
# e não nulo e por isso o modelo obtido é preferivel ao modelo nulo

summary(LogMod)
# Null deviance: 98.719  on 96  degrees of freedom
# Residual deviance: 52.876  on 94  degrees of freedom

# O modelo Logístico obtido é preferível ao modelo nulo, 
# pois apresenta uma deviance inferior à do modelo nulo.

# Comparar os modelos usando AIC

AIC(LogMod, Null_LogMod)
#LogMod       3  58.8757
#Null_LogMod  1 100.7186

# O modelo logístico ajustado é preferível ao modelo nulo, pois 
# apresenta um AIC significativamente menor. Isso implica que revenue e 
# vote_average são p#f44336itores importantes e contribuem para um melhor ajuste 
# do modelo em relação ao modelo nulo.

library(DescTools)
PseudoR2(LogMod, c("CoxSnell", "Nagelkerke", "McFadden"))

#Os valores indicam que o LogMod tem um ajuste razoável aos dados.

# Cox & Snell: 37.7%
# Nagelkerke: 58.98%
# McFadden: 46.44%

# Esses resultados sugerem que o modelo logístico é eficaz 
# em explicar a variabilidade da popularidade dos filmes com base nas 
# variáveis revenue e vote_average. Portanto, o modelo ajustado é 
# preferível ao modelo nulo e tem um ajuste relativamente bom, fornecendo
# uma boa capacidade preditiva.

library(pROC)

# Calcular a curva ROC
roccurve = roc(Ev ~ LogMod$fitted)
plot(roccurve)
auc(roccurve)

# Area under the curve: 0.9039

# Determinar o ponto de corte ótimo
optimal_cutoff<- coords(roccurve, "best", ret = "threshold")
optimal_cutoff<- 0.3282472

plot(roccurve, 
     main = "Curva ROC para Classificação de Popularidade de Filmes",
     col = "#374bfa",
     lwd = 2,
     print.auc = TRUE,
     print.auc.cex = 1.5,
     print.thres = TRUE,
     print.thres.cex = 1.2,
     cex.lab = 1.2,
     cex.main = 1.5)

# Legenda
legend("bottomright", 
       legend = c("Curva ROC", NA), 
       col = c("#374bfa", NA), 
       lwd = 2, 
       cex = 1.2, 
       bty = "n")

points(1 - optimal_cutoff[1], optimal_cutoff[2], col = "black", pch = 16, cex = 1.5)
text(1 - optimal_cutoff[1], optimal_cutoff[2], labels = sprintf(" %.3f (%.3f, %.3f)", optimal_cutoff[1], optimal_cutoff[2], optimal_cutoff[3]), pos = 4, cex = 1.2)


###################
# Criar o gráfico de densidade
plot(density(LogMod$fitted[Ev == 0]), col = "blue", lwd = 2, main = "Distribuição de Filmes Populares e Não Populares",
     xlab = "Probabilidade Prevista", ylab = "Densidade", xlim = c(0, 1), ylim = c(0, 5))
lines(density(LogMod$fitted[Ev == 1]), col = "red", lwd = 2)
abline(v = optimal_cutoff, col = "black", lwd = 2)
polygon(density(LogMod$fitted[Ev == 0]), col = rgb(0, 0, 1, 0.2), border = "blue")
polygon(density(LogMod$fitted[Ev == 1]), col = rgb(1, 0, 0, 0.2), border = "red")

# Adicionar legendas
text(x = 0.15, y = 4.5, "Não Populares", col = "blue", cex = 1.2, pos = 4)
text(x = 0.75, y = 4.5, "Populares", col = "red", cex = 1.2, pos = 2)
text(x = optimal_cutoff, y = 0.2, "Ponto de Corte", col = "black", cex = 1.2, pos = 4, srt = 90)

# Adicionar legenda
legend("topright", legend = c("Não Populares", "Populares"), col = c("blue", "red"), lwd = 2)

######

# Converter as probabilidades previstas em classes binárias usando o ponto de corte
predicted_classes <- ifelse(LogMod$fitted >= optimal_cutoff, 1, 0)

confusion_matrix <- table(Actual = Ev,Predicted = predicted_classes)

# Printar a matriz de confusão no console
print(confusion_matrix)

print(prop.table(confusion_matrix) * 100)
