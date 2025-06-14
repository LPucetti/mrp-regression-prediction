## GRUPO 3 ##

#### Bibliotecas ####

library(lmtest)
library(DescTools)
library(pROC)
library(corrplot)
library(readxl)
library(writexl)
library(plm)
library(readr)
library(ggplot2)
library(forecast)
library(tseries)


#### Exercicio 1 ####

load("C:/Users/Leo Nogueira/Documents/MestradoCienciadosDados/S2/MRP/Projeto/MoviesDB.RData")
attach(MoviesDB)

str(MoviesDB)

# tenho que usar uma base de dados aleatória (salvar essa nova lista de dados)
# com 100 observações menos 3 aleatórias. 

set.seed(123)  # Definir uma semente para reprodutibilidade
sample_indices <- sample(1:nrow(MoviesDB), 97)

# Criar a amostra aleatória
SMovies <- MoviesDB[sample_indices, ]

# Verificar a amostra

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

# Converter as probabilidades previstas em classes binárias usando o ponto de corte
predicted_classes <- ifelse(LogMod$fitted >= optimal_cutoff, 1, 0)

confusion_matrix <- table(Actual = Ev,Predicted = predicted_classes)

# Printar a matriz de confusão no console
print(confusion_matrix)

print(prop.table(confusion_matrix) * 100)

#### Exercicio 2 ####

#Dados a utlizar= de (2N+1) a (2N+11)= de 7 a 17

#Conversão dos dados para numeric
Crime_G3 <- sapply(Crime_G3_tratado, as.numeric)
Crime_G3 <- as.data.frame(Crime_G3)
attach(Crime_G3)


#Nota: Eliminaram-se as seguintes variáveis que continham valores nulos:
#clcrmrte	clprbarr	clprbcon	clprbpri	clavgsen	clpolpc	cltaxpc	clmix

#Eliminaram-se as variáveis binárias:
#west	central	urban d82	d83	d84	d85	d86	d87


#### Análise de Correlação (Spearman)

cor_matrix = cor(Crime_G3, method = "spearman")
#Gráfico (com escala de cores)

corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Tabela (com valores)
correlacao = cor(Crime_G3, method = c("spearman"))
correlacao

#Exportar tabela para excel
install.packages(c("readxl","writexl")) 

file="Matriz_Corr.xlsx"
correlacao=data.frame(correlacao)
write_xlsx(correlacao,file)

#Variáveis que serão utilizadas nos modelos:
densidade=Crime_G3$density
minorias=Crime_G3$pctmin80
criminalidade=Crime_G3$crmrte

#Pooled Model / MQO
PooledMod = plm(densidade ~ criminalidade + minorias, model="pooling", data=Crime_G3)

summary(PooledMod)
coefficients(PooledMod)

#Eq. Pooled Model: D^= 0.50712332 + 4.72695522*C -0.00445334*M
#R^2=0.77081

#Teste F para avaliação de sginificância do MQO
summary(PooledMod)

#H0:beta_1=beta_2=beta_3=0
#H1:Algum parametro é diferente de 0

#p-value: = 3.341e-13 < alpha (usual) => Rejeitar H0, 
#Existe evidência estatística para concluir que existem parametros não nulos, i.e. 
#o modelo obtido é preferível ao modelo constante (igual ao custo médio)

### Modelo de Efeitos Fixos (MEF)

FEMod = plm(densidade ~ criminalidade + minorias, model="within", data=Crime_G3)

summary(FEMod)
coefficients(FEMod)
fixef(FEMod)
#D-densidade populacional; C-criminalidade; M-minorias
#Eq. condado 7: D^_7 = 0.48745 + 0.10831c + 0M
#Eq. condado 9: D^_9 = 0.54035 + 0.10831c + 0M
#Eq. condado 11: D^_11 = 0.60080 + 0.10831c + 0M
#Eq. condado 13: D^_13 = 0.50859 + 0.10831c + 0M
#Eq. condado 15: D^_15 = 0.30113 + 0.10831c + 0M
#Eq. condado 17: D^_17 = 0.34748  + 0.10831c + 0M
#R^2=0.0030141

#### Teste F Verificação de existência de efeitos fixos

pFtest(FEMod,PooledMod)
#p-value = 2.2e-16 < alpha(usual)-> Rejeitar H0 
#Ou seja, existe evidência estatística para concluir que os efeitos fixos não são todos iguais
# Logo, o MEF é preferível ao de dados empilhados



#### Modelo Efeitos Aleatórios (MEA)
REMod = plm(densidade ~ criminalidade + minorias, model="random", random.method = "walhus", data=Crime_G3)
summary(REMod)

# Eq. Modelo Ef. Aleatórios: 
# D^= 0.57991629 + 0.43776198*C - 0.00397076*M
#R^2=0.58218

#### Teste de Breusch-Pagan

#H0: Var(u)=0 vs H1: Var(u)>0
#equivale a:
#H0: "Não há efeitos aleatórios" vs H1: "Existem efeitos aleatórios" 



plmtest(PooledMod, type="bp")
#p-value = 2.2e-16< alpha(usual) -> Rejeitar H0
#Ou seja,  existe evidência estatística para concluir que existem diferentes efeitos aleatórios
#Logo assume-se que existem efeitos aleatórios e portanto
#o modelo de efeitos aleatórios é preferível ao de dados empilhados



#### Teste de Hausman
#Como se concluiu que existem efeitos aleatórios e fixos, ou seja, que quer o MEF 
#quer o MEA são preferíveis aos Pooled Model, fez-se de seguida teste de Hausman
#para averiguar se MEA é confiável

#H0: MEF e MEA são consistentes
# vs H1:MEF é consistente e MEA é inconsistente
#(Tem a ver com estimação e não com se um modelo é melhor que o outro)
phtest(FEMod, REMod)
#p-value = 0.3553 > alpha(usual) -> Não Rejeitar H0
#não existe evidência estatística para afirmar que MEA é um modelo inconsistente pelo que, à falta de prova 
#em contrário, assume-se que este modelo é consistente e que que o MEF quer o MEA são modelos fiáveis.


#### Exercicio 3 ####

beer_data <- read_csv('/home/joao/Desktop/mestrado/MRP/projetoMRP/BeerProdAustralia.csv')

beerTS = ts(beer_data$`Monthly beer production`, start = c(1962,01), end= c(1964,12),frequency = 12)
plot(beerTS, ylab = "Produção mensal em megalitros", xlab = "Ano", main = "Produção de cerveja na Australia")

t = time(beerTS)
print(t)
Mod_tend = lm(beerTS ~ t)
Mod_semTend = beerTS - Mod_tend$fitted.values
coefficients(Mod_tend)


plot(beerTS, ylim = c(-30, 150), ylab = "Produção mensal em megalitros", xlab = "Ano", main = "Produção de cerveja na Austrália")
abline(Mod_tend, col = "green")
lines(Mod_semTend, col = "red")
legend("topleft", legend = c("Série Original", "Linha de Tendência", "Sem Tendência"), col = c("black", "green", "red"), lty = c(1, 1, 1), bty = "n")

Acf(beerTS, lag=36)
Pacf(beerTS, lag=36)

modelo_sarima <- auto.arima(beerTS, seasonal=TRUE)
summary(modelo_sarima)
plot(modelo_sarima$x, col="red", xlab="Ano", ylab="Produção de Cerveja (Megalitros)", main="Produção Mensal de Cerveja na Austrália")
lines(fitted(modelo_sarima), col="blue")
legend("topleft", legend=c("Valores Reais", "Valores Ajustados"), col=c("red", "blue"), lty=1, bty="n")


previsoes <- forecast(modelo_sarima, h=12)
plot(previsoes, main="Previsão da Produção de Cerveja para o Próximo Ano", xlab="Ano", ylab="Produção de Cerveja (Megalitros)")
lines(beerTS, col="red")
lines(fitted(modelo_sarima), col="green")
legend("topleft", legend = c("Previsões", "Valores Reais", "Valores Ajustados"), col = c("blue", "red", "green"), lty = 1)

valores_previstos <- as.numeric(previsoes$mean)
max(valores_previstos)
min(valores_previstos)
mean(valores_previstos)
