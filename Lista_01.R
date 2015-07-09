# PRO5837 - Princípios de Modelagem em Finanças
#
# Prof. Celma de Oliveira Ribeiro
# Auno: Fernando Itano
#
# 1ª Lista de Exercícios

### Com base em um conjunto de dados reais sobre preços de ações, faça, para duas ações:
# a) Gráfico de evolução dos preços
# b) Gráfico da série de retornos logarítmicos
# c) Histograma do retorno de cada ação
# d) Média, Desvio Padrão e Quantis referentes aos percentis 5% e 10%
# e) Verifique através do teste de Jarque-Bera se a distribuição de probabilidades dos
#    retornos logarítmicos pode ser considerada oriunda de uma distribuição Normal
# f) Com base na distribuição acumulada empírica, verifique se as distribuições são
#    aproximadamente Normais (empregue gráficos de probabilidade Normal)
# g) Extra: calcule a matriz de covariância e correlação dos ativos selecionados

# Importação da séries históricas
cotacoes.total = read.table("/Users/Fernando/Dropbox/02 - Mestrado EE Poli/Disciplinas/PRO5837/Cotacoes_Fechamento.csv", header=TRUE, sep = ";", dec = ",")
str(cotacoes.total)
cotacoes.total$Data <- as.Date(cotacoes.total$Data, format="%d/%m/%Y")
data.limite = as.Date(x = "31/12/2012",format = "%d/%m/%Y")
series = cotacoes.total[difftime(cotacoes.total$Data,data.limite)>0,c(1,21,24)] # Seleção das ações FIBR3 e ITSA4
str(series)


# a) Gráfico de evolução dos preços
plot(FIBR3 ~ Data, series, type="l", main="Histórico de Preços da Ação FIBR3", xlab="Data", ylab="Preço", xaxt = "n")
axis(1, series$Data, format(series$Data, "%b %Y"), cex.axis = .7)

plot(ITSA4 ~ Data, series, type="l", main="Histórico de Preços da Ação ITSA4", xlab="Data", ylab="Preço", xaxt = "n")
axis(1, series$Data, format(series$Data, "%b %Y"), cex.axis = .7)


# b) Gráfico das séries de retornos logaritmicos

# Cálculo do log retorno das 2 ações selecionadas
r1 = diff(log(series[,2]), lag=1) ; plot(r1, type="l")
r2 = diff(log(series[,3]), lag=1) ; plot(r2, type="l")
series.r = data.frame(Data=series[2:dim(series)[1],1],FIBR3=r1, ITSA4=r2)

plot(FIBR3 ~ Data, series.r, type="l", main="Histórico de Retornos Logarítmicos da Ação FIBR3", xlab="Data", ylab="Retorno Logarítmico", xaxt = "n")
axis(1, series$Data, format(series$Data, "%b %Y"), cex.axis = .7)

plot(ITSA4 ~ Data, series.r, type="l", main="Histórico de Retornos Logarítmicos da Ação ITSA4", xlab="Data", ylab="Retorno Logarítmico", xaxt = "n")
axis(1, series$Data, format(series$Data, "%b %Y"), cex.axis = .7)


# c) Histograma do retorno
hist(series.r$FIBR3,xlab = "Retornos Logarítmicos", freq = FALSE, main = "Distribuição dos Retornos Logarítmicos da Ação FIBR3", ylab="Densidade de Probabilidade")
hist(series.r$ITSA4,xlab = "Retornos Logarítmicos", freq = FALSE, main = "Distribuição dos Retornos Logarítmicos da Ação ITSA4", ylab="Densidade de Probabilidade")


#d) Média, Desvio Padrao, percentil 5% e 10%
(FIBR3.media = mean(series.r$FIBR3,na.rm = TRUE))
(FIBR3.dp = sd(series.r$FIBR3,na.rm = TRUE))
quantile(series.r$FIBR3, probs=c(0.05,0.10),na.rm = TRUE)

(ITSA4.media = mean(series.r$ITSA4,na.rm = TRUE))
(ITSA4.dp = sd(series.r$ITSA4,na.rm = TRUE))
quantile(series.r$ITSA4, probs=c(0.05,0.10),na.rm = TRUE)


#e) Distribuição dos retornos normal (Jarque-Bera)
library(tseries)
#library(help="tseries")
jarque.bera.test(x = series.r$FIBR3[-1])
jarque.bera.test(x = series.r$ITSA4[-1])


#f) Distr. acumulada empírica, verificar se é normal (empregue gráficos)
FIBR3.ecdf = ecdf(x = series.r$FIBR3)
plot(FIBR3.ecdf, main="Distribuição Empírica Acumulada dos Retornos Logarítmicos\nAção FIBR3")
lines(seq(from = -1,to = 1,length.out = 593),pnorm(q = seq(from = -1,to = 1,length.out = 593),mean = FIBR3.media,sd = FIBR3.dp), col="red", lwd=5, lty=2)
legend(x=-0.08, y=0.95, legend=c("FDP Empírica", "FDP Normal"), fill=c("black","red"), lty = c(1,2), col=c("black","red"), cex=0.8, box.col="white")

ITSA4.ecdf = ecdf(x = series.r$ITSA4)
plot(ITSA4.ecdf, main="Distribuição Empírica Acumulada dos Retornos Logarítmicos\nAção ITSA4")
lines(seq(from = -1,to = 1,length.out = 593),pnorm(q = seq(from = -1,to = 1,length.out = 593),mean = ITSA4.media,sd = ITSA4.dp), col="red", lwd=5, lty=2)
legend(x=-0.12, y=0.95, legend=c("FDP Empírica", "FDP Normal"), fill=c("black","red"), lty = c(1,2), col=c("black","red"), cex=0.8, box.col="white")


#g) Matrizes de Covariância e Correlação
cov(data.frame(FIBR3 = series$FIBR3, ITSA4 = series$ITSA4))
cov(data.frame(FIBR3 = series.r$FIBR3, ITSA4 = series.r$ITSA4))

cor(data.frame(FIBR3 = series$FIBR3, ITSA4 = series$ITSA4))
cor(data.frame(FIBR3 = series.r$FIBR3, ITSA4 = series.r$ITSA4))
