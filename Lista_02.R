# PRO5837 - Princípios de Modelagem em Finanças
#
# Prof. Celma de Oliveira Ribeiro
# Auno: Fernando Itano
#
# 2ª Lista de Exercícios

### Para a série de dados da primeira lista de exercícios construa:
### (sugestão: escolher ativos não correlacionados)
# a) Matriz de Covariância amostral entre 3 ativos de sua escolha considerando toda a série
# b) Subdivida a série em 4 períodos e analise o comportamento da matriz de covariância
# c) Defina alguns portfolios e esboce um gráfico no qual a abcissa corresponde a variância
#    e a ordenada ao retorno esperado do portfolio. Discuta o que ocorreu.
# d) Utilize simulação para construir portfolios (distribuição uniforme) e esboce os pontos
#    no gráfico risco x retorno.

# Importação da séries históricas
cotacoes.total = read.table("/Users/Fernando/Dropbox/02 - Mestrado EE Poli/Disciplinas/PRO5837/Cotacoes_Fechamento.csv", header=TRUE, sep = ";", dec = ",")
cotacoes.total$Data <- as.Date(cotacoes.total$Data, format="%d/%m/%Y")
str(cotacoes.total)

# Seleção de 3 ações
series = cotacoes.total[,c(1,21,24,13)]
str(series)
summary(series$Data)

# Cálculo do log retorno das 3 ações selecionadas
r1 = diff(log(series[,2]), lag=1) ; plot(r1, type="l")
r2 = diff(log(series[,3]), lag=1) ; plot(r2, type="l")
r3 = diff(log(series[,4]), lag=1) ; plot(r3, type="l")
series.r = data.frame(Data=series[2:854,1],FIBR3=r1, ITSA4=r2, DTEX3=r3)


# a) Matrizes de Covariância entre os ativos selecionados
round(cov(series.r[,2:4]),6)


# b) Matriz de Covariância em cada um dos 4 períodos

# Divisão da série em 4 períodos
d1 = floor(853/4*1)
d2 = floor(853/4*2)
d3 = floor(853/4*3)
series.r.p1 = series.r[1:d1,]     
series.r.p2 = series.r[(d1+1):d2,]
series.r.p3 = series.r[(d2+1):d3,]
series.r.p4 = series.r[(d3+1):853,]

series[1:d1,1]     
series[(d1+1):d2,1]
series[(d2+1):d3,1]
series[(d3+1):853,1]

covariancia.var = matrix(0,4,9)
covariancia.var[1,] = c(cov(series.r.p1[,2:4]))
covariancia.var[2,] = c(cov(series.r.p2[,2:4]))
covariancia.var[3,] = c(cov(series.r.p3[,2:4]))
covariancia.var[4,] = c(cov(series.r.p4[,2:4]))

round(matrix(covariancia.var[4,],3,3),6)

plot(1:4, covariancia.var[,1], type="b", main="Variância da ação FIBR3 por período", xlab="Período", ylab="Variância", axes = FALSE)
axis(side = 1,at = c(1:4)) ; axis(side = 2)

plot(1:4, covariancia.var[,5], type="b", main="Variância da ação ITSA4 por período", xlab="Período", ylab="Variância", axes = FALSE)
axis(side = 1,at = c(1:4)) ; axis(side = 2)

plot(1:4, covariancia.var[,9], type="b", main="Variância da ação DTEX3 por período", xlab="Período", ylab="Variância", axes = FALSE)
axis(side = 1,at = c(1:4)) ; axis(side = 2)

plot(1:4, covariancia.var[,2], type="b", main="Covariância entre as ações FIBR3 e ITSA4\npor período", xlab="Período", ylab="Covariância", axes = FALSE)
axis(side = 1,at = c(1:4)) ; axis(side = 2)

plot(1:4, covariancia.var[,3], type="b", main="Covariância entre as ações FIBR3 e DTEX3\npor período", xlab="Período", ylab="Covariância", axes = FALSE)
axis(side = 1,at = c(1:4)) ; axis(side = 2)

plot(1:4, covariancia.var[,6], type="b", main="Covariância entre as ações ITSA4 e DTEX3\npor período", xlab="Período", ylab="Covariância", axes = FALSE)
axis(side = 1,at = c(1:4)) ; axis(side = 2)


# c) Risco x Retorno de alguns portfolios

# Estatisticas das Séries
series.r.media = cbind(apply(series.r[,2:4],2,FUN = mean))
series.r.covar = cov(series.r[,2:4])

# Definição de alguns portfolios
portfolios = matrix(0,7,3)
portfolios[1,] = c(1/3, 1/3, 1/3)
portfolios[2,] = c(1/2, 1/2, 0)
portfolios[3,] = c(1/2, 0, 1/2)
portfolios[4,] = c(0, 1/2, 1/2)
portfolios[5,] = c(1, 0, 0)
portfolios[6,] = c(0, 1, 0)
portfolios[7,] = c(0, 0, 1)

# Cálculo do Retorno e Risco de cada portfolio
portfolios.rr = matrix(0,dim(portfolios)[1],2)

# Retornos por portfolio
for(i in 1:dim(portfolios)[1])
{
  portfolios.rr[i,1] = t(portfolios[i,]) %*% series.r.media
}

# Risco por portfolio
for(i in 1:dim(portfolios)[1])
{
  portfolios.rr[i,2] = t(portfolios[i,]) %*% series.r.covar %*% portfolios[i,]
}

portfolios.rr

plot(portfolios.rr[,2], portfolios.rr[,1], 
     xlab="Risco (variância portfolio)", ylab="Retorno", main="Risco x Retorno dos Portfolios")


# d) Simulação com 50.000 portfolios criados a partir de uma distribuição uniforme
n = 50000
portfolios = matrix(0,n,4)
portfolios[,1] = runif(n, min=0, max=1)
portfolios[,2] = runif(n, min=0, max=1)
portfolios[,3] = 1 - portfolios[,1] - portfolios[,2]
portfolios[,4] = 1:n

# Cálculo do Retorno e Risco de cada portfolio
portfolios.rr = matrix(0,dim(portfolios)[1],3)
portfolios.rr[,3] = 1:n

# Retornos por portfolio
for(i in 1:dim(portfolios)[1])
{
  portfolios.rr[i,1] = t(portfolios[i,1:3]) %*% series.r.media
}

# Risco por portfolio
for(i in 1:dim(portfolios)[1])
{
  portfolios.rr[i,2] = t(portfolios[i,1:3]) %*% series.r.covar %*% portfolios[i,1:3]
}

plot(portfolios.rr[,2], portfolios.rr[,1], 
     xlab="Risco (variância portfolio)", ylab="Retorno", main="Risco x Retorno dos Portfolios")

portfolios.rr[portfolios.rr[,2]==min(portfolios.rr[,2]),]
round(portfolios[1.4185e4,],4)

portfolios.rr[portfolios.rr[,1]==max(portfolios.rr[,1]),]
round(portfolios[4.9155e4,],4)
