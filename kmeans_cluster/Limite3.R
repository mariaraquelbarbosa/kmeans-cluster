#**********************************************************
#ANALISE DE CLUSTER - LIMITE
#**********************************************************

#**********************************************************
#Atencao: Alterar Diretorio
setwd("...")

#Retirar notacao cientifica no R
options(scipen = 999)

#**********************************************************
#Instalacao de pacotes (base realizar 1 vez)
#install.packages("cluster")
#install.packages("factoextra")
#install.packages("gridExtra")

#**********************************************************
#Leitura da Base 
library(readxl)
limite <- read_excel("Limite3.xlsx", sheet = 'Base de Dados')
nrow(limite)
ncol(limite)
#**********************************************************

#**********************************************************
# Faca uma analise exploratoria da base de dados 
# (obtenha as medidas de posicao e dispersao). 
summary(limite[,-1]) #Min, Q1, Q2, Q3 e Max
apply(limite[,-1] , 2 , sd) #Desvio Padrao

#Faca o box plot e histograma para todas as variaveis.
par(mfrow=c(1,3))
hist(limite$LIMITE, col="paleturquoise", main="LIMITE")  
hist(limite$IDADE, col="darkgoldenrod1", main="IDADE") 
hist(limite$PERC_USO_CARTAO, col="pink", main="PERC USO DO CARTAO") 

boxplot(limite$LIMITE, col="paleturquoise", main="LIMITE")  
boxplot(limite$IDADE, col="darkgoldenrod1", main="IDADE") 
boxplot(limite$PERC_USO_CARTAO, col="pink", main="PERC USO DO CARTAO")

#**********************************************************
#Padronize as variaveis.
limite_z<-scale(limite[,-1])
head(limite_z)

#Utilize o metodo K-Medias 
#Qual numero de grupos e melhor?
set.seed(12345) # Ao mudar essa semente, o agrupamento muda

library(cluster)    
library(factoextra) 
library(gridExtra)

#Metodo K-means, 2 grupos
dados.k2 <- kmeans(limite_z, centers = 2, nstart = 25, iter.max = 100)
dados.k3 <- kmeans(limite_z, centers = 3, nstart = 25, iter.max = 100)
dados.k4 <- kmeans(limite_z, centers = 4, nstart = 25, iter.max = 100)
dados.k5 <- kmeans(limite_z, centers = 5, nstart = 25, iter.max = 100)

#Graficos
par(mfrow=c(1,1))
G1 <- fviz_cluster(dados.k2, geom = "point", data = limite_z) + ggtitle("k = 2")
G2 <- fviz_cluster(dados.k3, geom = "point", data = limite_z) + ggtitle("k = 3")
G3 <- fviz_cluster(dados.k4, geom = "point", data = limite_z) + ggtitle("k = 4")
G4 <- fviz_cluster(dados.k5, geom = "point", data = limite_z) + ggtitle("k = 5")

#Criar uma matriz com 4 graficos
grid.arrange(G1, G2, G3, G4, nrow = 2)

#Agrupar cluster e base - foi considerado 3 grupos
limite<- data.frame(limite, dados.k3$cluster)
head(limite)

# numero de observacoes em cada grupo
table(dados.k3$cluster)

#Analise as caracteristicas de cada grupo.
#Distribuicao das variaveis por cluster
par(mfrow=c(1,3)) #coloca os graficos lado a lado
boxplot(limite$LIMITE ~ limite$dados.k3.cluster, col="paleturquoise", main="Limite")
boxplot(limite$IDADE ~ limite$dados.k3.cluster, col="darkturquoise", main="Idade")
boxplot(limite$PERC_USO_CARTAO ~ limite$dados.k3.cluster, col="paleturquoise", main="% Uso do Cartao")
