getwd()

# Carregar os pacotes necessários para realizar os c?lculos
library(lifecontingencies)
library(tidyverse)

# Importar a base de dados
dados <- read.csv("base_avaliação atuarial no R.csv", header = TRUE, sep = ";")
dados <- data.frame(ID = c(1,2,3), Idade = c(20, 30, 40), t = c(0, 10, 20), k = c(40, 25, 20), r = c(60, 55, 60), sal_m = c(2000, 2500, 3000))
write_csv(dados, "base_avaliação atuarial no R.csv")

# Visualizar os dados
print(dados)

# Importar a tábua de mortalidade
at2000 <- read.table("at2000.txt", header = TRUE)
head(at2000)

x <- as.numeric(at2000$x)
lx <- as.numeric(at2000$lx)
class(x)
class(lx)

# Criar a tábua atuarial com a comutação com juros a 6%
tabua <- new("actuarialtable", x = x, lx = lx, interest = 0.06)
tabua

# Função da probabilidade:
#pxt(tabua, x, t)

# Função anuidade:
#axn(tabua, x, i = 0.06, k = 12, payment = "advance") # antecipada
#axn(tabua, x, i = 0.06, k = 12) # postecipada

#axn(tabua, dados$Idade, i = 0.06, k = 12) # postecipada

# Projetar o salário anual
dados$sal_an <- dados$sal_m*13
dados

# Projetar o benefício de aposentadoria
dados$beneficio <- dados$sal_an*(1+0.01)^dados$k  
dados
# Aplicar a função da probabilidade 
#  for(i in 1:length(dados$ID)){
#    dados$kpx[i] <- pxt(tabua, dados$Idade[i], t = dados$k[i]) 
#    }
#dados

# Outra forma 
dados$kpx <- pxt(tabua, dados$Idade, t = dados$k)
dados

# Aplicar a função da anuidade vitalícia
#  for(i in 1:length(dados$ID)){
#    dados$ar[i] <- axn(tabua, x = dados$r[i], i = 0.06, payment = "advance")
#  }
#dados
# outra forma 
dados$ar <- axn(tabua, x = dados$r, i = 0.06, payment = "advance")
dados
# Calcular o VABF
#dados$vabf <- dados$beneficio*dados$kpx*dados$ar*(1/(1.06)^dados$k)
#dados
# Outra forma
VABF <- function(beneficio, kpx, ar, k, i){
  beneficio*kpx*ar*(1/(1+i)^k)
}
dados$vabf <- VABF(dados$beneficio, dados$kpx, dados$ar, dados$k, i = 0.06)
dados
# Calcular o CN - Crédito Unitário Projetado
#dados$cn <- dados$vabf/(dados$t+dados$k)
#dados
# outra forma
CN <- function(vabf, t, k){
  vabf/(t+k)
}
dados$cn <- CN(dados$vabf, dados$t, dados$k)
dados
# Calcular a RM pelo Crédito Unitário Projetado
#dados$rm <- dados$vabf*(dados$t/(dados$t+dados$k))
#dados
# outra forma
RM <- function(vabf, t, k){
  vabf*(t/(t+k))
}
dados$rm <- RM(dados$vabf, dados$t, dados$k)
dados

# Reserva Matemática 
RM <- sum(dados$rm)

# Ativo = R$50.000
Ativo <- 120000

# Resultado Atuarial = Ativo - RM
Resultado <- Ativo - RM
Resultado


# Como faço para adicionar um novo participante?
dados[4,] <- c(4, 33, 8, 27, 60, 3200, NA, NA, NA, NA, NA, NA, NA)
dados

# Agora tenho que calcular novamente o salário anual, o benefício e etc.
dados$sal_an <- dados$sal_m*13
dados$beneficio <- dados$sal_an*(1+0.01)^dados$k  
dados

# Calcular novamente as funções
dados$kpx <- pxt(tabua, dados$Idade, t = dados$k)
dados$ar <- axn(tabua, x = dados$r, i = 0.06, payment = "advance")
dados$vabf <- VABF(dados$beneficio, dados$kpx, dados$ar, dados$k, i = 0.06)
dados$cn <- CN(dados$vabf, dados$t, dados$k)
dados$rm <- RM(dados$vabf, dados$t, dados$k)

# Visualizar a tabela
dados

# VABF total
sum(dados$vabf)
# CN total
sum(dados$cn)
# RM total
sum(dados$rm)



