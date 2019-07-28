getwd()

# Carregar os pacotes necess?rios para realizar os c?lculos
library(lifecontingencies)
library(tidyverse)

# Importar a base de dados
dados <- read.csv("base_avaliação atuarial no R.csv", header = TRUE, sep = ";")
dados <- data.frame(ID = c(1,2,3), Idade = c(20, 30, 40), t = c(0, 10, 20), k = c(40, 25, 20), r = c(60, 55, 60), sal_m = c(2000, 2500, 3000))
write_csv(dados, "base_avaliação atuarial no R.csv")

# Visualizar os dados
print(dados)

# Importar a t?bua de mortalidade
at2000 <- read.table("at2000.txt", header = TRUE)
head(at2000)

x <- as.numeric(at2000$x)
lx <- as.numeric(at2000$lx)
class(x)
class(lx)

# Criar a t?bua atuarial com a comuta??o com juros a 6%
tabua <- new("actuarialtable", x = x, lx = lx, interest = 0.06)
tabua

# Fun??o da probabilidade:
pxt(tabua, x, t)

# Fun??o anuidade:
axn(tabua, x, i = 0.06, k = 12, payment = "advance") # antecipada
axn(tabua, x, i = 0.06, k = 12) # postecipada

# Projetar o sal?rio anual
dados$sal_an <- dados$sal_m*13
dados

# Projetar o benef?cio de aposentadoria
dados$beneficio <- dados$sal_an*(1+0.01)^dados$k  
dados
# Aplicar a fun??o da probabilidade 
  for(i in 1:length(dados$ID)){
    dados$kpx[i] <- pxt(tabua, dados$Idade[i], t = dados$k[i]) 
    }
dados

# Aplicar a fun??o da anuidade vital?cia
  for(i in 1:length(dados$ID)){
    dados$ar[i] <- axn(tabua, x = dados$r[i], i = 0.06, payment = "advance")
  }
dados

# Calcular o VABF
dados$vabf <- dados$beneficio*dados$kpx*dados$ar*(1/(1.06)^dados$k)
dados

# Calcular o CN
dados$cn <- dados$vabf/(dados$t+dados$k)
dados

# Calcular a rm
dados$rm <- dados$vabf*(dados$t/(dados$t+dados$k))
dados

# Como fa?o para adicionar um novo participante?
dados[4,] <- c(4, 33, 8, 27, 60, 3200, NA, NA, NA, NA, NA, NA, NA)
dados

# Agora tenho que calcular novamente o sal?rio anual, o benef?cio e etc.
dados$sal_an <- dados$sal_m*13
dados$beneficio <- dados$sal_an*(1+0.01)^dados$k  

for(i in 1:length(dados$ID)){
  dados$kpx[i] <- pxt(tabua, dados$Idade[i], t = dados$k[i]) 
}


for(i in 1:length(dados$ID)){
  dados$ar[i] <- axn(tabua, x = dados$r[i], i = 0.06, payment = "advance")
}

dados$vabf <- dados$beneficio*dados$kpx*dados$ar*(1/(1.06)^dados$k)
dados$cn <- dados$vabf/(dados$t+dados$k)
dados$rm <- dados$vabf*(dados$t/(dados$t+dados$k))
dados



