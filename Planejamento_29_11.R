y = c(73, 68, 74, 71, 67, 
      73, 67, 75, 72, 70, 
      75, 68, 78, 73, 68, 
      73, 71, 75, 75, 69)

trat = factor(rep(c(1,2,3,4),each = 5)); trat
bloco = rep(c(1,2,3,4,5), each = 4); bloco

cbind(y, trat, bloco)

library(ExpDes.pt)

boxplot(y~trat)
points(trat, y, pch = 19)

bartlett.test(y~trat)

tapply(y, trat, var)
tapply(y, trat, mean)

require(ExpDes.pt)

saida = dbc(trat, bloco, y, 
            quali = T,
            mcomp = "tukey")

names(saida)
cbind((y-saida$valores.ajustados), saida$residuos)

plot(saida$residuos/sqrt(1.817),
     ylim = c(-2.5, 2.5),
     pch = 19,
     col = (rep(c(3,4,5,6), each = 5))) #Resíduos padronizados
abline(h=2)
abline(h=0)
abline(h=-2)

qqnorm(saida$residuos)
qqline(saida$residuos)

yt=log(y)
saida2=dbc(trat, bloco, yt,
           quali = T, mcomp = "tukey")
qqnorm(saida2$residuos)
qqline(saida2$residuos)
