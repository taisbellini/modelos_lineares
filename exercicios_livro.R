# ECONOMETRIA BASICA #

### CAP 3 ###
## 3.6 Exemplo numerico 
## Consumo familiar semanal (y), renda familiar semanal (x)

X = c(80, 100, 120, 140, 160, 180, 200, 220, 240, 260)
y = c(70, 65, 90, 95, 110, 115, 120, 140, 155, 150)

X.matr = cbind(1, X)

result = f.mqo(X.matr, y)
b0 = result$coef[1]
b1 = result$coef[2]

#vemos que o reusultado bate com o do livro

# graficos
plot(X, y)
regress = function (x){b0 + b1*x}
curve(regress, min(X), max(X), add=T)
abline(h=mean(y), color="red")
abline(v=mean(X))

#o quanto a linha regressiva encaixa nos dados?
## r2 (minusculo pois eh amostra)
## r2 = 1 - SQR/SQT

sqr = sum((result$res)^2)
sqt = sum((y-mean(y))^2)
r2 = 1 - (sqr/sqt)
# resultado: 96%

# Cap 3 - Exercicio 2 #

X1 = matrix(c(1, 4, 5, 6),nrow = 4, ncol = (2-1))
X = cbind(1, X1)
y = c(4, 5, 7, 12)

f.mqo = function(X, y){
  b.hat = solve(t(X)%*%X)%*%t(X)%*%y
  y.hat = X%*%b.hat
  e.hat = y - y.hat
  return(list("coef"= b.hat, "y.hat"= y.hat, "res"= e.hat))
}
reg = f.mqo(X, y)
reg$coef
