# ECONOMETRIA BASICA #
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
