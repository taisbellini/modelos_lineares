#############
## Aula 02 ##
#############
# DGP #
set.seed(1234)
k = 4
n = 300
b = matrix(c(1,-0.5,0.5,1),ncol = 1)
X1 = matrix(runif(n*(k-1),-1,1),nrow = n, ncol = (k-1))
X = cbind(1,X1)
e = matrix(rnorm(n,0,1),ncol = 1) # nao precisa matrix
y = X%*%b + e

# funcao MQO #

f.mqo = function(X,y){
  bh = solve(t(X)%*%X)%*%t(X)%*%y
  yh = X%*%bh # y predito
  eh = y - yh # residuo
  return(list("coef"=bh,"fitted"=yh,"residuos"=eh))
}

reg = f.mqo(X,y)
reg.lm = lm(y ~ 1 + X1)
