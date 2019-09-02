#############
## Aula 03 ##
#############

#### Da Aula 02 ####
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
  aux.x = solve(t(X)%*%X)  #(X'X)^-1
  bh = aux.x%*%t(X)%*%y
  yh = X%*%bh # y predito
  eh = y - yh # residuo
  
  #aula 03#
  n = nrow(X)
  k = ncol(X)
  sig2 = sum(eh^2)/(n-k)
  sd.b = sqrt(sig2*diag(aux.x)) #desvio padrao de beta
  estatistica.t = bh/sd.b
  p.value = 2*(1 - pt(abs(estatistica.t), df = n-k, lower.tail = T)) #pt = acumulada 
  A = data.frame(bh, sd.b, estatistica.t, p.value)
  row.names(A) = paste("b", 0:(k-1), sep= "")
  return(list("coef"=bh,"fitted"=yh,"residuos"=eh, "summary" = A))
}

reg = f.mqo(X,y)
reg.lm = lm(y ~ 1 + X1)
######################

reg$summary

summary(reg.lm)
