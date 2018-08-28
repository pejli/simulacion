#GENERAR PRIMOS
hastaa<-13963
desde<-10000
hasta<-10471
replicas<-30
primo <- function(n) {
  for (i in 2:(n-1)) {
    if ((n > i && n %% i) == 0) { # residuo es cero
      return(FALSE)
    }
  }
  return(n)
}
primos <- numeric() # un vector vacio
for (n in desde:hastaa) {
  primos <- c(primos, primo(n)) # combinar vectores
}
#cat(primos, "\n")

noprimo <- function(n) {
  for (i in 2:(n-1)) {
    if ((n > i && n %% i) == 0) { # residuo es cero
      return(n)
    }
  }
  return(FALSE)
}
noprimos <- numeric() # un vector vacio
for (m in desde:hasta) {
  noprimos <- c(noprimos, noprimo(m)) # combinar vectores
}
#cat(noprimos, "\n")

a<-primos[which(primos>0)]
l<-noprimos[which(noprimos>0)]
#print(a)
#print(l)
b<- sort(a,decreasing = TRUE)
#print(b)
d<-sort(l,decreasing = TRUE)
v<-sample(a)
w<-sample(d)
#print(d)
# a vector de primos creciente
# b vector de primos decrecientes
# v vector de primos aleatorios
# l vector de no primos  crecientes
# d vector de no primos decrecientes
# w vector de no primos aleatorios 
# x vector con 420 primos y 420 no primos aletoriamente 
# y vector con 420 primos y 420 no primos crecientes
# k vector con 420 primos y 420 no primos decrecientes
e <- sample(a, size=420 ,replace = TRUE, prob = NULL)
f<-sample(l,size=420, replace = TRUE,prob=NULL)
x<- c(e,f)
k<-sort(x,decreasing=TRUE)
y<-sort(x,decreasing=FALSE)
###################### LO CHIDO
primx <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores()-2))

at <- numeric()
bt <-numeric()
vt <- numeric()
dt <-numeric()
lt <-numeric()
wt <-numeric()
xt <-numeric()
yt <-numeric()
kt <-numeric()

for (r in 1:replicas) {
  at <- c(at, system.time(foreach(n = a, .combine=c) %dopar% primx(n))[3]) # creceintes primos
  bt <- c(bt, system.time(foreach(n = b, .combine=c) %dopar% primx(n))[3]) # decrecientes  primos
  vt <- c(vt, system.time(foreach(n = v, .combine=c) %dopar% primx(n))[3]) # aleatorios primos
  dt <- c(dt, system.time(foreach(n = d, .combine=c) %dopar% primx(n))[3]) # crecientes  no primos
  lt <- c(lt, system.time(foreach(n = l, .combine=c) %dopar% primx(n))[3]) # decrecientes no primos
  wt <- c(wt, system.time(foreach(n = w, .combine=c) %dopar% primx(n))[3]) # alatorios no primos 
  xt <- c(xt, system.time(foreach(n = x, .combine=c) %dopar% primx(n))[3]) # aleatorio 420 primos y 420 no primos 
  yt <- c(yt, system.time(foreach(n = y, .combine=c) %dopar% primx(n))[3]) # crecientes 420 primos y 420 no primos 
  kt <- c(kt, system.time(foreach(n = k, .combine=c) %dopar% primx(n))[3]) # decrecientes 420 primos y 420 no primos
}

stopImplicitCluster()


mean(at)
mean(bt)
mean(dt)
mean(lt)
mean(xt)
mean(vt)
mean(wt)
mean(yt)
mean(kt)

nucleos <- rep(2,18)
nucleos<-c(nucleos,rep(3,18))
nucleos<-c(nucleos,rep(4,18))
digitos<-rep(5,9)
digitos<-c(digitos,rep(4,9))
digitos<-c(digitos,rep(5,9))
digitos<-c(digitos,rep(4,9))
digitos<-c(digitos,rep(5,9))
digitos<-c(digitos,rep(4,9))
proportion<- rep(1,3)
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))          
proportion<-c(proportion,rep(1,3))
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))
proportion<-c(proportion,rep(1,3))
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))
proportion<-c(proportion,rep(1,3))
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))
proportion<-c(proportion,rep(1,3))
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))
proportion<-c(proportion,rep(1,3))
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))
p<-c(1, 2, 3)
ordenamiento<-rep(p,18)
promedio<-c(0.6767,0.6883,0.6767,0.5520,0.559,0.5660,0.6493,0.641,0.6263,0.297,0.29166, 0.2983,0.2743,0.273,0.2786,0.5633,0.5553,0.5493, 0.6517,0.6520, 0.646, 0.5867, 0.58,0.5787, 0.6563,0.6643,0.6197,0.3686,0.3723,0.366,0.3273,0.329,0.3203,0.6216,0.621,0.613,0.746,0.71,0.764,0.666,0.642,0.66,0.742,0.812,0.698,0.40333,0.38633,0.3876,0.3543,0.354,0.3486,0.6653,0.648,0.6683)
datos<-cbind(nucleos,digitos,proportion,ordenamiento,promedio)
datos<-as.data.frame(datos)

png("nucleos.png")
boxplot(datos$promedio ~datos$nucleos, xlab="N\u{FA}cleos", ylab="tiempo promedio")
png("digitos.png")
boxplot(datos$promedio ~datos$digitos, xlab="N\u{FA}mero de d\u{ED}gitos", ylab="tiempo promedio")
png("ordenamiento.png")
boxplot(datos$promedio ~datos$ordenamiento, xlab="Ordenamiento", ylab="tiempo promedio")
png("proporcion.png")
boxplot(datos$promedio ~datos$proportion, xlab="Proporci\u{F3}n", ylab="tiempo promedio")
graphics.off()
