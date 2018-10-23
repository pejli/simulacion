library(doParallel)
cluster <- makeCluster(detectCores() - 1)
#FUNCIONES
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}
poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}
eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}
domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}
valor<-function(i){
  val <- matrix(rep(NA, k), ncol=k)
  for (j in 1:k) { # para todos los objetivos
    val[, j] <- eval(obj[[j]], sol[i,], tc)
  }
  return(val)
}
#VARIABLES
vc <- 4
md <- 3
tc <- 5
n=200
datos<-data.frame(funciones= integer(), replicas=integer(), 
                  soluciones= integer(), porcentaje=integer())

 for(k in 2:10){
    print(k)
for(replica in 1:100){
print(replica)
obj <- list()
for (i in 1:k) {
  obj[[i]] <- poli(vc, md, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
#EXPORTAR
clusterExport(cluster, c("n","k", "sol", "tc", "obj", "eval", "dim", "valor"))

#A PARALELIZAR!!!
val <- parSapply(cluster, 1:n, valor)
val <- t(val)
mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
no.dom <- logical()
dominadores <- integer()

for (i in 1:n) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
}
frente <- subset(val, no.dom) # solamente las no dominadas
porcentaje<-((dim(frente)[1])/n)*100
datos<-rbind(datos, data.frame(funciones=k, replicas=replica, soluciones=n, porcentaje))
}
 }
 #graficar 
ggplot(data=datos, aes(x =funciones, y=porcentaje, fill=funciones)) +
  +     geom_violin(scale="width") + scale_color_manual(values="#999999", "#E69F00", "#56B4E9") +
  geom_boxplot(width=0.2)   
  


