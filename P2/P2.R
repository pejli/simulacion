library(parallel)
dim <- 12
num <-  dim^2

datos<-data.frame()

#FUNCION DE LLENADO
paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == n))
}
##TRABAJO EN NUCLEOS
cluster <- makeCluster(detectCores())
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
#DECLARACION DE VARIABLES
datos<-data.frame()
columna1<-vector()
columna2<-vector()
columna3<-vector()
columna4<-vector()

###CAMINATAS:  c repeticiones, j para definir porbabilidad 
for(n in 2:4){
  columna1 <- c(columna1,rep(n, (30*10)))
    for(j in 1:10){
      proba<-j/10
      columna2 <- c(columna2, rep(proba,30))
        for(c in 1:30){ 
          columna3 <- c(columna3,c)
          actual <- matrix((runif(num)< (proba))*1, nrow=dim, ncol=dim)
           l<-0
           while(sum(actual)!=0 & (l < 50)){
             l<-l+1
             clusterExport(cluster, "actual")
             clusterExport(cluster, "n")
             siguiente <- parSapply(cluster, 1:num, paso)
             actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
           }
           columna4 <- c(columna4,l)
       }
    }
}
#IMPRIMIR
datos <- cbind(columna1, columna2, columna3, columna4)
datos <- as.data.frame(datos)

boxplot(datos$columna4[which(datos$columna1 == 3)] ~datos$columna2[which(datos$columna1 == 3)])
