DF <- function (x) {
  kmax <- floor((length(x)/2))
  df <- DFkmax(x, kmax)
  return(df)
}

DFkmax <- function (x, kmax) {
  l <- Elekaunomax(x, kmax)
  a <- 1:kmax
  b <- log(l, 10)
  a <- log(1/a, 10)
  dfkmax <- Pendienteminimoscuadrados(a, b)
  return(dfkmax)
}

Elekaunomax <- function (x, kmax) {
  n <- length(x)
  
  if (kmax > floor(n/2)) {
    print("kmax no puede ser mayor que la mitad de length(x)")
  } else {
    elekaunomax <- parSapply(cl, 1:kmax, Eleka)
    return(elekaunomax)
  }
}

Eleka <- function(k) {
  n <- length(x)
  suma <- rep(0, k)
  longituddecurvamk <- double()
  for (i in 1:k) {
    vec <- seq(i + k, i + (floor((n - i)/k)*k), k)
    
    for (j in vec) {
      anterior <- suma[i]
      suma[i] <- anterior + abs(x[j] - x[j - k])
    }
    
    facdenorm <- ((n-1)/(floor((n-i)/k)*k))/k
    longituddecurvamk <- c(longituddecurvamk, suma[i]*facdenorm)
  }
  
  promediodelaslongitudes <- sum(longituddecurvamk)/k
  return(promediodelaslongitudes)
}

Pendienteminimoscuadrados <- function(a, b) {
  if (length(a) != length(b)) {
    print("Las series deben er pareadas.")
  } else {
    n <- length(a)
    Sxy <- sum(a * b)
    Sxx <- sum(a * a)
    Sx <- sum(a)
    Sy <- sum(b)
    pendienteminimoscuadrados <- ((n * Sxy)-(Sx * Sy))/((n * Sxx)-(Sx * Sx))
    
    return(pendienteminimoscuadrados)
  }
}

library(parallel)
cl <- makeCluster(detectCores() - 1)

library(readr)
datos <- c(read_csv("C:/Users/Liliana Saus/Desktop/ProyectoElisa/A.txt", col_names = FALSE), 
       read_csv("C:/Users/Liliana Saus/Desktop/ProyectoElisa/B.txt", col_names = FALSE),
       read_csv("C:/Users/Liliana Saus/Desktop/ProyectoElisa/C.txt", col_names = FALSE),
       read_csv("C:/Users/Liliana Saus/Desktop/ProyectoElisa/D.txt", col_names = FALSE),
       read_csv("C:/Users/Liliana Saus/Desktop/ProyectoElisa/E.txt", col_names = FALSE),
       read_csv("C:/Users/Liliana Saus/Desktop/ProyectoElisa/F.txt", col_names = FALSE))

for (i in 1:6) {
  x <- as.double(unlist(datos[i]))
  clusterExport(cl, "x")
  print(DF(x))
}
