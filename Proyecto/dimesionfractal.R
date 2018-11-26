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
    elekaunomax <- double()
    for (i in 1:kmax) {
      elekaunomax[i] <- Eleka(x, i)
    }
    return(elekaunomax)
  }
}

Eleka <- function(x, k) {
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












