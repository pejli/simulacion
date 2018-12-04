source("DimensionFractal.R")
source("read.R")
library(parallel)
cl <- makeCluster(detectCores())

dimensionFractal <- data.frame(Estatus = character(), Dimension = double(), Tiempo = double())

for (i in 1:10) {
  y <- unlist(control[i])
  clusterExport(cl, "y")
  time <- system.time({dim <- DF(y)})[3]
  
  dimensionFractal <- rbind(dimensionFractal, 
                            data.frame(Estatus = "Control", Dimension = dim, Tiempo = time))
}

for (i in 1:10) {
  y <- unlist(pacientes[i])
  clusterExport(cl, "y")
  time <- system.time({dim <- DF(y)})[3]
  
  dimensionFractal <- rbind(dimensionFractal, 
                            data.frame(Estatus = "Pacientes", Dimension = dim, Tiempo = time))
}
stopCluster(cl)
