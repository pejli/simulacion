kruskal.test(dimensionFractal$Dimension[which(dimensionFractal$Estatus == "Pacientes")] ~
               dimensionFractal$Dimension[dimensionFractal$Estatus == "Control"])