install.packages("polycor")
install.packages("ggcorrplot")
library(psych)
library(polycor)
library(ggplot2)
library(ggcorrplot)

options(max.print=1000000)

#Cargue datos tipificados
library(readxl)
GruposInvest_Datos_R <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\Trabajo de grado II\\GruposInvest_Datos_R.xlsx", 
                                   sheet = "Tipificados", range = "A1:AY31")
View(GruposInvest_Datos_R)
corr_data_grupos <- cor(GruposInvest_Datos_R)
mat_cor_grupos <- hetcor(corr_data_grupos)$correlations

#Verificar que datos sean factorizables
  #Prueba de barlett
cortest.bartlett(mat_cor_grupos, n = 1000)->p_esf
p_esf$p
#Prueba de KMO
KMO(mat_cor_grupos)

#Identificar numero de factores
fa.parallel(mat_cor_grupos,n.obs=1620,fa="fa",fm="ml", main = '', ylabel = '')
title('Grafico de sedimentacion - Análisis Paralelo', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')

#Modelo factorial
library(GPArotation)

modelo_grupos<-fa(mat_cor_grupos, nfactors = 4, rotate = "varimax", fm="ml")


colnames(modelo_grupos$loadings) <- c("Factor_1", "Factor_2", "Factor_3", "Factor_4")


load_grupos <- modelo_grupos$loadings[,0:4]
load_grupos

comunalidades_grupos <- modelo_grupos$communality
especificidades_grupos <- modelo_grupos$uniquenesses

#Exportar datos excel
install.packages("xlsx")
library(xlsx)
write.xlsx(load_grupos, "AFE_results_grupos.xlsx")
write.xlsx(comunalidades_grupos, "comunalidades_grupos.xlsx")
write.xlsx(especificidades_grupos, "especificidades_grupos.xlsx")









