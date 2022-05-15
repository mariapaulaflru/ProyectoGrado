install.packages("polycor")
install.packages("ggcorrplot")
library(psych)
library(polycor)
library(ggplot2)
library(ggcorrplot)

options(max.print=1000000)

#Cargue datos tipificados
library(readxl)
Empresa_Datos_R <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\Trabajo de grado II\\Empresa_Datos_R.xlsx", 
                              sheet = "Tipificados", range = "A1:BB19")
View(Empresa_Datos_R)
corr_data_emp <- cor(Empresa_Datos_R) 
mat_cor_emp <- hetcor(corr_data_emp)$correlations

#Verificar que datos sean adecuados

  #Prueba de bartlett
cortest.bartlett(mat_cor_emp, n = 500)->p_esf
p_esf$p
  #Prueba de KMO
KMO(mat_cor_emp)


#Identificar numero de factores
fa.parallel(mat_cor_emp,n.obs=972,fa="fa",fm="ml", main = '', ylabel = '')
title('Grafico de sedimentacion - Análisis Paralelo', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')


#Modelo factorial
library(GPArotation)

modelo_empresas<-fa(mat_cor_emp,nfactors = 3,rotate = "varimax",
                   fm="ml")


colnames(modelo_empresas$loadings) <- c("Factor_1", "Factor_2", "Factor_3")


load_emp <- modelo_empresas$loadings[,0:3]
load_emp

comunalidades_emp <- modelo_empresas$communality
especificidades_emp <- modelo_empresas$uniquenesses


#Exportar datos excel
install.packages("xlsx")
library(xlsx)
write.xlsx(load_emp, "AFE_results_empresas.xlsx")
write.xlsx(comunalidades_emp, "comunalidades_empresas.xlsx")
write.xlsx(especificidades_emp, "especificidades_empresas.xlsx")

