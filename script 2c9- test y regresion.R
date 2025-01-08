rm(list=ls())
getwd()
setwd("C:/Users/Usuario/Desktop/RIBEF/Articulo geno-feno+NGS 2024.12.27")
#########################################################ECUADOR####################################################################
####################################################################################################################################
library(readxl)
Ecuador_2C9 <- read_excel("Ecuador_2C9.xlsx")
View(Ecuador_2C9)
class(Ecuador_2C9$GENOTIPO_CYP2C9)

#eliminar los NA
Ecuador_2C9depurado <- na.omit(Ecuador_2C9) #de 121 pasamos a 116 pacientes
class(Ecuador_2C9depurado)
# Convertir la columna 'genotipo' a factor
Ecuador_2C9$GENOTIPO_CYP2C9 <- factor(Ecuador_2C9$GENOTIPO_CYP2C9, levels = c("*1/*1", "*1/*2", "*2/*3"))
Ecuador_2C9depurado$GENOTIPO_CYP2C9 <- factor(Ecuador_2C9depurado$GENOTIPO_CYP2C9, levels = c("*1/*1", "*1/*2", "*2/*3"))
class(Ecuador_2C9depurado$GENOTIPO_CYP2C9)
table(Ecuador_2C9depurado$GENOTIPO_CYP2C9) #ver cuantos pacientes tienen los genotipos


library(car)
#no se hace shapiro para variables categoricas
shapiro.test(Ecuador_2C9depurado$LogIM_2C9) #si se hace shapiro para variables numericas. p-valor significativo, NO SIGUEN LA NORMALIDAD
#se haria entonces Krustall-Wallis. PARA SHAPIRO EN GENERAL
#Test de Krustal
kruskal_resultado <- kruskal.test(Ecuador_2C9depurado$LogIM_2C9 ~ Ecuador_2C9depurado$GENOTIPO_CYP2C9, data = Ecuador_2C9depurado)
summary(kruskal_resultado)
kruskal_ecuador <- kruskal_resultado$p.value

############################################################################################################
##############SI QUIERO VER LA NORMALIDAD PARA CADA GRUPO DE GENOTIPO, APLICO SHAPIRO POR SEPARADO##########
############################################################################################################
#aplicar shapiro en cada grupo de genotipo por separado
shapiro.test(Ecuador_2C9depurado$LogIM_2C9[Ecuador_2C9depurado$GENOTIPO_CYP2C9 == "*1/*1"]) #significativo
shapiro.test(Ecuador_2C9depurado$LogIM_2C9[Ecuador_2C9depurado$GENOTIPO_CYP2C9 == "*1/*2"]) #no significativo
shapiro.test(Ecuador_2C9depurado$LogIM_2C9[Ecuador_2C9depurado$GENOTIPO_CYP2C9 == "*2/*3"]) #no se puede hacer shapiro porque no hay genotipos *2/*3


#quiero hacer un analisis de regresion
regresion <- lm(LogIM_2C9 ~ GENOTIPO_CYP2C9, data = Ecuador_2C9depurado)
summary(regresion)

#RESULTADO: No hay una relación significativa entre el genotipo *1/*2 y los ratios metabólicos (LogIM_2C9) en comparación con el genotipo *1/*1, 
#ya que el valor p es alto (0.495).


#####################################################################NICARAGUA#############################################################################
Nicaragua_2C9 <- read_excel("CYP2C9/Nicaragua_2C9.xlsx")
View(Nicaragua_2C9)
Nicaragua_2C9depurado <- na.omit(Nicaragua_2C9) #pasamos de 212 a 209
class(Nicaragua_2C9depurado$Genotype)
class(Nicaragua_2C9depurado$Log_MR)
Nicaragua_2C9depurado$Genotype <- factor(Nicaragua_2C9depurado$Genotype, levels = c("wt/wt", "wt/*2", "wt/*3", "*3/3"))

shapiro.test(Nicaragua_2C9depurado$Log_MR) #significativo, no normal
kruskal_resultadonicaragua <- kruskal.test(Nicaragua_2C9depurado$Log_MR ~ Nicaragua_2C9depurado$Genotype, data = Nicaragua_2C9depurado)
kruskal_nicaragua <-kruskal_resultadonicaragua$p.value 
regresion_nicaragua <- lm(Log_MR ~ Genotype, data = Nicaragua_2C9depurado)
summary(regresion_nicaragua)


##################################################MEXICO#############################################################################
Mexico_2C9sinNA <- read_excel("CYP2C9/Mexico_2C9sinNA.xlsx")
View(Mexico_2C9sinNA)

class(Mexico_2C9sinNA$Log_MR)
Mexico_2C9sinNA$Genotype <- factor(Mexico_2C9sinNA$Genotype, levels = c("*1/*1", "*1/*2", "*1/*3", "*2/*3", "*3/*3"))

shapiro.test(Mexico_2C9sinNA$Log_MR) #no normalidad
kruskal_resultadomexico <- kruskal.test(Mexico_2C9sinNA$Log_MR ~ Mexico_2C9sinNA$Genotype, data = Mexico_2C9sinNA)
kruskal_mexico <- kruskal_resultadomexico$p.value

regresion_mexico <- lm(Log_MR ~ Genotype, data = Mexico_2C9sinNA)
summary(regresion_mexico)

kruskal_ecuador
kruskal_nicaragua
kruskal_mexico

#TABLA 

library(dplyr)

estadisticas <- c("Kruskal-Wallis", "Regresion")
ecuador <- c(0.3990247, 0.4946)       # Ejemplo de datos anuales para Ecuador
nicaragua <- c(0.05080486, 0.03103)     # Ejemplo de datos anuales para Nicaragua
mexico <- c(7.04e-8, 8.225e-9)        # Ejemplo de datos anuales para México

# Crear la tabla con dplyr
tabla <- tibble(
  Estadisticas = estadisticas,
  Ecuador = ecuador,
  Nicaragua = nicaragua,
  Mexico = mexico
)

# Mostrar la tabla
print(tabla)


###################################ANALISIS CON TRES POBLACIONES#########################
library(readxl)
X2C9_completo <- read_excel("2C9_completo.xlsx")
View(X2C9_completo)

class(X2C9_completo$GENOTYPE)
X2C9_completo$GENOTYPE <- factor(X2C9_completo$GENOTYPE, levels = c("*1/*1", "*1/*2", "*1/*3", "*2/*3", "*3/*3"))
class(X2C9_completo$GENOTYPE)
shapiro.test(X2C9_completo$LOG_MR)
kruskal_2c9 <- kruskal.test(X2C9_completo$LOG_MR ~ X2C9_completo$GENOTYPE, data = X2C9_completo)
kruskal_2C9 <- kruskal_2c9$p.value                 
kruskal_2C9

regresion_2C9 <- lm(LOG_MR ~ GENOTYPE, data = X2C9_completo)
summary((regresion_2C9))


