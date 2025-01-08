#POR ALELO ASIGNANDO 1/1 = 0, 1/2 o 1/3 = 1, 3/3 =2 y 2/3=1
##########################################ALELO 2###############################

library(readxl)
alelo2_2C9 <- read_excel("alelo2_2C9.xlsx")
View(alelo2_2C9)

class(alelo2_2C9$ALELO) #si es numerica, seria t-student pero si quiero categorizarla, puedo hacer un kruskal o anova
#conversion a factor para que me categorice
alelo2_2C9$ALELO <- as.factor(alelo2_2C9$ALELO)
#ver las categorias
levels(alelo2_2C9$ALELO)

#REGRESION ALELO 2
regresion_alelo2 <- lm(LOG_MR ~ ALELO, data = alelo2_2C9)
summary(regresion_alelo2)
library(car)
#ver normalidad de los datos
shapiro.test(alelo2_2C9$LOG_MR)

mann_whitney_alelo2 <- wilcox.test(LOG_MR ~ ALELO, data = alelo2_2C9)
mann_whitney_alelo2

####
# Realizar ANOVA para comparar las medias de los ratios metabólicos entre los alelos
anova_result <- aov(LOG_MR ~ ALELO, data = alelo2_2C9)
summary(anova_result)

# Crear un boxplot para visualizar las distribuciones de LOG_MR entre las categorías de ALELO
library(ggplot2)

ggplot(alelo2_2C9, aes(x = ALELO, y = LOG_MR, fill = ALELO)) +
  geom_boxplot() +
  labs(x = "CYP2C9*2", y = "LOG_MR", title = "Distribución de LOG_MR por Alelo") +
  scale_fill_manual(
    values = c("0" = "skyblue", "1" = "purple"), 
    name = "Genotipo", 
    labels= c("*1/*1", "*1/*2")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

###############################################ALELO3#########################################
library(readxl)
alelo3_2C9 <- read_excel("alelo3_2C9.xlsx")
View(alelo3_2C9)

class(alelo3_2C9$ALELO)
alelo3_2C9$ALELO <- as.factor(alelo3_2C9$ALELO)
library(car)
shapiro.test(alelo3_2C9$LOG_MR)
#no puedo hacer Mann-Whitney porque tengo tres categorias
mann_whitney_alelo3 <- wilcox.test(LOG_MR ~ ALELO, data = alelo3_2C9)
kruskal_alelo3 <- kruskal.test(alelo3_2C9$LOG_MR ~ alelo3_2C9$ALELO, data = alelo3_2C9)
kruskal_alelo3

#anova
anova_result3 <- aov(LOG_MR ~ ALELO, data = alelo3_2C9)
summary(anova_result3)
ggplot(alelo3_2C9, aes(x = ALELO, y = LOG_MR, fill = ALELO)) +
  geom_boxplot() +
  labs(x = "CYP2C9*3", y = "LOG_MR", title = "Distribución de LOG_MR por Alelo") +
  scale_fill_manual(
    values = c("0" = "skyblue", "1" = "purple", "2" = "yellow"),
    name = "Genotipo", 
    labels= c("*1/*1", "*1/*3-*2/*3", "*3/*3")) +
  theme_minimal() +  # Tema minimalista con fondo blanco
  theme(panel.background = element_rect(fill = "white"))