# Ecología teórica. Replicando resultados de un artículo
# Equipo 1. Canchola de Jesús Pedro, Flores Cortés Erick David, Galindo Celis Mario, Pérez Picazo Jonathan, Pérez Pineda Alba Denise, Robles Vieyra Helvia Jacqueline, 
# 14/08/25
# Tema: Organismos Geneticamente Modificados
# Artículo: Ongoing ecological and evolutionary consequences by the presence of transgenes in a wild cotton population
# Autores: Valeria Vázquez-Barrios, Karina Boege, Tania Gabriela Sosa-Fuentes, Patricia Rojas & Ana Wegier*. Posgrado en Ciencias Biológicas, Instituto de Biología, Universidad Nacional Autónoma de México, Mexico City, Mexico. 2 Laboratorio de Genética de la Conservación, Jardín Botánico, Instituto de Biología, Universidad Nacional Autónoma de México, Mexico City, Mexico.
#*Correspondence author: awegier@ib.unam.mx


#### La chambaaa ####
## Librerías que usaremos

library(multcomp) #Este paquete hace pruebas simultáneas e intervalos de confianza para hipótesis lineales generales en modelos paramétricos, incluyendo modelos lineales, lineales generalizados, lineales de efectos mixtos y de supervivencia.
library(dplyr) #Es un paquete que permite la manipulación de datos
library ( ggplot2 ) #Este paquete es para crear gráficos bonitos :)
library("wesanderson")# Paletas generadas principalmente a partir de películas de 'Wes Anderson', director de películas como: "El Gran Hotel Budapest"


## Datos (o de donde extraeremos nuestros datos)
getwd() # primero checamos donde estamos, y luego elegiremos la carpeta de ineterés (donde estan todos los archivos)
setwd("D:/Quinto Semestre/Ecología teórica/Expo_clase_genetica")#Adaptalo a tu propia carpeta

## Se realizaron varios experimentos (2), que iremos desglosando poco a poco. El primero es sobre el efecto de los transgenicos en la producción de EFN (néctar extrafloral) en cada genotipo, recordando que tenemos tres: 
#1) silvestres sin transgenes (W)
#2) Silvestres con cry, (W cry)
#3) Silvestres con cp4-epsps, (W cp4-epsps)



#### 1. Efecto de los transgénicos en el EFN ####
# Este experimento consiste en que agararron plantas, una por cada genotipo y la dividieron en dos zonas, aplicaron de un lado el MeJA (metil jasmonato, que regula el crecimiento vegetal y la respuesta al estrés ambiental), para que produciera néctar, y del otro lado nada (como control). Esto para probar con qué gen la planta produciría o no néctar.

#### Wild (genotipo silvestre)####

##Previamente se juntó la fecha (el día que se midió) junto al tipo de tratamiento (de control o si fue inoculada con MeJA) 

Wild<-read.csv("Wild_EFN_FV.csv") # Aquí estamos trayendo la base de datos sobre los individuos silvestres que fueron de control o "inducidos" por MeJA
names(Wild)

Wild$Treatment <- as.factor(Wild$Treatment)# Convertirlo la variable "Treatments" a factor (confía en el proceso!)

WildAllVar<- glm(Wild$Volumen.en.ug~Treatment, data = Wild, family = "quasipoisson")# Hacemos un modeo lineal generalizado, donde ponemos la variable de volumen en respuesta al tratamiento, y usamos el quiasipoisson,  esta distribución fue elegida específicamente para modelar datos de conteo con sobredispersión, como la producción de néctar.
summary(WildAllVar) #Pedimos los resultados del modelo lineal generalizado

anova(WildAllVar, test= "Chi") #El anova evalúa si el Treatment como un todo tiene un efecto significativo en la producción de EFN en las plantas silvestres,luego agregamos el test "chi", para examinar las diferencias entre variables categóricas. Es decir, si hay diferencias entre los resultados de los genotipos.

Tukey_glmw<- glht(WildAllVar, mcp(Treatment = "Tukey"))#glht() (de multcomp): Significa "General Linear Hypotheses Testing" (Prueba de Hipótesis Lineales Generales). Se usa para realizar comparaciones múltiples entre los niveles de un factor después de haber ajustado un modelo. mcp(Treatment = "Tukey"): Indica que vamos a comparar múltiples de Tukey (Tukey's Honestly Significant Difference, HSD) para el factor Treatment.


#Esto se hizo para saber cuáles niveles específicos del factor Treatment son significativamente diferentes entre sí. En este caso, se comparó el tratamiento "Control" con el tratamiento "MeJA" para la producción de EFN en las plantas silvestres.

cld(Tukey_glmw, Letters=letters) #cld(Tukey_glmw, Letters=letters): cld significa "Compact Letter Display" (Visualización Compacta de Letras). Toma los resultados de las comparaciones de Tukey (Tukey_glmw) y asigna letras a los grupos. Los grupos que comparten la misma letra no son significativamente diferentes entre sí, mientras que los grupos con letras diferentes sí lo son. Es una forma de presentar los resultados de las pruebas post-hoc, como se observa en las figuras del artículo para indicar diferencias significativas.







#### Modo matemáticoo ####
# Aquí sacaremos la desviación media, raíz cuadrada y media de las plantas de algodón clasificadas como "Wild" o silvestres 
meW<-with(Wild,tapply(Wild$Volumen.en.ug,Treatment,mean)) #La media de la variable Tratamiento. Con la función "with" se pueden hacer evaluaciones (u operaciones) modificando la base de datos original
meW

SEW<-with(Wild,tapply(Wild$Volumen.en.ug,Treatment,sd)/sqrt(summary(Treatment)))# Aqui aplicamos sd, que es desviación media y sqrt, que es la raíz cuadrada. Con la función "tapply", podemos aplicar una operacion matematica a un conjunto de datos previamente seleccionados (en este caso es la desviación media y la raíz cuadrada de la variable Tratamiento)
SEW 

SDW<-with(Wild,tapply(Wild$Volumen.en.ug,Treatment,sd))# Aqui solo aplicamos la desviación media
SDW #Presta atención a los números que salen de estas operaciones

#Estos calculos se hicieron para: comparar valores entre los grupos e identificar diferencias significativas y evaluar la precisión y la variabilidad natural dentro de cada grupo


#### Cry (con efecto insecticida) #### 

Cry<-read.csv("Cry_EFN_FV.csv")

Cry$Treatment <- as.factor(Cry$Treatment)#Convertimos a factor los tratamientos

CryTra<- glm(Cry$Volumen.en.ug~Treatment, data = Cry, family = "quasipoisson")#Aplicamos modelo lineal generalizado con el genotipo "Cry"
summary(CryTra)#Pedimos el resumen
anova(CryTra, test= "Chi") #Hacemos el test anova, con la prueba Chi
summary(CryTra)

meCry<-with(Cry,tapply(Cry$Volumen.en.ug,Treatment,mean)) #Sacamos la media del Tratamiento
meCry
SECry<-with(Cry,tapply(Cry$Volumen.en.ug,Treatment,sd)/sqrt(summary(Treatment)))#Sacamos la desviación media y además la raíz cuadrada
SECry




#### Cp4-epsps (tolerancia al glisofato) #### Insecticida

Cp4<-read.csv("Cp4-epsps_EFN_FV.csv")

Cp4$Treatment <- as.factor(Cp4$Treatment) #Pasamos a factor

Cp4Int<- glm(Cp4$Volumen.of.EFN~Treatment,data = Cp4, family = "quasipoisson")
summary(Cp4Int)# Modelo lineal generalizado del volumne del nectar floral respondiendo al tratamiento
anova(Cp4Int, test= "Chi") #Prueba de anova, con test Chi
summary(Cp4Int) # summary

meCp4<-with(Cp4,tapply(Cp4$Volumen.of.EFN,Treatment,mean)) #Media de la variable de Tratamiento
meCp4
SECp4<-with(Cp4,tapply(Cp4$Volumen.of.EFN,Treatment,sd)/sqrt(summary(Treatment)))# Sacamos la desviación media y además la raíz cuadrada del volumen del nectar floral y tratamiento
SECp4


# Control "Cry" y tratamiento de inducción negativa ####
CN<-read.csv("cry_wild_comparative_IT_FV.csv")
CNI<- glm(CN$EFN.volume.ug~CN$Treatment, data = CN, family = "quasipoisson")#Modelo lineal generalizado al nectar respondiendo al tratamiento
summary(CNI)
anova(CNI, test= "Chi") #Esto permite evaluar la significancia del efecto del tratamiento en la producción de EFN para el genotipo Wcry 



#¿Por qué se repite? Checa las bases de datos!



#### A graficar! ####

## Figura del tratamiento del néctar #### 

#Crear paleta apta:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #Vector


#Creamos una base de datos que tenga los genotipos, los tratamientos, la media y la desviación media. Los valores corresponden a los sacados anteriormente
nt<-data.frame(
  "Transgene"= c("S", "SCry", "SCp4-epsps", "S", "SCry", "SCp4-epsps"),
  "Tratamiento"= c("Control", "Control", "Control", "MeJa","MeJa","MeJa"),
  "Mean"=c(9.147619,19.910223,5.612791,26.078571,30.36818,7.195349),
  "SE"=c(1.94124,6.014835,1.420689,12.21541,13.695886,1.17479))

#Gráfica de barras con error

as.factor(nt$Transgene)#Pasamoa a factor
nt$Transgene = factor(nt$Transgene, levels=c("S", "SCry","SCp4-epsps"))
levels(nt$Transgene)

jpeg("Tratamiento_nectar_FV.jpeg", width = 15, height = 10,
     units = "cm", res = 300, pointsize = 0.25) #Guardamos imagen en formato JPG, con el nombre y las medidas que queremos


#Craemos objeto para primera parte de la gráfica
p1<-ggplot(nt, aes(nt$Transgene, Mean, fill=Tratamiento)) +
  scale_fill_manual(values = cbPalette)+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position = position_dodge(0.9), width=0.2)+
  labs(x = "Genotipos",y = "Nectar extrafloral (?g ?L-1)")+theme_classic()

#Creamos objeto para agregar más detalles a la gráfica
p2<-p1+  theme(axis.title.x = element_text(size=16, colour="black"))+
  theme(axis.title.y = element_text(size=16, colour = "black")) + 
  theme(axis.text.x = element_text(size=14, colour = "black"))+ 
  theme(axis.text.y = element_text(size=14, colour = "black")) +  
  theme(legend.position = "right")+
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  annotate("text", x=c(1,2,3), y=c(43,50,15), label= c("*","n.s","n.s"),size=c(13,6,6))

p2

dev.off()

