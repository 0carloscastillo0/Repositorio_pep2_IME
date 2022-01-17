#-------------------------------------------------------------------------------
#                                     CAPITULO 9
#-------------------------------------------------------------------------------
# PROCEDIMIENTO ANOVA DE UNA VIA PARA MUESTRAS INDEPENDIENTES
library ( tidyverse )
library ( ggpubr )
library ( ez )
library ( DescTools ) # Para procedimiento post-holm de scheefe

# Crear el data frame en formato ancho .
A <- c(23 , 19 , 25 , 23 , 20)

B <- c(26 , 24 , 28 , 23 , 29)
C <- c(19 , 24 , 20 , 21 , 17)
datos <- data.frame (A , B , C)

# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer (c("A", "B", "C") ,
                                 names_to = "algoritmo",
                                 values_to = "tiempo")

datos [["algoritmo"]] <- factor ( datos [["algoritmo"]])
datos [["instancia"]] <- factor (1: nrow ( datos ) )

# Comprobción de normalidad .
g <- ggqqplot (datos,
               x = "tiempo",
               y = "algoritmo",
               color = "algoritmo")

g <- g + facet_wrap (~ algoritmo )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )

# Procedimiento ANOVA con aov ().
cat (" Procedimiento ANOVA usando aov\n\n")
prueba <- aov ( tiempo ~ algoritmo , data = datos )
print ( summary ( prueba ) )

# Procedimiento ANOVA con ezANOVA ().
cat ("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba2 <- ezANOVA (
  data = datos ,
  dv = tiempo ,
  between = algoritmo ,
  wid = instancia ,
  return_aov = TRUE )

print ( prueba2 )

# Gráfico del tamaño del efecto .
g2 <- ezPlot (
  data = datos ,
  dv = tiempo ,
  wid = instancia ,
  between = algoritmo ,
  y_lab = " Tiempo promedio de ejecuci ón [ms]",
  x = algoritmo
)

print ( g2 )

# PROCEDIMIENTO POST-HOLM DE BONFENORRI
# Establecer nivel de significaci ón (el mismo usado en ANOVA ).
alfa <- 0.025

# Procedimiento post -hoc de Bonferroni .
cat (" Procedimiento post -hoc de Bonferroni \n\n")

bonferroni <- pairwise.t.test ( datos [["tiempo"]] ,
                                datos [["algoritmo"]] ,
                                p.adj = "bonferroni",
                                pool.sd = TRUE ,
                                paired = FALSE ,
                                conf.level = 1 - alfa )

print ( bonferroni )

# PROCEDIMIENTO POST-HOLM DE HOLM
# Procedimiento post -hoc de Holm .
cat ("\n\ nProcedimiento post - hoc de Holm \n\n")

holm <- pairwise.t.test ( datos [["tiempo"]] ,
                          datos [["algoritmo"]] ,
                          p.adj = "holm",
                          pool.sd = TRUE,
                          paired = FALSE,
                          conf.level = 1 - alfa )

print ( holm )

# PROCEDIMIENTO POST HOC DE SCHEFFÉ
# Crear matriz de contrastes .
contrastes <- matrix (c(1 , -1 , 0 ,
                        1 , 0 , -1 ,
                        0 , 1 , -1 ,
                        1 , -0.5 , -0.5 ,
                        -0.5 , 1 , -0.5 ,
                        -0.5 , -0.5 , 1) ,
                      nrow =6 ,
                      byrow = TRUE
)

# Trasponer matriz de contrastes ( para que cada contraste sea una columna ).
contrastes <- t( contrastes )

# Hacer prueba de Scheff é.
scheffe <- ScheffeTest ( x = prueba ,
                         which = "algoritmo",
                         contrasts = contrastes ,
                         conf.level = 1 - alfa
)
print ( scheffe )

#-------------------------------------------------------------------------------
#                                     CAPITULO 10
#-------------------------------------------------------------------------------
# PROCEDIMIENTO ANOVA DE UNA VÍA PARA MUESTRAS CORRELACIONADAS

library ( tidyverse )
library ( ggpubr )
library ( ez )
library ( nlme ) # para procedimiento post hoc de tukey y scheffé
library ( emmeans ) # para procedimiento post hoc de tukey y scheffé

# Crear el data frame .
instancia <- factor (1:6)
Quicksort <- c(23.2 , 22.6 , 23.4 , 23.3 , 21.8 , 23.9)
Bubblesort <- c(31.6 , 29.3 , 30.7 , 30.8 , 29.8 , 30.3)
Radixsort <- c(30.1 , 28.4 , 28.7 , 28.3 , 29.9 , 29.1)
Mergesort <- c(25.0 , 25.7 , 25.7 , 23.7 , 25.5 , 24.7)
datos <- data.frame ( instancia , Quicksort , Bubblesort , Radixsort , Mergesort )

# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer (c("Quicksort", "Bubblesort", "Radixsort",
                                   "Mergesort") ,
                                 names_to = "algoritmo", values_to = "tiempo")

datos [["algoritmo"]] <- factor ( datos [["algoritmo"]])

# Comprobción de normalidad .
g <- ggqqplot ( datos , x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap (~ algoritmo )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )

# Procedimiento ANOVA con aov.
cat (" Procedimiento ANOVA usando aov\n\n")

prueba <- aov ( tiempo ~ algoritmo + Error ( instancia /( algoritmo ) ) ,
                data = datos )

print ( summary ( prueba ) )

# Procedimiento ANOVA con ezANOVA ().
cat ("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")

prueba2 <- ezANOVA ( data = datos , dv = tiempo , within = algoritmo ,
                     wid = instancia , return_aov = TRUE )

print ( summary ( prueba2 $ aov) )
cat ("\n\ nPero ezANOVA entrega más informaci ón.\n")
cat ("El resultado de la prueba de esfericidad de Mauchly :\n\n")
#print ( prueba2 [[" Mauchly 's Test for Sphericity "]])
print ( prueba2$`Mauchly's Test for Sphericity`)

cat ("\n\nY factores de correcci ón para cuando no se cumple la\n")
cat (" condici ón de esfericidad :\n\n")
print ( prueba2$`Sphericity Corrections`)

# Gráfico del tama ño del efecto .
g2 <- ezPlot ( data = datos , dv = tiempo , wid = instancia , within = algoritmo ,
               y_lab = " Tiempo promedio de ejecución [ms]", x = algoritmo )

print ( g2 )

# PROCEDIMIENTO POST HOC DE BONFERRONI
# Procedimiento post -hoc de Bonferroni .
bonferroni <- pairwise.t.test ( datos [["tiempo"]] , datos [["algoritmo"]] ,
                                p.adj = "bonferroni", paired = TRUE )

cat (" Corrección de Bonferroni \n")
print ( bonferroni )

# PROCEDIMIENTO POST HOC DE HOLM
# Procedimiento post -hoc de Holm .
holm <- pairwise.t.test ( datos [["tiempo"]] , datos [["algoritmo"]] ,
                          p.adj = "holm", paired = TRUE )

cat ("\n\ nCorrección de Holm \n")

print ( holm )

# PROCEDIMIENTO POST HOC DE TUKEY
# Procedimiento post -hoc HSD de Tukey .
mixto <- lme ( tiempo ~ algoritmo , data = datos , random = ~1| instancia )
medias <- emmeans ( mixto , "algoritmo")
tukey <- pairs ( medias , adjust = "tukey")

cat ("\n\ nPrueba HSD de Tukey \n\n")
print ( tukey )

# PROCEDIMIENTO POST HOC DE SCHEFFÉ
# Procedimiento post -hoc de Scheff é
cat ("\n\ nComparación de Scheffé\n")
scheffe <- pairs ( medias , adjust = "scheffe")
print ( scheffe )

#-------------------------------------------------------------------------------
#                                     CAPITULO 11
#-------------------------------------------------------------------------------
# PRUEBA DE SUMA DE RANGOS DE WILCOXON O MANN - WHITNEY O Wilcoxon-Mann-Whitney (dos muestras independientes)
# Ingresar los datos .
a <- c(2.7 , 6.6 , 1.6 , 5.1 , 3.7 , 6.1 , 5.0 , 1.4 , 1.8 , 1.5 , 3.0 , 5.3)
b <- c(5.0 , 1.4 , 5.6 , 4.6 , 6.7 , 2.7 , 1.3 , 6.3 , 3.7 , 1.3 , 6.8)

# Establecer nivel de significaci ón.
alfa <- 0.05

# Hacer la prueba de Mann - Whitney .
prueba <- wilcox.test (a , b , alternative = "two.sided", conf.level = 1 - alfa )
print ( prueba )


# PRUEBA DE RANGOS CON SIGNO DE WILCOXON O SIGNOS DE WILCOXON (dos muestras correlacionadas)
# Ingresar los datos .
a <- c(2.9 , 6.1 , 6.7 , 4.7 , 6.4 , 5.7 , 2.7 , 6.9 , 1.7 , 6.4)
b <- c(6.0 , 2.8 , 1.3 , 4.7 , 3.1 , 1.8 , 2.9 , 4.0 , 2.3 , 1.6)

# Establecer nivel de significaci ón.
alfa <- 0.05

# Hacer la prueba de rangos con signo de Wilcoxon .
prueba <- wilcox.test (a , b , alternative = "greater", paired = TRUE ,
                       conf.level = 1 - alfa )

print ( prueba )


# PRUEBA DE KRUSKAL - WALLIS (PROCEDIMIENTO ANOVA DE UNA VIA PARA MUESTRAS INDEPENDIENTES)
# Construir la mariz de datos .
A <- c(24 , 23 , 26 , 21 , 24 , 24 , 25 , 22 , 23 , 22 , 23 , 23)
B <- c(17 , 15 , 18 , 20 , 19 , 21 , 20 , 18 , 19)
C <- c(10 , 11 , 14 , 11 , 15 , 12 , 12 , 10 , 9 , 13 , 12 , 12 , 10 , 10)
D <- c(18 , 16 , 18 , 15 , 16 , 15 , 18 , 16)
Tiempo <- c(A , B , C, D)

Algoritmo <- c(rep("A", length ( A ) ) ,
               rep ("B", length ( B ) ) ,
               rep ("C", length (C) ) ,
               rep ("D", length (D) ) )

Algoritmo <- factor ( Algoritmo )

datos <- data.frame ( Tiempo , Algoritmo )

# Establecer nivel de significaci ón
alfa <- 0.01

# Hacer la prueba de Kruskal - Wallis .
prueba <- kruskal.test ( Tiempo ~ Algoritmo , data = datos )
print ( prueba )

# Efectuar procedimiento post -hoc de Holm si se encuentran diferencias
# significativas .
if( prueba $ p.value < alfa ) {
  post_hoc <- pairwise.wilcox.test ( datos $ Tiempo ,
                                     datos$Algoritmo ,
                                     p.adjust.method = "holm",
                                     paired = FALSE )
  
  print ( post_hoc )
}


# PRUEBA DE FRIEDMAN (PROCEDIMIENTO ANOVA DE UNA VIA PARA MUESTRAS CORRELACIONADAS)
# Construir la mariz de datos .
A <- c(21 , 10 , 7 , 21 , 24 , 27 , 17)
B <- c(6 , 21 , 18 , 7 , 24 , 13 , 13)
C <- c(13 , 25 , 18 , 20 , 24 , 8 , 29)

Puntuacion <- c(A , B , C)

Interfaz <- c( rep("A", length ( A ) ) ,
               rep ("B", length ( B ) ) ,
               rep ("C", length (C) ) )

Sujeto <- rep (1:7 , 3)

Interfaz <- factor ( Interfaz )

datos <- data.frame ( Sujeto , Puntuacion , Interfaz )

# Establecer nivel de significaci ón
alfa <- 0.05

# Hacer la prueba de Friedman .
prueba <- friedman.test ( Puntuacion ~ Interfaz | Sujeto , data = datos )
print ( prueba )

# Efectuar procedimiento post -hoc de Holm si se encuentran diferencias
# significativas .
if( prueba $ p.value < alfa ) {
  post_hoc <- pairwise.wilcox.test ( datos$Puntuacion ,
                                     datos$Interfaz ,
                                     p.adjust.method = "holm",
                                     paired = TRUE )
  print ( post_hoc )
}

#-------------------------------------------------------------------------------
#                                     CAPITULO 12
#-------------------------------------------------------------------------------
# INTERVALO DE CONFIANZA PARA LA MEDIA POBLACIONAL MEDIANTE BOOTSTRAPPING
library ( boot )
library ( bootES )

# Crear muestra inicial , mostrar su histograma y calcular la media .
muestra <- c(79 , 75 , 84 , 75 , 94 , 82 , 76 , 90 , 79 , 88)
datos <- data.frame ( muestra )

# Establecer cantidad de remuestreos y nivel de significaci ón.
B = 2000
alfa <- 0.01

cat (" Paquete boot \n")

# Construir distribuci ón bootstrap usando el paquete boot .
media <- function ( valores , i ) {
  mean ( valores [ i ])
}

set.seed (432)
distribucion_b <- boot ( muestra , statistic = media , R = B )
print ( distribucion_b )

# Graficar distribuci ón bootstrap .
print ( plot ( distribucion_b ) )


# Construir intervalos de confianza .
intervalo_t <- boot.ci ( distribucion_b , conf = 1 - alfa , type = "norm")

cat ("\n\ nIntervalo de confianza usando distribuci ón t:\n")
print ( intervalo_t)

intervalo_per <- boot.ci ( distribucion_b , conf = 1 - alfa , type = "perc")

cat ("\n\ nIntervalo de confianza usando percentiles :\n")
print ( intervalo_per )

intervalo_bca <- boot.ci ( distribucion_b , conf = 1 - alfa , type = "bca")

cat ("\n\ nIntervalo de confianza BCa :\n")
print ( intervalo_bca )

# Construir distribución bootstrap usando el paquete bootES .
set.seed (432)

distribucion_bootstrapES <- bootES ( muestra , R = B , ci.type = "bca",
                                     ci.conf = 1 - alfa , plot = TRUE )

print ( distribucion_bootstrapES )



# INFERENCIA SOBRE LA MEDIA DE UNA MUESTRA CON BOOTSTRAPPIG
library ( boot )

set.seed (432)

# Crear muestra inicial , mostrar su histograma y calcular la media .
muestra <- c(79 , 75 , 84 , 75 , 94 , 82 , 76 , 90 , 79 , 88)
valor_observado <- mean ( muestra )
datos <- data.frame ( muestra )

# Construir distribuci ón bootstrap .
B <- 2000

media <- function ( valores , i ) {
  mean ( valores [ i ])
}

distribucion_b <- boot ( muestra , statistic = media , R = B )

# Desplazar la distribución bootstrap para que se centre en
# el valor nulo .
valor_nulo <- 75
desplazamiento <- mean ( distribucion_b [["t"]]) - valor_nulo
distribucion_nula <- distribucion_b [["t"]] - desplazamiento

# Determinar el valor p.
p <- ( sum( distribucion_nula > valor_observado ) + 1) / (B + 1)
cat ("Valor p:", p )


# BOOTSTRAPPING PARA LA DIFERENCIA DE MEDIA (INDEPENDIENTES)
library ( simpleboot )
library ( boot )
library ( ggpubr )

set.seed (432)


# Ingresar datos originales
hombres <- c(1.3 , 1.5 , 1.6 , 1.7 , 1.7 , 1.9 , 2.3 , 2.4 , 2.6 , 2.6 , 2.7 ,
             2.8 , 3.2 , 3.7 , 4.1 , 4.4 , 4.5 , 4.8 , 5.2 , 5.2 , 5.3 , 5.5 ,
             5.5 , 5.6 , 5.6 , 5.7 , 5.7)

mujeres <- c(3.5 , 3.6 , 3.8 , 4.3 , 4.5 , 4.5 , 4.9 , 5.1 , 5.3 , 5.3 , 5.5 ,
             5.8 , 6.0 , 6.3 , 6.3 , 6.4 , 6.4 , 6.6 , 6.7)

n_hombres <- length ( hombres )
n_mujeres <- length ( mujeres )

sexo <- c( rep ("Hombre", n_hombres ) , rep("Mujer", n_mujeres ) )
nota <- c( hombres , mujeres )
datos <- data.frame ( nota , sexo )

# Comprobar normalidad de las muestras .
print ( shapiro.test ( hombres ) )
print ( shapiro.test ( mujeres ) )

# Calcular la diferencia observada entre las medias muestrales .
media_hombres <- mean ( hombres )
media_mujeres <- mean ( mujeres )
diferencia_observada <- media_hombres - media_mujeres

cat ("diferencia observada :", media_hombres - media_mujeres , "\n\n")

# Establecer el nivel de significación.
alfa <- 0.05

# Crear la distribuci ón bootstrap .
B <- 9999
distribucion_bootstrap <- two.boot ( hombres , mujeres , FUN = mean , R = B )

# Examinar la distribuci ón bootstrap .
valores <- data.frame ( distribucion_bootstrap$t)
colnames ( valores ) <- "valores"

histograma <- gghistogram ( valores , x = "valores", color = "red",
                            fill = "red", bins = 100 ,
                            xlab = "Diferencia de medias",
                            ylab = "Frecuencia", add = "mean")

print ( histograma )

qq <- ggqqplot ( valores , x = "valores", color = "red")
print ( qq )

cat ("Distribución bootstrap :\n")
cat ("\ tMedia :", mean ( valores$valores ) , "\n")
cat ("\ tDesviación estándar :", sd( valores$valores ) , "\n\n")

# Construir el intervalo de confianza .
intervalo_bca <- boot.ci ( distribucion_bootstrap , conf = 1 - alfa ,
                           type = "bca")

print ( intervalo_bca )



# BOOTSTRAPPING PARA INFERIR ACERCA DE LAS DIFERENCIAS DE MEDIAS (INDEPENDIENTES)
library ( simpleboot )
library ( boot )
library ( ggpubr )

set.seed (432)

# Ingresar datos originales
hombres <- c(1.3 , 1.5 , 1.6 , 1.7 , 1.7 , 1.9 , 2.3 , 2.4 , 2.6 , 2.6 , 2.7 ,
             2.8 , 3.2 , 3.7 , 4.1 , 4.4 , 4.5 , 4.8 , 5.2 , 5.2 , 5.3 , 5.5 ,
             5.5 , 5.6 , 5.6 , 5.7 , 5.7)

mujeres <- c(3.5 , 3.6 , 3.8 , 4.3 , 4.5 , 4.5 , 4.9 , 5.1 , 5.3 , 5.3 , 5.5 ,
             5.8 , 6.0 , 6.3 , 6.3 , 6.4 , 6.4 , 6.6 , 6.7)

n_hombres <- length ( hombres )
n_mujeres <- length ( mujeres )

sexo <- c( rep("Hombre", n_hombres ) , rep("Mujer", n_mujeres ) )
nota <- c( hombres , mujeres )
datos <- data.frame ( nota , sexo )

# Calcular la diferencia observada entre las medias muestrales .
media_hombres <- mean ( hombres )
media_mujeres <- mean ( mujeres )
valor_observado <- media_hombres - media_mujeres

# Crear la distribuci ón bootstrap .
B <- 9999
valor_nulo <- 1.5
distribucion_bootstrap <- two.boot ( hombres , mujeres , FUN = mean , R = B )
desplazamiento <- mean ( distribucion_bootstrap [["t"]]) - valor_nulo
distribucion_nula <- distribucion_bootstrap [["t"]] - desplazamiento

# Determinar el valor p.
p <- ( sum ( abs( distribucion_nula ) > abs( valor_observado ) ) + 1) / ( B + 1)
cat ("Valor p:", p )


# BOOTSTRAPPING PARA LA DIFERENCIA DE MEDIA (PAREADAS)
library ( bootES )

set.seed (432)

# Ingresar datos originales .
alumno <- 1:20

prueba_1 <- c(3.5 , 2.7 , 1.0 , 1.8 , 1.6 , 4.3 , 5.8 , 6.4 , 3.9 , 4.3 , 3.4 ,
              5.3 , 5.8 , 5.3 , 2.0 , 1.3 , 4.0 , 5.3 , 1.6 , 3.6)

prueba_2 <- c(5.2 , 5.1 , 5.9 , 4.8 , 1.4 , 2.3 , 6.8 , 5.3 , 3.1 , 3.8 , 4.6 ,
              1.2 , 3.9 , 2.0 , 1.7 , 3.3 , 6.0 , 4.8 , 6.9 , 1.3)


# Establecer nivel de significaci ón.
alfa <- 0.05

# Calcular la diferencia entre ambas observaciones .
diferencia <- prueba_2 - prueba_1

# Generar la distribución bootstrap y su intervalo de confianza .
B <- 3999

distribucion_bootstrapES <- bootES ( diferencia , R = B , ci.type = "bca",
                                     ci.conf = 1 - alfa , plot = FALSE )

print ( distribucion_bootstrapES )



# BOOTSTRAPPING PARA INFERIR ACERCA DE LAS DIFERENCIAS DE MEDIAS (PAREADAS)
library ( bootES )

set.seed (432)

# Ingresar datos originales .
alumno <- 1:20

prueba_1 <- c(3.5 , 2.7 , 1.0 , 1.8 , 1.6 , 4.3 , 5.8 , 6.4 , 3.9 , 4.3 , 3.4 ,
              5.3 , 5.8 , 5.3 , 2.0 , 1.3 , 4.0 , 5.3 , 1.6 , 3.6)

prueba_2 <- c(5.2 , 5.1 , 5.9 , 4.8 , 1.4 , 2.3 , 6.8 , 5.3 , 3.1 , 3.8 , 4.6 ,
              1.2 , 3.9 , 2.0 , 1.7 , 3.3 , 6.0 , 4.8 , 6.9 , 1.3)

# Establecer nivel de significaci ón.
alfa <- 0.05

# Calcular la diferencia entre ambas observaciones .
diferencia <- prueba_2 - prueba_1

# Calcular la media observada de las diferencias .
valor_observado <- mean ( diferencia )

# Generar la distribución bootstrap y su intervalo de confianza .
B <- 3999
valor_nulo <- 0.5

distribucion_bootstrapES <- bootES ( diferencia , R = B , ci.type = "bca",
                                     ci.conf = 1 - alfa , plot = FALSE )

distribucion_nula <- distribucion_bootstrapES [["t"]] - valor_nulo

# Determinar el valor p.
p <- ( sum( abs( distribucion_nula ) > abs( valor_observado ) ) + 1) / ( B + 1)
cat ("Valor p:", p )



# PRUEBAS DE PERMUTACIONES PARA VARIABLES NUMÉRICAS (DOS MUESTRAS INFEPENDIENTES)
library ( ggpubr )

set.seed (432)

# Funci ón para calcular la diferencia de medias .
# Argumentos :
# - muestra _1 , muestra _2: vectores num é ricos con las muestras a comparar .
# - FUN: funci ón del estad í stico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
calcular_diferencia <- function ( muestra_1 , muestra_2 , FUN ) {
  diferencia <- FUN ( muestra_1) - FUN ( muestra_2)
  return ( diferencia )
}

# Funci ón para hacer una permutaci ón y calcular el estad í stico
# de inter és.
# Argumentos :
# - muestra _1 , muestra _2: vectores num é ricos con las muestras a comparar .
# - FUN: funci ón del estad í stico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
permutar <- function ( muestra_1 , muestra_2 , FUN ) {
  n_1 <- length ( muestra_1)
  n_2 <- length ( muestra_2)
  
  # Hacer la permutaci ón.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2 ,
                         replace = FALSE )
  
  # Asignar elementos a los dos grupos .
  
  permutacion_1 <- permutacion [1 : n_1]
  permutacion_2 <- permutacion [ n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias .
  return ( calcular_diferencia ( permutacion_1 , permutacion_2 , FUN ) )
}

# Funci ón para calcular el valor p.
# Argumentos :
# - distribucion : distribuci ón nula del estad í stico de inter és.
# - valor _ observado : valor del estad í stico de inter és para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipó tesis alternativa . "two. sided " para
# hipó tesis bilateral , " greater " o " less " para hip ó tesis unilaterales .
# Valor :
# - el valorp calculado .
calcular_valor_p <- function ( distribucion , valor_observado ,
                               repeticiones , alternative ) {
  if( alternative == "two.sided") {
    numerador <- sum(abs( distribucion ) > abs ( valor_observado ) ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if( alternative == "greater") {
    numerador <- sum( distribucion > valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum( distribucion < valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return ( valor_p )
}

# Funci ón para graficar una distribuci ón.
# Argumentos :
# - distribucion : distribuci ón nula del estad í stico de inter és.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function ( distribucion , ...) {
  observaciones <- data.frame ( distribucion )
  
  histograma <- gghistogram ( observaciones , x = "distribucion",
                              xlab = " Estadístico de interés",
                              ylab = "Frecuencia", ...)
  
  qq <- ggqqplot ( observaciones , x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura <- ggarrange ( histograma , qq , ncol = 2 , nrow = 1)
  print ( figura )
}

# Funci ón para hacer la prueba de permutaciones .
# Argumentos :
# - muestra _1 , muestra _2: vectores num é ricos con las muestras a comparar .

# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : funci ón del estad í stico E para el que se calcula la diferencia .
# - alternative : tipo de hipó tesis alternativa . "two. sided " para
# hip ó tesis bilateral , " greater " o " less " para hip ó tesis unilaterales .
# - plot : si es TRUE , construye el grá fico de la distribuci ón generada .
# - ...: otros argumentos a ser entregados a graficar _ distribucion .
contrastar_hipotesis_permutaciones <- function ( muestra_1 , muestra_2 ,
                                                 repeticiones , FUN ,
                                                 alternative , plot , ...) {
  cat ("Prueba de permutaciones \n\n")
  cat ("Hipótesis alternativa :", alternative , "\n")
  observado <- calcular_diferencia ( muestra_1 , muestra_2 , FUN )
  cat ("Valor observado :", observado , "\n")
  
  distribucion <- rep( NA , repeticiones )
  
  for ( i in 1: repeticiones ) {
    distribucion [ i ] <- permutar ( muestra_1 , muestra_2 , FUN )
  }
  
  if( plot ) {
    graficar_distribucion ( distribucion , ...)
  }
  
  valor_p <- calcular_valor_p ( distribucion , observado , repeticiones ,
                                "two.sided")
  
  cat ("Valor p:", valor_p , "\n\n")
}

# Crear muestras iniciales .
a <- c(5.4 , 4.7 , 6.3 , 2.9 , 5.9 , 5.1 , 2.1 , 6.2 , 1.6 , 6.7 , 3.0 , 3.3 ,
       5.0 , 4.1 , 3.3 , 3.4 , 1.2 , 3.8 , 5.8 , 4.2)

b <- c(4.0 , 4.1 , 4.3 , 4.3 , 4.3 , 4.2 , 4.3 , 4.3 , 4.4 , 4.1 , 4.3 , 4.0)

# Hacer pruebas de permutaciones para la media y la varianza .
R = 5999

contrastar_hipotesis_permutaciones (a , b , repeticiones = R, FUN = mean ,
                                    alternative = "two.sided", plot = TRUE ,
                                    color = "blue", fill = "blue")

contrastar_hipotesis_permutaciones (a , b , repeticiones = R, FUN = var ,
                                    alternative = "two.sided", plot = FALSE )


# PRUEBA DE PERMUTACIONES PARA MUESTRAS CORRELACIONADAS (3 MUESTRAS PAREADAS)
library ( boot )
library ( ggpubr )
library ( ez )
library ( tidyverse )

# Crear el data frame .
Quicksort <- c(11.2 , 22.6 , 23.4 , 23.3 , 21.8 , 40.1)
Bubblesort <- c(15.7 , 29.3 , 30.7 , 30.8 , 29.8 , 50.3)
Mergesort <- c(12.0 , 25.7 , 25.7 , 23.7 , 25.5 , 44.7)
Instancia <- factor (1:6)
datos_anchos <- data.frame ( Instancia , Quicksort , Bubblesort , Mergesort )

datos_largos <- datos_anchos %>% pivot_longer (c("Quicksort", "Bubblesort",
                                                 "Mergesort") ,
                                               names_to = "Algoritmo",
                                               values_to = "Tiempo")

datos_largos [["Algoritmo"]] <- factor ( datos_largos [["Algoritmo"]])

# Verificar condici ón de normalidad .
g <- ggqqplot ( datos_largos , "Tiempo", facet.by = "Algoritmo",
                color = "Algoritmo")

print ( g )

# Establecer nivel de significaci ón.
alfa <- 0.01

# Obtener el valor observado , correspondiente al estad í stico F entregado
# por ANOVA para la muestra original .
anova <- ezANOVA ( datos_largos , dv = Tiempo , within = Algoritmo ,
                   wid = Instancia , return_aov = TRUE )

valor_observado <- anova [["ANOVA"]][["F"]]

# Generar permutaciones .
R = 2999
permutaciones <- list ()
copia_ancha <- data.frame ( datos_anchos )

set.seed (432)

for ( i in 1:R) {
  copia_ancha [, 2:4] <- t( apply ( copia_ancha [ , 2:4] , 1 , sample ) )
  
  copia_larga <- copia_ancha %>% pivot_longer (c("Quicksort", "Bubblesort",
                                                 "Mergesort") ,
                                               names_to = "Algoritmo",
                                               
                                               values_to = "Tiempo")
  
  copia_larga [["Algoritmo"]] <- factor ( copia_larga [["Algoritmo"]])
  permutaciones <- append ( permutaciones , list ( copia_larga ) )
}

# Generar distribuci ón de estad í sticos F con las permutaciones .
distribucion <- c()

for ( i in 1:R) {
  datos <- as.data.frame ( permutaciones [ i ])
  
  anova <- ezANOVA ( datos , dv = Tiempo , within = Algoritmo , wid = Instancia ,
                     return_aov = TRUE )
  
  distribucion <- c( distribucion , anova [["ANOVA"]][["F"]])
}

# Obtener valor p.
p <- ( sum( distribucion > valor_observado ) + 1) / (R + 1)
cat (" ANOVA de una vía para muestras pareadas con permutaciones \n")
cat ("p =", p , "\n\n")

# Aná lisis post -hoc.
# Funci ón para calcular la media de las diferencias para dos columnas de una
# matriz de datos en formato ancho .
media_diferencias <- function ( datos , columna_1 , columna_2) {
  media <- mean ( datos [[ columna_1]] - datos [[ columna_2]])
  return ( media )
}

# Funci ón para generar la distribuciones de la diferencia de medias a
# partir de las permutaciones .
distribucion_diferencias <- function ( permutaciones , columna_1 , columna_2) {
  R <- length ( permutaciones )
  distribucion <- c()
  
  for ( i in 1:R) {
    datos <- as.data.frame ( permutaciones [ i ])
    
    datos <- datos %>% pivot_wider ( names_from = "Algoritmo",
                                     values_from = "Tiempo")
    
    diferencia <- media_diferencias ( datos , columna_1 , columna_2)
    distribucion <- c( distribucion , diferencia )
  }
  
  return ( distribucion )
}

if ( p < alfa ) {
  quick <- 2
  bubble <- 3
  merge <- 4
  
  # Calcular diferencias observadas en la muestra original .
  dif_obs_quick_bubble <- media_diferencias ( datos_anchos , quick , bubble )
  dif_obs_quick_merge <- media_diferencias ( datos_anchos , quick , merge )
  dif_obs_bubble_merge <- media_diferencias ( datos_anchos , bubble , merge )
  
  
  # Generar distribuciones para diferencias entre pares a partir de las
  # permutaciones .
  dif_quick_bubble <- distribucion_diferencias ( permutaciones , quick , bubble )
  dif_quick_merge <- distribucion_diferencias ( permutaciones , quick , merge )
  dif_bubble_merge <- distribucion_diferencias ( permutaciones , bubble , merge )
  
  # Obtener valores p.
  num <- sum(abs( dif_quick_bubble ) > (abs( dif_obs_quick_bubble ) + 1) )
  den <- R + 1
  p_quick_bubble <- num / den
  
  num <- sum(abs( dif_quick_merge ) > abs ( dif_obs_quick_merge ) + 1)
  den <- R + 1
  p_quick_merge <- num / den
  
  num <- sum(abs( dif_bubble_merge ) > abs ( dif_obs_bubble_merge ) + 1)
  den <- R + 1
  p_bubble_merge <- num / den
  
  cat ("\n\n")
  cat ("Análisis post -hoc ( permutaciones ) para la diferencia de las medias \n")
  cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - -\n")
  cat (" Valores p:\n")
  
  cat ( sprintf (" Quicksort - Bubblesort : %.3f\n", p_quick_bubble ) )
  cat ( sprintf (" Quicksort - Mergesort : %.3f\n", p_quick_merge ))
  cat ( sprintf (" Bubblesort - Mergesort : %.3f\n", p_bubble_merge ) )
  
  cat ("\ nDiferencias observadas :\n")
  cat ( sprintf (" Quicksort - Bubblesort : %.3f\n", dif_obs_quick_bubble ) )
  cat ( sprintf (" Quicksort - Mergesort : %.3f\n", dif_obs_quick_merge ) )
  cat ( sprintf (" Bubblesort - Mergesort : %.3f\n", dif_obs_bubble_merge ) )
}


#-------------------------------------------------------------------------------
#                                     CAPITULO 13
#-------------------------------------------------------------------------------
# AJUSTE DE UNA REGRESION LINEAL SIMPLE
library ( ggpubr )

# Cargar los datos .
datos <- mtcars

# Ajustar modelo con R.
modelo <- lm( mpg ~ wt , data = datos )
print ( summary ( modelo ) )

# Graficar el modelo .
p <- ggscatter ( datos , x = "wt", y = "mpg", color = "blue", fill = "blue",
                 xlab = "Peso [lb x 1000]", ylab = "Rendimiento [ millas /galón]")

p <- p + geom_smooth ( method = lm , se = FALSE , colour = "red")
print ( p )

# Crear gráficos para evaluar el modelo .
plot( modelo )

# Ingresar algunas instancias artificiales .
mpg <- c (23.714 , 19.691 , 19.242 , 12.430 , 10.090 , 9.565 , 18.171 , 26.492 , 7.054 ,
          24.447 , 15.683 , 17.403 , 13.465 , 18.850 , 29.493)

wt <- c (2.973 , 4.532 , 2.332 , 3.016 , 4.220 , 4.286 , 2.580 , 3.084 , 3.816 , 2.775 ,
         3.251 , 3.013 , 4.951 , 2.644 , 2.218)

nuevos <- data.frame( mpg , wt)

# Usar el modelo para predecir el rendimiento de los nuevos y ver los
# residuos resultantes .
predicciones <- predict ( modelo , nuevos )
residuos <- nuevos$mpg - predicciones
nuevos <- data.frame ( nuevos , residuos )

r <- ggscatter ( nuevos , x = "wt", y = "residuos", color = "blue",
                 fill = "blue", xlab = "Peso [lb * 1000]", ylab = "Residuo")

r <- r + geom_hline ( yintercept = 0 , colour = "red")
print ( r )


# REEMPLAZAR UNA VARIABLE DICOTÓMICA POR UNA INDICADORA
# Crear un data frame con una variable dicot ó mica .
alumno <- 1:5
sexo <- factor (c("F", "M", "F","F", "M") )
datos <- data.frame ( alumno , sexo )

# Crear una variable indicadora para sexo , con valor 0
# para hombres y 1 , para mujeres .
es_mujer <- rep (1 , length ( sexo ) )
es_mujer [ sexo == "M"] <- 0

# Reemplazar la variable sexo por lavariable indicadora .
datos <- cbind ( datos , es_mujer )
datos [["sexo"]] <- NULL


# ALTERNATIVA ROBUSTA PARA COMPARAR ENTRE MULTIPLES GRUPOS CORRELACIONADOS
library ( ggpubr )

# Cargar los datos .
datos <- mtcars

# Ajustar modelo con R.
modelo <- lm( mpg ~ vs , data = datos )
print ( summary ( modelo ) )

# Graficar el modelo .
p <- ggscatter ( datos , x = "vs", y = "mpg", color = "blue", fill = "blue",
                 xlab = "Forma del motor", ylab = "Rendimiento [ millas /galón]",
                 xticks.by = 1)

p <- p + geom_smooth ( method = lm , se = FALSE , colour = "red")
print ( p )

# Crear grá ficos para evaluar el modelo .
plot ( modelo )

# Graficar residuos .
residuos <- modelo$residuals
datos <- cbind ( datos , residuos )
datos [["vs"]] <- factor ( datos [["vs"]])
r <- ggqqplot ( datos , x = "residuos", facet.by = "vs", color = "vs",
                palette = c("blue", "red") )

print ( r )


# AJUSTE DE REGRESIÓN LINEAL SIMPLE USANDO VALIDACION CRUZADA
# Cargar los datos .
datos <- mtcars

# Crear conjuntos de entrenamiento y prueba .
set.seed (101)
n <- nrow ( datos )
n_entrenamiento <- floor (0.7 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo con el conjunto de entrenamiento .
modelo <- lm( mpg ~ wt , data = entrenamiento )
print ( summary ( modelo ) )

# Calcular error cuadrado promedio para el conjunto de entrenamiento .
mse_entrenamiento <- mean ( modelo$residuals ** 2)
cat (" MSE para el conjunto de entrenamiento :", mse_entrenamiento , "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict ( modelo , prueba )

# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba [["mpg"]] - predicciones
mse_prueba <- mean ( error ** 2)
cat (" MSE para el conjunto de prueba :", mse_prueba )


# AJUSTE DE REGRESIÓN LINEAL SIMPLE USANDO VALIDACION CRUZADA DE 5 PLIEGUES
library ( caret )

# Cargar los datos .
datos <- mtcars

# Crear conjuntos de entrenamiento y prueba .
set.seed (101)
n <- nrow ( datos )
n_entrenamiento <- floor (0.7 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo usando validaci ón cruzada de 5 pliegues .
modelo <- train ( mpg ~ wt , data = entrenamiento , method = "lm",
                  trControl = trainControl ( method = "cv", number = 5) )

print ( summary ( modelo ) )

# Obtener error cuadrado promedio para el conjunto de entrenamiento .
mse_entrenamiento <- modelo$results$RMSE
cat (" MSE para el conjunto de entrenamiento :", mse_entrenamiento , "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict ( modelo , prueba )

# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba [["mpg"]] - predicciones
mse_prueba <- mean ( error ** 2)
cat (" MSE para el conjunto de prueba :", mse_prueba )


# REGRESION LINEAL PARA LA CANTIDAD DE REQUISITOS FUNCIONALES DE ACUERDO A LA
# CANTIDAD DE STAKEHOLDERS (SE HACE INFFERENCIA USANDO REGRESIÓN LINEAL)
library ( ggpubr )

# Crear los datos originales .
requisitos <- c(11 , 10 , 12 , 14 , 8 , 13 , 18 , 15 , 20 , 16 , 21 , 13 , 10 , 9 , 21)
stakeholders <- c(8 , 8 , 6 , 6 , 8, 7 , 3 , 1 , 3, 4 , 5 , 4 , 4 , 9 , 2)
datos <- data.frame ( requisitos , stakeholders )

# Ajustar modelo .
modelo <- lm( requisitos ~ stakeholders , data = datos )
print ( summary ( modelo ) )

# Graficar el modelo .
p <- ggscatter (
  datos , x = "stakeholders", y = "requisitos", color = "blue", fill = "blue",
  xlab = "Stakeholders", ylab = "Requisitos funcionales")

p <- p + geom_smooth ( method = lm , se = FALSE , colour = "red")

# Graficar los residuos .
b_1 <- modelo$coefficients[2]
b_0 <- modelo$coefficients[1]
residuos <- datos[["requisitos"]] - ( b_1 * datos [["stakeholders"]] + b_0)
datos <- data.frame ( datos , residuos )

r <- ggscatter ( datos , x = "stakeholders", y = "residuos", color = "blue",
                 fill = "blue", xlab = "Stakeholders", ylab = "Residuo")

r <- r + geom_hline ( yintercept = 0 , colour = "red")

g <- ggarrange (p , r , ncol = 2 , nrow = 1)
print ( g )

# Verificar normalidad de los residuos .
cat (" Prueba de normalidad para los residuos \n")
print ( shapiro.test( datos$residuos ) )


#-------------------------------------------------------------------------------
#                                     CAPITULO 14
#-------------------------------------------------------------------------------
# REGRESIÓN LINEAL PARA PREDECIR EL RENDIMIENTO DE UN AUTOMÓVIL A PARTIR DE 
# DOS VARIABLES.
library ( scatterplot3d )

# Cargar los datos .
datos <- mtcars

# Ajustar modelo usando validaci ón cruzada de 5 pliegues .
modelo <- lm( mpg ~ wt + qsec , data = datos )
print ( summary ( modelo ) )

# Graficar modelo ajustado .
g <- scatterplot3d ( datos$wt , datos$qsec , datos$mpg , type = "p",
                     highlight.3d = TRUE , pch = 20 , xlab = "Peso [lb x 1000]",
                     ylab = "Rendimiento [ millas /galón]",
                     zlab = "1/4 de milla [s]")

g$plane3d ( modelo , draw_polygon = TRUE , draw_lines = TRUE )
print(g)


# CREACIÓN DE VARIABLES ARTIFICIALES PARA VARIABLES CATEGORICAS
library ( dummies )

# Crear una matrz de datos .
sujeto <- 1:10
sexo <- c("F", "F", "M", "M", "M", "M", "F", "M", "F", "F")
tipo <- c("B", "D", "A", "B", "A", "C", "D", "D", "D", "A")
valor <- c (1.68 , 2.79 , 1.92 , 2.26 , 2.1 , 2.63 , 2.19 , 3.62 , 2.76 , 1.26)
datos <- data.frame ( sujeto , sexo , tipo , valor )

# Crear variables artificiales .
datos.dummy <- dummy.data.frame ( datos , drop = TRUE )
datos.dummy [["sexoF"]] <- NULL
datos.dummy [["tipoA"]] <- NULL

# Crear modelos lineales .
m1 <- lm( valor ~ sexo + tipo , datos )
print ( m1 )

m2 <- lm( valor ~ sexoM + tipoB + tipoC + tipoD , datos.dummy )
print ( m2 )


# COMPARACIÓN DE DOS MODELOS LINEALES
# Cargar datos .
datos <- mtcars

# Ajustar modelo con el peso como predictor .
modelo_1 <- lm( mpg ~ wt , data = datos )
print ( summary ( modelo_1) )
aic_1 <- AIC ( modelo_1)
cat ("Modelo 1: AIC =", AIC( modelo_1) , "\n")

# Ajustar modelo con el peso y el cuarto de milla como predictores .
modelo_2 <- lm( mpg ~ wt + qsec , data = datos )
print ( summary ( modelo_2) )
aic_2 <- AIC ( modelo_2)
cat ("Modelo 2: AIC =", AIC ( modelo_2) , "\n")

# Comparar ambos modelos .
comparacion <- anova ( modelo_1 , modelo_2)
print ( comparacion )


# INCORPORACION Y ELIMINACION DE VARIABLES EN UN MODELO DE RLM
# Cargar datos .
datos <- mtcars

# Ajustar modelo inicial con la variable wt como predictor .
modelo <- lm( mpg ~ wt , data = datos )
cat ("=== Modelo inicial ===\ n")
print ( modelo )

# Incorporar el predictor cyl.
modelo <- update ( modelo , . ~ . + cyl )
cat ("=== Modelo con predictores wt y cyl ===\ n")
print ( modelo )

# Quitar el predictor wt.
modelo <- update ( modelo , . ~ . - wt)
cat ("=== Modelo con predictor cyl ===\ n")
print ( modelo )

# Agregar predictores wt y drat , y quitar predictor cyl.
modelo <- update ( modelo , . ~ . + wt + drat - cyl )
cat ("=== Modelo con predictores wt y drat ===\ n")
print ( modelo )


# EVALUACION DE VARIABLES A INCORPORAR Y ELIMINAR EN UN MODELO RLM 
# Cargar datos .
datos <- mtcars

# Ajustar modelo nulo .
nulo <- lm( mpg ~ 1 , data = datos )

# cat ("=== Modelo nulo ===\ n")
# print ( summary ( nulo ))

# Ajustar modelo completo .
completo <- lm( mpg ~ . , data = datos )
# cat ("=== Modelo completo ===\ n")
# print ( summary ( completo ))

# Evaluar variables para incorporar .
print ( add1 ( nulo , scope = completo ) )
cat ("\n\n")

# Agregar la variable con menor AIC .
modelo <- update ( nulo , . ~ . + wt)

# Evaluar variables para incorporar .
print ( add1 ( modelo , scope = completo ) )
cat ("\n\n")

# Agregar la variable con menor AIC .
modelo <- update ( modelo , . ~ . + cyl )

# Evaluar variables para eliminar .
print ( drop1 ( completo , scope = completo ) )
cat ("\n\n")

# Eliminar la variable con menor AIC .
modelo <- update ( modelo , . ~ . - cyl )


# SELECCION DE PREDICTORES A INCLUIR EN UNA RLM
library ( leaps )

# Cargar datos .
datos <- mtcars

# Ajustar modelo nulo .
nulo <- lm( mpg ~ 1 , data = datos )
cat (" === Modelo nulo ===\ n")
print ( summary ( nulo ) )

# Ajustar modelo completo .
completo <- lm( mpg ~ . , data = datos )
cat (" === Modelo completo ===\ n")
print ( summary ( completo ) )

# Ajustar modelo con selecci ón hacia adelante .
adelante <- step ( nulo , scope = list ( upper = completo ) , direction = "forward",
                   trace = 0)

cat (" === Modelo con selección hacia adelante ===\ n")
print ( summary ( adelante ) )
cat (" AIC =", AIC ( adelante ) , "\n\n")

# Ajustar modelo con eliminación hacia atrás.
atras <- step ( completo , scope = list ( lower = nulo ) , direction = "backward",
                trace = 0)
cat (" === Modelo con eliminación hacia atrás ===\ n")
print ( summary ( atras ) )
cat ("AIC =", AIC ( atras ) , "\n\n")

# Ajustar modelo con regresión escalonada .
escalonado <- step ( nulo , scope = list ( lower = nulo , upper = completo ) ,
                     direction = "both", trace = 0)

cat (" === Modelo con regresión escalonada ===\ n")
print ( summary ( escalonado ) )
cat ("AIC =", AIC ( escalonado ) , "\n\n")

# Ajustar modelo con todos los subconjuntos .
modelos <- regsubsets ( mpg ~ . , data = datos , method = "exhaustive",
                        nbest = 1 , nvmax = 10)

print ( plot ( modelos ) )


# IDENTIFICACION DE VALORES ATIPICOS
# Cargar datos .
datos <- mtcars

# Ajustar modelo .
modelo <- lm( mpg ~ wt + qsec + am , data = datos )
plot ( modelo )

# Reducir matriz de datos para que solo contenga los predictores
# empleados y la respuesta .
predictores <- names ( coef ( modelo ) ) [ -1]
datos <- datos [ , c( predictores , "mpg") ]

# Construir una matriz de datos con la respuesta predicha , los
# residuos y algunas estad í sticas para evaluar la influencia de
# cada observaci ón.
resultados <- data.frame ( respuesta_predicha = fitted ( modelo ) )
resultados [["residuos_estandarizados"]] <- rstandard ( modelo )
resultados [["residuos_estudiantizados"]] <- rstudent ( modelo )
resultados [["distancia_Cook"]] <- cooks.distance ( modelo )
resultados [["dfbeta"]] <- dfbeta ( modelo )
resultados [["dffit"]] <- dffits ( modelo )
resultados [["apalancamiento"]] <- hatvalues ( modelo )
resultados [["covratio"]] <- covratio ( modelo )

cat ("Identificación de valores atípicos :\n")
# Observaciones con residuos estandarizados fuera del 95 % esperado .
sospechosos1 <- which ( abs(
  resultados [["residuos_estandarizados"]]) > 1.96)

cat ("- Residuos estandarizados fuera del 95 % esperado :",
     sospechosos1 , "\n")

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which ( resultados [["cooks.distance"]] > 1)

cat ("- Residuos con una distancia de Cook alta :",
     sospechosos2 , "\n")

# Observaciones con apalancamiento mayor igual al doble del
# apalancamiento promedio .

apal_medio <- ( ncol ( datos ) + 1) / nrow ( datos )
sospechosos3 <- which ( resultados [["apalancamiento"]] > 2 * apal_medio )

cat ("- Residuos con apalancamiento fuera de rango :",
     sospechosos3 , "\n")

# Observaciones con DFBeta mayor o igual a 1.
sospechosos4 <- which ( apply ( resultados [["dfbeta"]] >= 1, 1 , any) )
names ( sospechosos4 ) <- NULL

cat ("- Residuos con DFBeta >= 1:",
     sospechosos4 , "\n")

# Observaciones con raz ón de covarianza fuera de rango .
inferior <- 1 - 3 * apal_medio
superior <- 1 + 3 * apal_medio
sospechosos5 <- which ( resultados [["covratio"]] < inferior |
                          resultados [["covratio"]] > superior )

cat ("- Residuos con razón de covarianza fuera de rango :",
     sospechosos5 , "\n")

# Resumen de valores sospechosos .
sospechosos <- c( sospechosos1 , sospechosos2 , sospechosos3 ,
                  sospechosos4 , sospechosos5 )

sospechosos <- sort ( unique ( sospechosos ) )

cat ("\ nResumen de valores sospechosos :\n")
cat ("Apalancamiento promedio :", apal_medio , "\n")

cat ("Intervalo razón de covarianza : [", inferior , "; ",
     superior , "]\n\n", sep = "")

print ( round ( resultados [ sospechosos , c("distancia_Cook", "apalancamiento",
                                             "covratio") ], 3) )


# VERIFICACION DE CONDICIONES PARA EL MODELO
library ( car )

# Cargar datos .
datos <- mtcars

# Ajustar modelo .
modelo <- lm( mpg ~ wt + qsec + am , data = datos )

# Comprobar independencia de los residuos .
cat (" Prueba de Durbin - Watson para autocorrelaciones ")
cat (" entre errores :\n")
print ( durbinWatsonTest ( modelo ) )

# Comprobar normalidad de los residuos .
cat ("\ nPrueba de normalidad para los residuos :\n")
print ( shapiro.test ( modelo $ residuals ) )

# Comprobar homocedasticidad de los residuos .
cat (" Prueba de homocedasticidad para los residuos :\n")
print ( ncvTest ( modelo ) )

# Comprobar la multicolinealidad .
vifs <- vif ( modelo )
cat ("\ nVerificar la multicolinealidad :\n")
cat ("- VIFs :\n")
print ( vifs )
cat ("- Tolerancias :\n")
print (1 / vifs )
cat ("- VIF medio :", mean ( vifs ) , "\n")

#-------------------------------------------------------------------------------
#                                     CAPITULO 15
#-------------------------------------------------------------------------------
# AJUSTE DE UN MODELO DE REGRESION LOGISTICA EN R
library ( pROC )
library ( caret )

set.seed (1313)

# Cargar los datos .
datos <- mtcars
datos $ am <- factor ( datos$am )

# Separar conjuntos de entrenamiento y prueba .
n <- nrow ( datos )
n_entrenamiento <- floor (0.8 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo .
modelo <- glm ( am ~ wt , family = binomial ( link = "logit") , data = entrenamiento )
print ( summary ( modelo ) )

# Evaluar el modelo con el conjunto de entrenamiento .
cat (" Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict ( modelo , entrenamiento , type = "response")

umbral <- 0.5
preds_e <- sapply ( probs_e , function ( p ) ifelse ( p >= umbral , "1", "0") )
preds_e <- factor ( preds_e , levels = levels ( datos [["am"]]) )

ROC_e <- roc ( entrenamiento [["am"]] , probs_e )
plot ( ROC_e )

matriz_e <- confusionMatrix ( preds_e , entrenamiento [["am"]])
print ( matriz_e )

# Evaluar el modelo con el conjunto de prueba .
cat ("Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict ( modelo , prueba , type = "response")

preds_p <- sapply ( probs_p , function ( p ) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor ( preds_p , levels = levels ( datos [["am"]]) )

ROC_p <- roc ( entrenamiento [["am"]] , probs_e )
plot ( ROC_p )

matriz_p <- confusionMatrix ( preds_p , prueba [["am"]])
print ( matriz_p )



# AJUSTE DE UN MODELO DE REGRESIÓN LOGISTICA USANDO VALIDACION CRUZADA
library ( caret )

set.seed (1313)

# Cargar los datos .
datos <- mtcars
datos$am <- factor ( datos$am )

# Ajustar modelo usando validación cruzada de 5 pliegues .
modelo <- train ( am ~ wt , data = entrenamiento , method = "glm",
                  family = binomial ( link = "logit") ,
                  trControl = trainControl ( method = "cv", number = 5 ,
                                             savePredictions = TRUE ) )

print ( summary ( modelo ) )

# Evaluar el modelo
cat (" Evaluaci ón del modelo basada en validación cruzada :\n")
matriz <- confusionMatrix ( modelo$pred$pred , modelo$pred$obs )
print ( matriz )



# AJUSTE Y EVALUACION DEL MEJOR MODELO PARA PREDECIR EL TIPO DE TRANSMISION DE 
# UN AUTOMOVIL.
library ( car )

set.seed (1313)

# Cargar los datos .
datos <- mtcars
am <- factor ( datos$am )
datos$am <- NULL
datos <- cbind ( am , datos )

# Separar conjuntos de entrenamiento y prueba .
n <- nrow ( datos )
n_entrenamiento <- floor (0.8 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo nulo .
nulo <- glm( am ~ 1 , family = binomial ( link = "logit") , data = entrenamiento )

# Ajustar modelo completo .
cat ("\n\n")
completo <- glm( am ~ . , family = binomial ( link = "logit") ,
                 data = entrenamiento )

# Ajustar modelo con regresi ón escalonada .
cat (" Modelo con regresión escalonada \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
mejor <- step ( nulo , scope = list ( lower = nulo , upper = completo ) ,
                direction = "both", trace = 0)
print ( summary ( mejor ) )

# Verificaci ón de multicolinealidad .
cat (" Verificación de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\ nVIF :\n")
vifs <- vif ( mejor )
print ( vifs )
cat ("\ nPromedio VIF: ")
print ( mean ( vifs ) )

# Ajustar modelo con el peso como predictor .
cat (" Modelo con el peso como predictor \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
modelo_peso <- glm( am ~ wt , family = binomial ( link = "logit") ,
                    data = entrenamiento )

print ( summary ( modelo_peso ) )

# Ajustar modelo con la potencia como predictor .
cat (" Modelo con la potencia como predictor \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
modelo_potencia <- glm( am ~ hp , family = binomial ( link = "logit") ,
                        data = entrenamiento )

print ( summary ( modelo_potencia ) )

# Comparar los modelos con el peso y la potencia como predictores .
cat ("\n\n")
cat (" Likelihood Ratio Test para los modelos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print ( anova ( modelo_peso , modelo_potencia , test = "LRT") )

# A modo de ejercicio , comparar el modelo obtenido mediante
# regresi ón escalonada con el que solo tiene el peso como predictor .
cat ("\n\n")
cat (" Likelihood Ratio Test para los modelos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print ( anova ( modelo_peso , mejor , test = "LRT") )

# Independencia de los residuos .
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print ( durbinWatsonTest ( modelo_peso , max.lag = 5) )

# Detectar posibles valores atípicos .
cat (" Identificación de posibles valores atípicos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
plot ( mejor )

# Obtener los residuos y las estadísticas .
output <- data.frame ( predicted.probabilities = fitted ( modelo_peso ) )
output [["standardized.residuals"]] <- rstandard ( modelo_peso )
output [["studentized.residuals"]] <- rstudent ( modelo_peso )
output [["cooks.distance"]] <- cooks.distance ( modelo_peso )
output [["dfbeta"]] <- dfbeta ( modelo_peso )
output [["dffit"]] <- dffits ( modelo_peso )
output [["leverage"]] <- hatvalues ( modelo_peso )

# Evaluar residuos estandarizados que escapen a la normalidad .
# 95 % de los residuos estandarizados deber ían estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which ( abs( output [["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort ( sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos1 , ]) )

# Revisar casos con distancia de Cook mayor a uno .
sospechosos2 <- which ( output [["cooks.distance"]] > 1)
sospechosos2 <- sort ( sospechosos2 )
cat ("\n\n")
cat (" Residuales con una distancia de Cook alta \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos2 , ]) )

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol ( entrenamiento ) / nrow ( datos )
sospechosos3 <- which ( output [["leverage"]] > leverage.promedio )
sospechosos3 <- sort ( sospechosos3 )
cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat ( round ( leverage.promedio , 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos3 , ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which ( apply ( output [["dfbeta"]] >= 1 , 1 , any) )
sospechosos4 <- sort ( sospechosos4 )
names ( sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos4 , ]) )

# Detalle de las observaciones posiblemente atí picas .
sospechosos <- c( sospechosos1 , sospechosos2 , sospechosos3 , sospechosos4 )
sospechosos <- sort ( unique ( sospechosos ) )
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print ( entrenamiento [ sospechosos , ])
cat ("\n\n")
print ( output [ sospechosos , ])

#-------------------------------------------------------------------------------
# MODO EJERCICIO
# Se elimina la observación "Maserati Bora" del conjunto de entrenamiento, y se
# re-hace el modelo logistico peso
entrenamiento <- entrenamiento[-7,]
modelo_peso <- glm( am ~ wt , family = binomial ( link = "logit") ,
                    data = entrenamiento )

print ( summary ( modelo_peso ) )

# Se evalúa nuevamente el modelo_peso, pero sin considerar la observación
# "Maserati Bora":

# Independencia de los residuos .
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print ( durbinWatsonTest ( modelo_peso , max.lag = 5) )

# Detectar posibles valores atípicos .
cat (" Identificación de posibles valores atípicos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
plot ( mejor )

# Obtener los residuos y las estadísticas .
output <- data.frame ( predicted.probabilities = fitted ( modelo_peso ) )
output [["standardized.residuals"]] <- rstandard ( modelo_peso )
output [["studentized.residuals"]] <- rstudent ( modelo_peso )
output [["cooks.distance"]] <- cooks.distance ( modelo_peso )
output [["dfbeta"]] <- dfbeta ( modelo_peso )
output [["dffit"]] <- dffits ( modelo_peso )
output [["leverage"]] <- hatvalues ( modelo_peso )

# Evaluar residuos estandarizados que escapen a la normalidad .
# 95 % de los residuos estandarizados deber ían estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which ( abs( output [["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort ( sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos1 , ]) )

# Revisar casos con distancia de Cook mayor a uno .
sospechosos2 <- which ( output [["cooks.distance"]] > 1)
sospechosos2 <- sort ( sospechosos2 )
cat ("\n\n")
cat (" Residuales con una distancia de Cook alta \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos2 , ]) )

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol ( entrenamiento ) / nrow ( datos )
sospechosos3 <- which ( output [["leverage"]] > leverage.promedio )
sospechosos3 <- sort ( sospechosos3 )
cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat ( round ( leverage.promedio , 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos3 , ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which ( apply ( output [["dfbeta"]] >= 1 , 1 , any) )
sospechosos4 <- sort ( sospechosos4 )
names ( sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print ( rownames ( entrenamiento [ sospechosos4 , ]) )

# Detalle de las observaciones posiblemente atí picas .
sospechosos <- c( sospechosos1 , sospechosos2 , sospechosos3 , sospechosos4 )
sospechosos <- sort ( unique ( sospechosos ) )
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print ( entrenamiento [ sospechosos , ])
cat ("\n\n")
print ( output [ sospechosos , ])
