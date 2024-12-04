## ----setup, include=FALSE------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------
a = c(T, F)
b = c(T, F, 1)
c = c(T, F, 1, '1')

vec <- list(a, b, c)
for (v in vec) {
  cat("El vector: ", v, " pertenece a la clase: ", class(v), "\n")
}


## ------------------------------------------------------------------------------------------------------------------------------
cat('Density tiene clase: ', class(density), "\n")

cat('Density(1:500) tiene clase: ', class(density(1:500)))



## ------------------------------------------------------------------------------------------------------------------------------
str(density(1:500))


## ------------------------------------------------------------------------------------------------------------------------------
library(sloop)
print(summary(s3_methods_generic("print")))


## ------------------------------------------------------------------------------------------------------------------------------
#metodos_de_density <- methods(class="density")
cat('los métodos de density son: \n')
methods(class = "density")
cat('la cantidad de métodos sin contar print son: ', length(metodos_de_density)-1)



## ------------------------------------------------------------------------------------------------------------------------------
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))

mu0 <- 0
alfa <- 0.05
test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu0,
  conf.level = 1 - alfa
)
print(unclass(test_t))


## ------------------------------------------------------------------------------------------------------------------------------
print(class(unclass(test_t)))


## ------------------------------------------------------------------------------------------------------------------------------
class(test_t)


## ------------------------------------------------------------------------------------------------------------------------------
todos_los_subconjuntos_de_5_elementos <- function() {
  elements <- 1:5
  subcojuntos <- list()
  for (k in 0:5) {
    subconjuntos_de_k <- combn(elements, k, simplify = FALSE)
    subcojuntos <- c(subcojuntos, subconjuntos_de_k)
  }
  return(subcojuntos)
}

df <- data.frame(T = numeric(), S = numeric(), Cantidad = numeric(), Probabilidad = numeric())

crear_tabla_observacion_6 <- function(){
  subconjuntos <- todos_los_subconjuntos_de_5_elementos()
  for (t in 0:15){
    subconjuntos_que_suman_t <- c()
    cantidad <- 0
    for (subconjunto in subconjuntos){
      if (sum(subconjunto) == t){
        subset_str <- if (length(sub) > 0) paste(subconjunto, collapse = ", ") else ""
        string_del_subconjunto <- c("{", subset_str,"}")
        subconjuntos_que_suman_t <- c(subconjuntos_que_suman_t, string_del_subconjunto)
        cantidad <- cantidad + 1
      }
    }
    df <- rbind(df, data.frame(T = t, 
                               S = paste(subconjuntos_que_suman_t, collapse = " "), 
                               Cantidad = cantidad, 
                               Probabilidad = cantidad/32))
  }
  return(df)
}
print(crear_tabla_observacion_6())


## ------------------------------------------------------------------------------------------------------------------------------
particiones <- function(t, n){
  if (n==0) {
    if (t == 0) return(1)
    return(0)
  }
  
  if (t < 0 || t > n*(n+1)/2) return(0)
  return(particiones(t, n-1) + particiones(t-n, n-1))
}

probabilidad <- function(t, n){
  return(particiones(t, n)/(2**n))
}

stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)


## ------------------------------------------------------------------------------------------------------------------------------

# proba puntual 

dTmas <- function(ts, n) {
  res <- vector(mode = "numeric", length = length(ts))
  for (j in seq_along(ts)) {
    res[j] <- probabilidad(ts[j], n) 
  }
  return(res)
}

# acumulada 

pTmas <- function(ts, n) {
  res <- vector(mode = "numeric", length = length(ts))
  for (j in seq_along(ts)) {
    res[j] <- sum(dTmas((0:ts[j]), n))
  }
  return(res)
}

n <- 15
t <- 34
stopifnot(
  dTmas(24, 12) == 67 / 2 ^ 12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2 ^ -55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)
)


## ------------------------------------------------------------------------------------------------------------------------------
library(stats)
set.seed(125)
n <- 15
X <- rnorm(n)
theta0 <- 2


estadistico <- function(x){
  signos <- ifelse(x >= 0, 1, -1)
  modx <- abs(x)
  rango <- rank(modx)
  producto <- signos*rango
  return(sum(producto))
}

mi.wilcox.test <- function(x, alternative, mu) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  x <- x - mu
  tobs <- estadistico(x)
  tMasObs <- (tobs/2) + n*(n+1)/4
  if (alternative == 'greater'){
    p.value <- 1 - pTmas(tMasObs - 1, length(x)) 
  } else if (alternative == 'two.sided'){
    p.value <- (1 - pTmas(abs(tobs)/2 + (n*(n+1)/4) - 1, length(x)))*2
  } else {
    p.value <- pTmas(tMasObs, length(x)) 
  }
  
  resultado <- list(
    statistic = tMasObs,
    p.value = p.value,
    alternative = alternative
  )
  
  class(resultado) <- "htest"
  return(resultado)
}

R_wilcox <- wilcox.test(X, alternative="two.sided", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="two.sided", mu = theta0)
print(mi_wilcox$p.value)
print(R_wilcox$p.value)

stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest",
  abs (mi_wilcox$p.value - R_wilcox$p.value) < 1E-6
)



## ------------------------------------------------------------------------------------------------------------------------------
set.seed(1234)
n <- 15
X <- rnorm(n)
theta0 <- 1
R_wilcox <- wilcox.test(X, alternative="two.sided", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="two.sided", mu = theta0)
stopifnot(
 mi_wilcox$statistic == R_wilcox$statistic,
 abs(mi_wilcox$p.value - R_wilcox$p.value) < 1E-10,
 mi_wilcox$alternative == R_wilcox$alternative,
 class(mi_wilcox) == "htest"
)


## ------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

grafico_de_distribuciones <- function(n){
  valores_Tmas <- 0:(n * (n + 1) / 2)
  probabilidades <- dTmas(valores_Tmas, n)
  
  media_asintotica <- n * (n + 1) / 4
  desv_asintotica <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  x_asintotico <- seq(min(valores_Tmas), max(valores_Tmas), length.out = 100)
  y_asintotico <- dnorm(x_asintotico, mean = media_asintotica, sd = desv_asintotica)
  
  data <- data.frame(
    valores_Tmas = valores_Tmas,
    probabilidades = probabilidades
  )
  
  data_asintotico <- data.frame(
    x_asintotico = x_asintotico,
    y_asintotico = y_asintotico
  )
  separacion <- 2
  if (n == 20){
    separacion <- 8
  }
  ggplot() +
    geom_bar(
      data = data,
      aes(x = valores_Tmas, y = probabilidades),
      stat = "identity",
      fill = "skyblue", 
      alpha = 0.8
    ) +
    geom_line(
      data = data_asintotico,
      aes(x = x_asintotico, y = y_asintotico),
      color = "red", 
      size = 1
    ) +
    labs(
      title = sprintf("Grafico de distribuciones para n = %d", n),
      x = "Valores de T+",
      y = "Probabilidades",
      fill = "Legend",
      color = "Legend"
    ) +
    scale_x_continuous(
      breaks = seq(min(valores_Tmas), max(valores_Tmas), by = separacion),
      labels = as.character(seq(min(valores_Tmas), max(valores_Tmas), by = separacion)),
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.box.spacing = unit(1, "cm"),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
grafico_de_distribuciones(4)
grafico_de_distribuciones(10)
grafico_de_distribuciones(20)



## ------------------------------------------------------------------------------------------------------------------------------
#Calculemos entonces el k*
# Buscar k* desde el valor máximo posible hasta 1

calculo_cuantil  <- function(n, alfa){
  T_max <- n * (n + 1) / 2
  suma_prob <- 0
  k <- 1
  iteraciones <- 0 
  for (t in T_max:0) {
    prob_t <- probabilidad(t, n)
    suma_prob <- suma_prob + prob_t
    if (suma_prob > alfa) {
      k <- t + 1
      break
    }
    
  }
  return(k)
}

print(calculo_cuantil(12, 0.05))
print(1 - pTmas(60,12)) #Esto es P(T+ >= 61), asi que esta bien el k
print(1 - pTmas(59,12)) #Esto es P(T+ >= 60), Vemos que nos pasamos del valor alpha = 0.05


## ------------------------------------------------------------------------------------------------------------------------------
calculo_estadisticoT_mas <- function(x){
  solo_positivos_filter <- ifelse(x >= 0, 1, 0)
  modx <- abs(x) #No es necesario pq a las negativas no las vamos a considerar
  rango <- rank(modx)
  producto <- solo_positivos_filter*rango
  return(sum(producto))
}
m = 10000
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
vector_Ys = c()
vector_Ys <- replicate(m, {
  X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  calculo_estadisticoT_mas(X)
}) #Esta forma es como un "for", pero en cada indice se guarda un valor del estadistico 
k <- calculo_cuantil(12,0.05)
vector_resultado <- ifelse(vector_Ys >= k, 1, 0) #IMPORTANTE el igual ya que es discreta
potencia_estimada = (1/m) * sum(vector_resultado)
potencia_estimada


## ------------------------------------------------------------------------------------------------------------------------------
theta <- 1
sigma <- 1
n <- 12
alfa <- 0.05
nu <- n - 1
centralidad <- theta / (sigma / sqrt(n))
k <- qt(1 - alfa, df = nu)

potencia <- 1 - (pt(k, df = nu, ncp = centralidad))
cat("Potencia para el t-test unilateral para theta = 1: ", potencia)


## ------------------------------------------------------------------------------------------------------------------------------

calculo_cuantil_binomial  <- function(n,alfa){
  T_max <- n   # Valor máximo posible de T^+
  suma_prob <- 0  # Suma acumulada de probabilidades
  k <- 1
  iteraciones <- 0 
  for (t in T_max:0) {
    prob_t <- dbinom(t, n, 0.5)  # Probabilidad puntual para T^+ = t
    suma_prob <- suma_prob + prob_t  # Acumular la probabilidad
    if (suma_prob > alfa) {
      k <- t + 1  # Guardar el valor k+1
      break
    }
    
  }
  return(k)
}

k = calculo_cuantil_binomial(12,0.05)
k


## ------------------------------------------------------------------------------------------------------------------------------

calculo_estadistico_signo <- function(x){
  signos <- ifelse(x >= 0, 1, 0)
  return(sum(signos))
}
m = 10000
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
vector_Ys = c()
vector_Ys <- replicate(m, {
  X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  calculo_estadistico_signo(X)
}) #Esta forma es como un "for", pero en cada indice se guarda un valor del estadistico 
k <- calculo_cuantil_binomial(12,0.05)
vector_resultado_bin <- ifelse(vector_Ys >= k, 1, 0) #IMPORTANTE EL IGUAL
potencia_estimada_bin = (1/m) * sum(vector_resultado_bin)
potencia_estimada_bin

