# Ejerc. 2 del examen escrito
##########################################
# Pregunta:
# La media de la población uno es mayor?
# Prueba de lado derecho o cola derecha
# H0: mu1-mu2 <= 0 vs H1: mu1-mu2 > 0
#1 calcular el estadístico de prueba
datos1 <- c(0.9,0.1,-0.2,1.1,0.8)
datos2 <- c(1.6,0.8,2.8,0.0,0.8,1.2,2.3,1.6,2.3,1.1)
boxplot(datos1,datos2)
n1 <- length(datos1)
n2 <- length(datos2)
xbarra1 <- mean(datos1); sigma1 <- sd(datos1) 
xbarra2 <- mean(datos2); sigma2 <- sd(datos2) 
alpha <- 0.01
delta0 <- 0
t0 <- (xbarra1-xbarra2-delta0)/sqrt(sigma1^2/n1+sigma2^2/n2) 
#2 calcular la región crítica
num <- (sigma1^2/n1+sigma2^2/n2)^2
den <- (sigma1^2/n1)^2/(n1-1) + (sigma2^2/n2)^2/(n2-1)
v <- num/den
limder <- -qt(alpha,v)
if(t0>limder){
        Resultado <- 'Rechazar H0'
}else{Resultado <- 'No rechazar H0'}
#4 concluir
Resultado 
##################
#5 Cálculo del p-value
pvalue <- 1-pt(t0,n1+n2-2)
pvalue
if(pvalue < alpha){
        Resultado2 <- 'Rechazar H0'
}else{Resultado2 <- 'No rechazar H0'}
Resultado2

## Usando R
t.test(datos1,datos2,alternative = 'greater')


# Pregunta:
# La media de la población dos es mayor?
# Prueba de lado izquierdo o cola izquierda
# H0: mu1-mu2 >= 0 vs H1: mu1-mu2 < 0
#1 calcular el estadístico de prueba
datos1 <- c(0.9,0.1,-0.2,1.1,0.8)
datos2 <- c(1.6,0.8,2.8,0.0,0.8,1.2,2.3,1.6,2.3,1.1)
boxplot(datos1,datos2)
n1 <- length(datos1)
n2 <- length(datos2)

xbarra1 <- mean(datos1); sigma1 <- sd(datos1) 
xbarra2 <- mean(datos2); sigma2 <- sd(datos2) 
alpha <- 0.01
delta0 <- 0
t0 <- (xbarra1-xbarra2-delta0)/sqrt(sigma1^2/n1+sigma2^2/n2) 
#2 calcular la región crítica
num <- (sigma1^2/n1+sigma2^2/n2)^2
den <- (sigma1^2/n1)^2/(n1-1) + (sigma2^2/n2)^2/(n2-1)
v <- num/den
limizq <- qt(alpha,v)
if(t0<limizq){
        Resultado <- 'Rechazar H0'
}else{Resultado <- 'No rechazar H0'}
#4 concluir
Resultado 
##################
#5 Cálculo del p-value
pvalue <- pt(t0,v)
pvalue
if(pvalue < alpha){
        Resultado2 <- 'Rechazar H0'
}else{Resultado2 <- 'No rechazar H0'}
Resultado2

## Usando R
t.test(datos1,datos2,alternative = 'less')

#################################################
#################################################

#Varianzas iguales?
datos1 <- c(0.9,0.1,-0.2,1.1,0.8)
datos2 <- c(1.6,0.8,2.8,0.0,0.8,1.2,2.3,1.6,2.3,1.1)
n1 <- length(datos1)
n2 <- length(datos2)
sigma1 <- sd(datos1) 
sigma2 <- sd(datos2) 
alpha <- 0.01
F0 <- sigma1^2/sigma2^2
newlimizq <- qf(alpha/2,n1-1,n2-1)
newlimizq <- qf(.005,n1-1,n2-1)
newlimder <- qf(1-alpha/2,n1-1,n2-1)
newlimder <- qf(0.995,n1-1,n2-1)
#3 contrastar estadístico de prueba con la región crítica
#Rechazar H0 si z0<=limizq, o si z0>=limder
if(F0<=newlimizq || F0>=newlimder){
        Resultado <- 'Rechazar H0'
}else{Resultado <- 'No rechazar H0'}
#4 concluir
Resultado 
##################
#5 Cálculo del p-value
aux <- min(pf(F0,n1-1,n2-1),1-pf(F0,n1-1,n2-1))
pvalue <- 2*aux
pvalue
if(pvalue < alpha){
        Resultado2 <- 'Rechazar H0'
}else{Resultado2 <- 'No rechazar H0'}
Resultado2

# Usando R
var.test(datos1,datos2)

