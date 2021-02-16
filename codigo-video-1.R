#############################
#   Pruebas de hipótesis    #
#############################
#Ejerc. 1 del examen escrito
# 1 calcular el estadístico de prueba
datos <- c(1496,1500,1510,1485,1503,1491,1506,1526,1493,1488,1495,1503,1528,1496,1491,1497)
n <- length(datos)
xbarra <- mean(datos); sigma <- 15; alpha <- 0.2
#mu0 <- 1520
mu0 <- 1506
z0 <- (xbarra-mu0)/(sigma/sqrt(n)) 
z0
#2 calcular la región crítica
limizq <- qnorm(alpha/2)
limder <- -limizq 
#equivalente: limizq <- qnorm(1-alpha/2)
#3 contrastar estadístico de prueba con la región crítica
#Rechazar H0 si z0<=limizq, o si z0>=limder
if(z0<=limizq || z0>=limder){
        Resultado <- 'Rechazar H0'
}else{Resultado <- 'No rechazar H0'}
#4 concluir
Resultado 
#######
#5 Cálculo del p-value
pvalue <- 2*(pnorm(-abs(z0))) # 2*(1- pnorm(abs(z0)))
pvalue

if(pvalue < alpha){
        Resultado2 <- 'Rechazar H0'
}else{Resultado2 <- 'No rechazar H0'}
Resultado2
####################

# Función en R
library(BSDA)
z.test(datos,mu = mu0 ,sigma.x = sigma)
#########################################################
z.test(datos,mu = mu0 ,sigma.x = sigma, conf.level = 0.8)

#### límites de la región de aceptación:
ejex <- seq(-3,3,0.01)
ejey <- dnorm(ejex)
plot(ejex,ejey,type='l', lwd=3)
polygon(c(3,-3,ejex),c(0,0,ejey),col='yellow')
abline(v=0)
####################
ejex <- seq(-3,3,0.01)
ejey <- dnorm(ejex)
plot(ejex,ejey,type='l', lwd=3)
lizq <- qnorm(alpha/2)
aux.x <- ejex[ejex<=lizq]
aux.y <- dnorm(aux.x)
polygon(c(3,-3,aux.x,lizq),c(0,0,aux.y,0),col='yellow')
polygon(c(-lizq,-rev(aux.x),3),c(0,rev(aux.y),0),col='yellow')
text(-2,0.3,'Región de rechazo área amarilla')
text(2,0.3,'Región de rechazo área amarilla')
text(0,0.1,'Región de aceptación')
abline(v=0)
###############################
abline(v=z0, col =2)
aux.z0 <- abs(z0)
aux2.x <- ejex[ejex<=-aux.z0]
aux2.y <- dnorm(aux2.x)
polygon(c(3,-3,aux2.x,-aux.z0),c(0,0,aux2.y,0),col='blue')
polygon(c(aux.z0,-rev(aux2.x),3),c(0,rev(aux2.y),0),col='blue')
##############################
title(paste('pvalue = 2P(Z<=-abs(z0))=',round(pvalue,5)))

        