a <- runif(12) # Crea 'a' con números aleatorios
for (i in 1:12) {a[i] <- a[i] + i}
print(a)

#-----------------------------------

raices<- function(a,b,c){
  raiz <- (b^2-4*a*c)
  completa1 <- (-b + raiz)/(2*a)
  completa2 <- (-b-raiz)/(2*a)
  return(c(completa1,completa2))
}
raices(2,3,1)

#-----------------------------------

factorial <-function(n){
resultado <-1
if (n==0){return(1)}
else{
for(i in 1:n){
resultado = resultado * i
} 
return (resultado)}}

print(factorial(5))

#-----------------------------------
Binarioadec <- function(x){
  B=0
  D=0
  H=1
  for (i in 1:11) {
    bit = as.integer(substr(x, i, i))
    B = B + bit * 2^(11 - i)
  }
  c = 2^(B - 1023)
  for (e in 12:63){
    bitf = as.integer(substr(x,e,e))
    D= D+ bitf*(1/2)^H
    H = H +1
  }
  f = 1 + D
  fn = c * f
  return(fn)
}
Binarioadec("100000000111011100100010000000000000000000000000000000000000000")
#-----------------------------------------

Polinomio45 <-function(x){
  f <- 3*x^3 + 5*x^2 - 2*x + 1
  f
}

xp <- seq(-3,0, by=0.1)
fxp <- Polinomio45(xp)
dfp <- data.frame(xp,fxp)
dfp

gg_plotp <- ggplot(data=dfp, aes(x=xp, y=fxp)) + geom_line(linetype=1, colour="darkblue")
gg_plotp <- gg_plotp + geom_hline(yintercept=0, linetype=1) + geom_vline(xintercept=0, linetype=1)
gg_plotp <- gg_plotp + scale_x_continuous(breaks = seq(-3,0, by=0.5)) + scale_y_continuous(breaks = seq(-1,1, by=0.5))
gg_plotp

Raiz_biseccion <-function(a,b, Tol, N){
  i <- 1
  FA <- Polinomio45(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-Polinomio45(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion(-3,2,0.0001,600)

#-----------------------------------------------
Funcion1 <- function(x){
  y <- x - 2^(-x)
  y
}

x1 <- seq(0,1, by=0.1)
fx1 <- Funcion1(x1)
df1 <- data.frame(x1,fx1)

ggplot1 <- ggplot(data = df1, aes(x= x1, y=fx1)) + geom_line(linetype=1, colour="blue")
ggplot1 <- ggplot1 + geom_hline(linetype=1, yintercept=0) + geom_vline(linetype=1, xintercept=0)
ggplot1 <- ggplot1 + ggtitle("raiz entre 0.5 y 0.75")
ggplot1

Raiz_biseccion1 <-function(a,b, Tol, N){
  i <- 1
  FA <- Funcion1(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-Funcion1(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion1(0,1,0.00001,600000)

#------------------------

funcion2<- function(x) {
  y <- exp(x) - x^2 + 3*x - 2
  return(y)}

Raiz_biseccion2 <-function(a,b, Tol, N){
  i <- 1
  FA <- funcion2(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-funcion2(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion2(0,1,0.00001,600000)

#-------------------------------------------------------------------

funcion3<- function(x) {
  y <- 2*x*cos(2*x) - (x + 1)^2
  return(y)}

Raiz_biseccion3 <-function(a,b, Tol, N){
  i <- 1
  FA <- funcion3(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-funcion3(p)
    if (FP == 0 | (b-a)/2 < Tol){
      print(i)
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion3(-1,0,0.00001,600000)

#-----------------------------------------------------------------
funcion4<- function(x) {
  y <- x*cos(x) - 2*(x^2)+3*x-1
  return(y)}

Raiz_biseccion4 <-function(a,b, Tol, N){
  i <- 1
  FA <- funcion4(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-funcion4(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion4(1.2,1.3,0.00001,14)
num_iteracion(1.2,1.3,0.00001)
#-----------------------------------------------------------
#Calculo para obtener una cota del número de iteraciones que se requieren:
#Si (b0-a0)/2^n <= error que estoy dispuesto a asumir:
# n >= [ln(b0-a0)-ln(error dispuesto)]/ln(2)

num_iteracion <- function(a,b,Tol){
  y<- (log(b-a)-log(Tol))/log(2)
  y
}

num_iteracion(1,4,10^-3)

#---------------------------------------------------------------

g1 <- function(x) {
  y <- (3+3*x^2)^(1/4)
  return(y)
}

x1 <- seq(0,3, by = 0.1)
fx1 <- g1(x1)
df1 <- data.frame(x1,fx1)

ggplotg1 <- ggplot(data = df1, aes(x=x1, y=fx1)) + geom_line(linetype=1, colour="red")
ggplotg1 <- ggplotg1 + geom_hline(yintercept=0, linetype =1) + geom_vline(xintercept=0, linetype=1)
ggplotg1 <- ggplotg1 + geom_abline(intercept = 0,slope=1,linetype=2)
ggplotg1

puntofijo1 <- function(p0, tol, N) {
  i <- 1
  while(i <= N) {
    p <- g1(p0)
    if(abs(p - p0) < tol) {
      return(p)
    }
    i <- i + 1
    p0 <- p
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

puntofijo1(1, 10^-2, 600)

#----------------------------------------------------------------
g2 <- function(x) {
  y <- (1+x)^(1/3)  #si hago g2(a) y g2(b) ambos valores se encuentran en [a,b]
  return(y)
}

puntofijo2 <- function(p0, tol, N) {
  i <- 1
  while(i <= N) {
    p <- g2(p0)
    if(abs(p - p0) < tol) {
      print(i)
      return(p)
    }
    i <- i + 1
    p0 <- p
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

puntofijo2(1, 10^-2, 600)

#--------------------------------------------------------------
g3 <- function(x) {
  y <- pi + 0.5*sin(x/2)
  return(y)
}

puntofijo3 <- function(p0, tol, N) {
  i <- 1
  while(i <= N) {
    p <- g3(p0)
    if(abs(p - p0) < tol) { 
      print(i)
      return(p)
    }
    i <- i + 1
    p0 <- p
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

puntofijo3(0, 10^-4, 600)  #utilizo 0 ya que el punto me señala que está dentro de [0,2pi]

Kteorico <- function(p0,p1,tol,K){
  n = (log(tol*(1-K))-log(abs(p1-p0)))/log(K)
  n
}
Kteorico(0,pi, 10^-4,0.25)
#-------------------------------------------------------------------------------
g4 <- function(x) {
  y <- cos(x)^2
  return(y)
}

puntofijo4 <- function(p0, tol, N) {
  i <- 1
  while(i <= N) {
    p <- g4(p0)
    if(abs(p - p0) < tol) { print(i)
      return(p)
    }
    i <- i + 1
    p0 <- p
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

puntofijo4(0, 10^-5, 600) 

#-------------------------------------------------------------------------------
g5 <- function(x) {
  y <- log(2 + cos(exp(x)-2))
  return(y)
}

puntofijo5 <- function(p0, tol, N) {
  i <- 1
  while(i <= N) {
    p <- g5(p0)
    if(abs(p - p0) < tol) { print(i)
      return(p)
    }
    i <- i + 1
    p0 <- p
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

puntofijo5(0, 10^-5, 600)
#-------------------------------------------------------------------------------
#Metodo Newton-Raphson

fx1 <- function(x){
  y <- -x^3 - cos(x)
  return(y)}
fx1der <- function(x){
 y <- -3*x^2 + sin(x)
 return(y)}

xn <- seq(-1,1, by=0.1)
fxn <- fx1(xn)
dfn <- data.frame(xn, fxn)
newton_gg_plot <- ggplot(data = dfn, aes(x=xn, y =fxn)) + geom_line(linetype = 1, colour="red")
newton_gg_plot <- newton_gg_plot + geom_hline(linetype = 1, yintercept=0) + geom_vline(linetype=1, xintercept=0)
newton_gg_plot

Newton1 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx1(p0)/fx1der(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    if(i==2){
      return(p)
    }
    i = i + 1
    p0 <- p
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton1(-1,10^-2,600)
#con 0 no se puede hacer ya que es un punto crítico, la pendiente en ese punto 
#en la función es paralela a x.  

#----------------------------------------------------------------------------
fx2 <- function(x){
  y <- x^3 - 2*x^2 - 5
  return(y)}
fx2der <- function(x){
 y <- 3*x^2 - 4*x
return(y)}

Newton2 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx2(p0)/fx2der(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton2(4,10^-4,600)

#--------------------------------------------------------------------
fx3 <- function(x){
  y <- x^3 - 3*x^2 - 1
  return(y)}
fx3der <- function(x){
  y <- 3*x^2 - 6*x
  return(y)}

Newton3 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx3(p0)/fx3der(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton3(-3,10^-4,600)
#--------------------------------------------------------------------
fx4 <- function(x){
  y <- x - cos(x)
  return(y)}
fx4der <- function(x){
  y <- 1 + sin(x)
  return(y)}

Newton4 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx4(p0)/fx4der(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton4(pi/2,10^-4,600)

#----------------------------------------------------------------

fx5 <- function(x){
  y <- x - 0.8 - 0.2*sin(x)
  return(y)}
fx5der <- function(x){
  y <- 1 - 0.2*cos(x)
  return(y)}

Newton5 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx5(p0)/fx5der(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton5(0,10^-4,600)

#------------------------------------------------------------------
install.packages("Deriv")

library(Deriv)
f <- function(x) exp(x) + 2^-x + 2*cos(x) - 6
fx6der <- Deriv(f, "x")

fx6 <- function(x){
  y <- exp(x) + 2^-x + 2*cos(x) - 6
  return(y)}

fx6deriv <- function(x){
  y <- exp(x) - (2^-x)*log(2) - 2*sin(x)
  y}
  
Newton6 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx6(p0)/fx6deriv(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton6(0,10^-4,600)
#-----------------------------------------------------------------
fx7 <- function(x){
  y <- 2*x*cos(2*x) - (x-2)^2
  return(y)}

fx7deriv <- function(x){
  y <- -4*x*sin(2*x) - 2*(x-2)
  y}

Newton7 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(fx7(p0)/fx7deriv(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton7(4,10^-4,600) #si pongo p0=2 me da la primera raiz y se pongo p0=4 me da la segunda raiz
#------------------------------------------------------------------
# Metodo de la Secante

fs1 <- function(x){
  y <- x^2 - 6
  return(y)}

secante1 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fs1(p1)*(p1-p0)/(fs1(p1)-fs1(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p1
    p1 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

secante1(3,2,10^-4,600)

P3s1 <- function(p0,p1,N){
  i=1
  while (i<=N){
    p <- p1-fs1(p1)*(p1-p0)/(fs1(p1)-fs1(p0))
    i = i + 1
    p0 <- p1
    p1 <- p}
  return(p)
}
P3s1(3,2,2)
#--------------------------------------------------------------------
fs2 <- function(x){
  y <- x^3 - 2*x^2 - 5
  return(y)}

secante2 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fs2(p1)*(p1-p0)/(fs2(p1)-fs2(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p1
    p1 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

secante2(1,4,10^-4,600)

#----------------------------------------------------------------
fs3 <- function(x){
  y <- x^3 - 3*x^2 - 1
  return(y)}

secante3 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fs3(p1)*(p1-p0)/(fs3(p1)-fs3(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p1
    p1 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

secante3(-3,-2,10^-4,600)
#--------------------------------------------------------------------
fs4 <- function(x){
  y <- x - cos(x)
  return(y)}

secante4 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fs4(p1)*(p1-p0)/(fs4(p1)-fs4(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p1
    p1 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

secante4(0,pi/2,10^-4,600)
#---------------------------------------------------------------
fs5 <- function(x){
  y <- x - 0.8 - 0.2*sin(x)
  return(y)}

secante5 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fs5(p1)*(p1-p0)/(fs5(p1)-fs5(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p1
    p1 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

secante5(0,pi/2,10^-4,600)
#------------------------------------------------------------------
fp1 <- function(x){
  y <- x^2 - 6
  return(y)}

Posicion1 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fp1(p1)*(p1-p0)/(fp1(p1)-fp1(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    q = fp1(p)
    if(q*fp1(p1) <0){p0 <-p1
    p1 <- p}
    else{p1 <-p}
    }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Posicion1(3,2,10^-4,600)

P3pf <- function(p0,p1,N){
  i=1
  while (i<=N){
    p <- p1-fp1(p1)*(p1-p0)/(fp1(p1)-fp1(p0))
    i = i + 1
    q = fp1(p)
    if(q*fp1(p1) <0){p0 <-p1
    p1 <- p}
    else{p1 <-p}
  }
  return(p)
}
P3pf(3,2,2)
#--------------------------------------------------------------

fp2 <- function(x){
  y <- x^3 - 2*x^2 - 5
  return(y)}

Posicion2 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fp2(p1)*(p1-p0)/(fp2(p1)-fp2(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    q = fp2(p)
    if(q*fp2(p1) <0){p0 <-p1
    p1 <- p}
    else{p1 <-p}
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Posicion2(1,4,10^-4,600)

#----------------------------------------------------------------
fp3 <- function(x){
  y <- x^3 - 3*x^2 - 1
  return(y)}

Posicion3 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fp3(p1)*(p1-p0)/(fp3(p1)-fp3(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    q = fp3(p)
    if(q*fp3(p1) <0){p0 <-p1
    p1 <- p}
    else{p1 <-p}
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Posicion3(-3,-2,10^-4,600)
#-------------------------------------------------------------------------
fp4 <- function(x){
  y <- x - cos(x)
  return(y)}

Posicion4 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fp4(p1)*(p1-p0)/(fp4(p1)-fp4(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    q = fp4(p)
    if(q*fp4(p1) <0){p0 <-p1
    p1 <- p}
    else{p1 <-p}
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Posicion4(0,pi/2,10^-4,600)

#-------------------------------------------------------------------
fp5 <- function(x){
  y <- x - 0.8 - 0.2*sin(x)
  return(y)}

Posicion5 <- function(p0,p1,tol,N){
  i=1
  while (i<=N){
    p <- p1-fp5(p1)*(p1-p0)/(fp5(p1)-fp5(p0))
    if (abs(p-p1)<tol){
      print(i) 
      return(p)}
    i = i + 1
    q = fp5(p)
    if(q*fp5(p1) <0){p0 <-p1
    p1 <- p}
    else{p1 <-p}
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Posicion5(0,pi/2,10^-4,600)

#------------------------------------------------------------------
#TIR 1
#inputs ----
cupon = 0.05
amortizaciones <- matrix(c(1,2,3,4,
                           25,25,25,25), ncol = 2, dimnames= list(NULL,c("t","amort")))
m = 2
  
#MarchaProgresiva ----
n = max(amortizaciones[,1])*m
marcha <- matrix(rep(NA,(n+1)*5), ncol=5, dimnames =list(NULL,c("t","saldo","amort","int","CF")))

marcha[,"t"] = seq(0,n/m,by=1/m)

k=1
marcha[1,"saldo"] <- sum(amortizaciones[,"amort"])

for (i in 1:(n+1)){
  if(marcha[i,"t"]==amortizaciones[k,"t"]){
    marcha[i,"amort"] = amortizaciones[k,"amort"]
    if(i>1){marcha[i,"saldo"]= marcha[i-1,"saldo"]- amortizaciones[k,"amort"]}
    k = k+1
  }   else {marcha[i,"amort"]=0
  if(i>1){marcha[i,"saldo"]=marcha[i-1,"saldo"]}
}}

marcha[1,"int"] = 0
for (i in 2:(n+1)){
  marcha[i,"int"]=marcha[i-1,"saldo"]*cupon/m
}

marcha[,"CF"]= marcha[,"amort"]+marcha[,"int"]
#Funcion de Precio ----

Precio <- function(tasa, t, CF){
  n = length(CF)
  P=0
  for (i in 1:n){
    P = P + CF[i]*(1+tasa)^-t[i]
  } 
  return(P)
}


#Grafico ----
x= seq(0,1, by=0.001)
y = -80 + Precio(x,marcha[,"t"],marcha[,"CF"])

plot(x,y, type ="l", ylim=c(-40,40), xlim = c(0,0.4))
abline(h=0) 
#veo que está entre 0.1 y 0.2 la raíz.
dftir <- data.frame(x,y)

library(ggplot2)

ggplotir <- ggplot(data = dftir, aes(x=x, y =y)) + geom_line(linetype =1, colour="red")
ggplotir <- ggplotir + geom_hline(yintercept = 0,linetype=1) + geom_vline(xintercept = 0,linetype=1)
ggplotir

#Raíz ----
z <- function(x){
  y = -80 + Precio(x,marcha[,"t"],marcha[,"CF"])
  y}

Raiz_biseccion <-function(a,b, Tol, N){
  i <- 1
  FA <- z(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-z(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion(0.1,0.2,0.000001,600)


#TIR 2 -----------------------------------------------------
#inputs ----
cupon = 0.22
amortizaciones <- matrix(c(1,2,
                           0,100), ncol = 2, dimnames= list(NULL,c("t","amort")))
m=2

#MarchaProgresiva ----
n = max(amortizaciones[,1])*m
marcha <- matrix(rep(NA,(n+1)*5), ncol=5, dimnames =list(NULL,c("t","saldo","amort","int","CF")))

marcha[,"t"] = seq(0,n/m,by=1/m)

k=1
marcha[1,"saldo"] <- sum(amortizaciones[,"amort"])

for (i in 1:(n+1)){
  if(marcha[i,"t"]==amortizaciones[k,"t"]){
    marcha[i,"amort"] = amortizaciones[k,"amort"]
    if(i>1){marcha[i,"saldo"]= marcha[i-1,"saldo"]- amortizaciones[k,"amort"]}
    k = k+1
  }   else {marcha[i,"amort"]=0
  if(i>1){marcha[i,"saldo"]=marcha[i-1,"saldo"]}
  }}

marcha[1,"int"] = 0
for (i in 2:(n+1)){
  marcha[i,"int"]=marcha[i-1,"saldo"]*cupon/m
}
marcha[,"CF"]= marcha[,"amort"]+marcha[,"int"]
#Funcion de Precio ----

Precio <- function(tasa, t, CF){
  n = length(CF)
  P=0
  for (i in 1:n){
    P = P + CF[i]*(1+tasa)^-t[i]
  } 
  return(P)
}

#Grafico ----
x= seq(0,1, by=0.001)
y = -103.50 + Precio(x,marcha[,"t"],marcha[,"CF"])

plot(x,y, type ="l", ylim=c(-40,40), xlim = c(0,0.4))
abline(h=0) 
#veo que está entre 0.2 y 0.3 la raíz.

#Raíz ----
z <- function(x){
  y = -103.5 + Precio(x,marcha[,"t"],marcha[,"CF"])
  y}

Raiz_biseccion <-function(a,b, Tol, N){
  i <- 1
  FA <- z(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-z(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion(0.1,1,0.000001,600)

#-----------------------------------------------------------

# TIR 3 ----------------------------------------------------------
d <- function(r) {
  P = 0
  for(t in 1:10) {
    P = P + 2.5 * (1 + 0.5 * r) ^ (-t)
  } 
  P = P + 100*(1+0.5*r)^-10
  return(P)
}
#Plot -----
x= seq(0,1, by=0.001)

y = -101.5 + d(x)

plot(x,y, type = "l", ylim=c(-10,10), xlim =c(0,0.2))
abline(h=0)
#veo que está entre 0 y 0.1
library(ggplot2)
datatir <- data.frame(x,y)
ggplottir2 <- ggplot(data=datatir, aes(x=x, y=y)) + geom_line(linetype=1, colour="blue")
ggplottir2 <- ggplottir2 + geom_hline(yintercept = 0, linetype=1) + geom_vline(xintercept = 0, linetype=1)
ggplottir2

#Pruebo Biseccion ----
g <- function(v){
  y = -101.5 + d(v)
  y
}

Raiz_biseccion <-function(a,b, Tol, N){
  i <- 1
  FA <- g(a)
  while (i <=N){
    p<- a + (b-a)/2
    FP<-g(p)
    if (FP == 0 | (b-a)/2 < Tol){
      return(p)
    } 
    i <- i +1
    if(FA*FP > 0){
      a<-p
      FA <- FP}
    else {b<-p}
  }
  return(paste("el método falló luego de ",N," iteraciones"))
}

Raiz_biseccion(0,0.1,0.000001,600)

#Pruebo Newton -----
derivg <- function(r) {
  P = 0
  for(t in 1:10) {
    P = P + 2.5 * (-t)*0.5*(1 + 0.5 * r) ^ (-t-1)
  } 
  P = P + 100*0.5*(-10)*(1+0.5*r)^-11
  return(P)
}

Newton6 <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(g(p0)/derivg(p0))
    if (abs(p-p0)<tol){
      print(i) 
      return(p)
      }
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newton6(0.05,10^-6,50)

#TIR 4-------------------------------------------------------------

frentac <- function(r){
  P = -1500000 + 25000*(1-(1+r/12)^-120)/(r/12)
  P
}

derivr <- function(r){
  P = 300000*(10*r*(1+r/12)^-121 - (1-(1+r/12)^-120))/r^2
  P
}

Newtonrenta <- function(p0,tol,N){
  i=1
  while (i<=N){
    p <- p0-(frentac(p0)/derivr(p0)) #utilizo g(r) como la funcion(r)/derivf(r)
    if (abs(p-p0)<tol){
      print(i) 
      return(p)}
    i = i + 1
    p0 <- p}
  return(paste("El método falló luego de", N, "iteraciones"))
}

Newtonrenta(0.1,10^-6,600)

#---------------------------------------------------