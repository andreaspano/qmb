

png(filename = "/home/enrico/projects/QUANTIDE/corsi/latex/statistica/images/regrPuntoLeva.png", width = 1500, height = 1000,res=200)
x=seq(from=1,to=49)
y=2+2*x+rnorm(49,sd=20)

lm1=lm(y~x)

x=c(x,200)
y=c(y,50)

lm2=lm(y~x)

pchv=rep(1,50)
pchv[50]=19
plot(x,y,pch=pchv,col="blue",main="Effetto di un punto leva sulla stima di regressione")
abline(coef=coef(lm1),col="red")
abline(coef=coef(lm2),col="green")
legend("bottom", c(paste("Stima senza punto leva: ",round(x=coef(lm1)[1],digits=2),"+",round(x=coef(lm1)[2],digits=2),"* x"), paste("Stima con punto leva:    ",round(x=coef(lm2)[1],digits=2),"+",round(x=coef(lm2)[2],digits=2),"* x")), col = c("red","green"),
       lty = c(1,1), 
       merge = TRUE, bg = 'gray90')

dev.off()


summary(influence.measures(lm2))

plot(cooks.distance(lm2))

cooks.distance(lm2)

hatvalues(lm2)


# altezza e peso. differenza di intercetta
#maschi
x1=140:200
y1=50-350/3+5/6*x1+rnorm(61,sd=3)
lm1=lm(y1~x1)
#femmine
x2=140:200
y2=40-350/3+5/6*x1+rnorm(61,sd=3)
lm2=lm(y2~x2)

png(filename = "/home/enrico/projects/QUANTIDE/corsi/latex/statistica/images/regrIntercettaDiff.png", width = 1200, height = 1000,res=200)

plot(x1,y1,pch=19,col="blue",xlab="Altezza (cm)",ylab="Peso (Kg)", main="Altezza Vs. Peso per Maschi e Femmine (int)")
abline(coef=coef(lm1),col="blue")

points(x2,y2,pch=17,col="pink")
abline(coef=coef(lm2),col="pink")

legend("bottomright", c("Maschi", "Femmine"), col = c("blue","pink"),
       lty = c(1,1), pch=c(19,17),
       merge = TRUE, bg = 'gray90')
dev.off()

# altezza e peso. differenza di coefficiente angolare
#maschi
x1=140:200
y1=50-350/3+5/6*x1+rnorm(61,sd=3)
lm1=lm(y1~x1)
#femmine
x2=140:200
y2=50-350/3+3/6*x1+rnorm(61,sd=3)
lm2=lm(y2~x2)

png(filename = "/home/enrico/projects/QUANTIDE/corsi/latex/statistica/images/regrCADiff.png", width = 1200, height = 1000,res=200)

plot(x1,y1,pch=19,col="blue",xlab="Altezza (cm)",ylab="Peso (Kg)", main="Altezza Vs. Peso per Maschi e Femmine (C.A.)",ylim=c(10,103))
abline(coef=coef(lm1),col="blue")

points(x2,y2,pch=17,col="pink")
abline(coef=coef(lm2),col="pink")

legend("topleft", c("Maschi", "Femmine"), col = c("blue","pink"),
       lty = c(1,1), pch=c(19,17),
       merge = TRUE, bg = 'gray90')
dev.off()


# altezza e peso. differenza totale
#maschi
x1=140:200
y1=70-350/3+5/6*x1+rnorm(61,sd=3)
lm1=lm(y1~x1)
#femmine
x2=140:200
y2=20-350/3+7/6*x1+rnorm(61,sd=3)
lm2=lm(y2~x2)

png(filename = "/home/enrico/projects/QUANTIDE/corsi/latex/statistica/images/regrAllDiff.png", width = 1200, height = 1000,res=200)

plot(x1,y1,pch=19,col="blue",xlab="Altezza (cm)",ylab="Peso (Kg)", main="Altezza Vs. Peso per Maschi e Femmine (All)")
abline(coef=coef(lm1),col="blue")

points(x2,y2,pch=17,col="pink")
abline(coef=coef(lm2),col="pink")

legend("topleft", c("Maschi", "Femmine"), col = c("blue","pink"),
       lty = c(1,1), pch=c(19,17),
       merge = TRUE, bg = 'gray90')
dev.off()
