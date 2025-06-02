
### Simulation Exercises

### Simulation Exercises 1A

#1
rm(list=ls())
n<-10000
v<-rnorm(n)
x<-rnorm(n,v)
y<-rnorm(n,x-v,1)

d.full<-data.frame(y,v,x)


#2 

mod.full<-lm(y~x+v, data=d.full)
summary(mod.full)

mod1.full<-lm(y~x, data=d.full)
summary(mod1.full)

mean(d.full$x, na.rm=TRUE)


#3

d.mcar<-d.full
d.mcar$r<-rbinom(n,1,0.5)
d.mcar$x<-with(d.mcar, ifelse(d.mcar$r==0, NA, d.mcar$x))

mod.mcar<-lm(y~x+v, data=d.mcar)
summary(mod.mcar)

mod1.mcar<-lm(y~x, data=d.mcar)
summary(mod1.mcar)

mean(d.mcar$x, na.rm=TRUE)


#4

d.mar.v<-d.full
p.rv<-exp(v)/(1+exp(v))
p.rv<-ifelse(v<0,0.05,0.95)  ## Another one that shows more bias
summary(p.rv)
d.mar.v$r<-rbinom(n,1,p.rv)
d.mar.v$x<-with(d.mar.v, ifelse(d.mar.v$r==0, NA, d.mar.v$x))

mod.mar.v<-lm(y~x+v, data=d.mar.v)
summary(mod.mar.v)

mod1.mar.v<-lm(y~x, data=d.mar.v)
summary(mod1.mar.v)

mean(d.mar.v$x, na.rm=TRUE)


#5

d.mar.x<-d.full
p.rx<-exp(x)/(1+exp(x))
summary(p.rx)
d.mar.x$r<-rbinom(n,1,p.rx)
d.mar.x$x<-with(d.mar.x, ifelse(d.mar.x$r==0, NA, d.mar.x$x))

mod.mar.x<-lm(y~x+v, data=d.mar.x)
summary(mod.mar.x)

mod1.mar.x<-lm(y~x, data=d.mar.x)
summary(mod1.mar.x)

mean(d.mar.x$x, na.rm=TRUE)




#6

d.mar.y<-d.full
p.ry<-exp(y)/(1+exp(y))
summary(p.ry)
d.mar.y$r<-rbinom(n,1,p.ry)
d.mar.y$x<-with(d.mar.y, ifelse(d.mar.y$r==0, NA, d.mar.y$x))

mod.mar.y<-lm(y~x+v, data=d.mar.y)
summary(mod.mar.y)

mod1.mar.y<-lm(y~x, data=d.mar.y)
summary(mod1.mar.y)

mean(d.mar.y$x, na.rm=TRUE)




### Simulation Exercises 1B

#1. Mean imputation 

d.mcar$x1<-with(d.mcar, ifelse(r==0, mean(x, na.rm=TRUE), x))
d.mcar$x1<-with(d.mcar, ifelse(is.na(x), mean(x, na.rm=TRUE), x))

d.mar.v$x1<-with(d.mar.v, ifelse(r==0, mean(x, na.rm=TRUE), x))
d.mar.x$x1<-with(d.mar.x, ifelse(r==0, mean(x, na.rm=TRUE), x))
d.mar.y$x1<-with(d.mar.y, ifelse(r==0, mean(x, na.rm=TRUE), x))

mod.mcar.1<-lm(y~x1+v, data=d.mcar)
mod.mar.v.1<-lm(y~x1+v, data=d.mar.v)
mod.mar.x.1<-lm(y~x1+v, data=d.mar.x)
mod.mar.y.1<-lm(y~x1+v, data=d.mar.y)

summary(mod.mcar.1)
summary(mod.mar.v.1)
summary(mod.mar.x.1)
summary(mod.mar.y.1)


#2. Conditional expectation imputation

fit.x.mcar<-lm(x ~ y + v, data=d.mcar)
pred.x.mcar<-predict(fit.x.mcar, newdata=d.mcar)
d.mcar$x2<-with(d.mcar, ifelse(r==0, pred.x.mcar, x))

fit.x.mar.v<-lm(x ~ y + v, data=d.mar.v)
pred.x.mar.v<-predict(fit.x.mar.v, newdata=d.mar.v)
d.mar.v$x2<-with(d.mar.v, ifelse(r==0, pred.x.mar.v, x))

fit.x.mar.x<-lm(x ~ y + v, data=d.mar.x)
pred.x.mar.x<-predict(fit.x.mar.x, newdata=d.mar.x)
d.mar.x$x2<-with(d.mar.x, ifelse(r==0, pred.x.mar.x, x))

fit.x.mar.y<-lm(x ~ y + v, data=d.mar.y)
pred.x.mar.y<-predict(fit.x.mar.y, newdata=d.mar.y)
d.mar.y$x2<-with(d.mar.y, ifelse(r==0, pred.x.mar.y, x))

mod.mcar.2<-lm(y~x2+v, data=d.mcar)
mod.mar.v.2<-lm(y~x2+v, data=d.mar.v)
mod.mar.x.2<-lm(y~x2+v, data=d.mar.x)
mod.mar.y.2<-lm(y~x2+v, data=d.mar.y)

summary(mod.mcar.2)
summary(mod.mar.v.2)
summary(mod.mar.x.2)
summary(mod.mar.y.2)


#3 Single imputation from fitted distribution

e.mcar<-rnorm(n,0,sd(fit.x.mcar$residuals))
e.mcar2<-sample(fit.x.mcar$residuals,n,replace=TRUE) ## Doing it a different way
d.mcar$x3<-with(d.mcar, ifelse(r==0, pred.x.mcar+e.mcar, x))
d.mcar$x3<-with(d.mcar, ifelse(r==0, pred.x.mcar+e.mcar2,x))

e.mar.v<-rnorm(n,0,sd(fit.x.mar.v$residuals))
d.mar.v$x3<-with(d.mar.v, ifelse(r==0, pred.x.mar.v+e.mar.v, x))

e.mar.x<-rnorm(n,0,sd(fit.x.mar.x$residuals))
d.mar.x$x3<-with(d.mar.x, ifelse(r==0, pred.x.mar.x+e.mar.x, x))

e.mar.y<-rnorm(n,0,sd(fit.x.mar.y$residuals))
d.mar.y$x3<-with(d.mar.y, ifelse(r==0, pred.x.mar.y+e.mar.y, x))

mod.mcar.3<-lm(y~x3+v, data=d.mcar)
mod.mar.v.3<-lm(y~x3+v, data=d.mar.v)
mod.mar.x.3<-lm(y~x3+v, data=d.mar.x)
mod.mar.y.3<-lm(y~x3+v, data=d.mar.y)

summary(mod.mcar.3)
summary(mod.mar.v.3)
summary(mod.mar.x.3)
summary(mod.mar.y.3)
