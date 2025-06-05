

### Simulation Exercises

### Simulation Exercises 1A

#1
rm(list=ls())
n<-1000
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
#p.rv<-ifelse(v<0,0.05,0.95)  ## Another one that shows more bias
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


#4. Missing indicator approach

d.mcar$x4<-with(d.mcar, ifelse(r==0, 0, x))
d.mar.v$x4<-with(d.mar.v, ifelse(r==0, 0, x))
d.mar.x$x4<-with(d.mar.x, ifelse(r==0, 0, x))
d.mar.y$x4<-with(d.mar.y, ifelse(r==0, 0, x))

mod.mcar.4<-lm(y~I(x4*r)+I(1-r)+v, data=d.mcar)
mod.mar.v.4<-lm(y~I(x4*r)+I(1-r)+v, data=d.mar.v)
mod.mar.x.4<-lm(y~I(x4*r)+I(1-r)+v, data=d.mar.x)
mod.mar.y.4<-lm(y~I(x4*r)+I(1-r)+v, data=d.mar.y)

summary(mod.mcar.4)
summary(mod.mar.v.4)
summary(mod.mar.x.4)
summary(mod.mar.y.4)


#5. Hot deck imputation

sd.y<-sd(d.mcar$y)
sd.v<-sd(d.mcar$v)
x.close<-NULL
for (i in 1:n){
  y.comp<-y[i]
  v.comp<-v[i]
  x.close[i]<-with(d.mcar, x[((y-y.comp)/sd.y)^2 + ((v-v.comp)/sd.v)^2==min((((y-y.comp)/sd.y)^2 + ((v-v.comp)/sd.v)^2)[r==1])])
}
d.mcar$x5<-with(d.mcar, ifelse(r==0, x.close, x))
d.mar.v$x5<-with(d.mar.v, ifelse(r==0, x.close, x))
d.mar.x$x5<-with(d.mar.x, ifelse(r==0, x.close, x))
d.mar.y$x5<-with(d.mar.y, ifelse(r==0, x.close, x))

mod.mcar.5<-lm(y~x5+v, data=d.mcar)
mod.mar.v.5<-lm(y~x5+v, data=d.mar.v)
mod.mar.x.5<-lm(y~x5+v, data=d.mar.x)
mod.mar.y.5<-lm(y~x5+v, data=d.mar.y)

summary(mod.mcar.5)
summary(mod.mar.v.5)
summary(mod.mar.x.5)
summary(mod.mar.y.5)



### Simulation Exercises 2A

mod.r.mcar<-glm(r~y+v, family="binomial", data=d.mcar)
mod.r.mar.v<-glm(r~y+v, family="binomial", data=d.mar.v)
mod.r.mar.x<-glm(r~y+v, family="binomial", data=d.mar.x)
mod.r.mar.y<-glm(r~y+v, family="binomial", data=d.mar.y)

pred.prob.mcar<-predict(mod.r.mcar, type="response")
pred.prob.mar.v<-predict(mod.r.mar.v, type="response")
pred.prob.mar.x<-predict(mod.r.mar.x, type="response")
pred.prob.mar.y<-predict(mod.r.mar.y, type="response")

#1-#4
with(d.mcar, sum((r*x/pred.prob.mcar), na.rm=TRUE)/sum((r/pred.prob.mcar), na.rm=TRUE))
with(d.mar.v, sum((r*x/pred.prob.mar.v), na.rm=TRUE)/sum((r/pred.prob.mar.v), na.rm=TRUE))
with(d.mar.x, sum((r*x/pred.prob.mar.x), na.rm=TRUE)/sum((r/pred.prob.mar.x), na.rm=TRUE))
with(d.mar.y, sum((r*x/pred.prob.mar.y), na.rm=TRUE)/sum((r/pred.prob.mar.y), na.rm=TRUE))


### Simulation Exercises 2B

#1-#4
mod.ipw.mcar<-lm(y~v+x, weights=1/pred.prob.mcar, data=d.mcar)
mod.ipw.mar.v<-lm(y~v+x, weights=1/pred.prob.mar.v, data=d.mar.v)
mod.ipw.mar.x<-lm(y~v+x, weights=1/pred.prob.mar.x, data=d.mar.x)
mod.ipw.mar.y<-lm(y~v+x, weights=1/pred.prob.mar.y, data=d.mar.y)

summary(mod.ipw.mcar)
summary(mod.ipw.mar.v)
summary(mod.ipw.mar.x)
summary(mod.ipw.mar.y)


### Simulation Exercises 2C

nboot<-1000
betav.mcar <- betax.mcar <- betav.mar.v<-betax.mar.v<-
  betav.mar.x<-betax.mar.x<-betav.mar.y<-betax.mar.y<-
  betav.full<-betax.full<-NULL
for (i in 1:nboot){
  bootsamp<-sample(1:n, n, replace=TRUE)
  
  d.mcar.b<-d.mcar[bootsamp,]
  d.mar.v.b<-d.mar.v[bootsamp,]
  d.mar.x.b<-d.mar.x[bootsamp,]
  d.mar.y.b<-d.mar.y[bootsamp,]
  d.full.b<-d.full[bootsamp,]
  
  mod.r.mcar.b<-glm(r~y+v, family="binomial", data=d.mcar.b)
  mod.r.mar.v.b<-glm(r~y+v, family="binomial", data=d.mar.v.b)
  mod.r.mar.x.b<-glm(r~y+v, family="binomial", data=d.mar.x.b)
  mod.r.mar.y.b<-glm(r~y+v, family="binomial", data=d.mar.y.b)
  
  pred.prob.mcar.b<-predict(mod.r.mcar.b, type="response")
  pred.prob.mar.v.b<-predict(mod.r.mar.v.b, type="response")
  pred.prob.mar.x.b<-predict(mod.r.mar.x.b, type="response")
  pred.prob.mar.y.b<-predict(mod.r.mar.y.b, type="response")
  
  mod.ipw.mcar.b<-lm(y~v+x, weights=1/pred.prob.mcar.b, data=d.mcar.b)
  mod.ipw.mar.v.b<-lm(y~v+x, weights=1/pred.prob.mar.v.b, data=d.mar.v.b)
  mod.ipw.mar.x.b<-lm(y~v+x, weights=1/pred.prob.mar.x.b, data=d.mar.x.b)
  mod.ipw.mar.y.b<-lm(y~v+x, weights=1/pred.prob.mar.y.b, data=d.mar.y.b)
  mod.full.b<-lm(y~v+x, data=d.full.b)
  
  betav.mcar[i]<-mod.ipw.mcar.b$coeff[2]
  betax.mcar[i]<-mod.ipw.mcar.b$coeff[3]
  betav.mar.v[i]<-mod.ipw.mar.v.b$coeff[2]
  betax.mar.v[i]<-mod.ipw.mar.v.b$coeff[3]
  betav.mar.x[i]<-mod.ipw.mar.x.b$coeff[2]
  betax.mar.x[i]<-mod.ipw.mar.x.b$coeff[3]
  betav.mar.y[i]<-mod.ipw.mar.y.b$coeff[2]
  betax.mar.y[i]<-mod.ipw.mar.y.b$coeff[3]
  betav.full[i]<-mod.full.b$coeff[2]
  betax.full[i]<-mod.full.b$coeff[3]
}

#1
sd(betax.mcar)
sd(betav.mcar)
summary(mod.ipw.mcar)

## 95% CI:
mod.ipw.mcar$coefficients["v"] + c(-1,1)*1.96*sd(betav.mcar)
quantile(betav.mcar, c(.025,.975))
mod.ipw.mcar$coefficients["x"] + c(-1,1)*1.96*sd(betax.mcar)
quantile(betax.mcar, c(.025,.975))


#2
sd(betax.mar.v)
sd(betav.mar.v)
summary(mod.ipw.mar.v)

mod.ipw.mar.v$coefficients["v"] + c(-1,1)*1.96*sd(betav.mar.v)
quantile(betav.mar.v, c(.025,.975))
mod.ipw.mar.v$coefficients["x"] + c(-1,1)*1.96*sd(betax.mar.v)
quantile(betax.mar.v, c(.025,.975))


#3
sd(betax.mar.x)
sd(betav.mar.x)
summary(mod.ipw.mar.x)

#4
sd(betax.mar.y)
sd(betav.mar.y)
summary(mod.ipw.mar.y)

#5
sd(betax.full)
sd(betav.full)
summary(mod.full)







### Simulation Exercises 3A

#1 Multiple imputation estimator by hand under MCAR "Predict"

library(mvtnorm)
n<-500 # sample size
M<- 100 ## number of imputations
beta0 <- 0; beta1=1; beta2=-1 # parameters to simulate data
# function to simulate MCAR data
simulation <- function(n,beta0,beta1,beta2) {
  v<-rnorm(n)
  x<-rnorm(n,v)
  y<-rnorm(n, beta0+beta1*x+beta2*v,1)
  d.full<-data.frame(y,v,x)
  d.mcar<-d.full
  d.mcar$r<-rbinom(n,1,0.5)
  d.mcar$x<-with(d.mcar, ifelse(d.mcar$r==0, NA, d.mcar$x))
  return(list(d.full=d.full, d.mcar=d.mcar))
}

set.seed(321) # set the random seed to reproduce the results
data <- simulation(n,beta0,beta1,beta2) # simulate the data  
d.mcar <- data$d.mcar # MCAR data  
# imputation step
fit.x.mcar<-lm(x ~ y + v, data=d.mcar, subset=(r==1))
betas.mi<-vars.mi<-matrix(NA, nrow=M, ncol=3)
for (i in 1:M){
  d.mi.mcar<-d.mcar
  # Predict method, to calculate the linear predictor for missing records
  d.mi.mcar$x[d.mi.mcar$r==0]<- predict(fit.x.mcar, newdata=subset(d.mcar, r==0))
  # analyze step
  mod.mi.mcar<-lm(y ~ x + v, data=d.mi.mcar)
  betas.mi[i,]<-mod.mi.mcar$coeff
  vars.mi[i,]<-diag(vcov(mod.mi.mcar))
}
# pool results using Rubin's Rule
beta.hat<-colMeans(betas.mi)
se.betas<-sqrt(colMeans(vars.mi) + (1+1/M)*c(var(betas.mi[,1]),var(betas.mi[,2]),var(betas.mi[,3])))

beta.hat
se.betas

# full data
summary(lm(y ~ x + v, data=data$d.full))


### Simulation Exercises 3B "Predict + noise"

fit.x.mcar<-lm(x ~ y + v, data=d.mcar, subset=(r==1))
n0 <- sum(d.mcar$r==0) # number of missing observations
betas.mi<-vars.mi<-matrix(NA, nrow=M, ncol=3)
for (i in 1:M){
  d.mi.mcar<-d.mcar
  # update imputation step
  d.mi.mcar$x[d.mi.mcar$r==0]<- 
    predict(fit.x.mcar, newdata=subset(d.mcar,r==0)) + rnorm(n0, 0, sd=summary(fit.x.mcar)$sigma)
  mod.mi.mcar<-lm(y ~ x + v, data=d.mi.mcar)
  betas.mi[i,]<-mod.mi.mcar$coeff
  vars.mi[i,]<-diag(vcov(mod.mi.mcar))
}
beta.hat<-colMeans(betas.mi)
se.betas<-sqrt(colMeans(vars.mi)+(1+1/M)*c(var(betas.mi[,1]),var(betas.mi[,2]),var(betas.mi[,3])))

beta.hat
se.betas

### Simulation Exercises 3C "Predict + noise + parameter uncertainty"

fit.x.mcar <-lm(x ~ y + v, data=d.mcar, subset=(r==1))
summary(fit.x.mcar)
sqrt(diag(vcov(fit.x.mcar)))

n0 <- sum(d.mcar$r==0) # number of missing observations
betas.mi<-vars.mi<-matrix(NA, nrow=M, ncol=3)
for (i in 1:M){
  d.mi.mcar<-d.mcar
  # update imputation step
  betas.sampled<-rmvnorm(1, fit.x.mcar$coefficients, sigma=vcov(fit.x.mcar))
  sigma2.sampled<-rchisq(1, fit.x.mcar$df.residual)*(summary(fit.x.mcar)$sigma^2)/fit.x.mcar$df.residual
  d.mi.mcar$x[d.mi.mcar$r==0]<- with(subset(d.mi.mcar,r==0), betas.sampled[1,"(Intercept)"]+
                                       betas.sampled[1,"y"]*y+betas.sampled[1,"v"]*v) + rnorm(n0, 0, sd=sqrt(sigma2.sampled))
  mod.mi.mcar<-lm(y ~ x + v, data=d.mi.mcar)
  betas.mi[i,]<-mod.mi.mcar$coeff
  vars.mi[i,]<-diag(vcov(mod.mi.mcar))
}
beta.hat<-colMeans(betas.mi)
se.betas<-sqrt(colMeans(vars.mi) + (1+1/M)*c(var(betas.mi[,1]),var(betas.mi[,2]),var(betas.mi[,3])))

beta.hat
se.betas

#3 What happens if you do not include the outcome in the imputation model?

fit.x.mcar <-lm(x ~ v, data=d.mcar, subset=(r==1))
summary(fit.x.mcar)
sqrt(diag(vcov(fit.x.mcar)))

n0 <- sum(d.mcar$r==0) # number of missing observations
betas.mi<-vars.mi<-matrix(NA, nrow=M, ncol=3)
for (i in 1:M){
  d.mi.mcar<-d.mcar
  # update imputation step
  betas.sampled<-rmvnorm(1, fit.x.mcar$coefficients, sigma=vcov(fit.x.mcar))
  sigma2.sampled<-rchisq(1, fit.x.mcar$df.residual)*(summary(fit.x.mcar)$sigma^2)/fit.x.mcar$df.residual
  d.mi.mcar$x[d.mi.mcar$r==0]<- with(subset(d.mi.mcar,r==0), betas.sampled[1,"(Intercept)"]+
                                       betas.sampled[1,"v"]*v) + rnorm(n0, 0, sd=sqrt(sigma2.sampled))
  mod.mi.mcar<-lm(y ~ x + v, data=d.mi.mcar)
  betas.mi[i,]<-mod.mi.mcar$coeff
  vars.mi[i,]<-diag(vcov(mod.mi.mcar))
}
beta.hat<-colMeans(betas.mi)
se.betas<-sqrt(colMeans(vars.mi) + (1+1/M)*c(var(betas.mi[,1]),var(betas.mi[,2]),var(betas.mi[,3])))

beta.hat
se.betas

### Simulation Exercises 3D "Predict + noise + parameter uncertainty" but using mice

library(mice)
d.mcar.imp <- subset(d.mcar, select=c("y", "x", "v"))
# imputation step
d.mcar.mice <- mice(d.mcar.imp, m = 100, method = "norm", seed = 2025, print=F)
# analysis step
fit.d.mcar <- with(d.mcar.mice, lm(y ~ x + v))
summary(pool(fit.d.mcar))


### Simulation Exercises 3E PMM in mice

d.mcar.imp <- subset(d.mcar, select=c("y", "x", "v"))
# imputation step
d.mcar.mice <- mice(d.mcar.imp, m = 100, method = "pmm", seed = 2025, print=F)
# analysis step
fit.d.mcar <- with(d.mcar.mice, lm(y ~ x + v))
summary(pool(fit.d.mcar))

