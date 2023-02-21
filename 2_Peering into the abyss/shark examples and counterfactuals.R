

#######################################################################################
#         Don't stare into the abyss - understanding what our models tell us          #
#            Neil Burns © 2022 - 23                                                   #
#######################################################################################


######################################################
#####           1. Script set up                 #####
######################################################

# clear everything in R session memory
rm(list=ls())

#library('gamm4') 
#library('lme4')
library('MASS')
#library('rethinking')

########################################################
#####           2. Example 1, Sharks and coral      ####
########################################################

#### normally you would gather the data in the field and bring it into R in a .csv file as above #####
### developing modeled, toy examples like the one below means we know exactly what we should get back
### so we can test our models to see if they are doing what we expect them to do


set.seed(67) ## makes sure we all generate the same set of numbers every time

samps<- 201

perc_cov<- runif(samps, 5, 98)
cor_col<- rep(c('Brown', 'Blue', 'Green'), length.out=samps)

shark_perH<- rnorm(samps, mean=perc_cov*(runif(1, 0.1, 0.3)), sd=rnorm(1, 0.7, 0.01))+ rpois(1,5)

count_shark<- rpois(samps, lambda = perc_cov*(runif(1, 0.01, 0.2))+ rpois(1,5))


dat<- data.frame('perc_cov'=as.numeric(perc_cov), 'count_shark'=as.integer(count_shark), 'shark_per_h'=as.numeric(shark_perH), 
                 'cor_col'=as.factor(cor_col))



####################################
### Understand your data        ####
####################################  

head(dat)
str(dat)
summary(dat)

plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col='dodgerblue3',
     cex=2, xlab='Percentage cover', ylab='count_shark')

plot(dat$cor_col, dat$count_shark, xlab='Coral colour', ylab='count_shark', col=c('cadetblue','brown','forestgreen'))

cols=c('cadetblue3','brown','forestgreen')
plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col=cols[dat$cor_col],
     cex=2, xlab='Percentage cover', ylab='count_shark')


##################################
### Model selection           ####
################################## 

mod1.p<- glm(count_shark ~ perc_cov + cor_col + perc_cov:cor_col, 
           data=dat, family=poisson(link = "log"))

mod2.p<- glm(count_shark~ perc_cov + cor_col, data=dat, family=poisson(link = "log")) 
### NB be explicit about link function - for teaching bit below

AIC(mod1.p, mod2.p)

summary(mod2.p)

mod3.p<- glm(count_shark~ perc_cov, data=dat, family=poisson(link = "log"))

AIC(mod2.p,mod3.p)

summary(mod3.p)


mod_null.p<- glm(count_shark ~ 1, data=dat, family=poisson(link = "log"))

AIC(mod3.p, mod_null.p)


###################################
### Understand the model       ####
################################### 



##### if it is a Gaussian
mod3<- glm(shark_per_h~ perc_cov, data=dat, family=gaussian)


plot(shark_per_h~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,25), pch=19, col='firebrick4',
     cex=2, xlab='Percentage cover', ylab='shark abundance')

abline(mod3, lwd=2)



#### but its Poisson
plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col='dodgerblue3',
     cex=2, xlab='Percentage cover', ylab='shark abundance')
summary(mod3.p)
coef(mod3.p)

abline(mod3.p, lwd=2, untf=F)

intercept<- (coef(mod3.p)[[1]])
slope<- (coef(mod3.p)[[2]])

abline(a=intercept, b=slope, lwd=2)


intercept<- exp(coef(mod3.p)[[1]])
slope<- exp(coef(mod3.p)[[2]])


abline(a=intercept, b=slope, lwd=2, untf=T)



plot(log(count_shark)~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,5), pch=19, col='darkorchid4',
     cex=2, xlab='Percentage cover', ylab='Log shark abundance')

abline(mod3.p, lwd=2, untf=F)

##### A better solution which always works is to predict from the model, within the data range

Neils_new_dat<- data.frame(perc_cov= c(0:100))
head(Neils_new_dat)

preds<- predict(mod3.p, newdata = Neils_new_dat, type='response' )


plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col='dodgerblue3',
     cex=2, xlab='Percentage cover', ylab='mean shark numbers')

lines(Neils_new_dat$perc_cov,  preds, lwd=4, col='cadetblue3')

                         


###############################################################
#####           3. Example 2, Sharks and coral, again      ####
###############################################################

set.seed(666)

samps.b<- 201/3
perc_cov<-NA
perc_cov1<- runif(samps.b, 5, 45)
perc_cov2<- runif(samps.b, 5, 98)
perc_cov3<- runif(samps.b, 42, 98)
perc_cov<- c(perc_cov1,perc_cov2,perc_cov3)


cor_col<- rep(c('Brown', 'Blue', 'Green'), each=samps.b)
sharks<- NA
for(i in 1:samps.b){
    sharks[i]<- rpois(1, lambda = perc_cov[i]*(runif(1, 0.09, 0.1)))
}
for(j in 68:(69+samps.b)){
    sharks[j]<- rpois(1, lambda = perc_cov[j]*(runif(1, 2, 2.51)))
}
for(k in (69+samps.b):201){
    sharks[k]<- rpois(1, lambda = perc_cov[k]*(runif(1, 1, 1.1)))
}

dat2 <- data.frame(perc_cov, cor_col=as.factor(cor_col), sharks=sharks)


####################################
### Understand your data 2      ####
####################################  

head(dat2)
str(dat2)


plot(sharks~ perc_cov, data=dat2,  xlim=c(0,100), ylim=c(0,250), pch=19, col='dodgerblue3',
     cex=2, xlab='Percentage cover', ylab='Shark abundance')

plot(dat2$cor_col, dat2$sharks, xlab='Coral colour', ylab='Shark abundance', col=c('cadetblue','brown','forestgreen'))

cols=c('cadetblue3','brown','forestgreen')
plot(sharks~ perc_cov, data=dat2,  xlim=c(0,100), ylim=c(0,250), pch=19, col=cols[dat2$cor_col],
     cex=2, xlab='Percentage cover', ylab='Shark abundance')


##################################
### Model selection 2         ####
################################## 



mod1.p<- glm(sharks ~ perc_cov + cor_col + perc_cov:cor_col, 
             data=dat2, family=poisson)

mod2.p<- glm(sharks~ perc_cov + cor_col, data=dat2, family=poisson)

AIC(mod1.p, mod2.p)

summary(mod1.p)

    #########################################
    # Discuss counterfactuals for factors  ##
    #########################################


#################################################
### Use prediction to understand the model   ####
################################################# 


Neils_new_dat2<- data.frame(expand.grid(cor_col=levels(dat$cor_col), perc_cov= c(0:100)))

head(Neils_new_dat2)

preds.eg<- predict(mod1.p, newdata = Neils_new_dat2, type='response' )
head(preds.eg)

################ adding error/confidence intervals ####################

preds<- data.frame(predict(mod1.p, newdata = Neils_new_dat2, type='response', se.fit = TRUE ))
head(preds)

# make upper and lower CI
up<- preds$fit+(preds$se.fit*1.96)
lo<- preds$fit-(preds$se.fit*1.96)


## attach all the data - makes it easier to do graphing later
pred.dat<- cbind(preds, up, lo, Neils_new_dat2)
head(pred.dat)


###################################################################################################################
#### Counterfactual effect of coral cover on shark abundance in the three different coral colour scenarios   ######
###################################################################################################################


blues<- subset(pred.dat, pred.dat$cor_col=='Blue')
browns<- subset(pred.dat, pred.dat$cor_col=='Brown')
greens<- subset(pred.dat, pred.dat$cor_col=='Green')


plot(fit~perc_cov, data=blues, type='n', xlim=c(0,100), ylim=c(0,250), ylab='Counterfactual shark abundance', xlab= 'Manipulated coral percentage cover')

lines(fit~perc_cov, data=blues, type='l', col='cadetblue3', lwd=3)
lines(up~perc_cov, data=blues, type='l', col='cadetblue3', lwd=1.5, lty=2)
lines(lo~perc_cov, data=blues, type='l', col='cadetblue3', lwd=1.5, lty=2)

lines(fit~perc_cov, data=browns, type='l', col='brown', lwd=3)
lines(up~perc_cov, data=browns, type='l', col='brown', lwd=1.5, lty=2)
lines(lo~perc_cov, data=browns, type='l', col='brown', lwd=1.5, lty=2)

lines(fit~perc_cov, data=greens, type='l', col='forestgreen', lwd=3)
lines(up~perc_cov, data=greens, type='l', col='forestgreen', lwd=1.5, lty=2)
lines(lo~perc_cov, data=greens, type='l', col='forestgreen', lwd=1.5, lty=2)
head(dat2)

cols=c('cadetblue3','brown','forestgreen')
points(sharks~perc_cov, data=dat2, col=cols[as.factor(dat2$cor_col)], pch=19)


                        #########################################
                        # Discuss EG 3 and counterfactuals  #####
                        #########################################



##################################################
######### 4. Example 3. Shark survival   #########
##################################################
set.seed(8)
## Model survivorship among different lengths of sharks which occur at different densities
co.m<- matrix(data=c(0.3697,0.1002, 0.2002,0.849), nrow = 2, ncol = 2)
n=200
s.survival<- data.frame(mvrnorm(n, mu= c(4,8), Sigma=co.m))
names(s.survival)<- c('density', 'length')

surv<- (s.survival$density*(1/s.survival$length))/max(s.survival$density*(1/s.survival$length))
s.survival<- cbind(s.survival, surv)


##############################
### Understand the data  #####
##############################

head(s.survival)

plot(surv~length, data=s.survival,ylim=c(0,1.2), col='darkorchid4', pch=19, cex=1.5, ylab=c('Shark survival'), xlab=c('Shark density'))
plot(surv~density,data=s.survival, ylim=c(0,1.2),col='dodgerblue4', pch=19, cex=1.5, ylab=c('Shark survival'), xlab=c('Shark length'))


###################################################
### Fit the model and make predictions        #####
###################################################


modA<- glm(surv~length+density+ length:density, data=s.survival, family=gaussian) ## there are some clear problems in the residuals but we will run with this for the sake of the example

summary(modA)

modB<- glm(surv~length+density, data=s.survival, family=gaussian)
AIC(modA, modB)

## predictions

Neils_new_dat3<- data.frame(expand.grid(length=(5:10), density = c(1:6)))
head(Neils_new_dat3)

preds<- data.frame(predict(modA, newdata = Neils_new_dat3, type='response', se.fit = TRUE ))

head(preds)

# make upper and lower CI
up<- preds$fit+(preds$se.fit*1.96)
lo<- preds$fit-(preds$se.fit*1.96)


## attach all the data - makes it easier to do graphing later
pred.dat<- cbind(preds, up, lo, Neils_new_dat3)
head(pred.dat)


###############################################################
### Counterfactuals to understand the combined, interacting ###
### effects of length and density on shark survival         ###
###############################################################

# makes sense to explore survival for different sized sharks at mid, high and low densities
# and to explore the effect of density on small or big sharks


####### effect of length on survival, holding densities at mid, low and high, values

at.mid.dens<- subset(pred.dat, pred.dat$density== 4)
at.lo.dens<- subset(pred.dat, pred.dat$density== 3)
at.hi.dens<- subset(pred.dat, pred.dat$density== 5)


plot(fit~length, data=at.mid.dens, type='n', xlim=c(5,10), ylim=c(0.3,1.1), ylab='Counterfactual shark survival',
     xlab= 'Manipulated shark length')
points(surv~length, data=s.survival, col='grey45', pch=19)

lines(fit~length, data=at.mid.dens, col='cadetblue3', lwd=3)
lines(up~length, data=at.mid.dens, type='l', col='cadetblue3', lwd=1.5, lty=2)
lines(lo~length, data=at.mid.dens, type='l', col='cadetblue3', lwd=1.5, lty=2)


lines(fit~length, data=at.hi.dens, col='cadetblue4', lwd=3)
lines(up~length, data=at.hi.dens, type='l', col='cadetblue4', lwd=1.5, lty=2)
lines(lo~length, data=at.hi.dens, type='l', col='cadetblue4', lwd=1.5, lty=2)

lines(fit~length, data=at.lo.dens, col='cadetblue2', lwd=3)
lines(up~length, data=at.lo.dens, type='l', col='cadetblue2', lwd=1.5, lty=2)
lines(lo~length, data=at.lo.dens, type='l', col='cadetblue2', lwd=1.5, lty=2)



#################### effects of density, holding length to small and large ################

at.short<- subset(pred.dat, pred.dat$length== 7)
at.long<- subset(pred.dat, pred.dat$length== 9)


plot(fit~density, data=at.short, type='n', xlim=c(1.5,6), ylim=c(0.3,1.0), ylab='Counterfactual shark survival',
     xlab= 'Manipulated shark density')
points(surv~density, data=s.survival, col='grey45', pch=19)

lines(fit~density, data=at.short, col='firebrick1', lwd=3)
lines(up~density, data=at.short, type='l', col='firebrick1', lwd=1.5, lty=2)
lines(lo~density, data=at.short, type='l', col='firebrick1', lwd=1.5, lty=2)


lines(fit~density, data=at.long, col='firebrick', lwd=3)
lines(up~density, data=at.long, type='l', col='firebrick', lwd=1.5, lty=2)
lines(lo~density, data=at.long, type='l', col='firebrick', lwd=1.5, lty=2)
