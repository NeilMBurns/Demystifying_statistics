

##########################################################
#         Demystifying statistics & programming in R     #
#                Neil Burns © 2024 - 25                  #
##########################################################


########################################################
#####           1. Setting up your script          #####
########################################################

# use "#" to create comments - do this A LOT!

# run code like this once to install a package
install.packages("gamm4", dependencies=TRUE)
library('MASS') # run to make this accessible in the current session
######## Tell them about this, its handy ######

# clear everything in R session memory
rm(list=ls())

# getwd tells us where R is looking
getwd()

# setwd tells R where to look
setwd("type/the/file/path/in/here") #or cut and paste in the sneaky way
getwd()


## import some data
my_cool_name_not_too_long_as_its_a_pain<- read.csv("the_name_of_the_data.csv")

######################### End of Section 1 #####################################



########################################################
#####           2. Understanding our data          #####
########################################################

#You need to run this section but you can ignore this bit 
#### or feel free to explore it a bit to see what it is doing 
#### in short...it generates our data for our toy example for us 
#### normally you would gather the data in the field and bring it into R in a .csv file as above #####
set.seed(67) # this means we get the same set of numbers each time

samps<- 200 # the number of samples e.g. n = 200 in our study

perc_cov<- runif(samps, 5, 98) # generates the percentage cover
cor_col<- rep(c('Brown', 'Blue', 'Green'), length.out=samps) # this is our list of 

# Simulate parameters
alpha <- 0.5
beta <- 0.25
sigma <- 2* rbeta(1, shape1 = 5, shape2 = 2)

# Generate the mean response
mu <- alpha + (beta * perc_cov)

# Simulate observed data from likelihood
shark_perH <- rnorm(samps, mu, sigma)
count_shark <- rpois(samps, lambda = mu)
# Make sure no negative abundance
shark_perH[shark_perH < 0] <- 0



dat<- data.frame('perc_cov'=as.numeric(perc_cov), 'count_shark'=as.integer(count_shark), 'shark_per_h'=as.numeric(shark_perH), 
                 'cor_col'=as.factor(cor_col))

##############################################################################
############################################################################


head(dat)
str(dat)
summary(dat)

hist(dat$perc_cov)




plot(count_shark ~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col='firebrick4',
     cex=2, xlab='Percentage cover', ylab='count_shark')



plot(dat$cor_col, dat$count_shark)

#### It is useful to order factors to 
Order.test <- factor(dat$cor_col, levels=c('Green', 'Brown', 'Blue'), ordered = TRUE)

plot(Order.test, dat$count_shark)

plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col=c('forestgreen', 'brown', 'steelblue')[dat$cor_col],
     cex=2, xlab='Percentage cover', ylab='count_shark')

######################### End of Section 2 #####################################





########################################################
#####           3. Model selection                 #####
########################################################

head(dat)

plot(shark_per_h ~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col=c('forestgreen', 'brown', 'steelblue')[dat$cor_col],
     cex=2, xlab='Percentage cover', ylab='count_shark')

#?glm


# Eg with "normal error structure"
mod1<- glm(shark_per_h ~ perc_cov + cor_col + perc_cov:cor_col,
           data=dat, family=gaussian)
# Or
#binomial
#gaussian
#poisson

mod2<- glm(shark_per_h ~ perc_cov + cor_col, data=dat, family=gaussian)

AIC(mod1,mod2)
summary(mod2)

mod3<- glm(shark_per_h ~ perc_cov, data=dat, family=gaussian)
AIC(mod2,mod3)

summary(mod3)


mod_null<- glm(shark_per_h ~ 1, data=dat, family=gaussian)

AIC(mod3, mod_null)
plot(mod3, which =2)




# Eg with counts data ##################################################
mod1.p<- glm(count_shark ~ perc_cov + cor_col + perc_cov:cor_col, 
           data=dat, family=poisson(link = "log"))



mod2.p<- glm(count_shark ~ perc_cov + cor_col, data=dat, family=poisson(link = "log"))

AIC(mod1.p, mod2.p)

summary(mod1.p)
# 


plot(mod1.p, which =2)





########################################################
#####           4. Model inference                 #####
########################################################



##### if it is a Gaussian
mod3<- glm(shark_per_h~ perc_cov, data=dat, family=gaussian)
summary(mod3)

plot(shark_per_h~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col='firebrick4',
     cex=2, xlab='Percentage cover', ylab='shark abundance')

abline(mod3, lwd=2)

#### but its Poisson
plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col=c('forestgreen', 'brown', 'steelblue')[dat$cor_col],
     cex=2, xlab='Percentage cover', ylab='shark abundance')


mod1.p<- glm(count_shark ~ perc_cov + cor_col + perc_cov:cor_col, 
             data=dat, family=poisson(link = "log"))


abline(mod1.p, lwd=2)



##### A better solution which always works is:

Neils_new_dat <- data.frame(
  expand.grid(perc_cov= c(0:100), cor_col= levels(dat$cor_col)
              ))


head(Neils_new_dat)

preds<- predict(mod1.p, newdata = Neils_new_dat, type='response' )
pred.dat<- cbind(Neils_new_dat, preds)

head(pred.dat)


plot(count_shark~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,30), pch=19, col='black',
     cex=2, xlab='Percentage cover', ylab='Number of Sharks')

line_blue <- subset(pred.dat, pred.dat$cor_col=='Blue')
line_brown <- subset(pred.dat, pred.dat$cor_col=='Brown')
line_green <- subset(pred.dat, pred.dat$cor_col=='Green')

points(count_shark~ perc_cov, data=dat, pch=19, col=c('forestgreen', 'brown', 'steelblue')[dat$cor_col],
     cex=1.5)


lines(preds ~ perc_cov, data=line_blue, lwd=5, col='steelblue')
lines(preds ~ perc_cov, data=line_brown, lwd=5, col='brown')
lines(preds ~ perc_cov, data=line_green, lwd=5, col='forestgreen')



##################################################
######### 4. Example 2. Shark survival   #########
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

plot(surv~length, data=s.survival,ylim=c(0,1), col='darkorchid4', pch=19, cex=1.5, ylab=c('Survival'), xlab=c('Shark length'))
plot(surv~density,data=s.survival, ylim=c(0,1),col='dodgerblue4', pch=19, cex=1.5, ylab=c('Survival'), xlab=c('Shark density'))


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


plot(fit~length, data=at.mid.dens, type='n', xlim=c(5,10), ylim=c(0.3,1.1), ylab='Counterfactual survival',
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


plot(fit~density, data=at.short, type='n', xlim=c(1.5,6), ylim=c(0.3,1.0), ylab='Counterfactual survival',
     xlab= 'Manipulated shark density')
points(surv~density, data=s.survival, col='grey45', pch=19)

lines(fit~density, data=at.short, col='firebrick1', lwd=3)
lines(up~density, data=at.short, type='l', col='firebrick1', lwd=1.5, lty=2)
lines(lo~density, data=at.short, type='l', col='firebrick1', lwd=1.5, lty=2)


lines(fit~density, data=at.long, col='firebrick', lwd=3)
lines(up~density, data=at.long, type='l', col='firebrick', lwd=1.5, lty=2)
lines(lo~density, data=at.long, type='l', col='firebrick', lwd=1.5, lty=2)

