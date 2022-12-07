

##########################################################
#         Demystifying statistics & programming in R     #
#            Neil Burns © 2022 - 23                      #
##########################################################


########################################################
#####           1. Setting up your script          #####
########################################################

# use "#" to create comments - do this A LOT!

# run code like this once to install a package
install.packages("gamm4", dependencies=TRUE)

######## Tell them about this, its handy ######

# clear everything in R session memory
rm(list=ls())

# getwd tells us where R is looking
getwd()

# setwd tells R where to look
setwd("type/the/file/path/in/here") #or cut and paste in the sneaky way
getwd()

library('gamm4') # run to make this accessable in the current session

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
set.seed(67)

samps<- 200

perc_cov<- runif(samps, 5, 98)
cor_col<- rep(c('Brown', 'Blue', 'Green'), length.out=samps)

mu<-  0.3*perc_cov + rnorm(samps, 12, 2)
sigma<- runif(samps, 2,8)
    
shark_perH<- rnorm(samps, mean=mu, sd=sigma)

count_shark<- rpois(samps, lambda = perc_cov*(runif(samps, 0.01, 0.2))+ rpois(samps,5))


dat<- data.frame('perc_cov'=as.numeric(perc_cov), 'count_shark'=as.integer(count_shark), 'shark_per_h'=as.numeric(shark_perH), 
                 'cor_col'=as.factor(cor_col))

##############################################################################
############################################################################


head(dat)
str(dat)
summary(dat)

plot(dat$perc_cov, dat$shark_per_h,  xlim=c(0,100), ylim=c(0,50), pch=19, col='darkblue',
     cex=2, xlab='Percentage cover', ylab='Number of sharks (per h)')

#OR
plot(shark_per_h~ perc_cov, data=dat,  xlim=c(0,100), ylim=c(0,50), pch=19, col='firebrick4',
     cex=2, xlab='Percentage cover', ylab='Number of sharks (per h)')


plot(dat$cor_col, dat$shark_per_h)


Order.test <- factor(dat$cor_col, levels=c('Green', 'Brown', 'Blue'), ordered = TRUE)

plot(Order.test, dat$shark_per_h)



######################### End of Section 2 #####################################



########################################################
#####           3. Models and inference            #####
########################################################


?glm

# Eg with "normal data"
 mod1<- glm(shark_per_h~ perc_cov + cor_col + perc_cov:cor_col, 
            data=dat, family=gaussian)
# # Or
# #binomial - use this one for presence absence
# #gaussian - this one for normal data and data with decimals
# #poisson - use this one for counts
 
 mod2<- glm(shark_per_h~ perc_cov + cor_col, data=dat, family=gaussian)
 
 AIC(mod1,mod2)
 summary(mod2)
 
 mod3<- glm(shark_per_h~ perc_cov, data=dat, family=gaussian)
 AIC(mod2,mod3)
 
 summary(mod3)
 
 
 mod_null<- glm(shark_per_h~1, data=dat, family=gaussian)
 
 AIC(mod3, mod_null)
 plot(mod3)




plot(shark_per_h~ perc_cov, data=dat, xlim=c(0,100), ylim=c(0,50), pch=19, col='firebrick4',
     cex=2, xlab='Percentage cover', ylab='shark abundance')

abline(mod3, lwd=2)




##### A better solution with woks always is:

Neils_new_dat<- data.frame(perc_cov= c(0:100))
head(Neils_new_dat)

preds<- predict(mod3, newdata = Neils_new_dat, type='response' )


plot(shark_per_h~ perc_cov, data=dat, xlim=c(0,100), ylim=c(0,50), pch=19, col='darkorchid4',
     cex=2, xlab='Percentage cover', ylab='mean shark numbers')

lines(Neils_new_dat$perc_cov,  preds, lwd=3, col='cadetblue')


#### - later we can explore adding confidence intervals


##########################################################################################
 # What if its wiggly - use a GAM
#########################################################################################

#You need to run this section but you can ignore this bit 
#### or feel free to explore it a bit to see what it is doing 
#### in short...it generates our data for our toy example for us 
#### normally you would gather the data in the field and bring it into R in a .csv file as above #####
set.seed(666)

samps2<- 200

perc_cov2<- runif(samps2, 5, 98)
cor_col2<- rep(c('Brown', 'Blue', 'Green'), length.out=samps2)

mu2<-  0.003*perc_cov2^2+ 0.003*perc_cov2 + rnorm(samps2, 0.12, 0.2)
sigma2<- runif(samps2, 2,8)

shark_perH2<- rnorm(samps2, mean=mu2, sd=sigma2)

dat2<- data.frame('perc_cov2'=as.numeric(perc_cov2), 'shark_per_h2'=as.numeric(shark_perH2), 
                 'cor_col2'=as.factor(cor_col2))

##############################################################################
############################################################################

plot(shark_per_h2~ perc_cov2, data=dat2, xlim=c(0,100),  pch=19, col='darkorchid4',
     cex=2, xlab='Percentage cover', ylab='mean shark numbers')

gam1<- gam(shark_per_h2~ s(perc_cov2), data=dat2, family = gaussian)
plot(gam1, residuals = TRUE, pch=19, cex=2, col = 'darkorchid4', shade = TRUE, lwd=2, rug= FALSE )  # check with ?plot.gam - you can add your data and all sorts in to make this look nice
summary(gam1)
gam.check(gam1)

