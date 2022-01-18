#########################################################
#        Demystifying statistics & programming in R     #
#            Neil Burns © 2021-2022                     #
#########################################################

########################################################
#####           3. Simulated data examples         #####
########################################################




rm(list=ls()) ######## run this to clear all data and values from R's memory. Its good practice to do this each time you run code.
# It will avoid accidentally using a variable from a previous run accidentally

                          ###############################################
                          #### Data set 1 - Model selection practice ####
                          ###############################################


### In the following set of experiments experiment we were interested in the behavioural responses
# of the Montane unicorn (you can read about their distribution here - https://doi.org/10.2307/3545216)
# to a frightening stimulus. We speculate that their mass and the type of diet the consume may affect the level
# of response. Mass is measured in kg and is similar to other equids (ie 400 - 700 kg). One of two diets was
# fed to randomly selected animals, one high in starch and the other high in fibre.

## Experiment 1 measured how bold the animals were when confronted with a sudden loud sound as the stimulus

## The following code specifies our sample number (ie the number of unicorns) and then generates the data. 
samp<- 60
mass<- rnorm(n = samp, mean = c(530, 630) , sd = 75)
diet = as.factor(rep(c('hi-starch', 'hi-fibre'), length.out = samp))
boldness<-  rnorm(samp, 1100, 4) + rnorm(n = samp, mean = mass, sd = 14)+ rnorm(samp, 0, 40)
behave_data<- data.frame(mass, diet, boldness)

## You can view the data frame with:
head(behave_data)
str(behave_data)
summary(behave_data)
## Our first step should always be to visually examine the data with some plots...with something like:
plot(boldness~mass, data=behave_data, pch=19, col='black')
plot(boldness ~ diet, data=behave_data)
plot(boldness~mass, data=behave_data, pch=19, col=c('cornflowerblue', 'firebrick')[behave_data$diet])


## Using the description, data, plots and your knowledge/intuition build a full linear model and then use
# backward stepwise model selection to arrive at an optimal model structure.
# You can compare models using AIC.
# (we could also use BIC or log likelihood ratio tests - they achieve the same thing)












## remember to look at residual plots for model validation
plot(modXXXX, 2)


### You should be able to describe the relationship suggested by the model simply by looking at the summary table
summary(modXXXXX)




                      ###############################################
                      #### Data set 2 - More complex models      ####
                      ###############################################

### This is the full code used in the worked example

###### As before clear the memory of previous data and variables
rm(list=ls())


## Experiment 2 measured how long it took animals to engage in social interaction again
# after the sudden loud sound stimulus

## The following code generates the number of samples and the explanatory variables we measured (diet and mass)
samp=200
set.seed(16777) # this just ensures we all get the same set of numbers during the class
masses<- rnorm(n = samp, mean = 550, sd = 75)
diet <- as.factor(rep(c('hi-fibre', 'hi-starch'), length.out=samp))
diet.num = as.numeric(as.factor(rep(c('hi-fibre', 'hi-starch'), length.out = samp)))
dat<- data.frame('mass' = masses, 'diet'= diet, 'diet.num' = diet.num) # it is stored in the "dat" data frame

## The following code develops the modeled relationships between the explanatory variables and
# the behavioral response variable , "soc.behav," which is a timed (in ms) behavioral response to the stimulus
a=  rnorm(samp, 1100, 4)
b= rnorm(samp, 3.5, 0.4)
b2=  rnorm(samp, 1.5, 0.2)
b3= -1.9
err=rnorm(samp, 0, 250) 
soc.behav<- a + b*dat$mass + b2*dat$diet.num +  b3*dat$diet.num*dat$mass + err
dat<- cbind(dat, soc.behav)

## You can view the data frame with:
head(dat)
str(dat)
summary(dat)
## and plot the data with something like
plot(soc.behav~mass, data=dat, col=c('black'), pch=19, ylim=c(0,3100), xlim=c(300,775))
plot(soc.behav~diet, data=dat)
## It might be useful to include our factor by colouring the points dependent on diet 
plot(soc.behav~mass, data=dat, col=c('cornflowerblue', 'firebrick')[dat$diet], pch=19, ylim=c(0,3100), xlim=c(300,775))



mod1<- lm(soc.behav~ mass+ diet  + mass:diet, data= dat)


mod2<- lm(soc.behav~ mass+ diet, data= dat)
AIC(mod1, mod2)

summary(mod1)
### this time looking at the summary table does not shed much light on what is going on.
# Because our diet variable is relatively simple we can take some time and work it out...but
# it is much easier to use model predictions to explore (and help validate) what our model says.


new_dat<- data.frame(expand.grid(mass=seq(350, 750), diet=as.factor(c('hi-fibre', 'hi-starch'))))



preds<- data.frame(predict(mod1, newdata = new_dat, se.fit = TRUE, type = 'response'))
up<- preds$fit+ (1.96*preds$se.fit)
lo<- preds$fit- (1.96*preds$se.fit)
preds<- cbind(new_dat, preds, up, lo)

diet1<- subset(preds, preds$diet=="hi-fibre")
diet2<- subset(preds, preds$diet=="hi-starch")

lines(fit~mass, data=diet1, lwd=4, col='dodgerblue4')
lines(fit~mass, data=diet2, lwd=4, col='firebrick4')

lines(lo~mass, data=diet1, lwd=2, col='dodgerblue4', lty=2)
lines(up~mass, data=diet1, lwd=2, col='dodgerblue4', lty=2)

lines(lo~mass, data=diet2, lwd=2, col='firebrick4', lty=2)
lines(up~mass, data=diet2, lwd=2, col='firebrick4', lty=2)



                      ###############################################
                      #### Data set 3 - for practice            ####
                      ###############################################

###### As before clear the memory of previous data and variables
rm(list=ls())


## Experiment 3 measured how long it took animals to feed again after the sudden loud sound stimulus

## The following code generates the number of samples and the explanatory variables we measured (diet and mass)
samp=60

masses<- rnorm(n = samp, mean = 550, sd = 75)
feed.behav<- rnorm(n = samp, mean = c(15,20), sd = c(0.6,0.7)) + rnorm(samp, 0, 2.3)
diet <- as.factor(rep(c('hi-fibre', 'hi-starch'), length.out=samp))
dat2<- data.frame('mass' = masses, 'diet'= diet) # it is stored in the "dat2" data frame

# the behavioral response variable , "feed.behav," which is a timed (in s) behavioral response to the stimulus

dat2<- cbind(dat2, feed.behav)

## You can view the data frame with:
head(dat2)
str(dat2)
summary(dat2)
## and plot the data with something like
plot(feed.behav~mass, data=dat2, col=c('black'), pch=19, xlim=c(300,775))
plot(feed.behav~diet, data=dat2)
## It might be useful to include our factor by colouring the points dependent on diet 
plot(feed.behav~mass, data=dat2, col=c('cornflowerblue', 'firebrick')[dat2$diet], pch=19, xlim=c(300,775))


### While the summary table here does tell us enough to generate some inferences you might still
# want to use the predict method to draw some nicer plots for publication that just a boxplot.
# the method is the same as above but plot points for the mean behaviour and use "arrow()" to generate bars
# displaying 95% confidence intervals


### You might want to consider playing about with the number of samples and the parameters used to generate the data
# in these scenarios and think about how this approach might be applied to power analysis.
# It can be a useful technique to help you work out how many samples you would need to detect
# a certain effect size. Intuitively, the smaller the effect size or difference between levels of a factor the more samples 
# will be required
