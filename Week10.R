library("lavaan")
dat <- read.csv("path.csv")

model <- ' grad ~ 1+ gre + col + hs '
fit <- cfa(model = model, data = dat)
summary(fit,  rsquare=TRUE)

model1 <-  lm(grad ~ gre + col + hs,data=dat)
summary(model1)

model2 <- 'col ~ hs
           gre ~ col
           grad ~ col + gre '
fit <- cfa(model = model2, data = dat)
summary(fit, fit.measures=TRUE, standardized= TRUE, rsquare=TRUE)

model <- '
# regression
gre ~ a*hs
gre ~ col

# regression
grad ~ c*hs
grad ~ col
grad ~ b*gre

# indirect effect (a*b)
hsgre := a*b
# total effect
total := c + (a*b)
'
fit <- cfa(model = model, data = dat) # sobel test
summary(fit, standardized= TRUE, rsquare=TRUE, ci=TRUE)

fit <- cfa(model = model, data = dat, se='bootstrap', bootstrap = 1000)



model <- '
# regression
gre ~ a*col
gre ~ hs

# regression
grad ~ c*col
grad ~ hs
grad ~ b*gre

# indirect effect (a*b)
hscol := a*b
# total effect
total := c + (a*b)
'
fit <- cfa(model = model, data = dat) # sobel test
summary(fit, standardized= TRUE, rsquare=TRUE, ci=TRUE)

dat2 <- read.csv("worland5.csv")
modelsem<- '
# measurement model/define latent variables
adjust=~ motiv + harm + stabi
risk=~ verbal + ppsych + ses 
achieve =~ read + arith + spell
# regression/ structural part
achieve ~ adjust + risk
'
fit3<- cfa(modelsem, data=dat2) # how I named the data
summary(fit3,  standardized= TRUE, fit.measures= TRUE)
