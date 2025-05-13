#Multicollinearity
library("faraway")
round(cor(seatpos),2)
fit <- lm(formula = hipcenter ~ ., data = seatpos)
summary(fit)
vif(fit)
fit2<-lm(hipcenter~ Age+ Weight +Ht, data=seatpos) 
summary(fit2) 
vif(fit2)

#PCA
round(cor(mtcars), 3)
library(corrgram)
corrgram(mtcars)

plot(prcomp(mtcars, scale = TRUE), type="line")     
summary(prcomp(mtcars, scale = TRUE))
prcomp(mtcars, scale = TRUE)$rotation[,1:2] 
biplot(prcomp(mtcars, scale = TRUE))
prcomp(mtcars, scale = TRUE)$x[,1:2]

library("psych")
library("GPArotation")
principal(mtcars, nfactors = 2, rotate="varimax")
biplot(principal(mtcars, nfactors = 2, rotate="varimax"))
principal(mtcars, nfactors = 2, rotate="varimax")$scores

# Extract rotated principal components using Varimax rotation
pca_rotated <- principal(mtcars[, -1], nfactors = 2, rotate = "varimax")

# View the rotated PCA scores for each car model
pca_scores <- as.data.frame(pca_rotated$scores)


# Add the dependent variable (mpg) to the PCA scores dataframe
pca_scores$mpg <- mtcars$mpg

# Check the new dataframe
head(pca_scores)

# Run linear regression using the rotated principal components as predictors
pca_lm <- lm(mpg ~ RC1 + RC2, data=pca_scores)

# Display the regression summary
summary(pca_lm)

# Original regression model using all variables
original_lm <- lm(mpg ~ ., data = mtcars)

# Compare R-squared values of both models
cat("Original Model R-squared:", summary(original_lm)$r.squared, "\n")

cat("PCA Model R-squared:", summary(pca_lm)$r.squared, "\n")
