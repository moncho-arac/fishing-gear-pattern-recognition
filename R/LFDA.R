library(lfda)
library(ggplot2)
library(kernlab)
library(splitTools)

fishing = read.csv("FishingTypesS2.csv")

Xf <- subset(fishing, select = -c(FT))
yf <- subset(fishing, select =  c(FT))

X     <- unname(as.matrix(Xf))
label <- unname(as.matrix(yf))

Xc <- scale(X,center=TRUE,scale=TRUE)
#y  <- label

# Create test and train
inds<-partition(label, p  = c(train = 0.7, test = 0.3), seed = 1)
str(inds)

Xtrain <- Xc[inds$train,]
Xtest  <- Xc[inds$test,]

ytrain <- label[inds$train]
ytest  <- label[inds$test]

#METRICS
#"weighted", "orthonormalized", "plain"

## calculate kernel matrix
r<-4
model <- lfda(Xtrain, ytrain, r, metric = "weighted", knn = 15)
prj_train <- model$Z

U  <- as.data.frame(prj_train)
ggplot(U, aes(x=V1, y=V3, color=ytrain)) + geom_point(size=1) 
