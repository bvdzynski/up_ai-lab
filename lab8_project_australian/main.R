library("party")
library("randomForest")

data<-read.csv('C:\\Users\\macik\\Desktop\\lab8_project\\data\\australian.dat', header=FALSE, sep = ' ')

#according to lesson - remove binary columns (except class column - v15)
australian<-data[,c(2:7,10,12:15)]

#get info about classes
australian$V15<-as.factor(australian$V15)
table(australian$V15)

#datasets
train<-sample(1:nrow(australian), 100)
australian.train<-australian[train,]
australian.test<-australian[-train,]

#running randomForest algorithm
rf <- randomForest(V15 ~ . , data = australian.train)

#confusion matrix
cm <- table(australian.test$V15, predict(rf, australian.test))

#calculate error
cm2 <- cm
err <- rep(0, nrow(cm2))
for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
round(cbind(cm2, err), 4)

#recognition rate
rr <- 0
for (a in 1:nrow(cm))
{
  rr <- rr + cm[a,a]
}
round(rr / sum(cm), 4)
