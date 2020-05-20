Data <- read.csv("ProjectData.csv", header=TRUE, sep= ",")

# str(Data)
# #attach create data frame
# attach(Data)
#View(Data)
# str(Data)
library("rpart")
library(tidyverse)

#Data1 <- filter(Data, Group == 0)
Data1 <- Data
# View(Data1)

DT_Model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7,data=Data1,control=rpart.control(minsplit=60,minbucket=30, maxdepth=4 ))
plot(DT_Model)

library("partykit")
plot(as.party(DT_Model))
print(DT_Model)
# attach(Data1)
Target=ifelse(Response==1,'Y','N')
# View(Target)
Data2 <- data.frame(Data1,Target)

s <- sample(296,236)
train <- Data2[s,]
test <- Data2[-s,]

DT_Model2<-rpart(Target~X1+X2+X3+X4+X5+X6+X7, data=train, control=rpart.control(minsplit=60, minbucket=30,maxdepth=10 ))
plot(as.party(DT_Model2))

 print(DT_Model2$cptable)
# 
 opt <- which.min(DT_Model2$cptable [, "xerror"])
# 
 cp <- DT_Model2$cptable [opt,"CP"]
 DT_Model_pruned <- prune(DT_Model2, cp=cp)
 plot(as.party(DT_Model_pruned))
# dim(Data1) #or dim(Data)
# dim(Data2)

# dim(test)


# DT_Model3<-rpart(Target~X1+X2+X3+X4+X5+X6+X7, data=train, method = "class")
p <- predict(DT_Model_pruned,test,type="class")
table(test[,18],p)

