Data <- read.csv("ProjectData.csv", header=TRUE, sep= ",")
# str(Data)
#attach create data frame
# attach(Data)
#View(Data)
# str(Data)
library("rpart")
library(tidyverse)

Data1 <- filter(Data, Group == 0)
 # View(Data1)

DT_Model <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7,data=Data1,control=rpart.control(minsplit=60,minbucket=30, maxdepth=4 ))
plot(DT_Model)

library("partykit")
plot(as.party(DT_Model))
# attach(Data1)
Target=ifelse(Data1$Response==1,'Y','N')
# View(Target)
Data2 <- data.frame(Data1,Target)
# dim(Data2)
s <- sample(96,77)
train <- Data2[s,]
test <- Data2[-s,]

# str(Data)
# View(Data)
# View(Data1)
DT_Model2<-rpart(Target~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=train, control=rpart.control(minsplit=60, minbucket=30,maxdepth=10 ))
plot(as.party(DT_Model2))

print(DT_Model2$cptable)

opt <- which.min(DT_Model2$cptable [, "xerror"]) 

cp <- DT_Model2$cptable [opt,"CP"]
DT_Model_pruned <- prune(DT_Model2, cp=cp)
plot(as.party(DT_Model_pruned))

p <- predict(DT_Model_pruned,test,type="class")
table(test[,18],p)
