rm(list=ls())

library(MASS)
library(caret)
library(rattle)

indivData <- read.csv('https://raw.githubusercontent.com/lwhite01/Data/main/indivData2.csv')
colNames <- as.list(colnames(indivData))
head(indivData)


l_fighters <- indivData[,c(4, 6:11,21)]
summary(l_fighters)

set.seed(1)
divideData<-createDataPartition(l_fighters$l_win, p=.8, list=FALSE)
train<-l_fighters[divideData,]
test<-l_fighters[-divideData,]

testNoNA <- test[complete.cases(test),]

logisticreg<-glm(l_win~., family=binomial, data=train)
summary(logisticreg)     

car::vif(logisticreg)

probs<-predict(logisticreg, testNoNA, type="response")
pred <- ifelse(probs > 0.5, 1, 0)

table <- table(pred, testNoNA$l_win)
table(pred, testNoNA$l_win)    # use confusion matrix command - for sensitivity and all that
mean(pred==testNoNA$l_win) 

Sensitivity <- (table[4]/(table[4]+table[3])); Sensitivity
Specificity <- (table[1]/(table[1]+table[2])); Specificity

# Sensitivity = 70.9%
# Specificity = 67.05%


# Testing different formulations:

# Without insignificant variables:
ltest1<-glm(l_win~.-l_height_dif_cm-l_weight_dif_lb, family=binomial, data=train)
summary(ltest1) 
car::vif(ltest1)

probs2<-predict(ltest1, testNoNA, type="response")
pred2 <- ifelse(probs2 > 0.5, 1, 0)

table2 <- table(pred2, testNoNA$l_win)
table(pred2, testNoNA$l_win)    # use confusion matrix command - for sensitivity and all that
mean(pred2==testNoNA$l_win)

Sensitivity2 <- (table2[4]/(table2[4]+table2[3])); Sensitivity2
Specificity2 <- (table2[1]/(table2[1]+table2[2])); Specificity2

# 71.8% Sensitivity
# 66.2% Specificity

# reach and total fights interaction raises specificity to 71.9%, insignificant interaction though
# reach and win dif interaction has about the same sensitivity but the interaction is significant




l_fightersTest <- indivData[,c(4,6,7,10,11,21)]
set.seed(1)
divideData2<-createDataPartition(l_fightersTest$l_win, p=.8, list=FALSE)
train2<-l_fightersTest[divideData2,]
test2<-l_fightersTest[-divideData2,]

testNoNA2 <- test2[complete.cases(test2),]

ltest2<-glm(l_win~., family=binomial, data=train2)
summary(ltest2) 
car::vif(ltest2)

probs3<-predict(ltest2, testNoNA2, type="response")
pred3 <- ifelse(probs3 > 0.5, 1, 0)

table3 <- table(pred3, testNoNA2$l_win)
table(pred3, testNoNA2$l_win)    # use confusion matrix command - for sensitivity and all that
mean(pred3==testNoNA2$l_win)

Sensitivity3 <- (table3[4]/(table3[4]+table3[3])); Sensitivity3
Specificity3 <- (table3[1]/(table3[1]+table3[2])); Specificity3



#################################

rm(list=ls())

# LDA

l_fightersLDA <- indivData[,c(4,6,7,10,11,21)]

set.seed(1)
divideData3<-createDataPartition(l_fightersLDA$l_win, p=.8, list=FALSE)
train3<-l_fightersLDA[divideData3,]
test3<-l_fightersLDA[-divideData3,]

testNoNA3 <- test3[complete.cases(test3),]

modelLDA<- lda(l_win~., data=train3)
modelLDA

predictions<-modelLDA %>% predict(testNoNA3)
mean(predictions$class==testNoNA3$l_win) ###accuracy rate
table(predictions$class,testNoNA3$l_win)
table4 <- table(predictions$class,testNoNA3$l_win)
Sensitivity4 <- (table4[4]/(table4[4]+table4[3])); Sensitivity4
Specificity4 <- (table4[1]/(table4[1]+table4[2])); Specificity4










