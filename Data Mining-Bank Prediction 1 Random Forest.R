# Defining sample for Ramdom Forest 
Thera_Bank <- read_excel("Thera Bank-Data Set.xlsx")
Thera_Bank = read_excel(file.choose())
Thera_Bank$random <- runif (nrow(Thera_Bank), 0, 1);
TBank.train <- Thera_Bank [which(Thera_Bank$random <= 0.7),]
Tbank.test <- Thera_Bank [which(Thera_Bank$random > 0.7),] 
c (nrow(TBank.train), nrow(Tbank.test))

library(randomForest)

## Missing value treatment 

str(Thera_Bank)
summary(Thera_Bank$`Experience (in years)`)
Thera_Bank$Familymembers[is.na(Thera_Bank$Familymembers)]=0
any(is.na(Thera_Bank)) ## no missing value
any(is.na(TBank.train)) ## no missing value
Thera_Bank$Experience_in_years [Thera_Bank$Experience_in_years<= 0.1 ]=0
summary(TBank.train)
## Negative values of experience changed to 0


str(TBank.train[,c(2:14)])
class(TBank.train)
str(Tbank.test)
RF <- randomForest(as.factor(TBank.train$PersonalLoan) ~ ., data = TBank.train[,c(2:14)], 
                   ntree=501, mtry = 2, nodesize =10,
                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Thera Bank Training Data")


min(RF$err.rate[,1])

impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]



## Tuning Random Forest
tRF <- tuneRF(x = TBank.train[,c(2,3,4,6,7,8,9,11,12,13,14)], 
              y=as.factor(TBank.train$PersonalLoan),
              mtryStart = 3, 
              ntreeTry=501, 
              stepFactor = 1.5, 
              improve = 0.00001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
)

# alternate try

tRF <- tuneRF(x = TBank.train[,-c(1,5,15,10)], 
              y=as.factor(TBank.train$PersonalLoan),
              mtryStart = 3, 
              ntreeTry=501, 
              stepFactor = 1.5, 
              improve = 0.00001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
              )

## 
#A plot for the mean decrease in accuracy levels
varImpPlot(tRF)
print(tRF)



#inserting class and prob score columns:

TBank.train$predict.class <- predict(tRF, TBank.train, type="class")
TBank.train$predict.score <- predict(tRF, TBank.train, type="prob")
head(TBank.train)
class(TBank.train$predict.score)



MLmetrics::AUC(TBank.train$predict.score[,2], TBank.train$PersonalLoan)




# Performance on Test data 

Tbank.test$predict.class <- predict(tRF, Tbank.test, type="class")
Tbank.test$predict.score <- predict(tRF, Tbank.test, type="prob")
head(Tbank.test)



MLmetrics::AUC(Tbank.test$predict.score[,2], 
               Tbank.test$PersonalLoan)


table(TBank.train$predict.class,TBank.train$PersonalLoan)
str(TBank.train$predict.class)
str(TBank.train$PersonalLoan)
RFConf.Matx = confusionMatrix(TBank.train$predict.class, as.factor(TBank.train$PersonalLoan),positive = "1")
RFConf.Matx

pred = prediction(TBank.train$predict.score[,2], TBank.train$PersonalLoan)
pred
perf = performance(pred, "tpr" , "fpr")
plot(perf)
perf

ks= max(attr(perf, 'y.values')[[1]])-attr(perf, 'x.values')[[1]])
ks

auc = performance(pred, "auc")
auc = as.numeric(auc@y.values)
gini = ineq(TBank.train$predict.score[,2],type = "Gini")


decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
  deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
  ifelse(x<deciles[1], 1,
         ifelse(x<deciles[2], 2,
                ifelse(x<deciles[3], 3,
                       ifelse(x<deciles[4], 4,
                              ifelse(x<deciles[5], 5,
                                     ifelse(x<deciles[6], 6,
                                            ifelse(x<deciles[7], 7,
                                                   ifelse(x<deciles[8], 8,
                                                          ifelse(x<deciles[9], 9, 10
                                                          ))))))))))
}
Tbank.test$deciles <- decile(Tbank.test$predict.score[,2])

tmp_DT = data.table(Tbank.test)
h_rank <- tmp_DT[, list(
  cnt = length(PersonalLoan), 
  cnt_resp = sum(PersonalLoan), 
  cnt_non_resp = sum(PersonalLoan == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,4);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),4);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),4);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);

## Performance of Random Forest
RFConf.Matx
auc
ks
gini



