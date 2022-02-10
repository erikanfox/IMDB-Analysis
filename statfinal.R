library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(stats)
library(car)
library(gridExtra)
library(cowplot)
library(leaps)
library(reshape2)
library(arm)
library(e1071)
library(caret)
library(pROC)
library(nnet)
library(texreg)
#options(scipen=999)

movies <- read.csv("moviesUSA.csv")
ratings <- read.csv("reviews2.csv")

#movies_mod <-movies[!apply(movies == "", 1, all),]
#movies.dropna()

#merge movie and rating data
imdb <- merge(movies,ratings,by="imdb_title_id")

# drop movies that have less than 100 reviews

#remove movies that don't have a release date
#imdb <- imdb[!is.na(imdb$year), ]

#make year a numeric variable
imdb$year <- as.integer(imdb$year)  


# create decade variable, as factor
imdb$decade <- as.factor(ifelse(imdb$year>=1930&imdb$year<1940, "30s",
                                ifelse(imdb$year>=1940&imdb$year<1950, "40s",
                                       ifelse(imdb$year>=1950&imdb$year<1960, "50s",
                                              ifelse(imdb$year>=1960&imdb$year<1970, "60s",
                                                     ifelse(imdb$year>=1970&imdb$year<1980, "70s",
                                                            ifelse(imdb$year>=1980&imdb$year<1990, "80s",
                                                                   ifelse(imdb$year>=1990&imdb$year<2000, "90s",
                                                                          ifelse(imdb$year>=2000&imdb$year<2010, "2000s",
                                                                                 ifelse(imdb$year>=2010&imdb$year<=2020, "2010's",
                                                                                        ifelse("Other")))))))))))
#imdb <- imdb[!is.na(imdb$decade), ]






# create categorical reception variable
#imdb$reception <- as.factor(ifelse(imdb$mean_vote>=1&imdb$mean_vote<6, "Bad",
#   ifelse(imdb$mean_vote>=6&imdb$mean_vote<=7.5, "Average",
# ifelse(imdb$mean_vote>7.5, "Good",
#ifelse("Other")))))

imdb$reception <- as.factor(ifelse(imdb$mean_vote>=1&imdb$mean_vote<7, 0,
                                   ifelse(imdb$mean_vote>=7, 1,
                                          ifelse("Other"))))


imdb$reception2 <- as.factor(ifelse(imdb$mean_vote>=1&imdb$mean_vote<6, "Bad",
                                    ifelse(imdb$mean_vote>=6&imdb$mean_vote<7, "Average",
                                           ifelse(imdb$mean_vote>=7, "Good",
                                                  ifelse("Other")))))

table(imdb$reception2)                                              
#drop<- c()

#remove gender data
for (x in colnames(imdb)){
  if (grepl("females",x)){
    drop <- c(drop, x)
  }
  if (grepl("males",x)){
    drop <- c(drop, x)
  }
  if (grepl("allgenders",x)){
    drop <- c(drop, x)
  }
  
}

#remove votes 1-11
for (x in 1:11){
  st<-paste("votes", x, sep="_")
  drop <- c(drop, st)
}

imdb <- imdb[,!(names(imdb) %in% drop)]

imdb$reception<-as.factor(imdb$reception)
cdplot(reception ~ budget_adj,data=imdb)

imdb$budget_adj<-imdb$budget_adj-mean(imdb$budget_adj)


bad <- imdb[imdb$reception==0,]
good <- imdb[imdb$reception==1,]
#apply(table(smoke_0[,c('premature_fac','parity')])/sum(table(babies1[,c('premature_fac','mrace')])),2,function(x)x/sum(x))
#apply(table(smoke_1[,c('premature_fac','parity')])/sum(table(babies1[,c('premature_fac','mrace')])),2,function(x)x/sum(x))
ggplot(imdb,aes(x=budget_adj,y=reception,fill=reception))+geom_boxplot()+labs(title= "budget vs reception",x="budget",y="reception")+theme_classic()+theme(legend.position = "none")

ggplot(imdb,aes(x=reception,y=reviews_from_critics,fill=reception))+geom_boxplot()+labs(title= "reception vs reviews_from_critics by genre",x="reception",y="reviews_from_critics")+theme_classic()+theme(legend.position = "none")+facet_wrap(~genre2)

ggplot(imdb,aes(x=reception,y=reviews_from_users,fill=reception))+geom_boxplot()+labs(title= "reception vs reviews_from_users by genre",x="reception",y="reviews_from_users")+theme_classic()+theme(legend.position = "none")+facet_wrap(~genre2)

ggplot(imdb,aes(x=reception,y=budget_adj,fill=reception))+geom_boxplot()+labs(title= "reception vs budget by genre",x="reception",y="budget")+theme_classic()+theme(legend.position = "none")+facet_wrap(~genre2)

#NullModel<-glm(reception ~ budget_adj, data=imdb,family=binomial)
#FullModel<-glm(reception ~ budget_adj+oscar2+usa_gross_income+genre2+production_company+reviews_from_users+reviews_from_critics+decade+duration+budget_adj:genre2, data=imdb, family=binomial)
#Model_stepwiseA <- step(NullModel, scope = list(upper=FullModel, lower=NullModel),direction="both",trace=0)
#Model_stepwiseA$call


hist(imdb$mean_vote)


imdb$logreviews<-log(imdb$reviews_from_users)

#model<-glm(formula = reception ~ budget_adj, family = binomial, data = imdb)

model<-glm(formula = reception ~ poly(budget_adj,3) + duration  + 
             genre2 + reviews_from_critics + logreviews + budget_adj:genre2, family = binomial, data = imdb)

#model <- glm(formula = reception ~ poly(budget_adj,3) + duration + usa_gross_income + 
# genre2 + logreviews + reviews_from_critics, family = binomial, data = imdb)

#model <- glm(reception~budget2+oscar2+usa_gross_income+genre2, data = imdb, family = binomial)

summary(model)
rawresid1 <- residuals(model, 'resp')

par(mar=c(1,1,1,1))
binnedplot(x=fitted(model),y=rawresid1,xlab="Predicted Probabilities", col.int="red4",ylab="Average Residuals", main="Binned Residuals Plot: Predicted Probabilities")

binnedplot(x=imdb$budget_adj,y=rawresid1,xlab="budget_adj",
           col.int="red4",ylab="Avg. residuals",main="Binned Residual Plot: budget_adj",col.pts="navy")
binnedplot(x=imdb$logreviews,y=rawresid1,xlab="log(reviews_from_users)",
           col.int="red4",ylab="Avg. residuals",main="Binned Residual Plot: log(reviews_from_users)",col.pts="navy")
binnedplot(x=imdb$reviews_from_critics,y=rawresid1,xlab="reviews_from_critics",
           col.int="red4",ylab="Avg. residuals",main="Binned Residual Plot: reviews_from_critics",col.pts="navy")
binnedplot(x=imdb$usa_gross_income,y=rawresid1,xlab="usa_gross_income",
           col.int="red4",ylab="Avg. residuals",main="Binned Residual Plot: usa_gross_income",col.pts="navy")
binnedplot(x=imdb$duration,y=rawresid1,xlab="duration",
           col.int="red4",ylab="Avg. residuals",main="Binned Residual Plot: duration",col.pts="navy")



imdb$reception<-as.numeric(imdb$reception)-1
x<-as.factor(ifelse(fitted(model) >= mean(imdb$reception), "1","0"))
Conf_mat2 <- confusionMatrix(x,as.factor(imdb$reception),positive="1")
Conf_mat2$table
Conf_mat2$overall["Accuracy"];
Conf_mat2$byClass[c("Sensitivity","Specificity")] 


invisible(roc(imdb$reception,fitted(model),plot=T,legacy.axes=F, print.thres='best',print.auc = T,col="red3"))


library(car)
vif(model)

# Graphing effect of age
newbudget_adj <- seq(from= min(imdb$budget_adj),to=max(imdb$budget_adj),by=.5)
newdata <- data.frame(matrix(0, nrow=length(newbudget_adj), ncol = 5))
names(newdata) <- c("genre2", "duration", "logreviews", "reviews_from_critics")
newdata$budget_adj <- newbudget_adj; 
newdata$genre2 <- "Family"; 
#newdata$usa_gross_income <- mean(imdb$usa_gross_income); 
newdata$logreviews <- mean(imdb$logreviews); 
newdata$reviews_from_critics <- mean(imdb$reviews_from_critics)
#newdata$decade<-"2010's"
newdata$duration<-mean(imdb$duration)
pred_family <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Comedy"; 
pred_comedy <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Action";
pred_action <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Horror";
pred_horror <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Romance";
pred_romance <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Drama";
pred_drama <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Other";
pred_other <- predict(model, newdata, type = "response", se.fit = TRUE)
newdata$genre2 <- "Thriller";
pred_thriller <- predict(model, newdata, type = "response", se.fit = TRUE)


plot(y=pred_family$fit,x=newbudget_adj,xlab="Budget",ylab="Reception",
     main="Audience Reception with Change in budget",col="darkblue", type = "l", lwd = 2)

points(y=pred_comedy$fit, x=newbudget_adj,col="red", pch = 19, type = "l", lwd = 2)

points(y=pred_action$fit, x=newbudget_adj, col="green", pch = 19, type = "l", lwd = 2)

points(y=pred_horror$fit, x=newbudget_adj, col="orange", pch = 19, type = "l", lwd = 2)

points(y=pred_romance$fit, x=newbudget_adj, col="pink", pch = 19, type = "l", lwd = 2)

points(y=pred_drama$fit, x=newbudget_adj, col="purple", pch = 19, type = "l", lwd = 2)

points(y=pred_other$fit, x=newbudget_adj, col="grey", pch = 19, type = "l", lwd = 2)

points(y=pred_thriller$fit, x=newbudget_adj, col="yellow", pch = 19, type = "l", lwd = 2)


legend("topright",c("Family","Comedy","Action","Horror","Romance","Drama","Thriller","Other"),col=c("darkblue","red","green", "orange", "pink","purple","yellow","grey"),lty=c(2,2))



######################MODEL2
imdb$reception2 <- ordered(imdb$reception2,
                           levels=c("Good","Average","Bad"))

#NullModel<-polr(reception2 ~ budget_adj, data=imdb)
#FullModel<-polr(reception2 ~ budget_adj+oscar2+genre2+production_company+reviews_from_users+reviews_from_critics+duration+budget_adj:genre2, data=imdb)
#Model_stepwiseA <- step(NullModel, scope = list(upper=FullModel, lower=NullModel),direction="both",trace=0)
#Model_stepwiseA$call

#str(imdb)
table(imdb$reception2)
prop.table(table(imdb$reception2,imdb$oscar2), 2)
prop.table(table(imdb$reception2,imdb$genre2), 2)
plot(imdb$reception2,imdb$budget_adj)
plot(imdb$reception2,imdb$reviews_from_critics)
plot(imdb$reception2,imdb$reviews_from_users)
ggplot(imdb,aes(x=budget_adj,y=reception2,fill=reception2))+geom_boxplot()+labs(title= "budget vs reception",x="budget",y="reception")+theme_classic()+theme(legend.position = "none")
ggplot(imdb,aes(x=reception2,y=budget_adj,fill=reception2))+geom_boxplot()+labs(title= "reception vs budget by genre",x="reception",y="budget")+theme_classic()+theme(legend.position = "none")+facet_wrap(~genre2)


model2<- polr(formula = reception2 ~ poly(budget_adj,3)+logreviews+genre2+duration+reviews_from_critics, data = imdb)


library(car)
vif(model2)


summary(model2)

coef(model2)
confint(model2)

exp(confint(model2))

###### Predictions
#predicted probabilities for cases in the model
predprobs <- fitted(model2) 
head(predprobs[sample(nrow(predprobs)),])
imdb$x<-as.numeric(imdb$reception2)
###### Diagnostics
#looking at the residuals may not be that meaningful here because we only have two binary variables

rawresid1 <- (as.numeric(imdb$reception2) == 1) -  predprobs[,1]
rawresid2 <- (as.numeric(imdb$reception2) <= 2) -  rowSums(predprobs[,1:2])
rawresid3 <- (as.numeric(imdb$reception2) <= 3) -  rowSums(predprobs[,1:3])
resid<-(as.numeric(imdb$reception2)) -  rowSums(predprobs)
tapply(rawresid1, imdb$genre2, mean)
tapply(rawresid2, imdb$genre2, mean)
tapply(rawresid3, imdb$genre2, mean)

predprobs1<- predprobs[,1]
predprobs2<- predprobs[,1:2]
predprobs3<- predprobs[,1:3]

binnedplot(x=predprobs1,y=rawresid1,
           xlab="Predicted Probabilities", col.int="red4",ylab="Average Residuals", main="Binned Residuals Plot: Predicted Probabilities")

binnedplot(x=predprobs2,y=rawresid2,
           xlab="Predicted Probabilities", col.int="red4",ylab="Average Residuals", main="Binned Residuals Plot: Predicted Probabilities")

binnedplot(x=predprobs3,y=rawresid3,
           xlab="Predicted Probabilities", col.int="red4",ylab="Average Residuals", main="Binned Residuals Plot: Predicted Probabilities")

binnedplot(x=imdb$budget_adj,y=rawresid1,xlab="budget_adj",
           col.int="red4",ylab="rawresid1",main="Binned Residual Plot: budget_adj",col.pts="navy")

binnedplot(x=imdb$budget_adj,y=rawresid2,xlab="budget_adj",
           col.int="red4",ylab="rawresid2",main="Binned Residual Plot: budget_adj",col.pts="navy")

binnedplot(x=imdb$budget_adj,y=rawresid3,xlab="budget_adj",
           col.int="red4",ylab="rawresid3",main="Binned Residual Plot: budget_adj",col.pts="navy")
##########

binnedplot(x=imdb$logreviews,y=rawresid1,xlab="log(reviews_from_users)",
           col.int="red4",ylab="Avg. residuals",main="Binned Residual Plot: log(reviews_from_users)",col.pts="navy")


# Graphing effect of age
newbudget_adj <- seq(from= min(imdb$budget_adj),to=max(imdb$budget_adj),by=.5)
newdata <- data.frame(matrix(0, nrow=length(newbudget_adj), ncol = 5))
names(newdata) <- c("genre2", "duration", "logreviews", "reviews_from_critics")
newdata$budget_adj <- newbudget_adj; 
newdata$genre2 <- "Family"; 
#newdata$usa_gross_income <- mean(imdb$usa_gross_income); 
newdata$logreviews <- mean(imdb$logreviews); 
newdata$reviews_from_critics <- mean(imdb$reviews_from_critics)
#newdata$decade<-"2010's"
newdata$duration<-mean(imdb$duration)
pred_family <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Comedy"; 
pred_comedy <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Action";
pred_action <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Horror";
pred_horror <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Romance";
pred_romance <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Drama";
pred_drama <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Other";
pred_other <- predict(model2, newdata, type = "prob", se.fit = TRUE)
newdata$genre2 <- "Thriller";
pred_thriller <- predict(model2, newdata, type = "prob", se.fit = TRUE)


plot(y=pred_family[,"Good"],x=newbudget_adj,xlab="Budget",ylab="Pred.Probs",
     main="Good Reception with Change in budget",col="darkblue", type = "l", lwd = 2)

points(y=pred_comedy[,"Good"], x=newbudget_adj,col="red", pch = 19, type = "l", lwd = 2)

points(y=pred_action[,"Good"], x=newbudget_adj, col="green", pch = 19, type = "l", lwd = 2)

points(y=pred_horror[,"Good"], x=newbudget_adj, col="orange", pch = 19, type = "l", lwd = 2)

points(y=pred_romance[,"Good"], x=newbudget_adj, col="pink", pch = 19, type = "l", lwd = 2)

points(y=pred_drama[,"Good"], x=newbudget_adj, col="purple", pch = 19, type = "l", lwd = 2)

points(y=pred_other[,"Good"], x=newbudget_adj, col="grey", pch = 19, type = "l", lwd = 2)

points(y=pred_thriller[,"Good"], x=newbudget_adj, col="yellow", pch = 19, type = "l", lwd = 2)

legend("topright",c("Family","Comedy","Action","Horror","Romance","Drama","Thriller","Other"),col=c("darkblue","red","green", "orange", "pink","purple","yellow","grey"),lty=c(2,2))


plot(y=pred_family[,"Average"],x=newbudget_adj,xlab="Budget",ylab="Pred.Probs",
     main="Average Reception with Change in budget",col="darkblue", type = "l", lwd = 2)

points(y=pred_comedy[,"Average"], x=newbudget_adj,col="red", pch = 19, type = "l", lwd = 2)

points(y=pred_action[,"Average"], x=newbudget_adj, col="green", pch = 19, type = "l", lwd = 2)

points(y=pred_horror[,"Average"], x=newbudget_adj, col="orange", pch = 19, type = "l", lwd = 2)

points(y=pred_romance[,"Average"], x=newbudget_adj, col="pink", pch = 19, type = "l", lwd = 2)

points(y=pred_drama[,"Average"], x=newbudget_adj, col="purple", pch = 19, type = "l", lwd = 2)

points(y=pred_other[,"Average"], x=newbudget_adj, col="grey", pch = 19, type = "l", lwd = 2)

points(y=pred_thriller[,"Average"], x=newbudget_adj, col="yellow", pch = 19, type = "l", lwd = 2)

legend("topright",c("Family","Comedy","Action","Horror","Romance","Drama","Thriller","Other"),col=c("darkblue","red","green", "orange", "pink","purple","yellow","grey"),lty=c(2,2))


plot(y=pred_family[,"Bad"],x=newbudget_adj,xlab="Budget",ylab="Pred.Probs",
     main="Bad Reception with Change in budget",col="darkblue", type = "l", lwd = 2)

points(y=pred_comedy[,"Bad"], x=newbudget_adj,col="red", pch = 19, type = "l", lwd = 2)

points(y=pred_action[,"Bad"], x=newbudget_adj, col="green", pch = 19, type = "l", lwd = 2)

points(y=pred_horror[,"Bad"], x=newbudget_adj, col="orange", pch = 19, type = "l", lwd = 2)

points(y=pred_romance[,"Bad"], x=newbudget_adj, col="pink", pch = 19, type = "l", lwd = 2)

points(y=pred_drama[,"Bad"], x=newbudget_adj, col="purple", pch = 19, type = "l", lwd = 2)

points(y=pred_other[,"Bad"], x=newbudget_adj, col="grey", pch = 19, type = "l", lwd = 2)

points(y=pred_thriller[,"Bad"], x=newbudget_adj, col="yellow", pch = 19, type = "l", lwd = 2)

legend("topright",c("Family","Comedy","Action","Horror","Romance","Drama","Thriller","Other"),col=c("darkblue","red","green", "orange", "pink","purple","yellow","grey"),lty=c(2,2))


