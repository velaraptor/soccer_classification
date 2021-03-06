library(plyr)
library(ggplot2)
top<-read.csv("top50_2010.csv")
top<-top[,1]

eigh<-read.csv("8_2010_features.csv")
twoone<-read.csv("21_2010_features.csv")
twotwo<-read.csv("22_2010_features.csv")
twothree<-read.csv("23_2010_features.csv")

eight<-rbind.fill(eigh,twoone,twotwo,twothree)

##Make position a number instead of category
eight$position= as.character(eight$position)
eight$position [ eight$position  == "Defender" ] <- "1"
eight$position [ eight$position  == "Forward" ] <- "2"
eight$position [ eight$position  == "Goalkeeper" ] <- "3"
eight$position [ eight$position  == "Midfielder" ] <- "4"
eight$position [ eight$position  == "Player" ] <- "5"
eight$position= as.factor(eight$position)

##clean data to include FIFA, and features that are relevant, and include zeros to features with NA
full<-eight[,c(1:14,45:64)]
full.1<-full[,c(15:length(full))]
full.1[is.na(full.1)]<-0
fulll<-full[,1:14]
fix.full<-cbind(fulll,full.1)

##add top indicator variable
fix.full[fix.full$full_name %in% top,35]<-1
colnames(fix.full)[35] <- "top"

##fix all the height, weight, and age to means for each position
defender<-fix.full[fix.full$position==1,]
mean.height<-mean(defender$height,na.rm=TRUE)
mean.age<-mean(defender$age,na.rm=TRUE)
mean.weight<-mean(defender$weight,na.rm=TRUE)
defender[is.na(defender$height),3]<-mean.height
defender[is.na(defender$weight),4]<-mean.weight
defender[is.na(defender$age),5]<-mean.age

forward<-fix.full[fix.full$position==2,]
mean.height.2<-mean(forward$height,na.rm=TRUE)
mean.weight.2<-mean(forward$weight,na.rm=TRUE)
mean.age.2<-mean(forward$age,na.rm=TRUE)
forward[is.na(forward$height),3]<-mean.height.2
forward[is.na(forward$weight),4]<-mean.weight.2
forward[is.na(forward$age),5]<-mean.age.2

goal<-fix.full[fix.full$position==3,]
mean.height.3<-mean(goal$height,na.rm=TRUE)
mean.weight.3<-mean(goal$weight,na.rm=TRUE)
mean.age.3<-mean(goal$age,na.rm=TRUE)
goal[is.na(goal$height),3]<-mean.height.3
goal[is.na(goal$weight),4]<-mean.weight.3
goal[is.na(goal$age),5]<-mean.age.3

mid<-fix.full[fix.full$position==4,]
mean.height.4<-mean(mid$height,na.rm=TRUE)
mean.weight.4<-mean(mid$weight,na.rm=TRUE)
mean.age.4<-mean(mid$age,na.rm=TRUE)
mid[is.na(mid$height),3]<-mean.height.4
mid[is.na(mid$weight),4]<-mean.weight.4
mid[is.na(mid$age),5]<-mean.age.4

##It's better not to include type player because there is no data on their height, weight, and we don't know who they are. 

##merge all the cleaned data
fix.merge<-rbind.fill(defender,forward,goal,mid)
##put features as rate by minute
byminutes.data<-fix.merge[,c(15,17:34)]/fix.merge$mins_played
##put the rate into the new dataset
fix.merge.1<-cbind(fix.merge[1:14],fix.merge$mins_played,byminutes.data,fix.merge$top)
## training the data, a dummy variable
fix.merge.1[is.na(fix.merge.1)]<-0

##Find Optimal K-means using Lloyd Algorithm
wss<-as.data.frame(NA,nrow=14,ncol=1)
 for (i in 2:15){
         wss[i] <- sum(kmeans(fix.merge.1[,c(2:5,15:(length(fix.merge)-1))],,centers=i)$withinss)
         }
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
k<-kmeans(fix.merge.1[,c(2:5,15:(length(fix.merge)))],centers=4,algorithm="Lloyd")
##not popular cluster
notpopular.1<-as.data.frame(fix.merge.1[k$cluster==4,1])

notpopular.1<-as.data.frame(fix.merge.1[k$cluster==4,1])
colnames(notpopular.1)<-"not"
write.csv(notpopular.1,file="notpop.csv")
nottop<-read.csv("notpop.csv")

fix.merge.1[fix.merge.1$full_name %in% nottop[,2],35]<-0
colnames(fix.merge.1)[35]<-"top"
train.1<-fix.merge.1[fix.merge.1$full_name %in% nottop[,2],]
train.2<-fix.merge.1[fix.merge.1$full_name %in% top,]
train<-rbind.fill(train.1,train.2)
fix.merge.2<-fix.merge.1[,1:(length(fix.merge.1)-1)]
test.1<-fix.merge.2[!fix.merge.1$full_name %in% nottop[,2],]
test.2<-fix.merge.2[!fix.merge.1$full_name %in% top,]
test<-rbind.fill(test.1,test.2)
colnames(train)[15]<-"mins_played"
colnames(test)[15]<-"mins_played"

library(randomForest)
rf<-randomForest(top~position+height+weight+age+mins_played+num_touches+num_touches_2st_third+num_appearances+num_touches_3st_third+num_tackles_attempted+num_shots+num_touches_1st_third+num_fouls_conceded+num_fouls_won+num_yellow_cards+num_aerial_successes+num_headed_shots+num_aerial_duels+num_headed_on_target+num_shots_on_target+num_goals+num_penalties_scored+num_red_cards+num_set_play_goals,data=train)
 pred<-predict(rf,newdata=test)
 
 ##look at data fo random forest test
test.3<-data.frame(test[,c(1:length(fix.merge.1)-1)],pred)
colnames(test.3)[35]<-'top'
check<-rbind.fill(train,test.3)
ggplot(check,aes(x=mins_played.1,y=num_touches,color=top))+geom_point()
 
 ##predict the train and test data
test.again<-train[,1:(length(fix.merge.1)-1)]
new.test<-rbind.fill(test.again,test)
pred.1<-predict(rf,newdata=new.test)
pred.3<-data.frame(new.test[,c(1:length(fix.merge.1)-1)],pred.1)
colnames(pred.3)[35]<-'top'
ggplot(pred.3,aes(x=mins_played,y=num_shots,color=top))+geom_point()+scale_colour_gradientn(colours=rainbow(2))
 
##look at cluster and change values for players that were wrongly identified 
k.1<-kmeans(pred.3[,c(2:5,15:(length(pred.3)))],centers=4,algorithm="Lloyd")
pred.3[k.1$cluster==3,35]<-.75
pred.3[k.1$cluster==1,35]<-.25
pred.3[pred.3$full_name %in% top,35]<-1

##creating a 10 fold cross validation using graditent boosting model 
library(caret)
inTraining <- createDataPartition(pred.3$top, p = 0.75, list = FALSE)
training<-pred.3[inTraining,]
testing<-pred.3[-inTraining,]
fitControl<-trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(800)
libary(gbm)
gbm.1<-train(top~position+height+weight+age+mins_played+num_touches+num_touches_2st_third+num_appearances+num_touches_3st_third+num_tackles_attempted+num_shots+num_touches_1st_third+num_fouls_conceded+num_fouls_won+num_yellow_cards+num_aerial_successes+num_headed_shots+num_aerial_duels+num_headed_on_target+num_shots_on_target+num_goals+num_penalties_scored+num_red_cards+num_set_play_goals,method="gbm",trControl=fitControl,verbose=FALSE)
gbm.predict<-predict(gbm.1,testing)
gbm.data<-data.frame(testing[,c(1:length(fix.merge.1)-1)],gbm.predict)
ggplot(gbm.data,aes(x=mins_played,y=num_touches,color=gbm.predict))+geom_point()+scale_colour_gradientn(colours=rainbow(2))
gbm.predict.1<-predict(gbm.1,new.test)
gbm.data.1<-data.frame(new.test[,c(1:length(fix.merge.1)-1)],gbm.predict.1)


##predict on 2013 data and clean the data
twotwo.2013<-read.csv("98_2013_features.csv")
twotwo.2013$position= as.character(twotwo.2013$position)
twotwo.2013$position [ twotwo.2013$position  == "Defender" ] <- "1"
twotwo.2013$position [ twotwo.2013$position  == "Forward" ] <- "2"
twotwo.2013$position [ twotwo.2013$position  == "Goalkeeper" ] <- "3"
twotwo.2013$position [ twotwo.2013$position  == "Midfielder" ] <- "4"
twotwo.2013$position [ twotwo.2013$position  == "Player" ] <- "5"
twotwo.2013$position= as.factor(twotwo.2013$position)

full.2013<-eight[,c(1:14,45:64)]
full.12<-full.2013[,c(15:length(full.2013))]
full.12[is.na(full.12)]<-0
fulll.1<-full.2013[,1:14]
fix.full.1<-cbind(fulll.1,full.12)

defender.1<-fix.full.1[fix.full.1$position==1,]
mean.height.2013<-mean(defender.1$height,na.rm=TRUE)
mean.age.2013<-mean(defender.1$age,na.rm=TRUE)
mean.weight.2013<-mean(defender.1$weight,na.rm=TRUE)
defender.1[is.na(defender.1$height),3]<-mean.height.2013
defender.1[is.na(defender.1$weight),4]<-mean.weight.2013
defender.1[is.na(defender.1$age),5]<-mean.age.2013

forward.1<-fix.full.1[fix.full.1$position==2,]
mean.height.2013.4<-mean(forward.1$height,na.rm=TRUE)
mean.weight.2013.4<-mean(forward.1$weight,na.rm=TRUE)
mean.age.2013.4<-mean(forward.1$age,na.rm=TRUE)
forward.1[is.na(forward.1$height),3]<-mean.height.2013.4
forward.1[is.na(forward.1$weight),4]<-mean.weight.2013.4
forward.1[is.na(forward.1$age),5]<-mean.age.2013.4

goal.1<-fix.full.1[fix.full.1$position==3,]
mean.height.2013.1<-mean(goal.1$height,na.rm=TRUE)
mean.weight.2013.1<-mean(goal.1$weight,na.rm=TRUE)
mean.age.2013.1<-mean(goal.1$age,na.rm=TRUE)
goal.1[is.na(goal.1$height),3]<-mean.height.2013.1
goal.1[is.na(goal.1$weight),4]<-mean.weight.2013.1
goal.1[is.na(goal.1$age),5]<-mean.age.2013.1

mid.1<-fix.full.1[fix.full.1$position==4,]
mean.height.2013.2<-mean(mid.1$height,na.rm=TRUE)
mean.weight.2013.2<-mean(mid.1$weight,na.rm=TRUE)
mean.age.2013.2<-mean(mid.1$age,na.rm=TRUE)
mid.1[is.na(mid.1$height),3]<-mean.height.2013.2
mid.1[is.na(mid.1$weight),4]<-mean.weight.2013.2
mid.1[is.na(mid.1$age),5]<-mean.age.2013.2

##merge all the cleaned data
fix.merge.2013<-rbind.fill(defender.1,forward.1,goal.1,mid.1)
##put features as rate by minute
byminutes.data.1<-fix.merge.2013[,c(15,17:34)]/fix.merge.2013$mins_played
##put the rate into the new dataset
fix.merge.2013.1<-cbind(fix.merge.2013[1:14],fix.merge.2013$mins_played,byminutes.data.1)
colnames(fix.merge.2013.1)[15]<-"mins_played"

## predict data and show order of popularity metric
gbm.predict.2<-predict(gbm.1,fix.merge.2013.1)
gbm.data.2<-data.frame(fix.merge.2013.1[,c(1:length(fix.merge.2013.1)-1)],gbm.predict.2)
top.order<-order(gbm.data.2[,34],decreasing=T)
gbm.data.2[top.order,1]
