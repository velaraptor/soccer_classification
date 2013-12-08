## IF YOU GUYS COULD START DOING THIS FOR EACH CSV FILE. WOULD BE GREAT
Chris-8_2010,21_2010,22_2010
Ali-8_2012,21_2012,23_2012,98_2011
Yuanda-22_2012,98_2012,23_2010

Remember for 2012 files, the read.csv("top50_2012.csv")

library(plyr)
library(ggplot2)
top<-read.csv("top50_2010.csv")
top<-top[,1]
eight<-read.csv("8_2010_features.csv")
eight$position= as.character(eight$position)
eight$position [ eight$position  == "Defender" ] <- "1"
eight$position [ eight$position  == "Forward" ] <- "2"
eight$position [ eight$position  == "Goalkeeper" ] <- "3"
eight$position [ eight$position  == "Midfielder" ] <- "4"
eight$position [ eight$position  == "Player" ] <- "5"
eight$position= as.numeric(eight$position)
full<-eight[,c(1:14,45:64)]
full.1<-full[,c(15:length(full))]
full.1[is.na(full.1)]<-0
fulll<-full[,1:14]
fix.full<-cbind(fulll,full.1)
fix.full[fix.full$full_name %in% top,35]<-1
colnames(fix.full)[35] <- "top"
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

player<-fix.full[fix.full$position==5,]
mean.height.5<-mean(player$height,na.rm=TRUE)
mean.weight.5<-mean(player$weight,na.rm=TRUE)
mean.age.5<-mean(player$age,na.rm=TRUE)
player[is.na(player$height),3]<-mean.height.5
player[is.na(player$weight),4]<-mean.weight.5
player[is.na(player$age),5]<-mean.age.5
fix.merge<-rbind.fill(defender,forward,goal,mid,player)
rows<-as.numeric(rownames(fix.merge[fix.merge[,2]==5,]))
fix.merge<-fix.merge[-rows,]
byminutes.data<-fix.merge[,c(15,17:34)]/fix.merge$mins_played
fix.merge.1<-cbind(fix.merge[1:14],fix.merge$mins_played,byminutes.data,fix.merge$top)

wss<-as.data.frame(NA,nrow=14,ncol=1)
 for (i in 2:15){
 	wss[i] <- sum(kmeans(fix.merge.1[,c(2:5,15:(length(fix.merge)-1))],,centers=i)$withinss)
 	}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
## FOR THIS DATA 6 was the best one, look at when it bends
##THE CENTER MIGHT BE DIFFERENT FOR THIS, I PUT SIX
set.seed(25)
k<-kmeans(fix.merge.1[,c(2:5,15:(length(fix.merge)-1))],,centers=6)
fix.merge.1[k$cluster==1,35]