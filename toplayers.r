topplayers<-read.csv("P.csv")
topplayers<-topplayers[,c(2,3)]
topplayers$NewCol <- do.call(paste, c(topplayers[c("f", "s")], sep = " "))
top<-topplayers[,3]
tenpremier<-read.csv("8_2010_features.csv")
tenpremier<-read.csv("8_2010_features.csv")
tenpremier<-tenpremier[,c(1:5,36:55)]
tenpremier.1<-tenpremier[,c(6:length(tenpremier))]
for(i in 1:length(tenpremier.1)){
	j=i;
	tenpremier.1[is.na(tenpremier.1)]<-0;
 }
 ten<-tenpremier[,1:5]
 fix.ten.premier<-cbind(ten,tenpremier.1)
