topplayers<-read.csv("P.csv")
topplayers<-topplayers[,c(2,3)]
topplayers$NewCol <- do.call(paste, c(topplayers[c("f", "s")], sep = " "))
top<-topplayers[,3]
tenpremier<-read.csv("8_2010_features.csv")
tenpremier<-tenpremier[,c(1:5,36:55)]
tenpremier.1<-tenpremier[,c(6:length(tenpremier))]
tenpremier.1[is.na(tenpremier.1)]<-0
ten<-tenpremier[,1:5]
fix.ten.premier<-cbind(ten,tenpremier.1)
ggplot(fix.ten.premier,aes(x=num_goals,y=num_shots_on_target))+geom_point()+geom_text(aes(label=full_name),hjust=0, vjust=0,size=2)
ggplot(fix.ten.premier,aes(x=num_fouls_conceded,y=num_aerial_duels))+geom_point()+geom_text(aes(label=full_name),hjust=0, vjust=0,size=2)
ggplot(fix.ten.premier,aes(x=num_appearances,y=mins_played))+geom_point()+geom_text(aes(label=full_name),hjust=0, vjust=0,size=2)
ggplot(fix.ten.premier,aes(x=num_appearances,y=num_touches))+geom_point()+geom_text(aes(label=full_name),hjust=0, vjust=0,size=2)
eight<-read.csv("8_2010_features.csv")
twentyone<-read.csv("21_2010_features.csv")
twentytwo<-read.csv("22_2010_features.csv")
twentythree<-read.csv("22_2010_features.csv")
merge=rbind.fill(eight,twentyone,twentytwo,twentythree)
merge$position= as.character(merge$position)
merge$position [ merge$position  == "Defender" ] <- "1"
merge$position [ merge$position  == "Forward" ] <- "2"
merge$position [ merge$position  == "Goalkeeper" ] <- "3"
merge$position [ merge$position  == "Midfielder" ] <- "4"
merge$position [ merge$position  == "Player" ] <- "5"
merge$position= as.numeric(merge$position)
full<-merge[,c(1:14,45:64)]
full.1<-full[,c(15:length(tenpremier))]
full.1[is.na(full.1)]<-0
kmean.full<-kmeans(fix.full[,c(2,15:length(fix.full))],centers=5)
ggplot(fix.full,aes(x=position,y=num_touches))+geom_point(color=kmean.full$cluster,group=kmean.full$cluster)
fix.full[fix.full$full_name %in% top,]
fix.full[fix.full$full_name %in% top,26]<-1
colnames(fix.full)[26] <- "top"

defender<-fix.full[fix.full$position==1,]
mean.height<-mean(defender$height,na.rm=TRUE)
mean.weight<-mean(defender$weight,na.rm=TRUE)
defender[is.na(defender$height),3]<-mean.height
defender[is.na(defender$weight),4]<-mean.weight
