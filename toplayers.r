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
