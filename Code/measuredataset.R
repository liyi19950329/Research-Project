install.packages("clusterProfiler")
library(infotheo)
library(plyr)
library(lubridate)
library(ggplot2)
library(stringr)
encounter <- read.csv("/Users/liyi/Desktop/day_in_the_life.csv")

#------how many participants in every unique location type------#
encounterForPie = data.frame(encounter[,1],encounter[,6])
colnames(encounterForPie)=c('subject','Location Type')
encounterForPie
encounterForPie_dup=encounterForPie[!duplicated(encounterForPie),]
encounterForPie_dup
nrow(encounterForPie_dup)
encounterForPie_dup_count <- count(encounterForPie_dup[,2])
encounterForPie_dup_count
sum(encounterForPie_dup_count[,2])
pie(encounterForPie_dup_count[,2],labels = encounterForPie_dup_count[,2],
    ,radius = 0.6,col = rainbow(length(encounterForPie_dup_count[,2])))
legend("bottomright", c("Arts and culuture","Car journey","Home","other","Private Transport",
                     "Public spaces","Public transport","Retail and hospitality","Sport and recreation",
                     "Study","Work"), cex = 0.8,fill = rainbow(length(encounterForPie_dup_count[,2])))

#------Duration at different location------#
encounter_durtation = data.frame(encounter[,1],encounter[,6],encounter[,11],encounter[,12])
colnames(encounter_durtation)=c('subject','location_type','come','go')
begins <- ymd_hms(encounter_durtation[,3])
ends <- ymd_hms(encounter_durtation[,4])
instantTable <- interval(begins,ends)
encounter_durtation$durtation <- time_length(instantTable,'minute')
boxplot(durtation ~ Location_Type, data = encounter_durtation, xlab = "Address",
        ylab = "Duration", main = "Duration at different location")
boxplot(durtation ~ Location_Type, data = encounter_durtation, xlab = "Address",
        ylab = "Duration", 
        main = "Duration at different location",
        col=terrain.colors(11),
        #col = c("aliceblue","gainsboro","beige","burlywood1","darkseagreen","darkslategrey","lavender","blue3","brown","darkslategrey","black"),
        names = c("","","","","","","","","","","")
)
legend("topright", title="Address",
     	 c("Arts and culuture","Car journey","Home","other","Private Transport",
     	   "Public spaces","Public transport","Retail and hospitality","Sport and recreation",
     	   "Study","Work"), fill=terrain.colors(11), horiz=FALSE) 

#------Duration at different location (by gender)------#
par(mfrow=c(1,2))
encounter_durtation_gender = data.frame(encounter[,1],encounter[,3],encounter[,6],encounter[,11],encounter[,12])
colnames(encounter_durtation_gender)=c('subject','gender','location_type','come','go')
begins_gender <- ymd_hms(encounter_durtation_gender[,4])
ends_gender <- ymd_hms(encounter_durtation_gender[,5])
instantTable_gender <- interval(begins_gender,ends_gender)
encounter_durtation_gender$durtation <- time_length(instantTable_gender,'minute')
encounter_durtation_gender
encounter_durtation_gender_1 <- encounter_durtation_gender[which(encounter_durtation_gender$gender== 1), ]
encounter_durtation_gender_2 <- encounter_durtation_gender[which(encounter_durtation_gender$gender== 2), ]
nrow(encounter_durtation_gender_1)
nrow(encounter_durtation_gender_2)

boxplot(durtation ~ location_type, data = encounter_durtation_gender_1, xlab = "Address",
        ylab = "Duration", 
        main = "Duration at different location(male)",
        col=terrain.colors(11),
        #col = c("aliceblue","gainsboro","beige","burlywood1","darkseagreen","darkslategrey","lavender","blue3","brown","darkslategrey","black"),
        names = c("","","","","","","","","","","")
)
boxplot(durtation ~ location_type, data = encounter_durtation_gender_2, xlab = "Address",
        ylab = "Duration", 
        main = "Duration at different location(male)",
        col=terrain.colors(11),
        #col = c("aliceblue","gainsboro","beige","burlywood1","darkseagreen","darkslategrey","lavender","blue3","brown","darkslategrey","black"),
        names = c("","","","","","","","","","","")
)
legend("topright", title="Address",
       c("Arts and culuture","Car journey","Home","other","Private Transport",
         "Public spaces","Public transport","Retail and hospitality","Sport and recreation",
         "Study","Work"), fill=terrain.colors(11), horiz=FALSE) 



nrow(count(encounter[,1]))
nrow(encounter)
unique(encounter[,1])
nrow(Freq <- tapply(encounter[,1], encounter[,6],nrow(unique(encounter[,1]))))

#extract trajectory patterns from processed data set 
day_in_the_life_trajectories <- read.csv("/Users/liyi/Desktop/day_in_the_life_drop_suffix_age_processed.csv")
day_in_the_life_trajectories_female<-day_in_the_life_trajectories[which(day_in_the_life_trajectories$Gender == 2), ]
day_in_the_life_trajectories_male<-day_in_the_life_trajectories[which(day_in_the_life_trajectories$Gender == 1), ]


ggplot(day_in_the_life_trajectories_female,aes(x=Trajectory_pattern)) + geom_bar(aes(color=factor(Age))) + coord_flip()

ggplot(day_in_the_life_trajectories_male,aes(x=Trajectory_pattern)) + geom_bar(aes(color=factor(Age))) + theme(axis.text.x=element_text(angle = 90, colour = "black"))













