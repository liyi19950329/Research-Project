install.packages("ggplot2")
library(infotheo)
library(plyr)
library(lubridate)
library(ggplot2)
library(stringr)
install.packages("gdata")
library(gdata)
install.packages("rmarkdown")
install.packages("caTools")
getOption('repo')
install.packages('Rcpp')

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


###################larger data set##################
encounter <- read.csv("/Users/liyi/Desktop/RP/day_in_the_life.csv")
encounter
#how many unique subjects in this data set
index <- duplicated(encounter[,1])
encounter_subset <- encounter[!index,]
(nrow(encounter_subset))
encounter_subset

#How many males / females are in the data set?
# # The number of male in this dataset
(nrow(encounter_subset[encounter_subset$Gender == 1,]))
#The number of female in this dataset
(nrow(encounter_subset[encounter_subset$Gender == 2,]))
# hist(encounter_subset$Gender,xlab = "gender",breaks = seq(0,2,1))

# Age distribution for all of the subjects
par(mfrow=c(2,1),mar=c(4,6,4,4))
## mean and sd of age
meanage = mean(encounter_subset$Age,rm.na = TRUE)
sdage = sd(encounter_subset$Age)
## plots of raw data of age
boxplot(encounter_subset$Age,horizontal = TRUE,xlab = "age")
## histgram
hist(encounter_subset$Age,xlab = "age",ylab = "freq",main = "age distribution")
# hist(encounter_subset$Age,xlab = "age",ylab = "freq",main = "age distribution",freq = FALSE)
# curve(dnorm(x, 
#             mean=meanage, 
#             sd=sdage), 
#       add=TRUE, 
#       col="darkblue", 
#       lwd=2)
## Plot distribution
# age_distribution <- dnorm(encounter_subset$Age, mean(encounter_subset$Age), sd = sd(encounter_subset$Age))
# plot(encounter_subset$Age,age_distribution, xlab = "age")
## The summary of age distribution
summary(encounter_subset$Age)

# Age distribution bases on gender

boxplot(Age~Gender,data = encounter_subset,xlab = "gender",ylab = "age",main = "age~gender")
plot(Age~Gender,data = encounter_subset,xlab = "gender",ylab = "age")

hist(encounter_subset$Age[which(encounter_subset$Gender==1)],xlab = "age", main = "Age distribution of male")
hist(encounter_subset$Age[which(encounter_subset$Gender==1)],freq = FALSE, xlab = "age", main = "Age distribution of male")
curve(dnorm(x,
            mean = mean(encounter_subset$Age[which(encounter_subset$Gender==1)]),
            sd = sd(encounter_subset$Age[which(encounter_subset$Gender==1)])),
      add = TRUE)

hist(encounter_subset$Age[which(encounter_subset$Gender==2)],xlab = "age", main = "Age distribution of female")
hist(encounter_subset$Age[which(encounter_subset$Gender==2)],freq = FALSE, xlab = "age", main = "Age distribution of female")
curve(dnorm(x,
            mean = mean(encounter_subset$Age[which(encounter_subset$Gender==2)]),
            sd = sd(encounter_subset$Age[which(encounter_subset$Gender==2)])),
      add = TRUE)
## The summary of age based on gender
summary(encounter_subset$Age[which(encounter_subset$Gender==1)])
summary(encounter_subset$Age[which(encounter_subset$Gender==2)])


# The distribution of the number of locations visited by each study participant
num_of_locations <- data.frame(num = numeric(0))
for(subject in encounter_subset$Subject){
  #print(subject)
  num_of_locations <- rbind(num_of_locations, nrow(encounter[which(encounter$Subject == subject), ]))
  #nrow(encounter[which(encounter$Subject == subject), ])
}
nrow(num_of_locations)
par(mfrow=c(2,1))
hist(num_of_locations$X1L,xlab = "number of locations",main = "distribution of number of locations")
hist(num_of_locations$X1L,freq = FALSE,xlab = "number of locations",main = "distribution of number of locations")
curve(dnorm(x,
            mean = mean(num_of_locations$X1L),
            sd = sd(num_of_locations$X1L)),
      add = TRUE)

boxplot(num_of_locations$X1L,horizontal = TRUE)
stripchart(num_of_locations$X1L)
par(mfrow=c(1,1))
num_of_locations_distribution <- dnorm(num_of_locations$X1L, mean=mean(num_of_locations$X1L), sd = sd(num_of_locations$X1L))
plot(num_of_locations$X1L,num_of_locations_distribution,xlab = "number of locations",main = "distribution of number of locations")

## number of locations based on gender


#What is the distribution of number of different *types* of locations visited by each study participant?
index <- duplicated(encounter[,c(1,6)])
encounter_subset_locationType <- encounter[!index,]
num_of_locationTypes <- data.frame(num = numeric(0))
for(subject in encounter_subset$Subject){
  num_of_locationTypes <- rbind(num_of_locationTypes, nrow(encounter_subset_locationType[which(encounter_subset_locationType$Subject == subject), ]))
}
par(mfrow=c(2,1))
hist(num_of_locationTypes$X1L,xlab = "number of location types",main = "distribution of number of location types")
hist(num_of_locationTypes$X1L,freq = FALSE,xlab = "number of location types",main = "distribution of number of location types")
curve(dnorm(x,
            mean = mean(num_of_locationTypes$X1L),
            sd = sd(num_of_locationTypes$X1L)),
      add = TRUE)

boxplot(num_of_locationTypes$X1L,horizontal = TRUE)
stripchart(num_of_locationTypes$X1L)
par(mfrow=c(1,1))
num_of_locationTypes_distribution <- dnorm(num_of_locationTypes$X1L, mean=mean(num_of_locationTypes$X1L), sd = sd(num_of_locationTypes$X1L))
plot(num_of_locationTypes$X1L,num_of_locationTypes_distribution)

# What is the distribution of times spent at locations?
encounter_duration <- read.csv("/Users/liyi/Desktop/RP/day_in_the_life.csv")
encounter_with_duration_subset <- encounter_duration[which(encounter_duration$Duration!="NULL"), ]
par(mfrow=c(2,1))
hist(as.numeric(as.character(encounter_with_duration_subset$Duration)),xlab = "duration",main = "distribution of duration")
hist(as.numeric(as.character(encounter_with_duration_subset$Duration)),freq = FALSE,xlab = "duration",main = "distribution of duration")
meanvalue <- mean(as.numeric(as.character(encounter_with_duration_subset$Duration)),na.rm=TRUE)
sdvalue <- sd(as.numeric(as.character(encounter_with_duration_subset$Duration)),na.rm=TRUE)
curve(dnorm(x,
            mean = meanvalue,
            sd = sdvalue),
      add = TRUE)

# encounter_with_duration_subset_single <- na.omit(encounter_with_duration_subset$Duration)
par(mfrow=c(1,1))
duration_location_distribution <- dnorm(na.omit(as.numeric(as.character(encounter_with_duration_subset$Duration))), mean = meanvalue, sd = sdvalue)
plot(na.omit(as.numeric(as.character(encounter_with_duration_subset$Duration))),duration_location_distribution,xlab = "duration",ylab = "distribution",main = "distribution of duration")

dbinom(2,10,0.4)+dbinom(3,10,0.4)+dbinom(4,10,0.4)+dbinom(5,10,0.4)+dbinom(6,10,0.4)+dbinom(7,10,0.4)+dbinom(8,10,0.4)+dbinom(9,10,0.4)
pbinom(9,10,0.4) - pbinom(1,10,0.4)
qnorm(0.5,3,2.9,lower.tail = FALSE)



########################analysis development########################
encounter <- read.csv("/Users/liyi/Desktop/RP/day_in_the_life.csv")
encounter
#how many unique subjects in this data set
index <- duplicated(encounter[,1])
encounter_subset <- encounter[!index,]
(nrow(encounter_subset))
encounter_subset

# Relationship between age and num of locations
age_range_rep <- encounter_subset$Age
i = 1
while(i<=length(age_range_rep)){
  age = age_range_rep[i]
  if(age>=18&&age<29)
  {
    age_range_rep[i] = "18-29"
  }
  else if(age>=30&&age<49){
    age_range_rep[i] = "30-49"
  }
  else if(age>=50&&age<59)
  {
    age_range_rep[i] = "50-59"
  }
  else if(age>=60&&age<69)
  {
    age_range_rep[i] = "60-69"
  }
  else if(age>=70)
  {
    age_range_rep[i] = "70+"
  }
  # else if(age>=50&&age<60)
  # {
  #   age_range_rep[i] = "50-60"
  # }
  # else if(age>=60&&age<70)
  # {
  #   age_range_rep[i] = "60-70"
  # }
  # else if(age>=70&&age<80)
  # {
  #   age_range_rep[i] = "70-80"
  # }
  # else if(age>=80&&age<90)
  # {
  #   age_range_rep[i] = "80-90"
  # }
  # else if(age>=90&&age<100)
  # {
  #   age_range_rep[i] = "90-100"
  # }
  else
  {
    age_range_rep[i] = "no age"
  }
  i = i+1
}

num_of_locations <- data.frame(num = numeric(0))
for(subject in encounter_subset$Subject){
  #print(subject)
  num_of_locations <- rbind(num_of_locations, nrow(encounter[which(encounter$Subject == subject), ]))
  #nrow(encounter[which(encounter$Subject == subject), ])
}
num_of_locations$age <- encounter_subset$Age
nrow(num_of_locations)
plot(num_of_locations$X1L~num_of_locations$age,xlab = "Age",ylab = "Number of locations")

num_of_locations$location_range <- num_of_locations$X1L
num_of_locations$location_range[num_of_locations$location_range > 12] <- 13
num_of_locations$location_range <- factor(num_of_locations$location_range,
                                          levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                                          labels = c("1","2","3","4","5","6","7","8","9","10","11","12","12+"))
num_of_locations$age_range <- age_range_rep
agelocation <- table(num_of_locations$location_range,num_of_locations$age_range)
lacationage <- table(num_of_locations$age_range,num_of_locations$location_range)
par(mar=c(5.1, 4.1, 4.1, 3.1), xpd=TRUE)
barplot(agelocation, main="Age range and locations",
        xlab="Age range", ylab = "How many locations has been to(Frequency)",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"),
        beside=TRUE)
text.legend=row.names(agelocation)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15,15,16,16),legend=text.legend,col=col2,bty="n",ncol = 1,,inset=c(-0.1,0))

barplot(agelocation, main="Age range and locations",
        xlab="Age range", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
        )
text.legend=row.names(agelocation)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.1,0))

barplot(lacationage, main="Age range and locations",
        xlab="Number of locations", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"),
        beside=TRUE)
text.legend=row.names(lacationage)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))


barplot(lacationage, main="Age range and locations",
        xlab="Number of locations", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
        )
text.legend=row.names(lacationage)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

agelocation_prop1 <- prop.table(agelocation,2)
barplot(agelocation_prop1, main="Proportion age range and locations",
        xlab="Age range", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(agelocation_prop1)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.25,0))

lacationage_prop1 <- prop.table(lacationage,2)
barplot(lacationage_prop1, main="Probabilities age range and locations",
        xlab="Locations", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(lacationage_prop1)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.1,0))


# Relationship between age and num of location types
index <- duplicated(encounter[,c(1,6)])
encounter_subset_locationType <- encounter[!index,]
num_of_locationTypes <- data.frame(num = numeric(0))
for(subject in encounter_subset$Subject){
  num_of_locationTypes <- rbind(num_of_locationTypes, nrow(encounter_subset_locationType[which(encounter_subset_locationType$Subject == subject), ]))
}
num_of_locationTypes
num_of_locationTypes$age <- encounter_subset$Age
plot(num_of_locationTypes$X1L~num_of_locationTypes$age,xlab = "Age",ylab = "Number of location types")

num_of_locationTypes$age_range <- age_range_rep

agetype <- table(num_of_locationTypes$X1L,num_of_locationTypes$age_range)
typeage <- table(num_of_locationTypes$age_range,num_of_locationTypes$X1L)
barplot(agetype, main="Age range and location types",
        xlab="Age range", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"),
        beside=TRUE)
text.legend=row.names(agetype)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(agetype, main="Age range and location types",
        xlab="Age range", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"))
text.legend=row.names(agetype)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(typeage, main="Age range and location types",
        xlab="Number of location types", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"),
        beside=TRUE)
text.legend=row.names(typeage)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(typeage, main="Age range and location types",
        xlab="Number of location types", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"))
text.legend=row.names(typeage)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

agetype_prop1 <- prop.table(agetype,2)
barplot(agetype_prop1, main="Probabilities age range and location types",
        xlab="Age range", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"))
text.legend=row.names(agetype_prop1)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.05,0))

typeage_prop <- prop.table(typeage,2)
barplot(typeage_prop, main="Probabilities age range and locations",
        xlab="Locations", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(typeage_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.1,0))

# relationship between gender and number of locations
num_of_locations$gender <- encounter_subset$Gender
num_of_locations$gender <- factor(num_of_locations$gender,
                                  levels = c(1,2),
                                  labels = c("Male","Female"))
genderlocation <- table(num_of_locations$gender,num_of_locations$location_range)
locationgender <- table(num_of_locations$location_range,num_of_locations$gender)

barplot(genderlocation,main = "Gender and num of locations", 
        xlab = "Locations", ylab = "Frequency",
        col = c("darkblue","red"),
        beside = TRUE)
text.legend=row.names(genderlocation)
col2 <- c("darkblue","red")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(locationgender,main = "Gender and num of locations", 
        xlab = "Locations", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"),
        beside = TRUE)
text.legend=row.names(locaitongender)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.1,0))

genderlocation_prop <- prop.table(genderlocation,2)
barplot(genderlocation_prop,main = "Gender and num of locations", 
        xlab = "Locations", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(genderlocation_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.07,0))

locationgender_prop <- prop.table(locationgender,2)
barplot(locationgender_prop,main = "Gender and num of locations", 
        xlab = "Locations",ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(locationgender_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.13,0))


# relationship between gender and number of location types
num_of_locationTypes$gender <- encounter_subset$Gender
num_of_locationTypes$gender <- factor(num_of_locationTypes$gender,
                                  levels = c(1,2),
                                  labels = c("Male","Female"))
gendertype <- table(num_of_locationTypes$gender,num_of_locationTypes$X1L)
typegender <- table(num_of_locationTypes$X1L,num_of_locationTypes$gender)

barplot(gendertype,main = "Gender and num of location Types", 
        xlab = "Location Types",ylab = "Frequency",
        col = c("darkblue","red"),
        beside = TRUE)
text.legend=row.names(gendertype)
col2 <- c("darkblue","red")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(typegender,main = "Gender and num of location Types", 
        xlab = "Location Types",ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"),
        beside = TRUE)
text.legend=row.names(typegender)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

gendertype_prop <- prop.table(gendertype,2)
barplot(gendertype_prop,main = "Gender and num of location Types", 
        xlab = "Location Types",ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(gendertype_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.07,0))

typegender_prop <- prop.table(typegender,2)
barplot(typegender_prop,main = "Gender and num of location Types", 
        xlab = "locationTypes",ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(typegender_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.03,0))

# unique location
drop_transportation <- encounter[which(encounter$Location_Type != "Private Transport (private or rented car, motorbike)"),]
drop_transportation_pri <- drop_transportation[which(drop_transportation$Location_Type != "Public Transport (train, tram, bus or taxi)"),]
drop_transportation_all <- drop_transportation_pri[which(drop_transportation_pri$Location_Type != "Car journey (respondent alone in the car)"),]
# based on (lat lon)
index <- duplicated(drop_transportation_all[,c(1,9,10)])
encounter_drop_transportation <- drop_transportation_all[!index,]

unique_location <- data.frame(num = numeric(0))
for(subject in encounter_subset$Subject){
  unique_location <- rbind(unique_location, nrow(encounter_drop_transportation[which(encounter_drop_transportation$Subject == subject), ]))
}
unique_location$subject <- encounter_subset$Subject
unique_location$age <- encounter_subset$Age
unique_location$gender <- encounter_subset$Gender
unique_location$age_range <- age_range_rep
boxplot(unique_location$X1L,main="boxplot for unique locations one has gone",ylab = "Num of locations")
hist(unique_location$X1L,main="histogram for unique locations one has gone",xlab = "Num of locations")

# Relationship: age and unique location
boxplot(unique_location$X1L~unique_location$age_range,ylab = "Num of unique locations")

plot(unique_location$X1L~unique_location$age,xlab = "Age", ylab = "Num of unique location")

ageunique <- table(unique_location$X1L,unique_location$age_range)
uniqueage <- table(unique_location$age_range,unique_location$X1L)
barplot(ageunique, main="Age range and location types",
        xlab="Age range", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"),
        beside=TRUE)
text.legend=row.names(ageunique)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(ageunique, main="age range and location types",
        xlab="Age range", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(ageunique)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(uniqueage, main="age range and location types",
        xlab="Unique location", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"),
        beside=TRUE)
text.legend=row.names(uniqueage)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(uniqueage, main="age range and location types",
        xlab="Unique location", ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray"))
text.legend=row.names(uniqueage)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

ageunique_prop1 <- prop.table(ageunique,2)
barplot(ageunique_prop1, main="Probabilities age range and unique location",
        xlab="Age range", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(ageunique_prop1)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.03,0))

uniqueage_prop <- prop.table(uniqueage,2)
barplot(uniqueage_prop, main="Probabilities age range and unique location",
        xlab="Unique locations", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(uniqueage_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.05,0))

# relationship: gender and num of unique location
boxplot(unique_location$X1L~unique_location$gender)
unique_location$gender <- factor(unique_location$gender,
                                      levels = c(1,2),
                                      labels = c("Male","Female"))
genderunique <- table(unique_location$gender,unique_location$X1L)
uniquegender <- table(unique_location$X1L,unique_location$gender)

barplot(genderunique,main = "Gender and num of unique location Types", 
        xlab = "Location types",ylab = "Frequency",
        col = c("darkblue","red"),
        beside = TRUE)
text.legend=row.names(genderunique)
col2 <- c("darkblue","red")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(uniquegender,main = "Gender and num of unique location", 
        xlab = "Location types",ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"),
        beside = TRUE)
text.legend=row.names(uniquegender)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

genderunique_prop <- prop.table(genderunique,2)
barplot(genderunique_prop,main = "Gender and num of unique location", 
        xlab = "Location", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(genderunique_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.07,0))

uniquegender_prop <- prop.table(uniquegender,2)
barplot(uniquegender_prop,main = "Gender and num of unique location", 
        xlab = "Location", ylab = "Probabilities",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk"))
text.legend=row.names(uniquegender_prop)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink","darkred","cornsilk")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(-0.03,0))

# The relationship between duration and different location types(plot)
encounter_duration <- encounter[which(encounter$Duration!="NULL"), ]
encounter_type_duration <- encounter[which(encounter$Location_Type != "NULL"),]
plot(encounter_type_duration$Duration ~ encounter_type_duration$Location_Type)

encounter_type_duration$Location_Type <- factor(encounter_type_duration$Location_Type,
                                  levels = c("Home","Private Transport (private or rented car, motorbike)",
                                             "Retail and hospitality (bars, cafes, shops, hair dressing, etc.) ",
                                             "Car journey (respondent alone in the car)",
                                             "Sport and recreation","Other ","Work",
                                             "Public spaces (parks, streets, stations, airports etc.)",
                                             "Study (Uni, school, kindergarden, childcare centre etc)",
                                             "Arts and culture (cinema, library, gigs, museum, theatre etc.) ",
                                             "Public Transport (train, tram, bus or taxi)"),
                                  labels = c("H","PriT",
                                             "RT",
                                             "CJ",
                                             "SR","Other","Work",
                                             "PS",
                                             "Study",
                                             "AC",
                                             "PubT"))
plot(encounter_type_duration$Duration ~ encounter_type_duration$Location_Type)
Location_Type <- data.frame(sapply(encounter_type_duration$Location_Type, function(x) { if(is.factor(x)) {
  as.character(x)
} else {
  x
}
}))

Duration <- data.frame(sapply(encounter_type_duration$Duration, function(x) { if(is.factor(x)) {
  as.numeric(as.character(x))
} else {
  x
}
}))
encounter_type_duration$Location_Type <- Location_Type$sapply.encounter_type_duration.Location_Type..function.x...
encounter_type_duration$Duration <- Duration$sapply.encounter_type_duration.Duration..function.x...
boxplot(encounter_type_duration$Duration ~ encounter_type_duration$Location_Type, xlab = "location types",ylab = "Duration of staying at on different location type(mins)")
plot(encounter_type_duration$Duration ~ encounter_type_duration$Location_Type)

encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 0 & encounter_type_duration$Duration < 60)] <- 1
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 60 & encounter_type_duration$Duration < 120)] <- 2
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 120 & encounter_type_duration$Duration < 180)] <- 3
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 180 & encounter_type_duration$Duration < 240)] <- 4
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 240 & encounter_type_duration$Duration < 300)] <- 5
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 300 & encounter_type_duration$Duration < 360)] <- 6
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 360 & encounter_type_duration$Duration < 420)] <- 7
encounter_type_duration$Duration[which(encounter_type_duration$Duration >= 420)] <- "more than 7"

type_duration <- table(encounter_type_duration$Location_Type,encounter_type_duration$Duration)
duration_type <- table(encounter_type_duration$Duration,encounter_type_duration$Location_Type)

barplot(type_duration,main = "Location type and duration", 
        xlab = "Duration",ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink"),
        beside = TRUE)
text.legend=row.names(type_duration)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(type_duration,main = "Location type and duration", 
        xlab = "Duration",ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink"))
text.legend=row.names(type_duration)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3","gray","black","pink")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(duration_type,main = "Location type and duration", 
        xlab = "Location type",ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3"),
        beside = TRUE)
text.legend=row.names(duration_type)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))

barplot(duration_type,main = "Location type and duration", 
        xlab = "Location type",ylab = "Frequency",
        col=c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3"))
text.legend=row.names(duration_type)
col2 <- c("darkblue","red","orange","yellow","green","blue","purple","darkslategray3")
legend("topright",pch=c(15),legend=text.legend,col=col2,bty="n",ncol = 1,inset=c(0.1,0))








