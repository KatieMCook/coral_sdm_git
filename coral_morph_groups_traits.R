####ECHINO TRAIT GROUP CATEGORIES#########
setwd("S:/Beger group/Katie Cook/Japan_data/coral_sdm_git")
library(dplyr)

traits<-read.csv('coral_clust_2801.csv')

coral_groups<-traits

coral_groups$Growth.form.typical<-as.character(coral_groups$Growth.form.typical)

coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'branching_closed']<-'branching'
coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'corymbose']<-'branching'
coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'digitate']<-'branching'
coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'hispidose']<-'branching'

traits<-coral_groups

traits$Growth.form.typical<- as.factor(traits$Growth.form.typical)

towrite<- traits[,c(3,12)]

write.csv(towrite, 'coral_groups_final.csv')

### categorical traits first


#wave exposure
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
food = table(traits$Wave.exposure.preference, traits$Growth.form.typical)
barplot(food, main = "Wave Exposure", col=terrain.colors(length(rownames(food))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(food))), legend = rownames(food))

#sexual system
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
food = table(traits$Sexual_system, traits$Growth.form.typical)
barplot(food, main = "Sexual System", col=terrain.colors(length(rownames(food))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(food))), legend = rownames(food))

#larval development
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
food = table(traits$larval_development, traits$Growth.form.typical)
barplot(food, main = "Larval Development", col=terrain.colors(length(rownames(food))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(food))), legend = rownames(food))


## First, do numerical traits with boxplots:
er = par(mfrow = c(2,2), pty = "s", mai=c(0.5,0.5,0.5,0.5))
boxplot(traits$Corallite.width.maximum ~ traits$Growth.form.typical, main = "Corralite width")
boxplot(traits$growth_rate ~ traits$Growth.form.typical, main = "Growth Rate")
boxplot(traits$Depth.lower~ traits$Growth.form.typical, main = "Depth Range")
