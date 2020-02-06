#### plotting the distribution of traits in the groups for all taxa.


setwd("S:/Beger group/Katie Cook/Japan_data/coral_sdm")


library(sdmpredictors)
library(dismo)
library(rJava)
library(reshape2)
library(dplyr)
library(maptools)
library(mapdata)
library(ggmap)
library(sf)
library(rgeos)
library(vegan)
library(corrplot)
library(usdm)
library(RStoolbox)
library(sdm)
library(ggmap)
library(ggplot2)
library(raster)
library(randomForest)
library(stringr)
library(mgcv)
library(betareg)




#####Coral ####
coral_groups<-read.csv('coral_clust_1612.csv')

#remove group 4, 5, 6, 7 as they are only build with one genera 
coral_groups<-coral_groups[-c(which(coral_groups$group==4)),]
coral_groups<-coral_groups[-c(which(coral_groups$group==5)),]
coral_groups<-coral_groups[-c(which(coral_groups$group==6)),]
coral_groups<-coral_groups[-c(which(coral_groups$group==7)),]

coral_groups$group[coral_groups$group==8]<-4
coral_groups$group[coral_groups$group==9]<-5

#ok now plot with the right traits 
#traits used: colonialty, corralite width (max), depth lower, growth_form, water clarity, wave exposure, sexual system, larval development, growth rate

###first categorical traits . coloniality, growth form, water clarity, wave exposure, sexual system, larval development

### coloniality
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
colon = table(coral_groups$Coloniality, coral_groups$grou)
barplot(colon, main = "Coloniality", col=terrain.colors(length(rownames(colon))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(colon))), legend = rownames(colon))

### growth form
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
growth = table(coral_groups$Growth.form.typical, coral_groups$group)
barplot(growth, main = "Growth Form", col=terrain.colors(length(rownames(growth))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(growth))), legend = rownames(growth))

### water clarity
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
clarity = table(coral_groups$Water.clarity.preference, coral_groups$group)
barplot(clarity, main = "Water Clarity Preference", col=terrain.colors(length(rownames(clarity))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(clarity))), legend = rownames(clarity))

### wave exposure 
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
wave = table(coral_groups$Wave.exposure.preference, coral_groups$group)
barplot(wave, main = "Wave Exposure", col=rainbow(length(rownames(wave))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = rainbow(length(rownames(wave))), legend = rownames(wave))

### sexual system  
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
sexs = table(coral_groups$Sexual_system, coral_groups$group)
barplot(wave, main = "Sexual System", col=terrain.colors(length(rownames(sexs))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(sexs))), legend = rownames(sexs))


### larval development 
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
larv = table(coral_groups$larval_development, coral_groups$group)
barplot(larv, main = "Larval Development", col=terrain.colors(length(rownames(larv))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(larv))), legend = rownames(larv))


## Now do numerical traits with boxplots: corralite width, depth, growth rate
er = par(mfrow = c(2,2), pty = "s", mai=c(0.5,0.5,0.5,0.5))
boxplot(coral_groups$Corallite.width.maximum ~ coral_groups$group, main = "Corallite Width Max")
boxplot(coral_groups$Depth.lower ~ coral_groups$group, main = "Depth Lower")
boxplot(coral_groups$growth_rate ~ coral_groups$group, main = "Growth Rate")















