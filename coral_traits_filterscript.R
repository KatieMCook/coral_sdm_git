setwd("S:/Beger group/Katie Cook/Japan_data/coral_sdm")


library(dplyr)
library(reshape2)


coral_data<-read.csv('coraldata.csv')

traits<-read.csv('coralDB_2811.csv')


#make coral data long form 
coral_data<-melt(coral_data)

names(coral_data)<-c('genus', 'site', 'abundance')

names(coral_data)

coral_data$genus<-as.character(coral_data$genus)

coral_data$genus[coral_data$genus=="Caulastrea" ]<-"Caulastraea"
coral_data$genus[coral_data$genus=="Dipsastrea"]<-"Dipsastraea" 
coral_data$genus[coral_data$genus=="Scolymia" ]<-"Homophyllia"
coral_data$genus[coral_data$genus=="Merulinid_sp1"]<-'Merulina'
coral_data$genus[coral_data$genus=="Montastrea"]<-"Micromussa"


#now compare with trait data
traits$genus<-as.character(traits$genus)


filter<- which(traits$genus %in% coral_data$genus)

traits_filter<- traits[filter, ]

#one doesnt match (59/60) check

trait_gen<-unique(traits$genus)
data_gen<-unique(coral_data$genus)

not_in <-which (! data_gen %in%  trait_gen)

data_gen[19]   #"Duncanopssammia" - doesnt exist in japan???


#ok check if its a typo
unmatch<- which(! traits$genus %in% coral_data$genus)

unmatch<- traits[unmatch, ]


duncan<-coral_data %>% filter(genus== 'Duncanopssammia' )

write.csv(traits_filter, 'coral_traits_filter2911.csv')

