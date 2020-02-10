
setwd("S:/Beger group/Katie Cook/Japan_data/coral_sdm_git")


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

coral_groups<-read.csv('coral_clust_2801.csv')

coral_data<-read.csv('coraldata.csv')

#make into long from df
coral_data<-melt(coral_data)

names(coral_data)<-c('genus', 'site', 'abundance')

#split into site and transect
coral_data$site<-as.character(coral_data$site)

coral_data$site_all<- substr(coral_data$site, 1, nchar(coral_data$site)-12)

coral_data<-coral_data[,-2]

names(coral_data)<-c('genus', 'abundance', 'site')

#now make sure the names are clean
unique(coral_data$site)

coral_data$site[coral_data$site=="Iriomote_S1_" ]<-"Iriomote_S1"
coral_data$site[coral_data$site=="Iriomote_S2_" ]<-"Iriomote_S2"
coral_data$site[coral_data$site== "Iriomote_S3_"]<-"Iriomote_S3"
coral_data$site[coral_data$site=="Iriomote_S4_" ]<-"Iriomote_S4"
coral_data$site[coral_data$site=="CapeSata_S1_Outside_" ]<-"Cape_Sata_Outside"
coral_data$site[coral_data$site== "Kochi_SW_S3_"]<- "Kochi_SW_S3"
coral_data$site[coral_data$site=="TosaBay_S2_" ]<-"TosaBay"
coral_data$site[coral_data$site=="Kushimoto_S1_2"  ]<-"Kushimoto_S1" 
coral_data$site[coral_data$site== "Kushimoto_S2_2"]<- "Kushimoto_S2"
coral_data$site[coral_data$site=="Izu_S1_" ]<-"Izu_S1"
coral_data$site[coral_data$site== "Izu_S2_" ]<-"Izu_S2"
coral_data$site[coral_data$site==  "Tateyama_S1_" ]<-"Tateyama" 

#ok now take average of transects
coral_av<- coral_data %>% group_by(genus, site) %>% summarise(abundance=mean(abundance))

#now add lat lon on 
latlon<-read.csv('coralLatlon.csv')

latlon<-latlon[,c(1,3,4)]
names(latlon)<-c('site', 'lat', 'lon')

latlon$site<-as.character(latlon$site)

coral_data<-left_join(coral_av, latlon, by='site')

#ok now join group 
#coral_groups<-coral_groups[,c(3,34)]  #k=12

coral_data$genus<-as.character(coral_data$genus)
coral_groups$genus<-as.character(coral_groups$genus)

data<-unique(coral_data$genus)
groups<-unique(coral_groups$genus)

missing_frdata<-which(! data %in% groups)
missing_frgroups<-which(! groups %in% data)

data[missing_frdata]
groups[missing_frgroups]



#change the genus names for those that have changes
coral_data$genus[coral_data$genus=="Caulastrea" ]<-"Caulastraea"
coral_data$genus[coral_data$genus=="Dipsastrea"]<-"Dipsastraea" 
coral_data$genus[coral_data$genus=="Scolymia" ]<-"Homophyllia"
coral_data$genus[coral_data$genus=="Merulinid_sp1"]<-'Merulina'
coral_data$genus[coral_data$genus=="Montastrea"]<-"Micromussa"

#this one uis missing from db? just remove those for now
rm<-which(coral_data$genus=="Duncanopssammia")
coral_data<-coral_data[-c(rm),]

#make the groups the growth form?
unique(coral_groups$Growth.form.typical)

coral_groups$Growth.form.typical<-as.character(coral_groups$Growth.form.typical)

coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'branching_closed']<-'branching'
coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'corymbose']<-'branching'
coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'digitate']<-'branching'
coral_groups$Growth.form.typical[coral_groups$Growth.form.typical== 'hispidose']<-'branching'

unique(coral_groups$Growth.form.typical)

coral_groups<-coral_groups[c(3,12)]


#ok now merge
names(coral_groups)<- c('genus', 'group')


coral_group_abun<-left_join(coral_data, coral_groups, by='genus')

unique(coral_group_abun$group) #no na good

#now summarise by group
coral_group_abun<-coral_group_abun %>% group_by(site, lat, lon, group) %>% summarise(abundance=sum(abundance))

coral_group_abun$group<-as.factor(coral_group_abun$group)





#ok now plot abundance by lat
ggplot(coral_group_abun, aes(x=lat, y=abundance, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=TRUE)+
  theme_bw()+
  labs(x='Latitude', y='Percentage Cover')+
  facet_wrap(~group, scales='free_y')

#   +facet_wrap(~group, scales='free_y')  #reductions in % cover over lat


#work out proportion of community
cgroup_site_prop<- coral_group_abun %>% group_by(site, lat, lon) %>% mutate(total_abun= sum(abundance)) %>% mutate(prop= abundance/total_abun)

#plot
ggplot(cgroup_site_prop, aes(x=lat, y=prop, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=TRUE)+
  facet_wrap(~group, scales="free_y") 
 
#rename groups with number
coral_group_abun$group<-as.numeric(coral_group_abun$group)

#can't see now so split into individual group data
for (i in 1: length(unique(coral_group_abun$group))){
  filter_group<-filter(coral_group_abun, group==i) 
  assign(paste0 ('cgroup_', i), filter_group)
}

hist(cgroup_1$abundance)

#ok now load env. layers
#read in Japan if starting ###### START HERE 
japan_outline<-readOGR('plotting/japan_outline.shp')

#check
plot(japan_outline)


##explore the data 
marine_layers<- list_layers(marine=TRUE)


#list layers from bio-oracle
layers.bio2 <- list_layers( datasets="Bio-ORACLE" ) 

#explore future layers 
future_layers<-list_layers_future( marine = TRUE)

#extract data 
current_preds<-load_layers(c('BO_sstmin','BO2_curvelmax_ss', 'BO2_salinitymean_ss', 'BO2_lightbotltmax_bdmin'))
res(current_preds)
plot(current_preds)

chlo<-load_layers('BO2_chlomean_ss')
res(chlo)
plot(chlo)

plot(current_preds[[3]])


#get my extent to crop


# Determine geographic extent of our data
max.lat <- ceiling(max(latlon$lat)+0.5)
min.lat <- floor(min(latlon$lat)-0.5)
max.lon <- ceiling(max(latlon$lon)+0.5)
min.lon <- floor(min(latlon$lon)-0.5)
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#crop the data to extent and combine
current_preds<-crop(current_preds, geographic.extent)

#plot
plot(current_preds, main=c(1,1,1,'Bottom Light'))

#crop chloro
chlo<-crop(chlo, geographic.extent)

plot(chlo)

current_preds<-stack(current_preds, chlo)

plot(current_preds, main=c('SST', 'Current Velocity', 'Salinity', 'Bottom Light', 'Chlorophyll'), mai = c(1, 0.1, 0.1, 0.1))



#extract future 
#RCP 85 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP85')

RCP85_2050<-load_layers(c(c('BO2_RCP85_2050_tempmin_ss','BO2_RCP85_2050_curvelmean_bdmin',
                            'BO2_RCP85_2050_salinitymean_ss')))

light<-load_layers('BO2_lightbotltmax_bdmin')
chlo_future<-load_layers('BO2_RCP85_2050_chlomean_ss')

#now crop
RCP85_2050<-crop(RCP85_2050, geographic.extent)
light<-crop(light, geographic.extent)

chlo_future<-crop(chlo_future, geographic.extent)

RCP85_2050<-stack(RCP85_2050, light, chlo_future)

#RCP 26 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP26')

RCP26_2050<-load_layers(c(c('BO2_RCP26_2050_tempmin_ss','BO2_RCP26_2050_curvelmean_bdmin',
                            'BO2_RCP26_2050_salinitymean_ss')))

chlo_future26<-load_layers('BO2_RCP26_2050_chlomean_ss')

#RCP26
par(mfrow=c(3,2))
RCP26_2050<-crop(RCP26_2050, geographic.extent)
chlo_future26<-crop(chlo_future26, geographic.extent)

RCP26_2050<-stack(RCP26_2050, light, chlo_future26)

plot(RCP26_2050)




#now predict area
#read in predict area
predict_area<-readOGR('plotting/japan_predictarea.shp')

par(mfrow=c(1,1))
plot(predict_area)

predict_area<-aggregate(predict_area)

plot(predict_area)

crs(predict_area)

plot(japan_outline)

plot(predict_area, add=TRUE)

crs(predict_area)<-('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

crs(predict_area)


#ok good

crs(predict_area)
crs(japan_outline)

#mask with the area

plot(current_preds[[1]])

area_pred<-mask(current_preds, predict_area)

plot(area_pred[[1]])

plot(japan_outline, add=TRUE)

future_preds<-mask(RCP85_2050, predict_area)

RCP85_2050<-mask(RCP85_2050, predict_area)
RCP26_2050<-mask(RCP26_2050, predict_area)


plot(RCP85_2050)
plot(RCP26_2050)


##now run loop for alllllllll ----
rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}


#extract the environmental values at lat lons 

rm(cgroup_site_prop)
group_list<-lapply(ls(pattern="cgroup_"),get)

extract_km<-function(data){
  latlon<-data.frame(lon=data$lon, lat=data$lat)
  extract1<-as.data.frame(extract(current_preds, latlon))
  extract1$lat<-latlon$lat
  extract1$lon<-latlon$lon
  extract1$abundance<-data$abundance
  extract1
  return(extract1)
}

extract_all<-lapply(group_list, extract_km)

View(extract_all[[1]])




#check for zeros #hmm zeros, transform?

extract_all[[1]]$abundance
extract_all[[2]]$abundance
extract_all[[3]]$abundance
extract_all[[4]]$abundance
extract_all[[5]]$abundance
extract_all[[6]]$abundance

#betareg between 0 and 1 so make data a proportion 

divide_func<- function(data){
 data$abundance<-data$abundance/100
  return(data)
}

extract_all<-lapply(extract_all, divide_func)

#make it all with 0.0001 added for betareg

add_func<- function(data){
  data$abundance<-data$abundance + 0.0001
  return(data)
}
extract_all<-lapply(extract_all, add_func)



#plot abundances
par(mfrow=c(3,3))
par(mar=c(2,2,2,2))

for (i in 1:length(extract_all)){
  hist(extract_all[[i]]$abundance, main=i)
}

par(mfrow=c(3,3))
for (i in 1:length(extract_all)){
  hist((extract_all[[i]]$abundance)^(1/3), main=i)
}

par(mfrow=c(3,3))
for (i in 1:length(extract_all)){
  hist(log(extract_all[[i]]$abundance), main=i)
}


#third root transform group 1,2 and 4
#4th root transform group 2

#try 3rd root transforming group 1, 2 and 4 and just running lm models with that 
#trans1<-extract_all[[1]]
#trans2<-extract_all[[2]]
#trans3<-extract_all[[4]]

#trans1$abundance<- ceiling(trans1$abundance)
#trans2$abundance<-ceiling(trans2$abundance)
#trans3$abundance<-ceiling(trans3$abundance)

#extract_all<-list(trans1, trans2, trans3)


#test the individual models 
library(MASS)
testm1<- betareg(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss, data=extract_all[[2]])
summary(testm1)


extract_all[[2]]$abundance

#residual deviance/ dfs. = dispersion
0.12969/ 21

qqnorm(resid(testm1))

#now beta/ binomial?

#create models 
#now model in loop with 15% test 85% training (n=5)

library(betareg)
library(MASS)
library(gamlss)
library(ggfortify)



models_all<- function(data){
  
  rmse_all<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, RFP=1)  
  
  
  for (i in 1:100){
    tryCatch( {
      
    #define test and training data
    test<- data[sample(nrow(data), size=5, replace=FALSE),]  
    train<- data[(! row.names(data) %in% row.names(test)), ]
    
    obvs<-test$abundance
    
    
    #make models (with stepwise beta regression as bounded between 0-1)
    
    ########GLM
    glm1<- betareg(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss, link='logit', data=train)
    
    #extract abundance 

    abundance<-train$abundance

    abundance<-as.data.frame(abundance)

    
 
    #make df for loop to fill 
    

    glm_step_table<-data.frame(summary(glm1)$coefficients )

    glm_step_table<-glm_step_table[-1,]
    
    out_varib<-row.names(glm_step_table[glm_step_table[,4]>=0.1,])


    #set up formula to change 
    form<-formula(paste(abundance, "~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + BO2_lightbotltmax_bdmin", sep=""))

    #run step loop 
    
    for(g in out_varib)
      
   
    {
 
      g_temp<-g

      if(g_temp=="BO_sstmin"){form_g1<-update(form, ~. -(BO_sstmin))}
      if(g_temp=="BO2_curvelmax_ss"){form_g1<-update(form, ~. -(BO2_curvelmax_ss)) }
      if(g_temp=="BO2_salinitymean_ss"){form_g1<-update(form, ~. -(BO2_salinitymean_ss))}
      if(g_temp=="BO2_chlomean_ss"){form_g1<-update(form, ~. -(BO2_chlomean_ss))}
      if(g_temp=="BO2_lightbotltmax_bdmin"){form_g1<-update(form, ~. -(BO2_lightbotltmax_bdmin))}

      glm2 <-betareg(form_g1, data=train, link='logit' , na.action=na.omit)

      if(AIC(glm2)<=AIC(glm1)){form<-form_g1

      print(paste(g, " dropped", sep=""))}

    }

    glm1 <-betareg(form, data=train, link='logit' , na.action=na.omit)
    

    summary(glm1)

    ##GAM 
    gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5) + s(BO2_lightbotltmax_bdmin, k=5),
         family=betar(link='logit')  , data=train) #family=nb(theta=NULL, link="log")    family=binomial()
    
    #extract abundance 
    abundance<-train$abundance
    abundance<-as.data.frame(abundance)
    
    
    #make df for loop to fill 
    gam_step_table<-data.frame(summary(gam1)$s.table)
    out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
    
    #set up formula to change 
    form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5)", sep=""))
    
    #run step loop 
    for(g in out_varib)
    {
      g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
      
      if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5))}
      if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
      if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
      if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
      if(g_temp=="s(BO2_lightbotltmax_bdmin, k=5)"){form_g1<-update(form, ~. -s(BO2_lightbotltmax_bdmin, k=5))}
      
      gam2 <-gam(form_g1, data=train,  family = betar(link='logit'), na.action=na.omit)
      
      if(AIC(gam2)<=AIC(gam1)){form<-form_g1
      print(paste(g, " dropped", sep=""))}
    }
    
    gam1 <-gam(form, data=train,  family=betar(link='logit'), na.action=na.omit)
    
    summary(gam1)
    
    #RF
    rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss +BO2_lightbotltmax_bdmin, data=train, ntree=300,   importance=TRUE   )
    
    
    #predict models for test
    prglm<-   predict( glm1, test, type='response')
    prgam<-predict(gam1, test, type='response')
    prRF<-predict(rf1, test, type='response')
    
    
    
    
    #now rmse for all
    rmse_all[i,1]<-rmse(obvs, prglm)
    rmse_all[i,2]<-rmse(obvs, prgam)
    rmse_all[i,3]<-rmse(obvs, prRF)
    
    #now pearsons correlation for all 
    rmse_all[i,4]<-cor(obvs, prglm, method = c("pearson"))
    rmse_all[i,5]<-cor(obvs, prgam, method = c("pearson"))
    rmse_all[i,6]<-cor(obvs, prRF, method = c("pearson"))
   
    
    } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
    print(i) 
  }
  
  return(rmse_all) 
  
  
}


#standardise co-efficients 
##testing beta reg with zero inflation ----

library(gamlss)

zinfl<-gamlss(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss, family='BEINF0', data=extract_all[[8]], trace=F)

summary(zinfl)

#can't use predict on spatial so do it manually

area_pred_df<-extract(area_pred, c(1:ncell(area_pred)))  #extract the values for each cell 
area_pred_df<-as.data.frame(area_pred_df) #make dataframe

hist(area_pred_df$BO_sstmin)

head(area_pred_df)

area_pred_comp<- area_pred_df[complete.cases(area_pred_df),] #remove NA values (not the study area)


pr_zinfl<-predict(zinfl, newdata= area_pred_comp) #predict using new model


hist(pr_zinfl)

summary(pr_zinfl) # DOESN'T WORKKKKKK

#re-add the data to the NA df 
area_pred_comp$prediction<- pr_zinfl

area_pred_comp$rownames<-rownames(area_pred_comp)

area_pred_df$rownames<-rownames(area_pred_df)

#merge
area_pred_pr<-left_join(area_pred_df, area_pred_comp, by="rownames")

area_pred_pr<-area_pred_pr[,12] 

#now add these values back to raster 

#get just one layer out of rasterbrick
area<- subset(area_pred, 'BO_sstmin')
plot(area)

par(mfrow=c(1,1))
area_predict<- setValues(area, area_pred_pr)

par(mfrow=c(1,1))
plot(area_predict)

max(area_predict)

#Hmmm -300?!!! not working!!! why?!!!

#remove groups with too many zeros (gr 4,5,6 only present at 1 site)

more_abun<-list(extract_all[[1]], extract_all[[2]], extract_all[[3]], extract_all[[8]], extract_all[[9]])

#more_abun[[2]]$abundance[12]<-0.00001







allrmse<-lapply(extract_all, models_all)  

#this isn't working come back to this tomorrow! ----



#get averages as it didnt work 

average_km<-function(data){
  
  glm_av<- mean(data$glmR, na.rm=TRUE)
  gam_av<-mean(data$gamR, na.rm=TRUE)
  rf_av<-mean(data$rfR, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

averages<-lapply(allrmse, average_km)


averages.df<-data.frame(glm=1, gam= 1, rf=1)

for (i in 1: length(averages)){
  averages.df[i,]<-(averages[[i]])
}

#gam is bad so dont use


#averages pearsons 
average_p<-function(data){
  
  glm_av<- mean(data$glmP, na.rm=TRUE)
  gam_av<-mean(data$gamP, na.rm=TRUE)
  rf_av<-mean(data$RFP, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

average_pear<-lapply(allrmse, average_p)

averages_pear.df<-data.frame(glm=1, gam= 1, rf=1)

for (i in 1: length(average_pear)){
  averages_pear.df[i,]<-(average_pear[[i]])
}





average_pear[[1]]
average_pear[[2]]




#full model, predict and ensemble   #loop

for (i in 1:length(extract_all)) {
  
  ########GLM
  glm1<- betareg(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss, link='logit', data=extract_all[[i]])
  
  #extract abundance 
  
  abundance<-extract_all[[i]]$abundance
  
  abundance<-as.data.frame(abundance)
  
  
  
  #make df for loop to fill 
  
  
  glm_step_table<-data.frame(summary(glm1)$coefficients )
  
  glm_step_table<-glm_step_table[-1,]
  
  out_varib<-row.names(glm_step_table[glm_step_table[,4]>=0.1,])
  
  
  #set up formula to change 
  form<-formula(paste(abundance, "~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + BO2_lightbotltmax_bdmin", sep=""))
  
  #run step loop 
  
  for(g in out_varib)
    
    
  {
    
    g_temp<-g
    
    if(g_temp=="BO_sstmin"){form_g1<-update(form, ~. -(BO_sstmin))}
    if(g_temp=="BO2_curvelmax_ss"){form_g1<-update(form, ~. -(BO2_curvelmax_ss)) }
    if(g_temp=="BO2_salinitymean_ss"){form_g1<-update(form, ~. -(BO2_salinitymean_ss))}
    if(g_temp=="BO2_chlomean_ss"){form_g1<-update(form, ~. -(BO2_chlomean_ss))}
    if(g_temp=="BO2_lightbotltmax_bdmin"){form_g1<-update(form, ~. -(BO2_lightbotltmax_bdmin))}
    
    glm2 <-betareg(form_g1, data=extract_all[[i]], link='logit'  , na.action=na.omit)
    
    if(AIC(glm2)<=AIC(glm1)){form<-form_g1
    
    print(paste(g, " dropped", sep=""))}
    
  }
  
  glm_gr <-betareg(form, data=extract_all[[i]], link='logit',  na.action=na.omit)
  
  
  summary(glm1)
  
  
  
  
  
  #GAM
  gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5), 
            family=betar(link='logit'), data=extract_all[[i]])
  
  #extract abundance 
  abundance<-extract_all[[i]]$abundance
  abundance<-as.data.frame(abundance)
  
  
  #make df for loop to fill 
  gam_step_table<-data.frame(summary(gam1)$s.table)
  out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
  
  #set up formula to change 
  form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5)", sep=""))
  
  #run step loop 
  for(g in out_varib)
  {
    g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
    
    if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
    if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
    if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
    if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
    if(g_temp=="s(BO2_lightbotltmax_bdmin, k=5)"){form_g1<-update(form, ~. -s(BO2_lightbotltmax_bdmin, k=5))}
    
    gam2 <-gam(form_g1, data=extract_all[[i]],  family=betar(link='logit'), na.action=na.omit)
    
    if(AIC(gam2)<=AIC(gam1)){form<-form_g1
    print(paste(g, " dropped", sep=""))}
  }
  
  gam_gr<-gam(form, data=extract_all[[i]],  family=betar(link='logit'), na.action=na.omit)  
  
  #RF
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss + BO2_lightbotltmax_bdmin, data=extract_all[[i]], ntree=300, importance=TRUE   )
  
  assign(paste0('glm_gr', i), glm_gr)
  assign(paste0('gam_gr', i), gam_gr)
  assign(paste0('rf_gr', i), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr, type='response')
  pr_gam<-predict(area_pred, gam_gr, type='response')
  pr_rf<-predict(area_pred, rf_gr, type='response')    
  
  assign(paste0('pr_glm_gr', i), pr_glm)
  assign(paste0('pr_gam_gr', i), pr_gam)
  assign(paste0('pr_rf_gr', i), pr_rf)
  
  #make it so only the significant models are included 
  
  #make all the RMSE values 1/ themselves so that the larger errors become smaller proportions
  averages[[i]]<- averages[[i]]/((averages[[i]])^2)
  
  
  #extract p values of glm coefs 
  glm_pvals<- summary(glm_gr)$coefficients$mean[,4]
  #remove intercept column 
  glm_pvals<- glm_pvals[-1]
  
  #trues are 1 and false 0. if sum =length then they are all over 0.05 and this makes averages 0
  if (sum(glm_pvals > 0.05) == length(glm_pvals)){
    averages[[i]][,1]<-0
    average_pear[[i]][,1]<- 0
  }                        
  
  #now say if the RMSE is larger then the mean, proportion= 0
  if (averages.df[i, 1]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,1]<- 0 
    average_pear[[i]][,1]<-0
  }
  
  #now same for gam 
  gam_step_table<-data.frame(summary(gam_gr)$s.table)
  
  if (sum (gam_step_table$p.value > 0.05)== length(gam_step_table$p.value)) {
    averages[[i]][,2]<-0
    average_pear[[i]][,2]<-0
  }
  
  #if RMSE is larger then mean make it zero
  if (averages.df[i, 2]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,2]<- 0 
    average_pear[[i]][,2]<- 0
  }
  
  #finally for RF
  if (averages.df[i, 3]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,3]<- 0
    average_pear[[i]][,3]<-0
  }
  
  
  
  
  
  
  #make the ensemble model from RMSE and Pearsons proportions 
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,5]<-abs(average_pear[[i]][1,2])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,6]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  assign(paste0('prop_gr', i), props)
  
  ensemble<- ((pr_glm*props[1,1])+(pr_gam*props[1,2])+ (pr_rf*props[1,3]))
  
  assign(paste0('ensemble_gr', i), ensemble)
  
}

plot(ensemble_gr1)

#check model 
summary(glm_gr1)
summary(gam_gr1)
varImpPlot(rf_gr1)

summary(glm_gr2) #gr2 no
summary(gam_gr2)
varImpPlot(rf_gr2)


summary(glm_gr3) #gr3 no
summary(gam_gr3)
varImpPlot(rf_gr3)

summary(glm_gr4)
summary(gam_gr4)
varImpPlot(rf_gr4)


summary(glm_gr5) #gr 5no
summary(gam_gr5)
varImpPlot(rf_gr4)

summary(glm_gr6)
summary(gam_gr6)
varImpPlot(rf_gr6)









#plot ensembles

ens_list<-lapply(ls(pattern="ensemble_gr"),get)

par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(2,3))

for ( i in 1:length(ens_list)){
  plot(ens_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}

#now future RCP85
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)

#make sure the colnames match
names(RCP85_2050)<-names(current_preds)


for (i in 1:length(more_abun)) {
  
  pr_glm_fut<-predict(RCP85_2050, glm_list[[i]])
  pr_rf_fut<-predict(RCP85_2050, rf_list[[i+2]])
  
  assign(paste0('glm_fut_85', i), pr_glm_fut)
  assign(paste0('rf_fut_85', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_85_gr', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_85_gr1)

fut_ens85_list<-lapply(ls(pattern='fut_ensemble_85_gr'), get)


library(viridis)
pal<-viridis(option='plasma', n=40, direction=-1)


#plot
par(mfrow=c(1,2))

for (i in 1:length(fut_ens85_list)){
  plot(fut_ens85_list[[i]], col=pal, main=paste0('Group', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
  
}


#NOW RCP26!!!!
#make sure the colnames match
names(RCP26_2050)<-names(current_preds)
plot(RCP26_2050)


for (i in 1:length(more_abun)) {
  
  pr_glm_fut<-predict(RCP26_2050, glm_list[[i]])
  pr_rf_fut<-predict(RCP26_2050, rf_list[[i+2]])
  
  assign(paste0('glm_fut_26', i), pr_glm_fut)
  assign(paste0('rf_fut_26', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_26_gr', i), ensemble_fut)
  
  
  
}

fut_ens26_list<-lapply(ls(pattern='fut_ensemble_26_gr'), get)


#plot
par(mfrow=c(1,2))

for (i in 1:length(fut_ens26_list)){
  plot(fut_ens26_list[[i]], col=pal, main=paste0('Group', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
  
}


#now get difference between two and plot
#now get difference between two and plot
#before getting difference standardise abundance between zero and 1 ----

normalize <- function(x) {
  return ((x -  x@data@min)/ (x@data@max -  x@data@min))
}

fut_ens85_norm<- lapply(fut_ens85_list, normalize)

ens_list_norm<-lapply(ens_list, normalize)

fut_ens26_norm<-lapply(fut_ens26_list, normalize)





#now get difference between two and plot
#RCP85
for ( i in 1:length(fut_ens85_norm)){
  
  diff<- fut_ens85_norm[[i]] - ens_list_norm[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list85<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(3,3))
for (i in 1:length(dif_list85)){
  plot(dif_list85[[i]], main=paste0('Group ', i), col=pal)
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}

#RCP26
for ( i in 1:length(fut_ens26_norm)){
  
  diff<- fut_ens26_norm[[i]] - ens_list_norm[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list26<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(3,3))
for (i in 1:length(dif_list26)){
  plot(dif_list26[[i]], main=paste0('Group ', i), col=pal)
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}





#now buffer 30km from the coast and crop by predict area----
#crs(japan_outline) #in wgs project into something in km 
#crs(predict_area)

#japan_outline_proj<-spTransform(japan_outline, '+proj=utm +zone=54 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')

#crs(japan_outline_proj)
#par(mfrow=c(1,1))
#plot(japan_outline_proj)

#predict_area_proj<- spTransform(predict_area, '+proj=utm +zone=54 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
#crs(predict_area_proj)

#plot(predict_area_proj, add=TRUE, col='pink')

#crop this by predict area
#crop_outline<-gIntersection(japan_outline_proj, predict_area_proj)

#plot(crop_outline)

#buffer this by 30km 
#hmm doesn't work project back 

#outline_30k<- buffer(crop_outline, width=30000, dissolve=TRUE)
#plot(outline_30k, col='yellow')
#plot(crop_outline, col='pink', add=TRUE)


#dif list in different CRS so project back
#crs(dif_list85[[1]])

#crop_outline<- spTransform(crop_outline, ' +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

#outline_30k<-st_as_sf(outline_30k)
#plot(outline_30k)

#st_write(outline_30k, '30k_coastplot', 'outline30k.shp', driver='ESRI Shapefile')

buffer30k<-readOGR('30k_coastplot/outline30k.shp')
crs(buffer30k)

buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

crs(japan_outline)
#now mask dif 85 by buffer crop

for (i in 1:length(dif_list85)){
  dif_mask<- mask(dif_list85[[i]], buffer30k)
  assign(paste0('dif85_mask_gr', i), dif_mask)
}

dif_mask85<-lapply(ls(pattern='dif85_mask_gr'), get)

plot(dif_mask85[[1]])

par(mfrow=c(3,2))

for (i in 1:length(dif_mask85)){
  plot(dif_mask85[[i]])
}

#ok now extract the lat lons of the raster and plot 
#turn all the variables to points then get the coords of points

par(mfrow=c(1,1))
points<- rasterToPoints(dif_mask85[[2]])
plot(points)
coords<-as.data.frame(coordinates(points))
coords<-coords[,-3]

#extract from the coords
values<- extract(dif_list85[[2]], coords)

coords$values<-values

coords$slope<-coords$x * coords$y

ggplot(coords, aes(x=slope, y=values))+
  geom_point()+
  geom_smooth(method='lm')


#make it for all RCP85
for (i in 1:length(dif_mask85)){
  points<-rasterToPoints(dif_mask85[[i]])
  coords<-as.data.frame(coordinates(points))
  coords<-coords[-3]
  values<-extract(dif_list85[[i]], coords)
  coords$values<-values
  coords$slope<-coords$ x * coords$y
  assign(paste0('dif85_values_gr', i), coords)
}

dif85_values_all<-lapply(ls(pattern='dif85_values_gr'), get)


#group into 1 df
for (i in 1: length(dif85_values_all)){
  dif85_values_all[[i]]$group<-i
  dif85_values_all[[i]]$climate<-'RCP85'
}

View(dif85_values_all[[1]])

#now merge them all together 
dif85_all_df<-do.call(rbind, dif85_values_all)

dif85_all_df$group<-as.factor(dif85_all_df$group)

ggplot(dif85_all_df, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)
# facet_zoom(ylim=c(-2, 2))  #zooms in on the smaller ones 


#RCP 26

#now mask dif 85 by buffer crop

for (i in 1:length(dif_list26)){
  dif_mask<- mask(dif_list26[[i]], buffer30k)
  assign(paste0('dif26_mask_gr', i), dif_mask)
}

dif_mask26<-lapply(ls(pattern='dif26_mask_gr'), get)

plot(dif_mask26[[1]])

par(mfrow=c(3,3))

for (i in 1:length(dif_mask26)){
  plot(dif_mask26[[i]])
}

#ok now extract the lat lons of the raster and plot 
#turn all the variables to points then get the coords of points


for (i in 1:length(dif_mask26)){
  points<-rasterToPoints(dif_mask26[[i]])
  coords<-as.data.frame(coordinates(points))
  coords<-coords[-3]
  values<-extract(dif_list26[[i]], coords)
  coords$values<-values
  coords$slope<-coords$ x * coords$y
  assign(paste0('dif26_values_gr', i), coords)
}

dif26_values_all<-lapply(ls(pattern='dif26_values_gr'), get)


#group into 1 df
for (i in 1: length(dif26_values_all)){
  dif26_values_all[[i]]$group<-i
  dif26_values_all[[i]]$climate<-'RCP26'
}

View(dif26_values_all[[1]])

#now merge them all together 
dif26_all_df<-do.call(rbind, dif26_values_all)

dif26_all_df$group<-as.factor(dif26_all_df$group)


ggplot(dif26_all_df, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)
# facet_zoom(ylim=c(-2, 2))  #zooms in on the smaller ones 

#now merge both climate scenarios
dif_values_all<- rbind(dif85_all_df, dif26_all_df)
dif_values_all$climate<-as.factor(dif_values_all$climate)


ggplot(dif_values_all, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)+
  facet_wrap(~climate)

write.csv(dif_values_all, 'dif_values_all_coral_norm.csv')


















#now future 
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)

#make sure the colnames match
names(future_preds)<-names(current_preds)
plot(future_preds)


for (i in 1:length(more_abun)) {
  
  pr_glm_fut<-predict(future_preds, glm_list[[i]])
  pr_rf_fut<-predict(future_preds, rf_list[[i+2]])
  
  assign(paste0('glm_fut', i), pr_glm_fut)
  assign(paste0('rf_fut', i), pr_rf_fut)
  
  props<-data.frame(glm=1, rf=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]++averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_gr', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_gr2)

fut_ens_list<-lapply(ls(pattern='fut_ensemble_gr'), get)

#plot
par(mfrow=c(1,2))

for (i in 1:length(fut_ens_list)){
  plot(fut_ens_list[[i]])
}

#now get difference between two and plot
for ( i in 1:length(fut_ens_list)){
  
  diff<- fut_ens_list[[i]] - ens_list[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(1,2))
for (i in 1:length(dif_list)){
  plot(dif_list[[i]])
}


#ooookkkkk
#now group 3 and 4
install.packages('zoib')
library(zoib)
install.packages('rjags')
library(rjags)

#repeat models 
models_all<- function(data){
  
  rmse_all<-data.frame(glm=1, gam=1, rf=1)  
  
  
  for (i in 1:100){
    
    #define test and training data
    test<- data[sample(nrow(data), size=5, replace=FALSE),]  
    train<- data[(! row.names(data) %in% row.names(test)), ]
    
    obvs<-test$abundance
    
    #make models (with zero inflated beta regression as bounded between 0-1)
    #glm1<-zoib(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss|1|BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss,  data=train, zero.inflation = TRUE, one.inflation = FALSE)
    glm1<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +BO2_chlomean_ss, 
               family=binomial(), data=train)
    
    gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5) +s(BO2_lightbotltmax_bdmin, k=5),
              family=binomial  , data=train) #family=nb(theta=NULL, link="log")    family=binomial()
    rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss +BO2_lightbotltmax_bdmin, data=train, ntree=300,   importance=TRUE   )
    
    #predict models for test
    prglm<-predict( glm1, test)
    prgam<-predict(gam1, test)
    prRF<-predict(rf1, test)
    
    #now rmse for all
    rmse_all[i,1]<-rmse(obvs, prglm)
    rmse_all[i,2]<-rmse(obvs, prgam)
    rmse_all[i,3]<-rmse(obvs, prRF)
    
    
  }
  
  return(rmse_all) 
  
  
}

#3 and 4 have mostly zeros so can;t bootstrap??? #just deal with group 1 and two for now
less_abun<-list(extract_all[[3]], extract_all[[4]])


allrmse<-lapply(less_abun, models_all)  # ok worked, need to sort out group 3 and 4

#get averages as it didnt work 

average_km<-function(data){
  
  glm_av<- mean(data$glm)
  gam_av<-mean(data$gam)
  rf_av<-mean(data$rf)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

averages<-lapply(allrmse, average_km)

averages[[1]]
averages[[2]]
#gam is bad so dont use #this is pretty bad sort this out

#full model, predict and ensemble   #loop

for (i in 1:length(less_abun)) {
  
  glm_gr<-betareg(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss ,  data=more_abun[[i]])
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss, data=less_abun[[i]], ntree=300, importance=TRUE   )
  
  assign(paste0('glm_gr2', i), glm_gr)
  assign(paste0('rf_gr2', i), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr)
  pr_rf<-predict(area_pred, rf_gr)    
  
  assign(paste0('pr_glm_gr2', i), pr_glm)
  assign(paste0('pr_rf_gr2', i), pr_rf)
  
  props<-data.frame(glm=1, rf=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]++averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  assign(paste0('prop_gr2', i), props)
  
  ensemble<- ((pr_glm*props[1,1])+(pr_rf*props[1,2]))
  
  assign(paste0('ensemble_gr2', i), ensemble)
  
  
}

#plot ensembles

ens2_list<-list(ensemble_gr21, ensemble_gr22)

par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(1,2))

for ( i in 1:length(ens2_list)){
  plot(ens_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}

library(viridis)
pal<-viridis(option='plasma', direction=-1, n=40)

ens_list_all<-list(ensemble_gr1, ensemble_gr2, ensemble_gr21, ensemble_gr22)

par(mfrow=c(2,2))
for(i in 1:length(ens_list_all)){
  plot(ens_list_all[[i]], col=pal, main=paste0('Group ', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
  
}

#now future 
rm(glm_gr)

glm_list2<-list(glm_gr21, glm_gr22)

rm(rf_gr)

rf_list2<-list(rf_gr21, rf_gr22)

#make sure the colnames match
names(future_preds)<-names(current_preds)
plot(future_preds)


for (i in 1:length(less_abun)) {
  
  pr_glm_fut<-predict(future_preds, glm_list2[[i]])
  pr_rf_fut<-predict(future_preds, rf_list2[[i]])
  
  assign(paste0('glm_fut2', i), pr_glm_fut)
  assign(paste0('rf_fut2', i), pr_rf_fut)
  
  props<-data.frame(glm=1, rf=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_gr2', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_gr21)

fut_ens_list2<-list(fut_ensemble_gr21, fut_ensemble_gr22)


#plot
par(mfrow=c(1,2))

for (i in 1:length(fut_ens_list2)){
  plot(fut_ens_list2[[i]])
}

#now get difference between two and plot
for ( i in 1:length(fut_ens_list2)){
  
  diff<- fut_ens_list2[[i]] - ens2_list[[i]]
  assign(paste0('dif_gr2', i), diff)
}

dif_list2<-list(dif_gr21, dif_gr22)


dif_all<-list(dif_gr1, dif_gr2, dif_gr21, dif_gr22)


#now plot
par(mfrow=c(2,2))
par(mar=c(3,2,2,2))
for (i in 1:length(dif_all)){
  plot(dif_all[[i]], col=pal, main=paste0('Group ',i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}








#split into below zero and above zero ## all tropical
dif_list<-dif_all

for ( i in 1:length(dif_list)){
  layer<-dif_list[[i]]
  increase<-layer
  decrease<-layer
  increase[increase < 0]<-NA  
  decrease[decrease>0]<-NA
  assign(paste0('increase_gr', i),increase)
  assign(paste0('decrease_gr', i),decrease)
  
}

increase_list<-lapply(ls(pattern='increase_gr'), get)
decrease_list<-lapply(ls(pattern='decrease_gr'), get)

increase_stack<-stack(increase_list)
decrease_stack<-stack(decrease_list)

plot(increase_stack)
plot(decrease_stack)



#subtropical group; 2, 6,9
#tropical group: 1, 3, 4, 5, 7, 8

#split in tropical and subtropical
increase_trop_stack<-increase_stack
plot(increase_trop_stack)


decrease_trop_stack<-decrease_stack
plot(decrease_trop_stack)


#where changes the most? values between 0, 1 for increase 

library(sdmvspecies)

rescale_increase_trop<-rescale(increase_trop_stack)
plot(rescale_increase_trop)

trop_increase<-sum(rescale_increase_trop)


plot(trop_increase)
plot(japan_outline, add=TRUE)

#flip the decreases so that highest rates of change are highest(positive values)

flip<-function(x){
  x/-1
}
r <- calc(s, fun=sum)


decrease_trop_stack<-flip(decrease_trop_stack)
decrease_trop_stack<-stack(decrease_trop_stack)

rescale_decrease_trop<-rescale(decrease_trop_stack)

trop_decrease<-(sum(rescale_decrease_trop)/-1)

plot(trop_decrease) 

trop_increase<-rescale(trop_increase)
trop_decrease<-(rescale(trop_decrease))/-1

library(RColorBrewer)
library(viridis)

#pal <- viridis(n=20, option='plasma' )
pal<-viridis(option='plasma', n=40, direction=-1)
palm<-viridis(option='plasma', n=40)


par(mfrow=c(1,2))
plot(trop_increase, main='Tropical FG Increase', col=pal)
plot(japan_outline, add=TRUE, col='light grey', border='light grey')
box()
plot(trop_decrease, main='Tropical FG Decrease', col=palm)
plot(japan_outline, add=TRUE, col='light grey', border='light grey')
box()

par(mfrow=c(1,2))
plot(trop_decrease, col=pal(50), main='Tropical FG Decrease')
plot(japan_outline, add=TRUE)
plot(subtrop_decrease, col=pal(50), main='Subtropical FG Decrease')
plot(japan_outline, add=TRUE)

writeRaster(trop_increase, 'change_hotspots/coral_trop_increase.tif', format='GTiff')

writeRaster(trop_decrease, 'change_hotspots/coral_trop_decrease.tif', format='GTiff')


#need to get the same legend
stack_posterplot<-stack(dif_list[[2]], dif_list[[4]], dif_list[[5]], dif_list[[8]])

par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(stack_posterplot)

p1<-spplot(stack_posterplot, col.regions=terrain.colors)
p1
p1+layer(sp.polygons(japan_outline))









