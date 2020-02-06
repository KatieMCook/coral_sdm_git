#checking coral FGS####
library(dplyr)


groups<-read.csv('coral_functional_groups.csv')

### How are the groups different? ie which traits influence their classification into trait-# groups

## First, do numerical traits with boxplots:
par(mar=c(2,2,2,2))
par(mfrow=c(2,2))
er = par(mfrow = c(2,2), pty = "s", mai=c(0.5,0.5,0.5,0.5))
boxplot(groups$Depth.lower ~groups$group, main='Depth')
boxplot(groups$Corallite.width.maximum ~groups$group, main='Corallite Width')
boxplot(groups$growth_rate ~groups$group, main='Growth Rate')


#Second, we look at categorical traits and how they are distributed amongst groups:
#*might need to redo with proportial values:*
#To build percentage-based barplots, we must use `prop.table()` to generate each #percentage for the columns: Here,
#`margin` represents whether it will be run on rows (1) or columns (2). We've selected to # use `prop.table()` on
# columns since that was how we built our data.
# `prop = prop.table(data,margin=2)`


### Coloniality groups
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)

Coloniatlity = table(groups$Coloniality, groups$group)
barplot(Coloniatlity, main = "Coloniality", col=topo.colors(length(rownames(Coloniatlity))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(Coloniatlity))), legend = rownames(Coloniatlity))

### Growth Form
par(mar=c(1,1,2,1))
form = table(groups$Growth.form.typical..Brig.revised., groups$group)
barplot(form, main = "Growth Form", col=heat.colors(length(rownames(form))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = heat.colors(length(rownames(form))), legend = rownames(form))

### Larval Development
par(mar=c(2,2,2,2))
larval = table(groups$larval_development, groups$group)
barplot(larval, main = "Larval Development", col=terrain.colors(length(rownames(larval))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(larval))), legend = rownames(larval))
larval

### Sexual System
sex_sys = table(groups$Sexual_system ,groups$group)
barplot(sex_sys, main = "Sexual System", col=rainbow(length(rownames(sex_sys))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = rainbow(length(rownames(sex_sys))), legend = rownames(sex_sys))

### Wave
wave = table(groups$Wave.exposure.preference, groups$group)
barplot(wave, main = "Wave Exposure", col=terrain.colors(length(rownames(wave))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(wave))), legend = rownames(wave))

##clarity
clarity = table(groups$Water.clarity.preference, groups$group)
barplot(clarity, main = "Water Clarity", col=terrain.colors(length(rownames(clarity))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(clarity))), legend = rownames(clarity))
