library(ggplot2)

dat = read.csv("~/Desktop/forest/forestplotwork.csv", header=T, as.is=T)

#Create 'cite' vector by merging author and year columns
dat$cite=NA
dat$cite=paste(dat$property_1, dat$property_2)

#Reorder bibliographic info based on value of d (yi), so effect sizes can be plotted in descending order
dat$cite=reorder(dat$cite, dat$Hedges.G, FUN=mean)

#Look at your overall meta-analytic estimate (random-effects)
random=rma(Hedges.G, SD, data=dat)
random


#Turn off annoying option that prevents you from binding rows of text
options(stringsAsFactors = FALSE)


%%
#You need to create a matrix for the new data row you wish to insert, give it the same column names as dat
#and then bind it to dat. You will need to do this for each row you wish to insert.
#In this case, 4 groups + 1 overall estimate = 5 entries...
group.row = matrix(c(NA, NA, NA, NA, 'Summary', 'Summary', setting$b[1], setting$se[1]^2, 'Group Studies'), nrow = 1)
group.row.df = as.data.frame (group.row)
names(group.row.df) = names(dat)
dat=rbind(dat, group.row.df)

individual.row = matrix(c(NA, NA, NA, NA, 'Summary', 'Summary', setting$b[2], setting$se[2]^2, 'Individual Studies'), nrow = 1)
individual.row.df = as.data.frame (individual.row)
names(individual.row.df) = names(dat)
dat=rbind(dat, individual.row.df)

aware.row = matrix(c(NA, NA, NA, NA, 'Summary', 'Summary', tester$b[1], tester$se[1]^2, 'Aware Studies'), nrow = 1)
aware.row.df = as.data.frame (aware.row)
names(aware.row.df) = names(dat)
dat=rbind(dat, aware.row.df)

blind.row = matrix(c(NA, NA, NA, NA, 'Summary', 'Summary', tester$b[2], tester$se[2]^2, 'Blind Studies'), nrow = 1)
blind.row.df = as.data.frame (blind.row)
names(blind.row.df) = names(dat)
dat=rbind(dat, blind.row.df)

overall.row = matrix(c(NA, NA, NA, NA, 'Summary', 'Summary', random$b, random$se^2, 'Overall Effect'), nrow = 1)
overall.row.df = as.data.frame (overall.row)
names(overall.row.df) = names(dat)
dat=rbind(dat, overall.row.df)

%%

#Make sure everything that is numeric is numeric, and everything that is a factor is a factor
dat$Hedges.G = as.numeric (dat$yi)
dat$SD = as.numeric (dat$vi)
dat$cite= as.factor (dat$cite)

#Get standard errors from variances
dat$se = sqrt(dat$SD)

#Calculate 95% CI values
dat$lowerci = (-1.96*dat$se)+dat$Hedges.G
dat$upperci = (1.96*dat$se)+dat$Hedges.G

#Rename levels of Setting factor
levels(dat$property_1) = c('grazing', 'tree/shrub_presence', 'grazing&tree/shrub_presence', 'riparian_restoration', 'grassland_composition', 'hedgerow_planting', 'organic_amendment')

%%

#APA-format theme
  apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.position='none')
  
  #Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
  #specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(dat, aes(y=property_2, x=Hedges.G, xmin=lowerci, xmax=upperci))+
  #Add data points and color them black
  geom_point(size = 4)+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  #geom_point(data=subset(dat), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-25,50), name='Hedges G')+
  #Give y-axis a meaningful label
  ylab('Relationship practice to outcome')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(property_1~., scales= 'free', space='free')+
  #Apply my APA theme
apatheme

p

#Save plot in your working directory
ggsave(p, file='ggforest.png', width = 8, height=8, dpi=300)

