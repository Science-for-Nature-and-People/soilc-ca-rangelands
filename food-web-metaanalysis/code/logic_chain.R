library(googlesheets)
library(tidyverse)
library(metafor)
library(reshape2)

all_data <- gs_title("data_collection.xlsx")
raw_data <- gs_read(ss=all_data, ws = "raw_evidence")

## General data cleaning

# exclude rows flagged for exclusion at in-person workshop
raw <- raw_data[raw_data$red_highlight==0,]

# response type (outcome or soil): there are a few labeled "outcome?"; replace these with "outcome"
raw$response_type <- as.factor(ifelse(raw$response_type == "outcome?", "outcome", as.character(raw$response_type)))

# treatment_category_1 (lumped treatment category): correct spelling
raw$treatment_category_1 <- as.factor(ifelse(raw$treatment_category_1 == "organic ammendment", "organic amendment", as.character(raw$treatment_category_1)))

# split out outcomes to make scanning for duplicates easier
outc <- raw[raw$response_type == "outcome",]
outc$response_lumped <- factor(outc$response_lumped)
as.data.frame(table(outc$response_lumped))

# fix duplicates in the full data set
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "forage production?", "forage production", as.character(raw$response_lumped)))
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "natural pest control?", "natural pest control", as.character(raw$response_lumped)))
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "plant diversity?", "plant diversity", as.character(raw$response_lumped)))
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "forage quality", "plant tissue quality", as.character(raw$response_lumped)))

# split out soil properties to make scanning for duplicates easier
soilp <- raw[raw$response_type == "soil",]
soilp$response_lumped <- factor(soilp$response_lumped)
as.data.frame(table(soilp$response_lumped))

# fix duplicates in the full data set
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "cation", "cations", as.character(raw$response_lumped)))
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "nematode abundance?", "nematode abundance", as.character(raw$response_lumped)))

# lump "microbial biomass" and "nematode abundance" into "soil organism abundance"; raw values remain in unlumped response column
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "nematode abundance", "soil organism abundance", as.character(raw$response_lumped)))
raw$response_lumped <- as.factor(ifelse(raw$response_lumped == "microbial biomass", "soil organism abundance", as.character(raw$response_lumped)))

# calculate SD from SE
raw$control_sd <- as.numeric(ifelse(raw$control_sd=="15%", 0.15, as.character(raw$control_sd)))
raw$control_sd <- ifelse(is.na(raw$control_sd)==T & is.na(raw$control_se)==F & is.na(raw$control_n)==F, raw$control_se * sqrt(raw$control_n), raw$control_sd)

raw$treatment_sd <- as.numeric(ifelse(raw$treatment_sd=="15%", 0.15, as.character(raw$treatment_sd)))
raw$treatment_sd <- ifelse(is.na(raw$treatment_sd)==T & is.na(raw$treatment_se)==F & is.na(raw$treatment_n)==F, raw$treatment_se * sqrt(raw$treatment_n), raw$treatment_sd)

## PRUNE data set to just focal management practices, soil properties, and outcomes
raw.mgmt.pruned <- raw[raw$treatment_category_1 %in% c("grazing","hedgerow planting","organic amendment","riparian restoration","tree/shrub presence"),]
# 369 of 396 data rows remain
d <- raw.mgmt.pruned[raw.mgmt.pruned$response_lumped %in% c("forage production", "water quality", "plant diversity", "native plant abundance", "plant tissue quality", "natural pest control", "carbon", "total N", "available phosphorous", "bulk density", "soil organism abundance"),]
# 227 data rows remain
d <- as.data.frame(d)

## EDIT data set to conform to uniform treatment definitions

# remove one misclassified row
d <- d[d$id != 270,]

# edit a few treatment categories
d$treatment_category_1 <- as.character(d$treatment_category_1)
d$treatment_category_1 <- ifelse(d$id %in% c(271, 272), "tree/shrub presence", d$treatment_category_1)
d$treatment_category_1 <- ifelse(d$id %in% c(164, 165), "riparian restoration", d$treatment_category_1)
d$treatment_category_1 <- as.factor(d$treatment_category_1)

# swap treatment and control for some rows
d[d$id %in% c(51, 52, 53, 54, 82, 83, 140, 141, 142, 152, 160, 164, 165, 183, 184, 185, 186, 187, 188, 207, 208, 209, 210, 211, 214, 215, 216, 217, 221, 222, 228, 229, 230, 244, 245, 246, 247, 249, 271, 272, 595, 597, 605), c("control", "control_mean", "control_sd", "control_se", "control_n", "treatment", "treatment_mean", "treatment_sd", "treatment_se", "treatment_n")] <- d[d$id %in% c(51, 52, 53, 54, 82, 83, 140, 141, 142, 152, 160, 164, 165, 183, 184, 185, 186, 187, 188, 207, 208, 209, 210, 211, 214, 215, 216, 217, 221, 222, 228, 229, 230, 244, 245, 246, 247, 249, 271, 272, 595, 597, 605), c("treatment", "treatment_mean", "treatment_sd", "treatment_se", "treatment_n", "control", "control_mean", "control_sd", "control_se", "control_n")] 


##### MANAGEMENT - SOIL PROPERTY RELATIONSHIPS #####

# subset to look just at soil property - management relationships (not outcome - management relationships) for now
soilp <- d[d$response_type == "soil",]
soilp$response_lumped <- factor(soilp$response_lumped)
soilp$treatment_category_1 <- factor(soilp$treatment_category_1)
soilp$ref_code <- factor(soilp$ref_code)

# calculate yi and vi for each row of data
res <- escalc(measure="ROM", m1i=treatment_mean, sd1i=treatment_sd, n1i=treatment_n, m2i=control_mean, sd2i=control_sd, n2i=control_n, data=soilp)

# analyze only those relationships that have 3 or more observations
soiltab <- as.data.frame(table(soilp$response_lumped, soilp$treatment_category_1))
soilan <- soiltab[soiltab$Freq >= 3,c(2,1)]
names(soilan) <- c("practice_predictor","property_response")

# create empty data frame for effect size calculation output
soilp.res <- data.frame(lnRR=numeric(), lnRR.ci.lower=numeric(), lnRR.ci.upper=numeric(), n.obs=integer(), n.papers=integer())

# loop through soil property - management pairs that have 3 or more observations and calculate effect sizes
for (i in 1:nrow(soilan)){
  dsub <- res[res$treatment_category_1 == soilan$practice_predictor[i] & res$response_lumped == soilan$property_response[i],]
  dsub$ref_code <- factor(dsub$ref_code)
  m1 <- rma.mv(yi, vi, random = ~ 1 | ref_code, method="ML", data=dsub)
  soilp.res[i,"lnRR"] <- coef(m1)
  soilp.res[i,"lnRR.ci.lower"] <- m1$ci.lb
  soilp.res[i,"lnRR.ci.upper"] <- m1$ci.ub
  soilp.res[i,"n.obs"] <- nrow(dsub)
  soilp.res[i,"n.papers"] <- nlevels(dsub$ref_code)
}

# make forest plot
soilp.all <- cbind(soilan, soilp.res)

soilp.all$property_response <- gsub("available phosphorous","available P", soilp.all$property_response)
soilp.all$property_response <- gsub("soil organism abundance","soil organisms", soilp.all$property_response)

soilp.all$order <- ifelse(soilp.all$practice_predictor=="tree/shrub presence", 4, 
                          ifelse(soilp.all$practice_predictor=="grazing", 3, 
                                 ifelse(soilp.all$practice_predictor=="organic amendment", 2, 
                                        ifelse(soilp.all$practice_predictor=="riparian restoration", 1, NA))))


setwd("~/Documents/Kelly/NatureNet_Postdoc/soilc-ca-rangelands/food-web-metaanalysis/figures/")

pdf("forest_plot_soil_properties.pdf")
ggplot(soilp.all, aes(y=order, x=lnRR, xmin=lnRR.ci.lower, xmax=lnRR.ci.upper))+
  #Add data points
  geom_point(data=subset(soilp.all, practice_predictor=='tree/shrub presence'), color='green', size=4) +
  geom_point(data=subset(soilp.all, practice_predictor=='grazing'), color='blue', size=4) +
  geom_point(data=subset(soilp.all, practice_predictor=='organic amendment'), color='red', size=4) +
  geom_point(data=subset(soilp.all, practice_predictor=='riparian restoration'), color='orange', size=4) +
  #add the CI error bars
  geom_errorbarh(height=.1)+ theme_bw()+ 
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-3,3), name='Log Response Ratio')+
  scale_y_continuous(limits=c(0,5), breaks = soilp.all$order, labels = soilp.all$practice_predictor) + 
  theme(axis.text.y=element_text(size=13), axis.text.x=element_text(size=13), axis.title.x=element_text(size=15), strip.text.y = element_text(size=13))+ 
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_grid(property_response~., scales= 'fixed', space='fixed', as.table=T)

dev.off()

##### MANAGEMENT - OUTCOME RELATIONSHIPS #####

# subset to look just at outcome - management relationships
out.temp <- d[d$response_type == "outcome",]

# exclude rows for which yi and vi cannot be calculated
outcomes <- out.temp[is.na(out.temp$control_mean)==F & is.na(out.temp$control_sd)==F & is.na(out.temp$control_n)==F & is.na(out.temp$treatment_mean)==F & is.na(out.temp$treatment_sd)==F & is.na(out.temp$treatment_n)==F,]

outcomes$response <- factor(outcomes$response)
outcomes$response_lumped <- factor(outcomes$response_lumped)

a <- as.data.frame(table(outcomes$response, outcomes$response_lumped))
b <- dcast(a, Var1 ~ Var2)
names(b)[1] <- "response"
names(b)[2:7] <- gsub(" ",".",names(b)[2:7])

forage.production <- b[b$forage.production > 0,c("response","forage.production")]
native.plant.abundance <- b[b$native.plant.abundance > 0,c("response","native.plant.abundance")]
natural.pest.control <- b[b$natural.pest.control > 0,c("response","natural.pest.control")]
plant.diversity <- b[b$plant.diversity > 0,c("response","plant.diversity")]
plant.tissue.quality <- b[b$plant.tissue.quality > 0,c("response","plant.tissue.quality")]
water.quality <- b[b$water.quality > 0,c("response","water.quality")]

forage.production
native.plant.abundance
natural.pest.control
plant.diversity
plant.tissue.quality
water.quality

# pH has site-specific interpretation in terms of whether higher pH makes water quality "better" or "worse"
# DO is the only water quality response for which higher values are better
# OMIT both of these for now, for simplicity

# OMIT "bare ground" as a positive indicator of forage production
# OMIT "pest abundance" and "probability of intense herbivory damage" as positive indicators of natural pest control

outcomes <- outcomes[ !(outcomes$response %in% c("DO (Spring water)","pH (spring water)","bare ground", "pest abundance", "probability of intense herbivory damage")) , ]

outcomes$treatment_category_1 <- factor(outcomes$treatment_category_1)
outcomes$ref_code <- factor(outcomes$ref_code)
outcomes$response <- factor(outcomes$response)
outcomes$response_lumped <- factor(outcomes$response_lumped)

# calculate yi and vi for each row of data
res <- escalc(measure="ROM", m1i=treatment_mean, sd1i=treatment_sd, n1i=treatment_n, m2i=control_mean, sd2i=control_sd, n2i=control_n, data=outcomes)

# analyze only those relationships that have 3 or more observations
outctab <- as.data.frame(table(outcomes$response_lumped, outcomes$treatment_category_1))
outcan <- outctab[outctab$Freq >= 3,c(2,1)]
names(outcan) <- c("practice_predictor","outcome_response")

# create empty data frame for effect size calculation output
outcomes.res <- data.frame(lnRR=numeric(), lnRR.ci.lower=numeric(), lnRR.ci.upper=numeric(), n.obs=integer(), n.papers=integer())

# loop through outc property - management pairs that have 3 or more observations and calculate effect sizes
for (i in 1:nrow(outcan)){
  dsub <- res[res$treatment_category_1 == outcan$practice_predictor[i] & res$response_lumped == outcan$outcome_response[i],]
  dsub$ref_code <- factor(dsub$ref_code)
  m1 <- rma.mv(yi, vi, random = ~ 1 | ref_code, method="ML", data=dsub)
  outcomes.res[i,"lnRR"] <- coef(m1)
  outcomes.res[i,"lnRR.ci.lower"] <- m1$ci.lb
  outcomes.res[i,"lnRR.ci.upper"] <- m1$ci.ub
  outcomes.res[i,"n.obs"] <- nrow(dsub)
  outcomes.res[i,"n.papers"] <- nlevels(dsub$ref_code)
}

# make forest plot
outcomes.all <- cbind(outcan, outcomes.res)

# make name shorter to fit in label box
outcomes.all$outcome_response <- gsub("native plant abundance","native plants", outcomes.all$outcome_response)
# change name to indicate directionality: higher values are bad
outcomes.all$outcome_response <- gsub("water quality","water pollution", outcomes.all$outcome_response)

outcomes.all$order <- ifelse(outcomes.all$practice_predictor=="tree/shrub presence", 3, 
                          ifelse(outcomes.all$practice_predictor=="grazing", 2, 
                                 ifelse(outcomes.all$practice_predictor=="organic amendment", 1, NA)))

setwd("~/Documents/Kelly/NatureNet_Postdoc/soilc-ca-rangelands/food-web-metaanalysis/figures/")

pdf("forest_plot_outcomes.pdf")
ggplot(outcomes.all, aes(y=order, x=lnRR, xmin=lnRR.ci.lower, xmax=lnRR.ci.upper))+
  #Add data points
  geom_point(data=subset(outcomes.all, practice_predictor=='tree/shrub presence'), color='green', size=4) +
  geom_point(data=subset(outcomes.all, practice_predictor=='grazing'), color='blue', size=4) +
  geom_point(data=subset(outcomes.all, practice_predictor=='organic amendment'), color='red', size=4) +
  #add the CI error bars
  geom_errorbarh(height=.1)+ theme_bw()+ 
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-6,6), name='Log Response Ratio')+
  scale_y_continuous(limits=c(0,5), breaks = outcomes.all$order, labels = outcomes.all$practice_predictor) + 
  theme(axis.text.y=element_text(size=13), axis.text.x=element_text(size=13), axis.title.x=element_text(size=15), strip.text.y = element_text(size=13))+ 
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_grid(outcome_response~., scales= 'fixed', space='fixed', as.table=T)

dev.off()


##### MANAGEMENT-SOIL PROPERTY AND MANAGEMENT-OUTCOME RELATIONSHIPS IN SAME PAPER? #####

d$management.ref <- paste0(d$treatment_category_1,".",d$ref_code)
c <- as.data.frame(table(d$management.ref, d$response_type))
e <- dcast(c, Var1~Var2)
e[e$outcome > 0 & e$soil > 0,]

#                                               Var1 outcome soil
#        grazing.Stahlheber_D'Antonio_and_Tyler_2016       1    4
#  tree/shrub presence.Stahlheber_and_D'Antonio_2014       8    4

# Just two papers measured BOTH soil property and outcome changes in response to the same management.
# So we can't correlated the lnRRs as Chelsea and I had hoped, at least with the current data set.


