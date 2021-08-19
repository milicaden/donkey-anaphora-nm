library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(sciplot)
library(lme4)
library(nlme) 
library(plyr)
library(lme4)
library("lsr")
library("pwr")
library(tidyr)
library(simr)
simrOptions(progress=FALSE)

options(scipen = 999)



##########################################################################################
#
# General settings and auxiliary functions
#
##########################################################################################
# Set the folder
##########################################################################################
Folder = "../data/"
theme_set(theme_bw())
##########################################################################################
#
# Define a function
#
##########################################################################################
length.unique <- function(x){length(unique(x))}
##########################################################################################
#
# Load and organize the data
#
##########################################################################################

rm(d)
for (filenumber in 1:3) {
  
  # Load the result files
  filename <- paste(Folder, "results", filenumber, ".txt", sep="")
  all <- readLines(filename)
  
  # Extract the questionnaire data
  questionnaire <- subset(all, grepl(",form,", all))
  questionnaire <- read.csv(textConnection(questionnaire), header = F)
  names(questionnaire) <- c("timeresults", "ip", "controller", "itemnumber", "elementnumber", "type", "group", "fieldname", "fieldvalue")
  questionnaire <- dcast(questionnaire, timeresults + ip + controller + itemnumber + elementnumber + type + group ~ fieldname, value.var="fieldvalue")
  
  # Extract the responses
  results <- subset(all, grepl("SliderQuestion", all))
  results <- read.csv(textConnection(results), header = F)	
  names(results) <- c("timeresults", "ip", "Rcontroller", "Ritemnumber", "Relementnumber", "Rtype", "Rgroup", "Rname", "answer")
  #results <- subset(results, !is.na(RT))
  
  # Merge the data from the questionnaire with responses
  E <- merge(questionnaire, results)
  E$Exp <- filenumber
  E$comments <- NULL
  E$answer <- factor(E$answer)
  
  if (exists("d")) {
    d <- rbind(d, E)
  }
  else
  { d <- E }
  
}

# Load the design files
design1 <- read.csv(paste(Folder, "donkeys-all.csv", sep=""), header = TRUE, sep = ",")
design2 <- read.csv(paste(Folder, "donkeys-nm.csv", sep=""), header = TRUE, sep = ",")
design <- rbind(design1, design2)

# Merge the results with the design file 
d <- merge(design, d , by="Rtype")

#Recode responses numerically
d$answer <- as.numeric(as.character(d$answer))

# Gender of participants (pre-exclusions) 
for(i in c("Exactly3", "Allbutone", "All")){
  print(i)
  print(table(unique(subset(d, Quantifier == i)[, c("idturk", "sex")])$sex)) 
}

#Transform ratings into % (extremes of continuous scale coded as 0 and 5 in raw data)
d$answer <- 20*d$answer

##########################################################################################
#
# Exclusions and data organization
#
##########################################################################################
# Exclude non-native speakers of English :
names(table(d$language))
d$NativeEnglish <- toupper(d$language) %in% c("ENGLISH", "ENG", "EN", "ENGLISH ", "UNITED STATES", "ENLISH", "ENGLISH (US)", "ENLGISH")
d <- subset(d, NativeEnglish)

#Exclusion details per experiment based on language
for(i in c("Exactly3", "Allbutone", "All")){
  print(i)
  print(length.unique(subset(d, Quantifier == i)$idturk))
}

# Exclusions TVJT: false controls < the rest
ctl.tvjt <- subset(d, Task == "TVJT") 
ctlscore.tvjt <- ddply(ctl.tvjt, c("idturk", "Conditiontype"), 
                       function(df)c(CTLmean=mean(df$answer)))
ctlscore.tvjt <- spread(ctlscore.tvjt, Conditiontype, CTLmean)

# Exclusions IJT: invalid controls < valid controls
ctl.ijt <- subset(d, Task == "IJT" & (Conditiontype == "validcontrols"|Conditiontype == "invalidcontrols")) 
ctlscore.ijt <- ddply(ctl.ijt, c("idturk", "Conditiontype"), 
                      function(df)c(CTLmean=mean(df$answer)))
ctlscore.ijt <- spread(ctlscore.ijt, Conditiontype, CTLmean)

# Keep only the participants who satisfy the exclusion conditions: 
BADSUBJ <- union(subset(ctlscore.tvjt, readings-`false controls` <=0)$idturk, subset(ctlscore.ijt, validcontrols-invalidcontrols <=0)$idturk)
d <- subset(d, !(idturk %in% BADSUBJ))

# Exclusion details per experiment based on tvjt and ijt excl criteria
for(i in c("Exactly3", "Allbutone", "All")){
  print(i)
  print(length.unique(subset(d, Quantifier == i)$idturk))
}

##########################################################################################
#
# Plots
#
##########################################################################################

###########TVJT (Picture verification task), Exps 1,2,3################

# Calculate mean per quantifier per condition for each participant
partmean <- ddply(subset(d, Task == "TVJT"), c("idturk", "Quantifier", "Condition"), 
                  function(df)c(participantmean=mean(df$answer)))


# Calculate mean and se per quantifier and per condition across participants
conditionmean <- ddply(partmean, c("Quantifier", "Condition"), 
                       function(df)c(condition.mean=mean(df$participantmean), condition.se=se(df$participantmean)))

# Adding the conditiontype column
conditionmean <- merge(conditionmean, unique(d[, c("Quantifier", "Condition", "Conditiontype")]), by = c("Quantifier", "Condition"))


# Plot the proportion of true responses with respsect to a condition and a quantifier
levels(conditionmean$Conditiontype)[levels(conditionmean$Conditiontype)=="readings"] <- "test cond."

#Exp 1

#png("Exactly3.png", width = 4.5, height = 5, units = 'in', res = 300)
response.dist <-  ggplot(subset(conditionmean, Quantifier =='Exactly3'), aes(x= Condition, y = condition.mean)) + 
  geom_bar(stat="identity") +
  facet_grid(Quantifier~Conditiontype, scales = "free", space="free")+  
  geom_errorbar(aes(ymin=condition.mean-condition.se, ymax=condition.mean+condition.se, width=.5),
                width=.2)+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  labs(x = "Condition", y = "Rating")
print(response.dist)
#dev.off()

#Exp 2

#png("Allbutone.png", width = 4.5, height = 5, units = 'in', res = 300)
response.dist <-  ggplot(subset(conditionmean, Quantifier =='Allbutone'), aes(x= Condition, y = condition.mean)) + 
  geom_bar(stat="identity") +
  facet_grid(Quantifier~Conditiontype, scales = "free", space="free")+  
  geom_errorbar(aes(ymin=condition.mean-condition.se, ymax=condition.mean+condition.se, width=.5),
                width=.2)+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  labs(x = "Condition", y = "Rating")
print(response.dist)
#dev.off()

#Exp 3

#png("All.png", width = 4.5, height = 5, units = 'in', res = 300)
response.dist <-  ggplot(subset(conditionmean, Quantifier =='All'), aes(x= Condition, y = condition.mean)) + 
  geom_bar(stat="identity") +
  facet_grid(Quantifier~Conditiontype, scales = "free", space="free")+  
  geom_errorbar(aes(ymin=condition.mean-condition.se, ymax=condition.mean+condition.se, width=.5),
                width=.2)+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  labs(x = "Condition", y = "Rating")
print(response.dist)
#dev.off()

#Means in Weak condition in Exp 3


# Weak condition means distribution
weak.means <- ddply(subset(d, Quantifier =='All' & Condition == "Weak"), c("idturk"), 
                    function(df)c(univmean=mean(df$answer)))

#png("All-mean-dist-weak.png", width = 4.5, height = 5, units = 'in', res = 300)
p <- ggplot(weak.means, aes(x=univmean)) +
  geom_histogram(binwidth=10, color="black", fill="white") +
  scale_y_continuous(breaks=c(1,3,5,7,9,11))+
  labs(x="Participants' means in Weak condition")
print(p)
#dev.off()


###########IJT (Inference judgment task), Exp 3################

# Calculate the mean in ijt per participant per quantifier per condition 
partmean.ijt <- ddply(subset(d, Task == "IJT"), c("idturk", "Quantifier", "Condition"), 
                           function(df)c(participantmean=mean(df$answer)))

# Calculate mean and se per quantifier and per condition across participants
conditionmean.ijt <- ddply(partmean.ijt, c("Quantifier", "Condition"), 
                                function(df)c(condition.mean=mean(df$participantmean), condition.se=se(df$participantmean)))

# Give clearer names to levels of Condition in IJT
conditionmean.ijt$Condition <- mapvalues(conditionmean.ijt$Condition, from = c("InfRestrictorDE", "InfFalsecontrols1", "InfFalsecontrols2", "InfRestrictorUE", "InfScopeDE", "InfScopeUE", "InfSymmetry", "InfTruecontrols1", "InfTruecontrols2" ), to = c("RestrictorDE", "Invalid_controls1", "Invalid_controls2", "RestrictorUE", "ScopeDE", "ScopeUE", "Symmetry", "Valid_controls1", "Valid_controls2"))

###plot the responses in IJT###
#png("All-IJT.png", width = 4.5, height = 5, units = 'in', res = 300)
response.dist <-  ggplot(conditionmean.ijt, aes(x= Condition, y = condition.mean)) + 
  geom_bar(stat="identity") +
  facet_grid(.~Quantifier, scales = "free")+  
  geom_errorbar(aes(ymin=condition.mean-condition.se, ymax=condition.mean+condition.se, width=.5),
                width=.2)+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  labs(x = "Condition", y = "Rating")
print(response.dist)
#dev.off()

##########################################################################################
#
# Stats
#
##########################################################################################

#########################################################
#NON-MONOTONIC QUANTIFIERS: all but one and exactly 3: READINGS
#########################################################

#Question 0: Is there UEexist-DEuniv reading (is there more true responses in DEweak-UEweak condition than in false controls?)
df <- subset(d, Quantifier == "Allbutone" & (Condition =="DEfalse-UEstrong"|Condition == "DEweak-UEweak"))
df$Condition <- factor(df$Condition)
a <- lmer(answer ~ Condition + (1+Condition|idturk), REML = F, data=df)
print(summary(a))
a.null <- lmer(answer ~ (1+Condition|idturk), REML = F, data=df)
print(summary(a.null))
print(anova(a , a.null))

df <- subset(d, Quantifier == "Exactly3" & (Condition =="DEfalse-UEstrong"|Condition == "DEweak-UEweak"))
df$Condition <- factor(df$Condition)
b <- lmer(answer ~  Condition + (1+Condition|idturk), REML = F, data=df)
print(summary(b))
b.null <- lmer(answer ~ (1+Condition|idturk), REML = F, data=df)
print(summary(b.null))
print(anova(b , b.null))

#Question 1: Is there UEexist-DEexist (ie.existential on both parts) (is there more true responses to it than to DEweak-UEweak, the only reading entailed by it)?

df <- subset(d, Quantifier == "Allbutone"  & (Condition =="DEstrong-UEweak"|Condition == "DEweak-UEweak"))
df$Condition <- factor(df$Condition)
c <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(c))
c.null <- lmer(answer ~  (1+ Condition|idturk), REML = F, data=df)
print(summary(c.null))
print(anova(c , c.null))


df <- subset(d, Quantifier == "Exactly3" & (Condition =="DEstrong-UEweak"|Condition == "DEweak-UEweak"))
df$Condition <- factor(df$Condition)
e <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(e))
e.null <- lmer(answer ~ (1+ Condition|idturk), REML = F, data=df)
print(summary(e.null))
print(anova(e, e.null))

#Question 2: Is there UEuniv-DEuniv (ie. universal on both parts) (is there more true responses to it than to DEweak-UEweak, the only reading entailed by it)?

df <- subset(d, Quantifier == "Allbutone" &  (Condition =="DEweak-UEstrong"|Condition == "DEweak-UEweak"))
df$Condition <- factor(df$Condition)
f <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(f))
f.null <- lmer(answer ~  (1+ Condition|idturk), REML = F, data=df)
print(summary(f.null))
print(anova(f, f.null))

df <- subset(d, Quantifier == "Exactly3" &  (Condition =="DEweak-UEstrong"|Condition == "DEweak-UEweak"))
df$Condition <- factor(df$Condition)
g <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(g))
g.null <- lmer(answer ~  (1+ Condition|idturk), REML = F, data=df)
print(summary(g.null))
print(anova(g, g.null))

#Question 3: Is UEuniv-DEexist (stronger mixed) there?

#Excluding people who rate both DEweak-UEstrong and DEstrong-UEweak better than DEweak-UEweak.
partmean.nm <- ddply(subset(d, Quantifier != "All"), c("idturk", "Quantifier", "Condition"), 
                  function(df)c(participantmean=mean(df$answer)))
tworeadings <-subset(partmean.nm, Condition == "DEstrong-UEstrong"|Condition == "DEweak-UEstrong"|Condition == "DEstrong-UEweak"|Condition =="DEweak-UEweak")
tworeadings <- dcast(tworeadings, idturk+Quantifier~Condition, value.var = "participantmean")
tworeadings$value <- tworeadings$`DEweak-UEstrong`>tworeadings$`DEweak-UEweak` & tworeadings$`DEstrong-UEweak`>tworeadings$`DEweak-UEweak`

# Subset to those who do not access both readings
conj.read <- subset(tworeadings, tworeadings$value == FALSE)

#Check out how many people remain from the two experiments
for(i in c("Exactly3", "Allbutone")){
  print(i)
  print(length.unique(subset(conj.read, Quantifier == i)$idturk))
}

#Creating a variable that stores how much each participant accesses to the one of DEuniv-UEuniv and DEexist-UEexist reading (whichever they access, returns 0 if neither)
conj.read$Other <- pmax(conj.read$`DEweak-UEstrong`, conj.read$`DEstrong-UEweak`)

#Mean rating of DEstrong-UEstrong and Other
conj.read_long = conj.read[,c("idturk", "Quantifier", "DEstrong-UEstrong", "Other")]
conj.read_long = gather(conj.read_long, Condition, participantmean, `DEstrong-UEstrong`, Other, factor_key=TRUE)

conj.readmean <- ddply(conj.read_long, c("Quantifier", "Condition"), 
                       function(df)c(condition.mean=mean(df$participantmean), condition.se=se(df$participantmean)))

#Plot this
#Exp 1 mixed readding

#png("Exactly3-mixed.png", width = 2.5, height = 4.5, units = 'in', res = 300)
response.dist <-  ggplot(subset(conj.readmean, Quantifier =='Exactly3'), aes(x= Condition, y = condition.mean)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=condition.mean-condition.se, ymax=condition.mean+condition.se, width=.5),
                width=.2)+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  labs(x = "Condition", y = "Rating")
print(response.dist)
#dev.off()

#Exp 2 mixed readding

png("Allbutone-mixed.png", width = 2.5, height = 4.5, units = 'in', res = 300)
response.dist <-  ggplot(subset(conj.readmean, Quantifier =='Allbutone'), aes(x= Condition, y = condition.mean)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=condition.mean-condition.se, ymax=condition.mean+condition.se, width=.5),
                width=.2)+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  labs(x = "Condition", y = "Rating")
print(response.dist)
dev.off()

#Do we have evidence for the DEexist-UEuniv reading as opposed to the Other reading?

#Subset the full data set to DEstrong-UEstrong and Other for the participants satisfying the above conditions for linear modeling
reading1 <- subset(subset(d, idturk %in% conj.read$idturk), idturk %in% subset(conj.read, (`DEweak-UEstrong` == `DEstrong-UEweak`)|(`DEweak-UEstrong` > `DEstrong-UEweak`))$idturk & Condition %in% c("DEstrong-UEstrong", "DEweak-UEstrong"))
reading2 <- subset(subset(d, idturk %in% conj.read$idturk), idturk %in% subset(conj.read, `DEstrong-UEweak` > `DEweak-UEstrong`)$idturk & Condition %in% c("DEstrong-UEstrong", "DEstrong-UEweak"))
readings1and2 <- rbind(reading1, reading2)
readings1and2 <- mutate(readings1and2, TwoConditions = ifelse(Condition == "DEstrong-UEstrong", "DEstrong-UEstrong", "Other"))


#Allbutone:
#borderline; the optimizer changed to bobyqa because of convergence issues with the default 'nloptwrap'
df <- subset(readings1and2, Quantifier == "Allbutone")
df$TwoConditions <- factor(df$TwoConditions)
y <- lmer(answer ~TwoConditions + (1+TwoConditions|idturk), REML=F, data=df, control=lmerControl(optimizer="bobyqa"))
print(summary(y))
y.null <- lmer(answer ~ (1+TwoConditions|idturk), REML=F, data=df,  control=lmerControl(optimizer="bobyqa"))
print(summary(y.null))
print(anova(y , y.null))

#Exactly3
#n.s.
df <- subset(readings1and2, Quantifier == "Exactly3")
df$TwoConditions <- factor(df$TwoConditions)
y <- lmer(answer ~TwoConditions + (1+TwoConditions|idturk), REML=F, data=df)
print(summary(y))
y.null <- lmer(answer ~ (1+TwoConditions|idturk), REML=F, data=df)
print(summary(y.null))
print(anova(y , y.null))

# Reviewer's question: evaluate whether there is any people who may only access the conjunctive reading (but because this population is small, it doesn't show in the previous analysis)
a = (subset(tworeadings, Quantifier == "Allbutone" & (`DEstrong-UEstrong` > `DEweak-UEweak`) & (`DEweak-UEweak` >= `DEstrong-UEweak`) & (`DEweak-UEweak` >= `DEweak-UEstrong`)))
e = (subset(tworeadings, Quantifier == "Exactly3" & (`DEstrong-UEstrong` > `DEweak-UEweak`) & (`DEweak-UEweak` >= `DEstrong-UEweak`) & (`DEweak-UEweak` >= `DEweak-UEstrong`)))

#Question 4a: Is there universal reading with all?

df <- subset(d, Quantifier == "All" &  (Condition =="Strong"|Condition == "Weak"))
df$Condition <- factor(df$Condition)
j <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(j))
j.null <- lmer(answer ~  (1+ Condition|idturk), REML = F, data=df)
print(summary(j.null))
print(anova(j, j.null))

#Question 4b: Is there existential reading with all?

df <- subset(d, Quantifier == "All" &  (Condition =="False"|Condition == "Weak"))
df$Condition <- factor(df$Condition)
k <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(k))
k.null <- lmer(answer ~  (1+ Condition|idturk), REML = F, data=df)
print(summary(k.null))
print(anova(k, k.null))

#Question 5: Existential reading preferred to universal reading with 'all but one'

df <- subset(d, Quantifier == "Allbutone" &  (Condition =="DEweak-UEstrong"|Condition == "DEstrong-UEweak"))
df$Condition <- factor(df$Condition)
l <- lmer(answer ~  Condition + (1+ Condition|idturk), REML = F, data=df)
print(summary(l))
l.null <- lmer(answer ~  (1+ Condition|idturk), REML = F, data=df)
print(summary(l.null))
print(anova(l, l.null))

#Holm-Bonferroni adjustments
#Experiment 1
p.adjust(c( 0.8013, 0.00000000000000022, 0.8854, 0.9665), method = "holm", n = 4)
#Experiment 2
p.adjust(c(0.1255, 0.0000000000000008304, 0.001196, 0.05741, 0.0000002214), method = "holm", n = 5)
#Experiment 3
p.adjust(c(0.000000000002349, 0.00000000000000022), method = "holm", n = 2)

###########################################
#DOES SUBJECTIVE MONOTONICITY OR SYMMETRY PREDICT THE AMOUNT OF UNIVERSAL READINGS? Experiment 3.
###########################################

#First, we subset the data for the universal quantifier 'all' only. 
all <- subset(d, Quantifier == 'All')

#Second, we normalize the responses for each participant in condition Weak from the truth value judgment task.
all.tvjt <- subset(all, Task == "TVJT")
temp.all.tvjt <- ddply(subset(all.tvjt, (Condition == "Strong"|Condition =="False")), c("Condition","idturk"), 
                   function(df)c(cond.mean=mean(df$answer)))

temp.all.tvjt <- spread(temp.all.tvjt, Condition, cond.mean)
all.tvjt <- merge(subset(all.tvjt, Condition == "Weak"), temp.all.tvjt, by = c("idturk"))
all.tvjt$NormResp <- (all.tvjt$answer - all.tvjt$False)/(all.tvjt$Strong - all.tvjt$False)
all.tvjt <- subset(all.tvjt, -0.5 < all.tvjt$NormResp & all.tvjt$NormResp < 1.5)

#Third, we give a directional interpretation to monotonicity ratings 
all.ijt <- subset(all, Task == "IJT")
all.ijt$answerdir <- all.ijt$answer
all.ijt$answerdir[all.ijt$Condition=="InfRestrictorUE"] <- 100-all.ijt$answerdir[all.ijt$Condition=="InfRestrictorUE"]
all.ijt$answerdir[all.ijt$Condition=="InfScopeUE"] <- 100-all.ijt$answerdir[all.ijt$Condition=="InfScopeUE"]

#Fourth, we normalize the responses for each participant in non-control conditions in Inference judgment task.

temp.all.ijt <- ddply(subset(all.ijt, (Conditiontype == "validcontrols"|Conditiontype == "invalidcontrols")), c("idturk", "Conditiontype"), 
                  function(df)c(cond.mean=mean(df$answerdir)))

temp.all.ijt <- spread(temp.all.ijt, Conditiontype, cond.mean)
all.ijt <- merge(subset(all.ijt, !(Conditiontype %in% c('validcontrols', 'invalidcontrols'))), temp.all.ijt, by = c("idturk"))
all.ijt$NormResp <- (all.ijt$answerdir - all.ijt$invalidcontrols)/(all.ijt$validcontrols-all.ijt$invalidcontrols)
all.ijt <- subset(all.ijt, -0.5 < all.ijt$NormResp & all.ijt$NormResp < 1.5)

#Now we calculate Monotonicity index and Symmetry index
all.ijt.norm <- ddply(all.ijt, c("idturk", "Condition"), 
                      function(df)c(cond.mean=mean(df$NormResp)))#Calculate mean of normalized responses per condition
all.ijt.norm <- dcast(all.ijt.norm, idturk ~ Condition, value.var = "cond.mean")
all.ijt.norm$Monotonicity <- abs((all.ijt.norm$InfScopeDE+all.ijt.norm$InfScopeUE)/2 - (all.ijt.norm$InfRestrictorDE+all.ijt.norm$InfRestrictorUE)/2) 

#Merge the TVJT frame with data on participants indices and remove participants for whom Monotonicity index couldn't be evaluated bc all of their responses in some of the monotonicity conditions ended up excluded by -0.5 < r < 1.5 condition
corrdf <- merge(all.tvjt, all.ijt.norm[,c("idturk","Monotonicity","InfSymmetry", "InfScopeDE", "InfScopeUE", "InfRestrictorDE", "InfRestrictorUE")], by = c("idturk"))
corrdf <- na.omit(corrdf)

######
#Stats
######
#Does symmetry index predict the amount of universal readings?
sym <- lmer(NormResp ~ InfSymmetry + (1|idturk), REML=F, data=corrdf)
print(summary(sym))
sym.null <- lmer(NormResp ~ (1|idturk), REML=F, data=corrdf)
print(summary(sym.null))
print(anova(sym , sym.null))


#Does monotonicity index predict the amount of universal readings?
mon <- lmer(NormResp ~ Monotonicity + (1|idturk), REML=F, data=corrdf)
print(summary(mon))
mon.null <- lmer(NormResp ~  (1|idturk), REML=F, data=corrdf)
print(summary(mon.null))
print(anova(mon , mon.null))

#Does any combination of monotonicity ratings predict the amount of universal readings?
mon2 <- lmer(NormResp ~ InfScopeDE+InfScopeUE+InfRestrictorDE+InfRestrictorUE +  (1|idturk), REML=F, data=corrdf)
print(summary(mon2))
mon2.null <- lmer(NormResp ~  (1|idturk), REML=F, data=corrdf)
print(summary(mon2.null))
print(anova(mon2 , mon2.null))



###########################################
#Comparing the rate of existential reading across quantifiers: Section A.8
###########################################
# Is there less existential readings with 'All' than with 'All but one' and 'Exactly 3'?

dff <- subset(d, Task == "TVJT" & (Quantifier =='All'|grepl("^FirstBlock", d$Rtype)))

#Create a factor Stength with levels Strong and Weak and False
dff$Strength <- factor(dff$Condition)
levels(dff$Strength)[levels(dff$Strength)=="DEstrong-UEstrong"] <- "Strong"
levels(dff$Strength)[levels(dff$Strength)=="DEstrong-UEweak"] <- "Weak"
levels(dff$Strength)[levels(dff$Strength)=="DEstrong-UEfalse"] <- "False"
levels(dff$Strength)[levels(dff$Strength)=="DEfalse-UEstrong"] <- "False"
levels(dff$Strength)

#All vs. all but one
df <- subset(dff, (Quantifier == "All"|Quantifier == "Allbutone") & (Strength == "Strong"|Strength == "Weak"))
df$Strength <- factor(df$Strength)
c <- lmer(answer ~  Strength*Quantifier + (1+Strength|idturk), REML=F, data=df)
print(summary(c))
c.null <- lmer(answer ~ Strength+Quantifier+(1+Strength|idturk), REML = F, data=df)
print(summary(c.null))
print(anova(c , c.null))

#All vs. exactly 3
df <- subset(dff, (Quantifier == "All"|Quantifier == "Exactly3") & (Strength == "Strong"|Strength == "Weak"))
df$Strength <- factor(df$Strength)
c <- lmer(answer ~  Strength*Quantifier + (1+Strength|idturk), REML=F, data=df)
print(summary(c))
c.null <- lmer(answer ~ Strength+Quantifier+(1+Strength|idturk), REML = F, data=df)
print(summary(c.null))
print(anova(c , c.null))
