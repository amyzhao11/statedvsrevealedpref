#library(sjmisc)

library(lmerTest)
library(lme4)
library(lattice)
library(ggplot2)
#descriptive statistics
setwd("###")


recent=read.csv("Speed-dating data.csv")

recent17=subset(recent,year %in% 17:19)
######################################################importance################
personlevel=unique(recent[,c("id","sessionid","sex",
                             "impfa","impba","impku","impam","impin",
                             "impcr","impco","imphp","impha",
                             "prefa","preba","preku","pream","prein",
                             "precr","preco","prehp","preha","age","year"
                             )])

apply(personlevel[,4:21],2,mean,na.rm=T)
apply(personlevel[,4:21],2,sd,na.rm=T)

maleinfo=personlevel[personlevel$sex=="M",]
femaleinfo=personlevel[personlevel$sex=="F",]
round(colMeans(maleinfo[,4:21],na.rm=TRUE),2)
round(colMeans(femaleinfo[,4:21],na.rm=TRUE),2)

round(apply(maleinfo[,4:21],2,sd,na.rm=TRUE),2)
round(apply(femaleinfo[,4:21],2, sd,na.rm=TRUE),2)

#age
mean(maleinfo$age,na.rm=TRUE)
mean(femaleinfo$age,na.rm=TRUE)

sd(maleinfo$age,na.rm=TRUE)
sd(femaleinfo$age,na.rm=TRUE)


######################################################
#Study 1 descriptives
#2017 data for preference levels only
#Absolute difference/Euclidean distance study
#prefs only
unique(recent$year[!is.na(recent$pream)])
#17, 18, 19
mean(personlevel$age[personlevel$sex=="M" & personlevel$year %in% c(17,18,19)],na.rm=T)
mean(personlevel$age[personlevel$sex=="F"& personlevel$year %in% c(17,18,19)],na.rm=T)
sd(personlevel$age[personlevel$sex=="M"& personlevel$year %in% c(17,18,19)],na.rm=T)
sd(personlevel$age[personlevel$sex=="F"& personlevel$year %in% c(17,18,19)],na.rm=T)

######################################################
##parameters for ABM##
mean(recent$males,na.rm=TRUE)
mean(recent$females,na.rm=TRUE)
sd(recent$males,na.rm=TRUE)
sd(recent$females,na.rm=TRUE)


######################################################rating################
#descriptives
maleratings=recent[recent$sex=="M",]
femaleratings=recent[recent$sex=="F",]


#ratings but not relevant
mean(na.omit(unlist(femaleratings[,c(4,23:31)])))
sd(na.omit(unlist(femaleratings[,c(4,23:31)])))
mean(na.omit(unlist(maleratings[,c(4,23:31)])))
sd(na.omit(unlist(maleratings[,c(4,23:31)])))




############################################################################
#Study 1, level metric
###########interaction#########
options(digits=8)
#gfattrac
rel=data.frame("id"=recent$id,
               "sex"=recent$sex,
               "year"=recent$year,
               "partnerid"=recent$partnerid,
               "sessionid"=recent$sessionid,
               "goattrac"=scale(recent$goattrac),
               "gfattrac"=scale(recent$gfattrac),
               "impfa"=scale(recent$impfa),
               "gbattrac"=scale(recent$gbattrac),
               "impba"=scale(recent$impba),
               "gkindunder"=scale(recent$gkindunder),
               "impku"=scale(recent$impku),
               "gambitious"=scale(recent$gambitious),
               "impam"=scale(recent$impam),
               "gintelligent"=scale(recent$gintelligent),
               "impin"=scale(recent$impin),
               "gconfident"=scale(recent$gconfident),
               "impco"=scale(recent$impco),
               "gfunny"=scale(recent$gfunny),
               "imphp"=scale(recent$imphp),
               "gpercfunny"=scale(recent$gpercfunny),
               "impha"=scale(recent$impha),
               "gcreative"=scale(recent$gcreative),
               "impcr"=scale(recent$impcr)
)
#gfattrac
model<-lmer(goattrac~ 1
            +gfattrac*impfa+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfattrac| id)
            ,data=rel)
summary(model)

#gbattrac
model<-lmer(goattrac~ 1
            +gbattrac*impba+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gbattrac| id)            
            ,data=rel)
summary(model)

#gkindunder
model<-lmer(goattrac~ 1
            +gkindunder*impku+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gkindunder| id)
            ,data=rel)
summary(model)

#gambitious

model<-lmer(goattrac~ 1
            +gambitious*impam+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gambitious| id)
            ,data=rel)
summary(model)

#gintelligent
model<-lmer(goattrac~ 1
            +gintelligent*impin+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gintelligent| id)
            ,data=rel)
summary(model)

#gconfident

model<-lmer(goattrac~ 1
            +gconfident*impco+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gconfident| id)
            ,data=rel)
summary(model)

#gfunny

model<-lmer(goattrac~ 1
            +gfunny*imphp+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfunny| id)
            ,data=rel)
summary(model)

#gpercfunny

model<-lmer(goattrac~ 1
            +gpercfunny*impha+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gpercfunny| id)
            ,data=rel)
summary(model)

#gcreative

model<-lmer(goattrac~ 1
            +gcreative*impcr+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gcreative| id)
            ,data=rel)

summary(model)
################################################################################
################################################################################


################################################################################
#Importance for 2017 onwards

options(digits=8)
#gfattrac
rel=data.frame("id"=recent$id,
               "sex"=recent$sex,
               "year"=recent$year,
               "partnerid"=recent$partnerid,
               "sessionid"=recent$sessionid,
               "goattrac"=scale(recent$goattrac),
               "gfattrac"=scale(recent$gfattrac),
               "impfa"=scale(recent$impfa),
               "gbattrac"=scale(recent$gbattrac),
               "impba"=scale(recent$impba),
               "gkindunder"=scale(recent$gkindunder),
               "impku"=scale(recent$impku),
               "gambitious"=scale(recent$gambitious),
               "impam"=scale(recent$impam),
               "gintelligent"=scale(recent$gintelligent),
               "impin"=scale(recent$impin),
               "gconfident"=scale(recent$gconfident),
               "impco"=scale(recent$impco),
               "gfunny"=scale(recent$gfunny),
               "imphp"=scale(recent$imphp),
               "gpercfunny"=scale(recent$gpercfunny),
               "impha"=scale(recent$impha),
               "gcreative"=scale(recent$gcreative),
               "impcr"=scale(recent$impcr)
)
rel=subset(rel,as.numeric(id)>=17000000)

model<-lmer(goattrac~ 1
            +gfattrac*impfa+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfattrac| id)
            ,data=rel)
summary(model)

#gbattrac
model<-lmer(goattrac~ 1
            +gbattrac*impba+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gbattrac| id)            
            ,data=rel)
summary(model)

#gkindunder
model<-lmer(goattrac~ 1
            +gkindunder*impku+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gkindunder| id)
            ,data=rel)
summary(model)

#gambitious

model<-lmer(goattrac~ 1
            +gambitious*impam+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gambitious| id)
            ,data=rel)
summary(model)

#gintelligent
model<-lmer(goattrac~ 1
            +gintelligent*impin+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gintelligent| id)
            ,data=rel)
summary(model)

#gconfident

model<-lmer(goattrac~ 1
            +gconfident*impco+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gconfident| id)
            ,data=rel)
summary(model)

#gfunny

model<-lmer(goattrac~ 1
            +gfunny*imphp+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfunny| id)
            ,data=rel)
summary(model)

#gpercfunny

model<-lmer(goattrac~ 1
            +gpercfunny*impha+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gpercfunny| id)
            ,data=rel)
summary(model)

#gcreative

model<-lmer(goattrac~ 1
            +gcreative*impcr+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gcreative| id)
            ,data=rel)

summary(model)
################################################################################
################################################################################

################################################################################
##################STUDY 1    Absolute difference################################
################################################################################

prefs=recent17[,c("id","sessionid","sex","age","prefa","preba","preku","pream","prein","precr","preco","prehp","preha")]

completeprefs=recent[which(complete.cases(recent[,c("id","sessionid","sex","age","prefa","preba","preku","pream","prein","precr","preco","prehp","preha")])),]
sum(complete.cases(recent[,c("prefa","preba","preku","pream","prein","precr","preco","prehp","preha")]))
personlevelcomplete=unique(completeprefs[,c("id","sessionid","sex",
                             "prefa","preba","preku","pream","prein",
                             "precr","preco","prehp","preha","age"
)])
#1740 complete rows

completeprefs$ED=sqrt((completeprefs$prefa-completeprefs$gfattrac)^2+
  (completeprefs$preba-completeprefs$gbattrac)^2+
  (completeprefs$preku-completeprefs$gkindunder)^2+
  (completeprefs$pream-completeprefs$gambitious)^2+
  (completeprefs$prein-completeprefs$gintelligent)^2+
  (completeprefs$precr-completeprefs$gcreative)^2+
  (completeprefs$preco-completeprefs$gconfident)^2+
  (completeprefs$prehp-completeprefs$gfunny)^2+
  (completeprefs$preha-completeprefs$gpercfunny)^2)


recent17$EDfa=abs(recent17$prefa-recent17$gfattrac)
recent17$EDba=sqrt((recent17$preba-recent17$gbattrac)^2)
recent17$EDku= sqrt((recent17$preku-recent17$gkindunder)^2)
recent17$EDam= sqrt((recent17$pream-recent17$gambitious)^2)
recent17$EDin= sqrt((recent17$prein-recent17$gintelligent)^2)
recent17$EDcr= sqrt((recent17$precr-recent17$gcreative)^2)
recent17$EDco= sqrt((recent17$preco-recent17$gconfident)^2)
recent17$EDhp= sqrt((recent17$prehp-recent17$gfunny)^2)
recent17$EDha= sqrt((recent17$preha-recent17$gpercfunny)^2)

### Study 3 omnibus Euclidean distance
model<-lmer(scale(goattrac)~ 1
            +scale(ED)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)


# > model<-lmer(scale(goattrac)~ 1
# +             +scale(ED)+sex
# +             +(1|sessionid )
# +             +(1| id)
# +             +(1|partnerid)
# +             +(1|year)
# +             ,data=prefs)
# boundary (singular) fit: see help('isSingular')
# > summary(model)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: scale(goattrac) ~ 1 + scale(ED) + sex + (1 | sessionid) + (1 |      id) + (1 | partnerid) + (1 | year)
# Data: prefs
# 
# REML criterion at convergence: 4084
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5227 -0.5402  0.0046  0.5294  3.3524 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# partnerid (Intercept) 0.15252  0.3905  
# id        (Intercept) 0.25752  0.5075  
# sessionid (Intercept) 0.00000  0.0000  
# year      (Intercept) 0.01046  0.1023  
# Residual              0.33578  0.5795  
# Number of obs: 1724, groups:  partnerid, 561; id, 553; sessionid, 89; year, 3
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.05269    0.07400    2.54869  -0.712  0.53604    
# scale(ED)     -0.54581    0.02166 1632.23871 -25.195  < 2e-16 ***
#   sexM           0.16082    0.06192  644.44231   2.597  0.00961 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) sc(ED)
# scale(ED) -0.010       
# sexM      -0.415  0.022
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

###Study 1 absolute difference

model<-lmer(scale(goattrac)~ 1
            +scale(EDfa)
            +scale(prefa)
            +scale(gfattrac)
            +sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDba)
            +scale(preba)
            +scale(gbattrac)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDku)            
            +scale(preku)
            +scale(gkindunder)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDam)            
            +scale(pream)
            +scale(gambitious)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDin)
            +scale(prein)
            +scale(gintelligent)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)



model<-lmer(scale(goattrac)~ 1
            +scale(EDco)            
            +scale(preco)
            +scale(gconfident)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDhp)            
            +scale(prehp)
            +scale(gfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDha)            
            +scale(preha)
            +scale(gpercfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)


model<-lmer(scale(goattrac)~ 1
            +scale(EDcr)            
            +scale(precr)
            +scale(gcreative)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

######################################################################################
######################importances######################################################

#Study 1, investigating disconcordant preference measurement type with method (i.e. using abs difference with importances)
completeimps=recent[which(complete.cases(recent[,c("id","sessionid","sex","age","impfa",
                                                   "impba","impku","impam","impin","impcr","impco","imphp","impha")])),]
sum(complete.cases(recent[,c("impfa","impba","impku","impam","impin","impcr","impco","imphp","impha")]))
personlevelcomplete=unique(completeprefs[,c("id","sessionid","sex",
                                            "impfa",
                                            "impba","impku","impam","impin","impcr","impco","imphp","impha","age"
)])

#1740 complete rows

recent17$EDfa_i=sqrt((recent17$impfa-recent17$gfattrac)^2)
recent17$EDba_i=sqrt((recent17$impba-recent17$gbattrac)^2)
recent17$EDku_i= sqrt((recent17$impku-recent17$gkindunder)^2)
recent17$EDam_i= sqrt((recent17$impam-recent17$gambitious)^2)
recent17$EDin_i= sqrt((recent17$impin-recent17$gintelligent)^2)
recent17$EDcr_i= sqrt((recent17$impcr-recent17$gcreative)^2)
recent17$EDco_i= sqrt((recent17$impco-recent17$gconfident)^2)
recent17$EDhp_i= sqrt((recent17$imphp-recent17$gfunny)^2)
recent17$EDha_i= sqrt((recent17$impha-recent17$gpercfunny)^2)





###

model<-lmer(scale(goattrac)~ 1
            +scale(EDfa_i)
            +scale(impfa)
            +scale(gfattrac)
            +sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDba_i)
            +scale(impba)
            +scale(gbattrac)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDku_i)            
            +scale(impku)
            +scale(gkindunder)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDam_i)            
            +scale(impam)
            +scale(gambitious)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDin_i)
            +scale(impin)
            +scale(gintelligent)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDco_i)            
            +scale(impco)
            +scale(gconfident)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)




model<-lmer(scale(goattrac)~ 1
            +scale(EDhp_i)            
            +scale(imphp)
            +scale(gfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDha_i)            
            +scale(impha)
            +scale(gpercfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDcr_i)            
            +scale(impcr)
            +scale(gcreative)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17)
summary(model)

######################################
#############pref level, level metric
###########interaction#########
options(digits=8)
#gfattrac
rel=data.frame("id"=recent$id,
               "sex"=recent$sex,
               "year"=recent$year,
               "partnerid"=recent$partnerid,
               "sessionid"=recent$sessionid,
               "goattrac"=scale(recent$goattrac),
               "gfattrac"=scale(recent$gfattrac),
               "prefa"=scale(recent$prefa),
               "gbattrac"=scale(recent$gbattrac),
               "preba"=scale(recent$preba),
               "gkindunder"=scale(recent$gkindunder),
               "preku"=scale(recent$preku),
               "gambitious"=scale(recent$gambitious),
               "pream"=scale(recent$pream),
               "gintelligent"=scale(recent$gintelligent),
               "prein"=scale(recent$prein),
               "gconfident"=scale(recent$gconfident),
               "preco"=scale(recent$preco),
               "gfunny"=scale(recent$gfunny),
               "prehp"=scale(recent$prehp),
               "gpercfunny"=scale(recent$gpercfunny),
               "preha"=scale(recent$preha),
               "gcreative"=scale(recent$gcreative),
               "precr"=scale(recent$precr)
)

model<-lmer(goattrac~ 1
            +gfattrac*prefa+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfattrac| id)
            ,data=rel)
summary(model)

#gbattrac

model<-lmer(goattrac~ 1
            +gbattrac*preba+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gbattrac| id)            
            ,data=rel)
summary(model)

#gkindunder

model<-lmer(goattrac~ 1
            +gkindunder*preku+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gkindunder| id)
            ,data=rel)
summary(model)

#gambitious

model<-lmer(goattrac~ 1
            +gambitious*pream+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gambitious| id)
            ,data=rel)
summary(model)

#gintelligent

model<-lmer(goattrac~ 1
            +gintelligent*prein+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gintelligent| id)
            ,data=rel)
summary(model)

#gconfident

model<-lmer(goattrac~ 1
            +gconfident*preco+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gconfident| id)
            ,data=rel)
summary(model)

#gfunny

model<-lmer(goattrac~ 1
            +gfunny*prehp+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfunny| id)
            ,data=rel)
summary(model)

#gpercfunny

model<-lmer(goattrac~ 1
            +gpercfunny*preha+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gpercfunny| id)
            ,data=rel)
summary(model)

#gcreative

model<-lmer(goattrac~ 1
            +gcreative*precr+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gcreative| id)
            ,data=rel)

summary(model)
################################################################################
#############SHUFFLE ED#########################################################
#Study 3
#shuffle preferences

############### SHUFFLE ED  #################################################

install.packages("tictoc")
library(tictoc)
tic()
n=50000
record=data.frame(traits=rep(NA,n),
                  estimate=rep(NA,n),
                  t=rep(NA,n),
                  p=rep(NA,n),
                  samplesize=rep(NA,n),
                  blanks=rep(NA,n))

######################################################
#raw and preferences

for (j in 1:n){
  
  seed=sample(1:9,9)
  record$traits[j]<- toString(seed)
  shuffled=personprefs  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personprefs[,1+seed[a]]
    #print(a-1)
    #print(seed[a]-1)
  }
  tail(personprefs)
  tail(shuffled)
  seed
  
  
  
  shuffall=merge(shuffled,ratingsonly,by="id")
  head(shuffall)
  tail(shuffall)
  
  
  
  
  shuffall$ED=rep(NA,length(shuffall[,1]))
  for (i in 1:length(shuffall[,1])){
    
    x=shuffall[i,c("prefa","preba","preku","pream","prein",
                   "precr","preco","prehp","preha")]
    y=shuffall[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                   "gintelligent","gcreative","gconfident","gfunny","gpercfunny")]
    
    shuffall$ED[i]=sqrt((x$prefa-y$gfattrac)^2+
                           (x$preba-y$gbattrac)^2+
                           (x$preku-y$gkindunder)^2+
                           (x$pream-y$gambitious)^2+
                           (x$prein-y$gintelligent)^2+
                           (x$precr-y$gcreative)^2+
                           (x$preco-y$gconfident)^2+
                           (x$prehp-y$gfunny)^2+
                           (x$preha-y$gpercfunny)^2)
    
    
  }
  #0 instances of no ratings or preferences
  
  #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
  
  
  
  
  
  
  
  model<-lmer(scale(goattrac)~ 1
              +scale(ED)+sex
              +(1|sessionid )
              +(1| id)
              +(1|partnerid)
              +(1|year)
              #+(Trait1|Importance1)
              #+(Trait1:Importance1|ParticipantID)
              ,data=shuffall)
  summary(model)
  
  record$estimate[j]<-summary(model)$coefficients[2,1]
  record$t[j]<-summary(model)$coefficients[2,4]
  record$p[j]<-summary(model)$coefficients[2,5]
  
}
toc()
write.csv(record,"Raw and pref EUCLIDEAN.csv")
#sum(record$t>= -25.195)
#head(record)














################################################################################
##############################PATTERN METRIC####################################
################################################################################
################################################################################
ratingsonly=subset(recent17,select=c("id","sex","sessionid","year", "partnerid","goattrac","gfattrac","gbattrac",
                                   "gkindunder","gambitious","gintelligent","gcreative",
                                   "gconfident","gfunny","gpercfunny"))
#PREFERENCES

#RAW PATTERN METRIC

count=0
recent17$rpre=rep(NA,length(recent17[,1]))
pre=recent17[,c("prefa","preba","preku","pream","prein",
              "precr","preco","prehp","preha")]

for (i in 1:length(pre[,1])){
  
  x=as.numeric(pre[i,c("prefa","preba","preku","pream","prein",
                          "precr","preco","prehp","preha")])
  y=as.numeric(ratingsonly[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                          "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])

  nax=which(is.na(x))
  nay=which(is.na(y))
  #rmindex=NULL
  if (length(nax)==0 & length(nay)==0){
    recent17$rpre[i]=cor(x,y)
  }else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    recent17$rpre[i]=cor(x,y)
    if(length(rmindex)==9){
      #print("zero")
      count=count+1
    }
  }
  #0 instances of no ratings or preferences
  
  #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
  
}
count
#12 all missing
sum(recent17$rpre==0,na.rm=T)
#74 instances of r=0
sum(recent17$rpre==1,na.rm=T)
sum(recent17$rpre==-1,na.rm=T)

sum(is.na(recent17$rpre))
#70 NAs
sum(!is.na(recent17$rpre))
#1674
recent17$zpre=.5*(log(1+recent17$rpre) -log(1-recent17$rpre))

ggplot(recent17, aes(x=rpre)) + geom_histogram(color="black", fill="white")
ggplot(recent17, aes(x=zpre)) + geom_histogram(color="black", fill="white")
plot(goattrac~rpre,data=recent)
#

model<-lmer(scale(goattrac)~ 1
            +scale(rpre)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            #+(Trait1|Importance1)
            #+(Trait1:Importance1|ParticipantID)
            ,data=recent17)
summary(model)




################################################################################
###Corrected


recent17$rpre_c=rep(NA,length(recent17[,1]))
pre_c=apply(pre,2,scale,scale=F)
rating_c=apply(ratingsonly[6:15],2,scale,scale=F)

count=0
for (i in 1:length(recent17[,1])){
  
  x=as.numeric(pre_c[i,c("prefa","preba","preku","pream","prein",
                       "precr","preco","prehp","preha")])
  y=as.numeric(rating_c[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                          "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])
  #  x=x-4
  #  y=y-4
  nax=which(is.na(x))
  nay=which(is.na(y))
  #rmindex=NULL
  if (length(nax)==0 & length(nay)==0){
    recent17$rpre_c[i]=cor(x,y)
  }
  else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    recent17$rpre_c[i]=cor(x,y)
    if(length(rmindex)==9){
      print("zero")
      count=count+1
    }
  }
  #0 instances of no ratings or preferences
  
  #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
  
}
count
#12 all missing

model<-lmer(scale(goattrac)~ 1
            +scale(rpre_c)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            #+(Trait1|Importance1)
            #+(Trait1:Importance1|ParticipantID)
            ,data=recent17)
summary(model)




###############################################################


#shuffle preferences
head(personlevel)
personprefs=subset(personlevel,select=c("id","prefa","preba","preku","pream","prein",
                                          "precr","preco","prehp","preha"))
personimps=subset(personlevel,select=c("id","impfa","impba","impku","impam","impin",
                                        "impcr","impco","imphp","impha"))


personprefs_c=personprefs
personimps_c=personimps
personprefs_c[,2:10]=apply(personprefs[,2:10],2,scale,scale=F)

personimps_c[,2:10]=apply(personimps[,2:10],2,scale,scale=F)







tail(personprefs_c)

ratingsonly=subset(recent,select=c("id","sex","sessionid","year", "partnerid","goattrac","gfattrac","gbattrac",
                                   "gkindunder","gambitious","gintelligent","gcreative",
                                   "gconfident","gfunny","gpercfunny"))
head(ratingsonly)
ratingsonly_c=ratingsonly

ratingsonly_c[,6:14]=apply(ratingsonly[,6:14],2,scale,scale=F)
######################################################
####permutation test
####################################################
install.packages("tictoc")
library(tictoc)
tic()
n=50000
record=data.frame(traits=rep(NA,n),
                  estimate=rep(NA,n),
                  t=rep(NA,n),
                  p=rep(NA,n),
                  samplesize=rep(NA,n),
                  blanks=rep(NA,n))

######################################################
#Raw and preferences

for (j in 1:n){
  

seed=sample(1:9,9)
record$traits[j]<- toString(seed)
shuffled=personprefs

ntraits=9
for (a in 1:ntraits){
  shuffled[,1+a]=personprefs[,1+seed[a]]
  #print(a-1)
  #print(seed[a]-1)
}
tail(personprefs)
tail(shuffled)
seed



shuffall=merge(shuffled,ratingsonly,by="id")
head(shuffall)
tail(shuffall)




count=0
shuffall$rpre=rep(NA,length(shuffall[,1]))
for (i in 1:length(shuffall[,1])){
  
  x=as.numeric(shuffall[i,c("prefa","preba","preku","pream","prein",
                          "precr","preco","prehp","preha")])
  y=as.numeric(shuffall[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                          "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])
  nax=which(is.na(x))
  nay=which(is.na(y))
  #rmindex=NULL
  if (length(nax)==0 & length(nay)==0){
    shuffall$rpre[i]=cor(x,y)
  }
  else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    shuffall$rpre[i]=cor(x,y)
    if(length(rmindex)==9){
      #print("zero")
      count=count+1
    }
  }
  #0 instances of no ratings or preferences
  
  #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
  
}
count
record$blanks[j]<-count
sum(shuffall$rpre==0,na.rm=T)
#74 instances of r=0
sum(shuffall$rpre==1,na.rm=T)
sum(shuffall$rpre==-1,na.rm=T)

sum(is.na(shuffall$r))
#2225 NAs
sum(!is.na(shuffall$r))
record$samplesize[j]<-sum(!is.na(shuffall$rpre))
#1674 

#shuffall$zpre=.5*(log(1+shuffall$rpre) -log(1-shuffall$rpre))

ggplot(shuffall, aes(x=rpre)) + geom_histogram(color="black", fill="white")
#ggplot(shuffall, aes(x=zpre)) + geom_histogram(color="black", fill="white")

#

model<-lmer(scale(goattrac)~ 1
            +scale(rpre)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            #+(Trait1|Importance1)
            #+(Trait1:Importance1|ParticipantID)
            ,data=shuffall)
summary(model)

record$estimate[j]<-summary(model)$coefficients[2,1]
record$t[j]<-summary(model)$coefficients[2,4]
record$p[j]<-summary(model)$coefficients[2,5]

}
toc()
write.csv(record,"Raw and pref.csv")

sum(record$t>= 2.118)
15721/50000
head(record)


####################################################################################
install.packages("tictoc")
library(tictoc)
tic()
n=50000
record=data.frame(traits=rep(NA,n),
                  estimate=rep(NA,n),
                  t=rep(NA,n),
                  p=rep(NA,n),
                  samplesize=rep(NA,n),
                  blanks=rep(NA,n))

######################################################
#permutation test
#corrected pattern metric and preferences

for (j in 1:n){
  
  
  seed=sample(1:9,9)
  record$traits[j]<- toString(seed)
  shuffled=personprefs_c  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personprefs_c[,1+seed[a]]
    #print(a-1)
    #print(seed[a]-1)
  }
  tail(personprefs_c)
  tail(shuffled)
  seed
  
  
  
  shuffall=merge(shuffled,ratingsonly_c,by="id")
  head(shuffall)
  tail(shuffall)
  
  
  
  
  count=0
  shuffall$rpre=rep(NA,length(shuffall[,1]))
  for (i in 1:length(shuffall[,1])){
    
    x=as.numeric(shuffall[i,c("prefa","preba","preku","pream","prein",
                              "precr","preco","prehp","preha")])
    y=as.numeric(shuffall[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                              "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])
    nax=which(is.na(x))
    nay=which(is.na(y))
    #rmindex=NULL
    if (length(nax)==0 & length(nay)==0){
      shuffall$rpre[i]=cor(x,y)
    }
    else {
      rmindex=c(na.omit(nax),na.omit(nay))
      rmindex=unique(rmindex)
      x=x[-rmindex]
      y=y[-rmindex]
      shuffall$rpre[i]=cor(x,y)
      if(length(rmindex)==9){
        #print("zero")
        count=count+1
      }
    }
    #0 instances of no ratings or preferences
    
    #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
    
  }
  count
  record$blanks[j]<-count
  sum(shuffall$rpre==0,na.rm=T)
  #74 instances of r=0
  sum(shuffall$rpre==1,na.rm=T)
  sum(shuffall$rpre==-1,na.rm=T)
  
  sum(is.na(shuffall$r))
  #2225 NAs
  sum(!is.na(shuffall$r))
  record$samplesize[j]<-sum(!is.na(shuffall$rpre))
  #1674 
  
  #shuffall$zpre=.5*(log(1+shuffall$rpre) -log(1-shuffall$rpre))
  
  ggplot(shuffall, aes(x=rpre)) + geom_histogram(color="black", fill="white")
  #ggplot(shuffall, aes(x=zpre)) + geom_histogram(color="black", fill="white")
  
  #
  
  model<-lmer(scale(goattrac)~ 1
              +scale(rpre)+sex
              +(1|sessionid )
              +(1| id)
              +(1|partnerid)
              +(1|year)
              #+(Trait1|Importance1)
              #+(Trait1:Importance1|ParticipantID)
              ,data=shuffall)
  summary(model)
  
  record$estimate[j]<-summary(model)$coefficients[2,1]
  record$t[j]<-summary(model)$coefficients[2,4]
  record$p[j]<-summary(model)$coefficients[2,5]
  
}
toc()
write.csv(record,"Corrected and pref.csv")
sum(record$t>= 3.719)
#8
8/50000
head(record)


####################################################################################
################################################################################


###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################

#trait appeal
##importance



#9 traits

recent17$traitappeal=(recent17$impfa*(recent17$gfattrac-4)+
  recent17$impba*(recent17$gbattrac-4)+
  recent17$impku*(recent17$gkindunder-4)+
  recent17$impam*(recent17$gambitious-4)+
  recent17$impin*(recent17$gintelligent-4)+
  recent17$impcr*(recent17$gcreative-4)+
  recent17$impco*(recent17$gconfident-4)+
  recent17$imphp*(recent17$gfunny-4)+
  recent17$impha*(recent17$gpercfunny-4))
model<-lmer(scale(goattrac)~ 1
            +scale(traitappeal)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            #+(Trait1|Importance1)
            #+(Trait1:Importance1|ParticipantID)
            ,data=recent17)
summary(model)






################shuffle#################################
install.packages("tictoc")
library(tictoc)
tic()
n=50000
record=data.frame(traits=rep(NA,n),
                  estimate=rep(NA,n),
                  t=rep(NA,n),
                  p=rep(NA,n),
                  samplesize=rep(NA,n),
                  blanks=rep(NA,n))
personimps=subset(personlevel,personlevel$year %in% c(17,18,19),select=c("id","impfa","impba","impku","impam","impin",
                                       "impcr","impco","imphp","impha"))


ratingsonly=subset(recent,recent$year %in% c(17,18,19),select=c("id","sex","sessionid","year", "partnerid","goattrac","gfattrac","gbattrac",
                                   "gkindunder","gambitious","gintelligent","gcreative",
                                   "gconfident","gfunny","gpercfunny"))
#n=50000
for (j in 1:n){
  
  
  seed=sample(1:9,9)
  record$traits[j]<- toString(seed)
  shuffled=personimps  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personimps[,1+seed[a]]
    #print(a-1)
    #print(seed[a]-1)
  }
  tail(personimps)
  tail(shuffled)
  seed
  
  
  
  shuffall=merge(shuffled,ratingsonly,by="id")
  head(shuffall)
  tail(shuffall)
  
  
  
  
  count=0
  shuffall$traitappeal=rep(NA,length(shuffall[,1]))
  for (i in 1:length(shuffall[,1])){
    
    x=as.numeric(shuffall[i,c("impfa","impba","impku","impam","impin",
                              "impcr","impco","imphp","impha")])
    y=as.numeric(shuffall[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                              "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])
    nax=which(is.na(x))
    nay=which(is.na(y))
    #rmindex=NULL
    if (length(nax)==0 & length(nay)==0){
      shuffall$traitappeal[i]=x %*% (y-4)
    }else {
      
        #print("zero")
        count=count+1
        shuffall$traitappeal[i]=NA
      
      }
    
    #0 instances of no ratings or preferences
    
    #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
    
  
  count
  record$blanks[j]<-count
  record$samplesize[j]<-sum(!is.na(shuffall$traitappeal))
  #1744
}
  

  

  
  #shuffall$zpre=.5*(log(1+shuffall$rpre) -log(1-shuffall$rpre))
  
  ggplot(shuffall, aes(x=traitappeal)) + geom_histogram(color="black", fill="white")
  #ggplot(shuffall, aes(x=zpre)) + geom_histogram(color="black", fill="white")
  
  #
  
  model<-lmer(scale(goattrac)~ 1
              +scale(traitappeal)+sex
              +(1|sessionid )
              +(1| id)
              +(1|partnerid)
              +(1|year)
              #+(Trait1|Importance1)
              #+(Trait1:Importance1|ParticipantID)
              ,data=shuffall)
  summary(model)
  
  record$estimate[j]<-summary(model)$coefficients[2,1]
  record$t[j]<-summary(model)$coefficients[2,4]
  record$p[j]<-summary(model)$coefficients[2,5]
  
}
toc()
write.csv(record,"Trait appeal with imps test1112.csv")
sum(record$t>= 34.84)
head(record)




