library(lmerTest)
library(lme4)
library(lattice)
library(ggplot2)
#descriptive statistics

setwd("../Data and code/Analyses")
yeah=read.csv("2014-2019 Speed Dating Data.csv")
#years above 2014


yeah=subset(yeah,yeah$sessionid>=1400)

recent=data.frame(id=yeah$id,sessionid=yeah$sessionid, sex=yeah$sex,
                  goattrac=yeah$goattrac,
                  impfa=yeah$impfa,impba=yeah$impba,impku=yeah$impku,impam=yeah$impam,
                  impin=yeah$impin,impcr=yeah$impcr, impco=yeah$impco,
                  imphp=yeah$imphp, impha=yeah$impha,
                  prefa=yeah$prefa,preba=yeah$preba,preku=yeah$preku,pream=yeah$pream,
                  prein=yeah$prein,precr=yeah$precr, preco=yeah$preco,
                  prehp=yeah$prehp, preha=yeah$preha,
                  gfattrac=yeah$gfattrac,gbattrac=yeah$gbattrac,gkindunder=yeah$gkindunder,
                  gambitious=yeah$gambitious,
                  gintelligent=yeah$gintelligent,gcreative=yeah$gcreative, 
                  gconfident=yeah$gconfident,
                  gfunny=yeah$gfunny, gpercfunny=yeah$gpercfunny,males=yeah$males,
                  females=yeah$females,partnerid=yeah$partnerid,age=yeah$ageyo)

recent$year=rep(NA,length(recent$id))
recent$year=floor(recent$sessionid/100)
recent$year=as.factor(recent$year)
recent$age=as.numeric(recent$age)
recent$sessionid=as.factor(recent$sessionid)

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

maleratings=recent[recent$sex=="M",]
femaleratings=recent[recent$sex=="F",]

##
#correlation between preference levels and preference importance
cor.test(formula = ~prefa + impfa, data = personlevel)
cor.test(formula = ~preba + impba, data = personlevel)
cor.test(formula = ~preku + impku, data = personlevel)
cor.test(formula = ~pream + impam, data = personlevel)
cor.test(formula = ~prein + impin, data = personlevel)
cor.test(formula = ~precr + impcr, data = personlevel)
cor.test(formula = ~preco + impco, data = personlevel)
cor.test(formula = ~prehp + imphp, data = personlevel)
cor.test(formula = ~preha + impha, data = personlevel)


######################################################
#Info for preference level simulations (2017 to 2019)
unique(recent$year[!is.na(recent$pream)])
#17, 18, 19
mean(personlevel$age[personlevel$sex=="M" & personlevel$year %in% c(17,18,19)],na.rm=T)
mean(personlevel$age[personlevel$sex=="F"& personlevel$year %in% c(17,18,19)],na.rm=T)
sd(personlevel$age[personlevel$sex=="M"& personlevel$year %in% c(17,18,19)],na.rm=T)
sd(personlevel$age[personlevel$sex=="F"& personlevel$year %in% c(17,18,19)],na.rm=T)
min(personlevel$age[personlevel$year %in% c(17,18,19)])
max(personlevel$age[personlevel$year %in% c(17,18,19)])


######################################################
##Info for importance simulations 2014-2019##
mean(recent$males,na.rm=TRUE)
mean(recent$females,na.rm=TRUE)
sd(recent$males,na.rm=TRUE)
sd(recent$females,na.rm=TRUE)
#ratings but not relevant
mean(na.omit(unlist(femaleratings[,c(4,23:31)])))
sd(na.omit(unlist(femaleratings[,c(4,23:31)])))
mean(na.omit(unlist(maleratings[,c(4,23:31)])))
sd(na.omit(unlist(maleratings[,c(4,23:31)])))
#imp
mean(na.omit(unlist(maleinfo[,4:12])))
sd(na.omit(unlist(maleinfo[,4:12])))
mean(na.omit(unlist(femaleinfo[,4:12])))
sd(na.omit(unlist(femaleinfo[,4:12])))


######################################################rating################

round(colMeans(maleratings[,c(4,23:31)],na.rm=TRUE),2)
round(colMeans(femaleratings[,c(4,23:31)],na.rm=TRUE),2)


round(apply(maleratings[,c(4,23:31)],2,sd,na.rm=TRUE),2)
round(apply(femaleratings[,c(4,23:31)],2, sd,na.rm=TRUE),2)




################################################################################
# calculating the level metric ----


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
# using level metric on preference levels (for supplementary materials) ----
#we only have observations from 2017 onwards
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
#apply level metric to preference importances to the same years (2017 onwards)

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
######################Euclidean distance and absolute difference################
################################################################################
#study 1 results

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

completeprefs$EDfa=sqrt((completeprefs$prefa-completeprefs$gfattrac)^2)
completeprefs$EDba=sqrt((completeprefs$preba-completeprefs$gbattrac)^2)
completeprefs$EDku= sqrt((completeprefs$preku-completeprefs$gkindunder)^2)
completeprefs$EDam= sqrt((completeprefs$pream-completeprefs$gambitious)^2)
completeprefs$EDin= sqrt((completeprefs$prein-completeprefs$gintelligent)^2)
completeprefs$EDcr= sqrt((completeprefs$precr-completeprefs$gcreative)^2)
completeprefs$EDco= sqrt((completeprefs$preco-completeprefs$gconfident)^2)
completeprefs$EDhp= sqrt((completeprefs$prehp-completeprefs$gfunny)^2)
completeprefs$EDha= sqrt((completeprefs$preha-completeprefs$gpercfunny)^2)

### for study 3
model<-lmer(scale(goattrac)~ 1
            +scale(ED)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)


### for study 1

model<-lmer(scale(goattrac)~ 1
            +scale(EDfa)
            +scale(prefa)
            +scale(gfattrac)
            +sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDba)
            +scale(preba)
            +scale(gbattrac)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDku)            
            +scale(preku)
            +scale(gkindunder)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDam)            
            +scale(pream)
            +scale(gambitious)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDin)
            +scale(prein)
            +scale(gintelligent)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDcr)            
            +scale(precr)
            +scale(gcreative)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDco)            
            +scale(preco)
            +scale(gconfident)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDhp)            
            +scale(prehp)
            +scale(gfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDha)            
            +scale(preha)
            +scale(gpercfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeprefs)
summary(model)




################################################################################
###########apply above to preference importance#################################
#for supplementary materials

completeimps=recent[which(complete.cases(recent[,c("id","sessionid","sex","age","impfa",
                                                   "impba","impku","impam","impin","impcr","impco","imphp","impha")])),]
sum(complete.cases(recent[,c("impfa","impba","impku","impam","impin","impcr","impco","imphp","impha")]))
personlevelcomplete=unique(completeprefs[,c("id","sessionid","sex",
                                            "impfa",
                                            "impba","impku","impam","impin","impcr","impco","imphp","impha","age"
)])

#1740 complete rows

completeimps$EDfa=sqrt((completeimps$impfa-completeimps$gfattrac)^2)
completeimps$EDba=sqrt((completeimps$impba-completeimps$gbattrac)^2)
completeimps$EDku= sqrt((completeimps$impku-completeimps$gkindunder)^2)
completeimps$EDam= sqrt((completeimps$impam-completeimps$gambitious)^2)
completeimps$EDin= sqrt((completeimps$impin-completeimps$gintelligent)^2)
completeimps$EDcr= sqrt((completeimps$impcr-completeimps$gcreative)^2)
completeimps$EDco= sqrt((completeimps$impco-completeimps$gconfident)^2)
completeimps$EDhp= sqrt((completeimps$imphp-completeimps$gfunny)^2)
completeimps$EDha= sqrt((completeimps$impha-completeimps$gpercfunny)^2)



###

model<-lmer(scale(goattrac)~ 1
            +scale(EDfa)
            +scale(impfa)
            +scale(gfattrac)
            +sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDba)
            +scale(impba)
            +scale(gbattrac)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDku)            
            +scale(impku)
            +scale(gkindunder)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDam)            
            +scale(impam)
            +scale(gambitious)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDin)
            +scale(impin)
            +scale(gintelligent)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDco)            
            +scale(impco)
            +scale(gconfident)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDcr)            
            +scale(impcr)
            +scale(gcreative)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)



model<-lmer(scale(goattrac)~ 1
            +scale(EDhp)            
            +scale(imphp)
            +scale(gfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDha)            
            +scale(impha)
            +scale(gpercfunny)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=completeimps)
summary(model)

################################################################################
# Study 3 Permutation test ----
################################################################################
#############SHUFFLE EUCLIDEAN DISTANCES########################################
#shuffle preferences


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
#preference levels

for (j in 1:n){
  
  neworder=sample(1:9,9)
  record$traits[j]<- toString(neworder)
  shuffled=personprefs  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personprefs[,1+neworder[a]]
    #print(a-1)
    #print(neworder[a]-1)
  }
  tail(personprefs)
  tail(shuffled)
  neworder
  
  
  
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
  
  
  
  model<-lmer(scale(goattrac)~ 1
              +scale(ED)+sex
              +(1|sessionid )
              +(1| id)
              +(1|partnerid)
              +(1|year)
              ,data=shuffall)
  summary(model)
  
  record$estimate[j]<-summary(model)$coefficients[2,1]
  record$t[j]<-summary(model)$coefficients[2,4]
  record$p[j]<-summary(model)$coefficients[2,5]
  
}
toc()
write.csv(record,"Permutation test pref EUCLIDEAN.csv")
sum(record$t <= -25.195)
#head(record)



################################################################################
##############################PATTERN METRIC####################################
################################################################################

################################################################################
#IMPORTANCE
#pattern metric
count=0
recent$r=rep(NA,length(recent[,1]))
imp=recent[,c("impfa","impba","impku","impam","impin",
              "impcr","impco","imphp","impha")]
rating=recent[,c("gfattrac","gbattrac","gkindunder","gambitious",
                 "gintelligent","gcreative","gconfident","gfunny","gpercfunny")]

#raw
for (i in 1:length(recent[,1])){
  
  x=as.numeric(imp[i,c("impfa","impba","impku","impam","impin",
                          "impcr","impco","imphp","impha")])
  y=as.numeric(rating[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                          "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])

  nax=which(is.na(x))
  nay=which(is.na(y))

  if (length(nax)==0 & length(nay)==0){
    recent$r[i]=cor(x,y)
  }
  else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    recent$r[i]=cor(x,y)
    if(length(rmindex)==7){
      print("zero")
      count=count+1
    }
  }

}



#
model<-lmer(scale(goattrac)~ 1
            +scale(r)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent)
summary(model)



#corrected pm importances
#mean center prefs and ratings
imp_c=apply(imp,2,scale,scale=F)
rating_c=apply(rating,2,scale,scale=F)

recent$r_c=rep(NA,length(recent[,1]))

count=0
#corrected
for (i in 1:length(recent[,1])){
  
  x=as.numeric(imp_c[i,c("impfa","impba","impku","impam","impin",
                       "impcr","impco","imphp","impha")])
  y=as.numeric(rating_c[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                          "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])
  #y=y-4
  nax=which(is.na(x))
  nay=which(is.na(y))
  #rmindex=NULL
  if (length(nax)==0 & length(nay)==0){
    recent$r_c[i]=cor(x,y)
  }
  else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    recent$r_c[i]=cor(x,y)
    if(length(rmindex)==7){
      print("zero")
      count=count+1
    }
  }

}


sum(na.omit(recent$r==1))
sum(na.omit(recent$r==-1))
model<-lmer(scale(goattrac)~ 1
            +scale(r_c)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            #+(Trait1|Importance1)
            #+(Trait1:Importance1|ParticipantID)
            ,data=recent)
summary(model)



ggplot(recent, aes(x=r)) + geom_histogram(color="black", fill="white")
ggplot(recent, aes(x=r_c)) + geom_histogram(color="black", fill="white")
#recent$z=.5*(log(1+recent$r) -log(1-recent$r))
#ggplot(recent, aes(x=z)) + geom_histogram(color="black", fill="white")

sum(recent$r==0,na.rm=T)
sum(recent$r_c==0,na.rm=T)
sum(is.na(recent$r))



################################################################################
#Raw pattern metric
#PREFERENCE level


count=0
recent$rpre=rep(NA,length(recent[,1]))
pre=recent[,c("prefa","preba","preku","pream","prein",
              "precr","preco","prehp","preha")]

for (i in 1:length(recent[,1])){
  
  x=as.numeric(pre[i,c("prefa","preba","preku","pream","prein",
                          "precr","preco","prehp","preha")])
  y=as.numeric(rating[i,c("gfattrac","gbattrac","gkindunder","gambitious",
                          "gintelligent","gcreative","gconfident","gfunny","gpercfunny")])
#  x=x-4
#  y=y-4
  nax=which(is.na(x))
  nay=which(is.na(y))
  #rmindex=NULL
  if (length(nax)==0 & length(nay)==0){
    recent$rpre[i]=cor(x,y)
  }
  else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    recent$rpre[i]=cor(x,y)
    if(length(rmindex)==7){
      print("zero")
      count=count+1
    }
  }
  #0 instances of no ratings or preferences
  
  #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
  
}
count
sum(recent$rpre==0,na.rm=T)
#74 instances of r=0
sum(recent$rpre==1,na.rm=T)
sum(recent$rpre==-1,na.rm=T)

sum(is.na(recent$r))
#157 NAs
sum(!is.na(recent$r))
#3781
recent$zpre=.5*(log(1+recent$rpre) -log(1-recent$rpre))

ggplot(recent, aes(x=rpre)) + geom_histogram(color="black", fill="white")
ggplot(recent, aes(x=zpre)) + geom_histogram(color="black", fill="white")
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
            ,data=recent)
summary(model)


################################################################################
###Corrected pattern metric


recent$rpre_c=rep(NA,length(recent[,1]))
pre_c=apply(pre,2,scale,scale=F)

for (i in 1:length(recent[,1])){
  
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
    recent$rpre_c[i]=cor(x,y)
  }
  else {
    rmindex=c(na.omit(nax),na.omit(nay))
    rmindex=unique(rmindex)
    x=x[-rmindex]
    y=y[-rmindex]
    recent$rpre_c[i]=cor(x,y)
    if(length(rmindex)==7){
      print("zero")
      count=count+1
    }
  }
  #0 instances of no ratings or preferences
  
  #recent$r[i]=cor(as.numeric(recent[i,5:11]),as.numeric(recent[i,23:29]))
  
}
count


model<-lmer(scale(goattrac)~ 1
            +scale(rpre_c)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            #+(Trait1|Importance1)
            #+(Trait1:Importance1|ParticipantID)
            ,data=recent)
summary(model)



ggplot(recent, aes(x=rpre)) + geom_histogram(color="black", fill="white")
ggplot(recent, aes(x=rpre_c)) + geom_histogram(color="black", fill="white")



###############################################################
# permutation test

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
#Raw pattern metric and preferences

for (j in 1:n){
  

neworder=sample(1:9,9)
record$traits[j]<- toString(neworder)
shuffled=personprefs

ntraits=9
for (a in 1:ntraits){
  shuffled[,1+a]=personprefs[,1+neworder[a]]
  #print(a-1)
  #print(neworder[a]-1)
}
tail(personprefs)
tail(shuffled)
neworder



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
write.csv(record,"Permutation test pm raw and pref.csv")
sum(record$t>= 2.118)
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
#corrected and preferences

for (j in 1:n){
  
  
  neworder=sample(1:9,9)
  record$traits[j]<- toString(neworder)
  shuffled=personprefs_c  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personprefs_c[,1+neworder[a]]
    #print(a-1)
    #print(neworder[a]-1)
  }
  tail(personprefs_c)
  tail(shuffled)
  neworder
  
  
  
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
write.csv(record,"Permutation test pm corrected and pref.csv")
sum(record$t>= 3.719)
head(record)


####################################################################################
################################################################################
# permutation test raw pattern metric importance

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
#raw and importance

for (j in 1:n){
  
  
  neworder=sample(1:9,9)
  record$traits[j]<- toString(neworder)
  shuffled=personimps
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personimps[,1+neworder[a]]
    #print(a-1)
    #print(neworder[a]-1)
  }
  tail(personimps)
  tail(shuffled)
  neworder
  
  
  
  shuffall=merge(shuffled,ratingsonly,by="id")
  head(shuffall)
  tail(shuffall)
  
  
  
  
  count=0
  shuffall$rpre=rep(NA,length(shuffall[,1]))
  for (i in 1:length(shuffall[,1])){
    
    x=as.numeric(shuffall[i,c("impfa","impba","impku","impam","impin",
                              "impcr","impco","imphp","impha")])
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
              ,data=shuffall)
  summary(model)
  
  record$estimate[j]<-summary(model)$coefficients[2,1]
  record$t[j]<-summary(model)$coefficients[2,4]
  record$p[j]<-summary(model)$coefficients[2,5]
  
}
toc()
write.csv(record,"permutation test raw and imp pm.csv")
sum(record$t>= -0.525)
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
#corrected and importances

for (j in 1:n){
  
  
  neworder=sample(1:9,9)
  record$traits[j]<- toString(neworder)
  shuffled=personimps_c  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personimps_c[,1+neworder[a]]
    #print(a-1)
    #print(neworder[a]-1)
  }
  tail(personimps_c)
  tail(shuffled)
  neworder
  
  
  
  shuffall=merge(shuffled,ratingsonly_c,by="id")
  head(shuffall)
  tail(shuffall)
  
  
  
  
  count=0
  shuffall$rpre=rep(NA,length(shuffall[,1]))
  for (i in 1:length(shuffall[,1])){
    
    x=as.numeric(shuffall[i,c("impfa","impba","impku","impam","impin",
                              "impcr","impco","imphp","impha")])
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
write.csv(record,"permutation test corrected and imps pm.csv")
sum(record$t>= 4.684)
head(record)


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
#study 3

##importance
# years 2017-19 measure all 9 traits

#9 traits
recent9=subset(recent,recent$year %in% c(17,18,19))
recent9$traitappeal=(recent9$impfa*(recent9$gfattrac-4)+
  recent9$impba*(recent9$gbattrac-4)+
  recent9$impku*(recent9$gkindunder-4)+
  recent9$impam*(recent9$gambitious-4)+
  recent9$impin*(recent9$gintelligent-4)+
  recent9$impcr*(recent9$gcreative-4)+
  recent9$impco*(recent9$gconfident-4)+
  recent9$imphp*(recent9$gfunny-4)+
  recent9$impha*(recent9$gpercfunny-4))
model<-lmer(scale(goattrac)~ 1
            +scale(traitappeal)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent9)
summary(model)




#permutation test trait appeal


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
n=50000
for (j in 1:n){
  
  
  neworder=sample(1:9,9)
  record$traits[j]<- toString(neworder)
  shuffled=personimps  
  
  ntraits=9
  for (a in 1:ntraits){
    shuffled[,1+a]=personimps[,1+neworder[a]]
    #print(a-1)
    #print(neworder[a]-1)
  }
  tail(personimps)
  tail(shuffled)
  neworder
  
  
  
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
  if (j %% 10000==0){
    write.csv(record,"Permutation test trait appeal with imps.csv")
  }
}
toc()
write.csv(record,"Permutation test trait appeal with imps.csv")
sum(record$t>= 34.842, na.rm = T)
head(record)







#####################################################################
#testing 2017 onwards data for study 1 supplementary materials
#concordant prefs and analyses

recent17_scaled=recent17
recent17_scaled[,c("goattrac_c","gfattrac_c","gbattrac_c",
                   "gkindunder_c","gambitious_c","gintelligent_c","gcreative_c",
                   "gconfident_c","gfunny_c","gpercfunny_c")]=apply(recent17_scaled[,c("goattrac","gfattrac","gbattrac",
                         "gkindunder","gambitious","gintelligent","gcreative",
                         "gconfident","gfunny","gpercfunny")],2,scale)

personlevel17=unique(recent17_scaled[,c("id","sessionid","sex",
                               "impfa","impba","impku","impam","impin",
                               "impcr","impco","imphp","impha",
                               "prefa","preba","preku","pream","prein",
                               "precr","preco","prehp","preha","age","year","males","females")])
personlevel17[,c("prefa_c","preba_c","preku_c","pream_c","prein_c",
                 "precr_c","preco_c","prehp_c","preha_c","impfa_c","impba_c","impku_c","impam_c","impin_c",
                 "impcr_c","impco_c","imphp_c","impha_c")]=apply(personlevel17[,c("prefa","preba","preku","pream","prein",
                                     "precr","preco","prehp","preha","impfa","impba","impku","impam","impin",
                                     "impcr","impco","imphp","impha")],2,scale)

recent17_scaled=merge(personlevel17,recent17_scaled[,c("id","partnerid", "goattrac","gfattrac","gbattrac",
                                       "gkindunder","gambitious","gintelligent","gcreative",
                                       "gconfident","gfunny","gpercfunny","goattrac_c","gfattrac_c","gbattrac_c",
                                       "gkindunder_c","gambitious_c","gintelligent_c","gcreative_c",
                                       "gconfident_c","gfunny_c","gpercfunny_c")],by="id")


options(digits=8)
#gfattrac
model<-lmer(goattrac~ 1
            +gfattrac*prefa_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfattrac| id)
            ,data=recent17_scaled)
summary(model)

#gbattrac

model<-lmer(goattrac~ 1
            +gbattrac*preba_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gbattrac| id)            
            ,data=recent17_scaled)
summary(model)

#gkindunder
model<-lmer(goattrac~ 1
            +gkindunder*preku_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gkindunder| id)
            ,data=recent17_scaled)
summary(model)

#gambitious
model<-lmer(goattrac~ 1
            +gambitious*pream_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gambitious| id)
            ,data=recent17_scaled)
summary(model)

#gintelligent
model<-lmer(goattrac~ 1
            +gintelligent*prein_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gintelligent| id)
            ,data=recent17_scaled)
summary(model)

#gconfident
model<-lmer(goattrac~ 1
            +gconfident*preco_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gconfident| id)
            ,data=recent17_scaled)
summary(model)

#gfunny
model<-lmer(goattrac~ 1
            +gfunny*prehp_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfunny| id)
            ,data=recent17_scaled)
summary(model)

#gpercfunny
model<-lmer(goattrac~ 1
            +gpercfunny*preha_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gpercfunny| id)
            ,data=recent17_scaled)
summary(model)

#gcreative
model<-lmer(goattrac~ 1
            +gcreative*precr_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gcreative| id)
            ,data=recent17_scaled)

summary(model)
###########################################################
#imp
#gfattrac
model<-lmer(goattrac~ 1
            +gfattrac*impfa_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfattrac| id)
            ,data=recent17_scaled)
summary(model)

#gbattrac

model<-lmer(goattrac~ 1
            +gbattrac*impba_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gbattrac| id)            
            ,data=recent17_scaled)
summary(model)

#gkindunder
model<-lmer(goattrac~ 1
            +gkindunder*impku_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gkindunder| id)
            ,data=recent17_scaled)
summary(model)

#gambitious
model<-lmer(goattrac~ 1
            +gambitious*impam_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gambitious| id)
            ,data=recent17_scaled)
summary(model)

#gintelligent
model<-lmer(goattrac~ 1
            +gintelligent*impin_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gintelligent| id)
            ,data=recent17_scaled)
summary(model)

#gconfident
model<-lmer(goattrac~ 1
            +gconfident*impco_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gconfident| id)
            ,data=recent17_scaled)
summary(model)

#gfunny
model<-lmer(goattrac~ 1
            +gfunny*imphp_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gfunny| id)
            ,data=recent17_scaled)
summary(model)

#gpercfunny
model<-lmer(goattrac~ 1
            +gpercfunny*impha_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gpercfunny| id)
            ,data=recent17_scaled)
summary(model)

#gcreative
model<-lmer(goattrac~ 1
            +gcreative*impcr_c+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            +(gcreative| id)
            ,data=recent17_scaled)
summary(model)

##########prefs
recent17_scaled$EDfa_i=abs(recent17_scaled$impfa-recent17_scaled$gfattrac)
recent17_scaled$EDba_i=abs(recent17_scaled$impba-recent17_scaled$gbattrac)
recent17_scaled$EDku_i= abs(recent17_scaled$impku-recent17_scaled$gkindunder)
recent17_scaled$EDam_i= abs(recent17_scaled$impam-recent17_scaled$gambitious)
recent17_scaled$EDin_i= abs(recent17_scaled$impin-recent17_scaled$gintelligent)
recent17_scaled$EDcr_i= abs(recent17_scaled$impcr-recent17_scaled$gcreative)
recent17_scaled$EDco_i= abs(recent17_scaled$impco-recent17_scaled$gconfident)
recent17_scaled$EDhp_i= abs(recent17_scaled$imphp-recent17_scaled$gfunny)
recent17_scaled$EDha_i= abs(recent17_scaled$impha-recent17_scaled$gpercfunny)



model<-lmer(scale(goattrac)~ 1
            +scale(EDfa_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDba_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDku_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDam_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDin_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDcr_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDco_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDha_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDhp_i)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)


recent17_scaled$EDfa=abs(recent17_scaled$prefa-recent17_scaled$gfattrac)
recent17_scaled$EDba=abs(recent17_scaled$preba-recent17_scaled$gbattrac)
recent17_scaled$EDku= abs(recent17_scaled$preku-recent17_scaled$gkindunder)
recent17_scaled$EDam= abs(recent17_scaled$pream-recent17_scaled$gambitious)
recent17_scaled$EDin= abs(recent17_scaled$prein-recent17_scaled$gintelligent)
recent17_scaled$EDcr= abs(recent17_scaled$precr-recent17_scaled$gcreative)
recent17_scaled$EDco= abs(recent17_scaled$preco-recent17_scaled$gconfident)
recent17_scaled$EDhp= abs(recent17_scaled$prehp-recent17_scaled$gfunny)
recent17_scaled$EDha= abs(recent17_scaled$preha-recent17_scaled$gpercfunny)


model<-lmer(scale(goattrac)~ 1
            +scale(EDfa)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDba)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDku)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDam)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDin)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDcr)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDco)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDha)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

model<-lmer(scale(goattrac)~ 1
            +scale(EDhp)+sex
            +(1|sessionid )
            +(1| id)
            +(1|partnerid)
            +(1|year)
            ,data=recent17_scaled)
summary(model)

mean(recent17$males)
mean(recent17$females)
sd(recent17$males)
sd(recent17$females)





