#library(lme4)
#library(lmerTest)
library(parallel)
library(foreach)
library(doParallel)
#length(unique(yeah$sessionid[yeah$sessionid>=1400]))
#171 speed-dating sessions from real data

rm(list=ls(all=TRUE)) #remove all saved variables in workspace

packages <- c("parallel", "foreach", "doParallel")
num_sims <- 1000 #This is the number of simulations I want, I’m going to ask foreach to run through 1:num_sims in parallel

registerDoParallel(min(detectCores()-1,num_sims)) #This establishes how many cores to use (it will use the minimum of either the total number of cores for this computer – 1, or the total number of sims)





euclidean <- function(a, b) sqrt(sum((a - b)^2))
traitstested=25#c(2,3,5,9,10,15,20,25)#c(2,3,5,9,10,15,20,25)#, 15)#, 20, 25)
#noisetested=c(0,5,10,20,30,40,50)
num=1000 #number of simulations



  

abm=function(t){


#for (t in traitstested){
#  for (noisy in noisetested){
#saved <- data.frame(estimate=rep(NA,num),p=rep(NA,num)) #save MLM output
data=vector(mode = "list", length = num)
    
for (iterate in 1:num){
  
  nsession=89 #number of speed dating sessions
  ntraits=t #number of traits
  #noise=noisy #scaling of noise, if 0, stated preference fully informs revealed preferences

  ndatesm<-rnorm(nsession,3.157,0.732)
  ndatesm<-round(ndatesm)
  ndatesf<-rnorm(nsession,3.433,0.641)
  ndatesf<-round(ndatesf)
  
  #no more than 5 of each sex per session, no less than 2 of each sex per session
  ndatesm[ndatesm <2] <- 2
  ndatesm[ndatesm >5] <- 5
  ndatesf[ndatesf <2] <- 2
  ndatesf[ndatesf >5] <- 5
  
  #generate all ID numbers
  IDm=1000:(sum(ndatesm)+1000-1) #Male ID
  IDf=2000:(sum(ndatesf)+2000-1) #Female ID
  
  ##########################################################################
  #############make variable#####################
  #initialise data frame, ID of participants for each session:
  IDDatesm=matrix(nrow=nsession,ncol=max(ndatesm))
  IDDatesm=as.data.frame(IDDatesm)
  
  IDDatesf=matrix(nrow=nsession,ncol=max(ndatesf))
  IDDatesf=as.data.frame(IDDatesf)
  
  #initialise data frame, possible partner options for each participant
  optionsm=data.frame(participantf=numeric(), partner_m=numeric()); #female participant, m partner options
  optionsf=data.frame(participantm=numeric(), partner_f=numeric()); #male participant, f partner options
  
  count=0
  count1=0
  iter=1
  iter1=1
  
  for (i in 1:nsession){
    k=ndatesm[i] #number of male participants per session
    g=ndatesf[i] #number of female participants per session
    IDDatesm[i,1:k]=IDm[(count+1):(count+k)] #%male IDs per session
    IDDatesf[i,1:g]=IDf[(count1+1):(count1+g)] #%female IDs per session
    
    for (j in 1:g){
      for (m in 1:k){
        #%female participant ID and male partner ID
        optionsm[iter,]=c(IDDatesf[i,j], IDDatesm[i,m])
        iter=iter+1 #%new row in dataframe
      }
    }
    
    for (j in 1:k)
      for (m in 1:g){
        #%male participant ID and female partner ID
        optionsf[iter1,]=c(IDDatesm[i,j], IDDatesf[i,m])
        iter1=iter1+1
      }
    count=count+k;
    count1=count1+g
  }
  
  
  # show(ndatesf)
  # show(ndatesm)
  # show(optionsm)
  # show(optionsf)
  
  #########################################################################
  #datem: keeps track of current session (male participants)
  datem=data.frame(sessionID=numeric(length(optionsf$participantm)));
  
  #interactionm: keeps track of current interaction within session (male participants)
  interactionm=data.frame(interact=numeric(length(optionsf$participantm)));
  
  datef=data.frame(sessionID=numeric(length(optionsm$participantf)));
  interactionf=data.frame(interact=numeric(length(optionsm$participantf)));
  
  n=1 
  n1=1
  jcount=1 
  jcount1=1
  #for male participants with female partners
  for (i in 1:nsession){
    for (j in 0:(ndatesm[i]-1)){
      for (k in 1:(ndatesf[i])){
        datem[n,1]=i
        interactionm[n,1]=k
        n=n+1
      }
      jcount=jcount+1
    }
    for (j in 0:(ndatesf[i]-1)){
      for (k in 1:(ndatesm[i])){
        datef[n1,1]=i
        interactionf[n1,1]=k
        n1=n1+1
      }
      jcount1=jcount1+1
    }
  }
  
  #store pairings, session number, interaction number in data frame
  m=data.frame(optionsf,datem,interactionm)
  f=data.frame(optionsm,datef,interactionf)
  
  ####################################################################################
  
  #generate latent trait vales for females
  latentf=matrix(NA,nrow=sum(ndatesf),ncol=ntraits)
  latentf=as.data.frame(latentf)
  #individual rating bias
  biasf=data.frame(bias=numeric(sum(ndatesf)))
  #importance of traits
  importancef=matrix(NA,nrow=sum(ndatesf),ncol=ntraits)
  importancef=as.data.frame(importancef)
  
  latentm=matrix(NA,nrow=sum(ndatesm),ncol=ntraits)
  latentm=as.data.frame(latentm)
  biasm=data.frame(bias=numeric(sum(ndatesm)))#%people have different rating biases
  importancem=matrix(NA,nrow=sum(ndatesm),ncol=ntraits)
  importancem=as.data.frame(importancem)
  
  
  ######males########
  for (i in 1:sum(ndatesm)){
    for (j in 1:ntraits){
      importancem[i,j]=rnorm(1,4,1.5) #range from 1 to 7
      latentm[i,j]=rnorm(1,4,1.5) #%std dev=1.7, mean=4
      biasm[i,1]= rnorm(1,0,1.5) #mean=0, std=1.5
      
      
    }
  }
  #latent trait value cannot be less than 1 or more than 7
  latentm[latentm<1]<-1
  latentm[latentm>7]<-7
  
  #importance cannot be less than 1 or more than 7
  importancem[importancem<1]<-1
  importancem[importancem>7]<-7
  importancem<-round(importancem)
  
  ###########female#############
  for (i in 1:sum(ndatesf)){
    for (j in 1:ntraits){
      importancef[i,j]=rnorm(1,4,1.5)
      latentf[i,j]=rnorm(1,4,1.5) #%std dev=2, mean=4
      biasf[i,1]= rnorm(1,0,1.5) #mean=0, std=1.5
      
    }
  }
  #latent trait value cannot be less than 1 or more than 7
  latentf[latentf<1]<-1
  latentf[latentf>7]<-7
  
  #importance cannot be less than 1 or more than 7
  importancef[importancef<1]<-1
  importancef[importancef>7]<-7
  importancef<-round(importancef)
  
  #traits of male participants (ID, latent traits, rating bias, importance of trait)
  traitsm=data.frame(IDm,latentm, biasm, importancem)
  colnames(traitsm)[2:(1+ntraits)]<-paste("latent",1:ntraits)
  colnames(traitsm)[(1+ntraits+1+1):(1+ntraits+1+ntraits)]<-paste("importance",1:ntraits)
  
  traitsf=data.frame(IDf,latentf, biasf, importancef)
  colnames(traitsf)[2:(1+ntraits)]<-paste("latent",1:ntraits)
  colnames(traitsf)[(1+ntraits+1+1):(1+ntraits+1+ntraits)]<-paste("importance",1:ntraits)
  
  #ratings of male partners
  rating=matrix(NA,nrow=length(f$partner_m),ncol=ntraits)
  ratingm=as.data.frame(rating)
  
  
  #trait appeal of male partners, females rate males
  #ED=matrix(NA,nrow=length(f$partner_m),ncol=1)
  #EDm=as.data.frame(ED)
  
  #ratings of female partners
  rating=matrix(NA,nrow=length(m$partner_f),ncol=ntraits)
  ratingf=as.data.frame(rating)
  
  
  #trait appeal of female partners
  #ED=matrix(NA,nrow=length(m$partner_f),ncol=1)
  #EDf=as.data.frame(ED)
  
  #################################################################################
  
  #%males rate females
  for (i in 1:length(f$participantf)){
    for (j in 1:ntraits){
      #%bias for each rater
      #rating= bias*random +latent
      ratingf[i,j]=traitsm$bias[match(m$participantm[i],IDm)]+rnorm(1,0,1)+
        latentf[match(m$partner_f[i],IDf),j]
      
      #rating can't be less than 1, no more than 7
      if (ratingf[i,j] >7){
        ratingf[i,j]=7
      }
      else if (ratingf[i,j]<1){
        ratingf[i,j]=1
      }
      else{
        ratingf[i,j]=round(ratingf[i,j])
      }

      
      
    }

  }
  
  #########################for scaling
  
  #importancef=apply(importancef,2,scale,scale=F)
  #ratingf=apply(ratingf,2,scale,scale=F)
  #for (i in 1:length(f$participantf)){
  #  vec=c(m$participantm[i],m$partner_f[i])
  #  EDf[i,]=euclidean(importancem[match(m$participantm[i],IDm),],ratingf[which(m$participantm==vec[1] & m$partner_f==vec[2]),]+
  #                      rnorm(ntraits,0,1)*noise)
  #}
  
  #%females rate males
  for (i in 1:length(m$participantm)){
    for (j in 1:ntraits){
      #%bias for each rater
      
      ratingm[i,j]=traitsf$bias[match(f$participantf[i],IDf)]+rnorm(1,0,1)+
        latentm[match(f$partner_m[i],IDm),j]
      
      if (ratingm[i,j] >7){
        ratingm[i,j]=7
      }
      else if (ratingm[i,j]<1){
        ratingm[i,j]=1
      }
      else{
        ratingm[i,j]=round(ratingm[i,j])
      }
      #calculate trait appeal score of male partner
      
       #unique f participant/m partner pair
      
      #trait appeal male partner for pair i, trait j
      #trait appeal= importance of trait j to female participant* female's rating of male for trait j
      
      
    }
    
  }

  ########################for scaling
  
  #importancem=apply(importancem,2,scale,scale=F)
  #ratingm=apply(ratingm,2,scale,scale=F)
  
  #for (i in 1:length(m$participantm)){
  #  vec=c(f$participantf[i],f$partner_m[i])
  #  EDm[i,]=euclidean(importancef[match(f$participantf[i],IDf),],ratingm[which(f$participantf==vec[1] & f$partner_m==vec[2]),]+
  #                      rnorm(ntraits,0,1)*noise)
      
  #}
  
  ###########attraction scores#########

  ###########################################################################################
  #######################EDITED SECTION REMOVED###############################
  ###attraction scores of males rated by females
  #attrm=data.frame(attr=numeric(length(f$partner_m)))
  
  #for (i in 1:length(f$partner_m)){
    #arithmetic mean
    #for each partner, take the average
  #  attrm[i,1]= mean(data.matrix(traitappealm[i,]))
  #}
  
  #attrf=data.frame(attr=numeric(length(m$partner_f)))
  #for (i in 1:length(m$partner_f)){
  #  attrf[i,1]=mean(data.matrix(traitappealf[i,]))
  #}
  ###########################################################################################
  
  m=data.frame(m,ratingf)#,EDf)#,attrf)
  f=data.frame(f,ratingm)#,EDm)#,attrm)
  #print(m) 
  #print(f)
  
  #m
  #maleID, female partnerID, SessionID, interaction,
  #"V1, V2..."female rating by male for each trait, "V1.1, V2.1,..." trait appeal of female to male, overall attractiveness of female partner
  
  #f
  #femaleID, male partnerID, SessionID, interaction,
  #"V1, V2..."male rating by female for each trait, "V1.1, V2.1,..." trait appeal of male relative to female, overall attractiveness of male partner
  
  #print(traitsm) #traits of male participant
  #male ID, "V1, V2,..." Latent trait scores, individual rating bias, "V1.1, V2.1,..." importance of each trait
  
  #print(traitsf) #traits of female participants
  
  colnames(m)[colnames(m)=="participantm"] <- "participantID"
  colnames(f)[colnames(f)=="participantf"] <- "participantID"
  
  colnames(m)[colnames(m)=="partner_f"] <- "partnerID"
  colnames(f)[colnames(f)=="partner_m"] <- "partnerID"
  rate<-rbind(m,f) #combine male and female data
  
  colnames(traitsm)[colnames(traitsm)=="IDm"] <- "participantID"
  colnames(traitsf)[colnames(traitsf)=="IDf"] <- "participantID"
  person<-rbind(traitsm,traitsf) #combine male and female data
  total<-merge(person,rate,by="participantID") #combine level 1 and level 2 data
  colnames(total)[(1+1+ntraits+1+ntraits+3):(1+ntraits+1+ntraits+3+ntraits)]<-paste("rating",1:ntraits)
  
  #relevant<-data.frame("ParticipantID"=total$participantID,
                       # "SessionID"=total$sessionID,
                       # "PartnerID"=total$partnerID,
                       # "gOattrac"=scale(total$attr),
                       # "Trait1"=scale(total$V1.y),
                       # "impTrait1"=scale(total$V1.1.x))
  
  #model<-lmer(gOattrac~ 1
  #             +Trait1*impTrait1
  #             +(1|SessionID)
  #             +(1|ParticipantID)
  #             +(1|PartnerID)
  #             +(Trait1|ParticipantID)
  #             #+(Trait1:impTrait1|ParticipantID)
  #             ,data=relevant)
  # saved$estimate[iterate]=summary(model)$coefficients[4,1]
  # saved$p[iterate]=summary(model)$coefficients[4,5]
  # #save MLM output
  data[[iterate]]<-total
}

#capture.output(saved, file = paste("session",deparse(nsession),"traits",deparse(ntraits),"noise",deparse(noise),".txt"))
save(data,file=paste("ABM core data",deparse(nsession),"traits",deparse(ntraits),".RData"))
  }
#}









foreach (noisy = noisetested, .packages = packages) %dopar% {
  t=traitstested
  data=vector(mode = "list", length = num)
  abm(t)
}


abm(25)

