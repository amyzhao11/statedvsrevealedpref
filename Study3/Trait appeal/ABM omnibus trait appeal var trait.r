install.packages(c("lme4","lmerTest","parallel","foreach","doParallel","stringr"))
library(lme4)
library(lmerTest)

library(parallel)
library(foreach)
library(doParallel)
library(stringr)

setwd("###")
#length(unique(yeah$sessionid[yeah$sessionid>=1400]))
#171 speed-dating sessions from real data

rm(list=ls(all=TRUE)) #remove all saved variables in workspace

packages <- c("parallel", "foreach", "doParallel","stringr","lme4","lmerTest")
num_sims <- 1000 
registerDoParallel(min(detectCores()-1,num_sims)) 



noisetested=c(0,1,5,10,20,30,40,50)
#num=1000 #number of simulations
load("ABM core data 171 traits 25 .RData")

#preprocess data for storage reasons
for (i in 1:1000){
  data[[i]]<-subset(data[[i]],select=c("participantID",paste0("importance ", 1:25),
                                       "partnerID","sessionID","interact",paste0("rating ", 1:25)))
}



calc <- function(a,noise,test=num_sims) {
  options(digits=10)

  saved=list()
  for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {
    # Pre-allocate the data frame to store the estimates and p-values
    saved[[paste0(k)]]<- data.frame(estimate = numeric(test), p = numeric(test))
  }
  
  # Calculate trait appeal and attractiveness for each test case
  for (i in 1:test) {
    a_i <- a[[i]]
    nint <- nrow(a_i)
    #variable names have spaces, remove them
    names(a_i)<-str_replace_all(names(a_i), c(" " = ""))
    
    # Attractiveness calculated by trait appeal
    a_i[[paste0("attr", 25)]] <- rowMeans(a_i[,paste0("importance", 1:25)]*(a_i[,paste0("rating", 1:25)]-4)+noise*matrix( rnorm(25*nint,mean=0,sd=1), nint, 25))
    
    for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {

      
      
      
      a_i[[paste0("traitappeal", k)]]<-   rowMeans(a_i[,paste0("importance", 1:k)]*(a_i[,paste0("rating", 1:k)]-4))
      
      # Fit the linear mixed-effects model for each trait parameter
      model <- lmer(scale(get(paste0("attr", 25))) ~ 1 +
                      scale(get(paste0("traitappeal", k))) +
                      #check real analyses
                      (1 | sessionID) +
                      (1 | participantID) +
                      (1 | partnerID),
                    data = a_i)
      
      saved[[paste0(k)]]$estimate[i] <- summary(model)$coefficients[2, 1]
      saved[[paste0(k)]]$p[i]<-summary(model)$coefficients[2, 5]
    }
    
    
    a[[i]] <- a_i
  }
  for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {
    write.csv(saved[[paste0(k)]], file = paste0("Omnibus traitappeal imp 171 traits var traits", deparse(k), " noise ", deparse(noise), ".csv"))
  }

}
  
##################################################################################################
#run simulations

foreach (noisy = noisetested, .packages = packages) %dopar% {
  #data=vector(mode = "list", length = num)
  calc(data,noisy)
}



