library(lme4)
library(lmerTest)
library(parallel)
library(foreach)
library(doParallel)
library(stringr)

setwd("###")


rm(list=ls(all=TRUE)) #remove all saved variables in workspace

packages <- c("parallel", "foreach", "doParallel","stringr","lme4","lmerTest")
num_sims <- 1000 
registerDoParallel(min(detectCores()-1,num_sims)) 




#traitstested=c(2,3,5,9,10,15,20,25)
noisetested=c(0,5,10,20,30,40,50)
#num=1000 #number of simulations
load("ABM core data 89 traits 25 .RData")

##note that importances and preference levels here are interchangeable

#preprocess data for storage reasons

for (i in 1:1000){
  data[[i]]<-subset(data[[i]],select=c("participantID",paste0("importance ", 1:25),
                                       "partnerID","sessionID","interact",paste0("rating ", 1:25)))
}

euclidean <- function(a, b) {
  sqrt(rowSums((a - b) ^ 2))
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
    

    
    for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {

      
      # Attractiveness calculated by Euclidean distance * -1
      #high ED means low attractiveness
      a_i[[paste0("attr", k)]] <- -1*euclidean(a_i[,paste0("importance", 1:k)], (a_i[, paste0("rating", 1:k)]+noise*matrix( rnorm(k*nint,mean=0,sd=1), nint, k)) )
      
      a_i$singleED=sqrt((a_i$importance1-a_i$rating1)^2)
      # Fit the linear mixed-effects model for each trait parameter
      model <- lmer(scale(get(paste0("attr", k))) ~ 1 +
                      scale(singleED) +
                      (1 | sessionID) +
                      (1 | participantID) +
                      (1 | partnerID)#+(rating1| participantID)
                      ,
                    data = a_i)
      
      saved[[paste0(k)]]$estimate[i] <- summary(model)$coefficients[2, 1]
      saved[[paste0(k)]]$p[i]<-summary(model)$coefficients[2, 5]
    }
    
    
    a[[i]] <- a_i
  }
  for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {
    write.csv(saved[[paste0(k)]], file = paste0("Traitbytrait pref 89 traits ", deparse(k), " noise ", deparse(noise), ".csv"))
  }

}
  
##################################################################################################
#run simulations

foreach (noisy = noisetested, .packages = packages) %dopar% {
  #data=vector(mode = "list", length = num)
  calc(data,noisy)
}




