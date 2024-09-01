#library(sjmisc)

library(lmerTest)
library(lme4)
library(lattice)
library(ggplot2)
#descriptive statistics
setwd(".../Revision/Data and code/Analyses")


recent=read.csv("2014-2019 Speed Dating Data.csv")

recent$year=rep(NA,length(recent$id))
recent$year=floor(as.numeric(recent$sessionid)/100)
recent$year=as.factor(recent$year)
recent$age=as.numeric(recent$age)
recent$sessionid=as.factor(recent$sessionid)



##################
## "metaanalysis"
##reshaped data


recent=subset(recent,year %in% c(14,15,16,17,18,19))

relevant=data.frame("id"=recent$id,
                 "sex"=recent$sex,
                 "year"=recent$year,
                 "partnerid"=recent$partnerid,
                 "sessionid"=recent$sessionid,
                 "goattrac"=scale(recent$goattrac),
                 "gfa"=scale(recent$gfattrac),
                 "impfa"=scale(recent$impfa),
                 "gba"=scale(recent$gbattrac),
                 "impba"=scale(recent$impba),
                 "gku"=scale(recent$gkindunder),
                 "impku"=scale(recent$impku),
                 "gam"=scale(recent$gambitious),
                 "impam"=scale(recent$impam),
                 "gin"=scale(recent$gintelligent),
                 "impin"=scale(recent$impin),
                 "gco"=scale(recent$gconfident),
                 "impco"=scale(recent$impco),
                 "ghp"=scale(recent$gfunny),
                 "imphp"=scale(recent$imphp),
                 "gha"=scale(recent$gpercfunny),
                 "impha"=scale(recent$impha),
                 "gcr"=scale(recent$gcreative),
                 "impcr"=scale(recent$impcr)
  )

library(tidyverse)
library(tidyr)

#traits=c("gfattrac","gbattrac","gkindunder","gambitious","gintelligent", "gconfident","gfunny", "gpercfunny",
#"gcreative")

reshaped_data <- relevant %>%
  pivot_longer(cols = starts_with("imp") ,names_pattern="imp(\\w+)", names_to = "trait", values_to = "importances")#%>%
    
reshaped_data=  reshaped_data%>%
  pivot_longer(cols =contains(c("gfa","gba","gku","gam","gin","gco","gha","ghp","gcr")) ,names_pattern="g(\\w+)", names_to = "trait1", values_to = "rating")

#only keep the ones where trait 1 and trait 2 are the same
reshaped_data=subset(reshaped_data,trait==trait1)

reshaped_data_imp=reshaped_data

head(reshaped_data_imp)
write.csv(reshaped_data_imp,"Reshaped data for omni meta analysis importances.csv")

library(lmerTest)
summary(lmer(scale(goattrac)~importances*rating+sex+
               (1|partnerid)+
               (1+rating|id)+
               (rating+importances|trait)+
               (1|sessionid)
               ,data=reshaped_data_imp))



#Trait by trait absolute difference using pref levels

relevant=data.frame("id"=recent$id,
                    "sex"=recent$sex,
                    "year"=recent$year,
                    "partnerid"=recent$partnerid,
                    "sessionid"=recent$sessionid,
                    "goattrac"=scale(recent$goattrac),
                    "gfa"=recent$gfattrac,
                    "prefa"=recent$prefa,
                    "gba"=recent$gbattrac,
                    "preba"=recent$preba,
                    "gku"=recent$gkindunder,
                    "preku"=recent$preku,
                    "gam"=recent$gambitious,
                    "pream"=recent$pream,
                    "gin"=recent$gintelligent,
                    "prein"=recent$prein,
                    "gco"=recent$gconfident,
                    "preco"=recent$preco,
                    "ghp"=recent$gfunny,
                    "prehp"=recent$prehp,
                    "gha"=recent$gpercfunny,
                    "preha"=recent$preha,
                    "gcr"=recent$gcreative,
                    "precr"=recent$precr)


library(tidyverse)
library(tidyr)

#traits=c("gfattrac","gbattrac","gkindunder","gambitious","gintelligent", "gconfident","gfunny", "gpercfunny",
#"gcreative")

reshaped_data <- relevant %>%
  pivot_longer(cols = starts_with("pre") ,names_pattern="pre(\\w+)", names_to = "trait", values_to = "preference")#%>%

reshaped_data=  reshaped_data%>%
  pivot_longer(cols =contains(c("gfa","gba","gku","gam","gin","gco","gha","ghp","gcr")) ,names_pattern="g(\\w+)", names_to = "trait1", values_to = "rating")

#only keep the ones where trait 1 and trait 2 are the same
reshaped_data=subset(reshaped_data,trait==trait1)

reshaped_data$absdiff=abs(reshaped_data$preference-reshaped_data$rating)


reshaped_data_preflevel=reshaped_data
summary(lmer(goattrac~scale(absdiff)+scale(rating)+scale(preference)+sex+
               (1|partnerid)+
               (1+rating|id)+
               (rating+preference|trait)+
               (1|sessionid)
             ,data=reshaped_data))











#graph of variances for prefs and ratings
library(ggplot2)


reshaped_data_imp

custom_labels <- c(
  "am" = "Ambitiousness",
  "ba" = "Bodily attractiveness",
  "co" = "Confidence",
  "cr" = "Creativity",
  "fa" = "Facial attractiveness",
  "ha" = "Being perceived as funny by the partner",
  "hp" = "Funniness",
  "in" = "Intelligence",
  "ku" = "Kindness and understanding"
)
ggplot(reshaped_data_imp, aes(x = importances, y = rating)) +
  geom_jitter(alpha=0.1) +
  #geom_histogram()+
  facet_wrap(~ trait , ncol = 3,labeller = labeller(trait = custom_labels)) +  # Create 3x3 grid
  labs(x = "Preference importance", y = "Trait Rating") +
  theme_minimal() +
  coord_fixed(ratio = 1)
