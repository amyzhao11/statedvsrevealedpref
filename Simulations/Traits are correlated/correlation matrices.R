setwd(".../Data and code/Analyses")


recent=read.csv("2014-2019 Speed Dating Data.csv")

recent$year=rep(NA,length(recent$id))
recent$year=floor(as.numeric(recent$sessionid)/100)
recent$year=as.factor(recent$year)
recent$age=as.numeric(recent$age)
recent$sessionid=as.factor(recent$sessionid)

personlevel=unique(recent[,c("id","sessionid","sex",
                             "impfa","impba","impku","impam","impin",
                             "impcr","impco","imphp","impha",
                             "prefa","preba","preku","pream","prein",
                             "precr","preco","prehp","preha","age","year"
)])

#let's calculate the correlation matrix between traits for preference importance
names(personlevel)
impcor=cor(personlevel[,4:12],use="complete.obs")

#let's calculate the correlation matrix between traits for preference level
names(personlevel)
prefcor=cor(personlevel[,13:21],use="complete.obs")

#let's calculate the correlation matrix between traits for ratings
ratcor=cor(recent[,c("gfattrac","gbattrac","gkindunder","gambitious","gintelligent","gcreative", "gconfident",
                     "gfunny", "gpercfunny")],use="complete.obs")

cormatrix=matrix(nrow=27,ncol=9,dimnames=list(c(1:27),c(1:9)))

for (i in 1:9){
  cormatrix[3*i-2,]<- impcor[i,]
  cormatrix[3*i-1,]<- prefcor[i,]
  cormatrix[3*i,]<- ratcor[i,]
  rownames(cormatrix)[3*i-2]=rownames(impcor)[i]
  rownames(cormatrix)[3*i-1]=rownames(prefcor)[i]
  rownames(cormatrix)[3*i]=rownames(ratcor)[i]
}

colnames(cormatrix)<- c("Facial attractiveness",
                        "Bodily attractiveness",
                        "Kindness and understanding",
                        "Ambitiousness",
                        "Intelligence",
                        "Creativity",
                        "Confidence",
                        "Funniness",
                        "Perceived as funny")

write.csv(cormatrix, "correlation matrix for traits.csv")

write.csv(impcor,"Importance correlation matrix.csv")
write.csv(prefcor,"Preference level correlation matrix.csv")
write.csv(ratcor,"Rating correlation matrix.csv")