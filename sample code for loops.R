

#You will want to change 


colnames(PollockParasites)

#Create a list of parasites that you are interested in 
PollockParasiteList=c('NEM.LA','NEM.LB','NEM.A','NEM.B','NEM.C','NEM.D','NEM.E','TREM.A','TREM.B','TREM.C','TREM.D','CYST.A','CYST.B','CYST.C','CYST.D','CYST.E','CYST.F','CYST.G','CYST.J','CYST.K','ACANTH.A','COPE.A','COPE.B','TRYP.A')


#Create a blank data frame to store output from the models
PollockModels=data.frame(ParasiteID=PollockParasiteList,Estim=NA,STD=NA,z=NA,p=NA)



#Loop across all interested parasites and form a negative binomai model
for(x in c(1,4:6,8:10,12:19,21,23:length(PollockParasiteList))){
  print(x) #Tell me what parasite you are on
  eval(parse(text=paste0('',PollockParasiteList[x],'.Pollock = glmer.nb(',PollockParasiteList[x],'~Treatment+I(Length/10)+(1|Location),data=PollockDat)')))
  
  #run a model for negative binomial distribution
  eval(parse(text=paste0('try=summary(',PollockParasiteList[x],'.Pollock)'))) #Create a summary object to pull information from 
  PollockModels$Estim[x]=try$coefficients[2,1]#save the estimate of treatment
  PollockModels$STD[x]=try$coefficients[2,2]#save the error of the treatment
  PollockModels$p[x]=try$coefficients[2,4]#save the p value
  PollockModels$z[x]=try$coefficients[2,3]#save the z value
  
}



