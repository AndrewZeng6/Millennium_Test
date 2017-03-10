#Maximal Intervap
df=data.frame(id=seq(10,80,by=10),
              anest=c("baker","baker",rep("dow",6)), 
              start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
              end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))

maxInterval<-function(df){
  t=sort(unique(c(as.character(df$start),as.character(df$end))))
  res=data.frame(matrix(nrow=length(df$id),ncol=length(t)-1),row.names=rownames(df))
  colnames(res)=sapply(1:(length(t)-1),function(n){paste(t[n],t[n+1],sep="-")})
  v=t[1:(length(t)-1)]
  #res[,]=apply(df,1,function(x){as.integer(v>=as.character(x["start"]) & v<as.character(x["end"]))})
  for(x in rownames(res)){
    res[x,]=as.integer(v>=as.character(df[x,"start"]) & v<as.character(df[x,"end"]))
  }
  res["sum",]=apply(res,2,sum)
  
  #maximum intersection
  df[,"s"]=sapply(rownames(res)[-length(rownames(res))],
                  function(x){max(as.integer(res["sum",as.integer(res[x,])>0]))})
  
  #list id each row is intersecting with
  for(x in rownames(df)){
    df[x,"w"]=paste(as.character(df$id)[as.character(df$start)<as.character(df[x,"end"]) & 
                                   as.character(df$end)>as.character(df[x,"start"])],collapse=",")
  }
  
  return(df)
}

results=ddply(df,"anest",maxInterval)
write.csv(results,file="maxInterval.csv")