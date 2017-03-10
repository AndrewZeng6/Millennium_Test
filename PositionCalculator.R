library("dplyr")
library("plyr")
setwd("C:/Users/AndrewZeng/Desktop/2016/Interview_Prepare/Millennium")



pos=read.csv("input\\pos.csv")
trade=read.csv("input\\trd.csv")


#part 1
df=pos
print(df)
netPos=ddply(df,c("user","sym"),function(x){sum(x$pos)})
colnames(netPos)=c("user","sym","netPosition")
write.csv(netPos,"output\\netPositions.csv")

#part 2
#groupby multiple columns and apply by multiple functions
boxPos<-function(x){
  if (length(unique(x$pb))>1){#more than one broker
    if(prod(x$pos)<0){
      return(x)
    }
  }
}
write.csv(ddply(df,c("user","sym"),.fun=boxPos),file="output\\boxPositions.csv")



#part 3
getJournalTrade<-function(x){
  if(dim(x)[1]>1){
    #all quantity the same sign
    if(prod(as.integer(x$qty>0))>0 || prod(as.integer(x$qty<0))>0){
      x$jrnl=0
      x$trd=x$qty
      return(x)
    } 
    #quantity with different signs
    if(sum(x$qty)>=0){#more buy than sell
      x[x$qty<0,"jrnl"]=-x[x$qty<0,"qty"]
      x[x$qty>0,"jrnl"]=sum(x[x$qty<0,"qty"])*(x[x$qty>0,"qty"]/sum(x[x$qty>0,"qty"]))
      x$trd=x$qty+x$jrnl
    }else{#more sell than buy
      x[x$qty>0,"jrnl"]=-x[x$qty>0,"qty"]
      x[x$qty<0,"jrnl"]=sum(x[x$qty>0,"qty"])*(x[x$qty<0,"qty"]/sum(x[x$qty<0,"qty"]))
      x$trd=x$qty+x$jrnl
    }
  }else{
    x$jrnl=0
    x$trd=x$qty
  }
  x
}

#Journal calculation with rounding to integer number of trades
getJournalTrade2<-function(x){
  if(dim(x)[1]>1){
    #all quantity the same sign
    if(prod(as.integer(x$qty>0))>0 || prod(as.integer(x$qty<0))>0){
      x$jrnl=0
      x$trd=x$qty
      return(x)
    } 
    #quantity with different signs
    if(sum(x$qty)>=0){#more buy than sell
      x[x$qty<0,"jrnl"]=-x[x$qty<0,"qty"]
      #rounding first n-1 users and assign the last one with whatever is left
      temp0=sum(x[x$qty<0,"qty"])*(x[x$qty>0,"qty"]/sum(x[x$qty>0,"qty"]))
      temp1=round(temp0[-1])
      temp1=c(sum(x[x$qty<0,"qty"])-sum(temp1),temp1)
      x[x$qty>0,"jrnl"]=temp1
      x$trd=x$qty+x$jrnl
    }else{#more sell than buy
      x[x$qty>0,"jrnl"]=-x[x$qty>0,"qty"]
      #rounding first n-1 users and assign the last one with whatever is left
      temp0=sum(x[x$qty>0,"qty"])*(x[x$qty<0,"qty"]/sum(x[x$qty<0,"qty"]))
      temp1=round(temp0[-1])
      temp1=c(sum(x[x$qty<0,"qty"])-sum(temp1),temp1)
      x[x$qty<0,"jrnl"]=temp1
      x$trd=x$qty+x$jrnl
    }
  }else{
    x$jrnl=0
    x$trd=x$qty
  }
  x
}


journal=ddply(trade,c("sym"),.fun=getJournalTrade)
journal2=ddply(trade,c("sym"),.fun=getJournalTrade2)
write.csv(journal,"output\\journal.csv")
write.csv(journal2,"output\\journal2.csv")




#part 4
totalTrades=ddply(journal,c("sym"),.fun=function(x){(sum(x$trd))})
colnames(totalTrades)=c("sym","total Quantity to trade")
write.csv(totalTrades,"output\\toTrade.csv")


#Part 5

currentPositions=ddply(pos[,c("user","sym","pos")],c("user","sym"),function(x){sum(x$pos)})
colnames(currentPositions)=c("user","sym","pos")
currentPositions$key=paste(currentPositions$user,currentPositions$sym)
trade$key=paste(trade$user,trade$sym)
finalPositions=merge(currentPositions,trade,by.x="key",by.y="key",all=TRUE)
finalPositions$finalPos=rowSums(finalPositions[,c("pos","qty")],na.rm=T)
write.csv(finalPositions,"output\\finalPositions.csv")




















