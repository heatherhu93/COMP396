#params <- list(lookbackmacd1=12,lookbackmacd2=25,lookbackmacd3=9,
#               lookbackmacd4=13,lookbackmacd5=25,lookbackmacd6=8,
#               atr1=30, sl1=40, p1=3.1,
#               atr2=35, sl2=30, p2=2.9,
#               atr3=30, sl3=30, p3=1,
#               atr4=30, p4=0.9,
#               lookbackema1=15, lookbackema2=35,
#               lookbackbbandsstop1=20,lookbackbbandsstop2=30,sdParam=2.5,
#               lookbackrsi=25,threshold=15,
#               series=1:10)

maxRows <- 1300

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  #initial position sizes
  positionSizes<-rep(1,length(newRowList))
  #postion sizing using open price difference
  if(store$iter>2){
  openDiffs <- apply(store$open[1:store$iter,],2,diff)
  absOpenDiffs <- apply(openDiffs,2,abs)
  avgAbsDiffs <- apply(absOpenDiffs,2,mean,na.rm=TRUE)
  largestAvgAbsDiffs <- max(avgAbsDiffs)
  positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
  #set limit sizes for series 3
  if(positionSizes[3]>5000){
    positionSizes[3]<-5000
  }
  #decrease the sizes of series 10
  positionSizes[10]<-round(positionSizes[10]/10)
  
  }

  limitOrders1  <- rep(0,length(newRowList))
  limitPrices1  <- rep(0,length(newRowList))
  limitOrders2  <- rep(0,length(newRowList)) 
  limitPrices2  <- rep(0,length(newRowList))
  
  marketOrders <- -currentPos
  #series 10 is only for limit order
  marketOrders[10]<-0
  #temperory position for all series
  pos<- allzero
  #temperory position for series 8
  pos1<-allzero

  
 for(i in 1:length(params$series)){ 
   #macd strategy with profit target for series 1
   if(i==1){
     if (store$iter > max(params$atr1,params$sl1,(params$lookbackmacd2+params$lookbackmacd3-2))) {
       startIndexmacd1 <-  store$iter - (params$lookbackmacd2+params$lookbackmacd3-2)-1
       startIndexsl1 <-  store$iter - params$sl1-1
       startIndexatr1 <-  store$iter - params$atr1-1
       
       #use MACD indicator to set initial trading rule
       macds1<-MACD(store$cl[startIndexmacd1:store$iter,i],params$lookbackmacd1,params$lookbackmacd2,params$lookbackmacd3)
       macd1<-as.matrix(macds1)[,'macd']
       signal1<-as.matrix(macds1)[,'signal']
       histogram1<-macd1-signal1
       
       High<-store$high[startIndexatr1:store$iter,i]
       Low<-store$low[startIndexatr1:store$iter,i]
       Close<-store$cl[startIndexatr1:store$iter,i]
       HLC<-cbind(High,Low,Close)
       #use ATR to set the profit target
       atr<-last(as.matrix(ATR(HLC,n=params$atr1))[,'atr'])
       
       cl<-store$cl[store$iter,i]
       
       #set profitTarget1 to stop long position
       profitTarget1<-min(store$high[startIndexsl1:(store$iter-1),i])+atr*params$p1 
       #set profitTarget2 to stop short position
       profitTarget2<-max(store$low[startIndexsl1:(store$iter-1),i])-atr*params$p1 
       
       #buy when last histogram above zero and below zero for sell
       if(last(histogram1)>0){ 
         pos[i] <- positionSizes[i]
         #when the profit target is touched, stop the trade
         if(cl>profitTarget1){
           pos[i] <- 0}
       }
       else if(last(histogram1)<0){
         pos[i] <- -positionSizes[i] 
         if(cl<profitTarget2){
           pos[i] <- 0}
       }    
     }
   }
   
   #macd strategy with and without profit target for series 8
   if(i==8){
     
     if (store$iter > (params$lookbackmacd5+params$lookbackmacd6)) {
       startIndexmacd2 <-  store$iter - (params$lookbackmacd5+params$lookbackmacd6)-1
       
       
       macds2<-MACD(store$cl[startIndexmacd2:store$iter,i],params$lookbackmacd4,params$lookbackmacd5,params$lookbackmacd6)
       macd2<-as.matrix(macds2)[,'macd']
       signal2<-as.matrix(macds2)[,'signal']
       histogram2<-macd2-signal2
       
       #buy when recent three histograms above zero
       #sell when last histogram is below zero
       if(histogram2[(params$lookbackmacd5+params$lookbackmacd6)-1]>0 
          && histogram2[(params$lookbackmacd5+params$lookbackmacd6)]>0 
          && histogram2[(params$lookbackmacd5+params$lookbackmacd6)+1]>0){ 
         pos[i] <- positionSizes[i]/2
       }
       else if(last(histogram2)<0){
         pos[i] <- -positionSizes[i]/2    
       }    
     }
     
     if (store$iter > max(params$atr2,params$sl2,(params$lookbackmacd5+params$lookbackmacd6-2))) {
       startIndexmacd3 <-  store$iter - (params$lookbackmacd5+params$lookbackmacd6-2)-1
       startIndexsl2 <-  store$iter - params$sl2-1
       startIndexatr2 <-  store$iter - params$atr2-1
       
       macds3<-MACD(store$cl[startIndexmacd3:store$iter,i],params$lookbackmacd4,params$lookbackmacd5,params$lookbackmacd6)
       macd3<-as.matrix(macds3)[,'macd']
       signal3<-as.matrix(macds3)[,'signal']
       histogram3<-macd3-signal3
       
       High<-store$high[startIndexatr2:store$iter,i]
       Low<-store$low[startIndexatr2:store$iter,i]
       Close<-store$cl[startIndexatr2:store$iter,i]
       HLC<-cbind(High,Low,Close)
       
       atr<-last(as.matrix(ATR(HLC,n=params$atr2))[,'atr'])
       
       cl<-store$cl[store$iter,i]
       profitTarget1<-min(store$high[startIndexsl2:(store$iter-1),i])+atr*params$p2      
       profitTarget2<-max(store$low[startIndexsl2:(store$iter-1),i])-atr*params$p2 
       
       #buy when last histogram above zero and below zero for sell
       if(last(histogram3)>0){ 
        pos1[i] <- positionSizes[i]/2
        #when the profit target is touched, stop the trade
        if(cl>profitTarget1){
           pos1[i] <- 0}
       }
       else if(last(histogram3)<0){
         pos1[i] <- -positionSizes[i]/2
         if(cl<profitTarget2){
           pos1[i] <- 0}
       }    
     }
   }
  
  #double crossover strategy for series 3 and 9 
  if(i==3 || i==9) {
    
    if (store$iter > params$lookbackema2) {
      
      startIndexema1 <- store$iter - params$lookbackema1 - 1
      startIndexema2 <- store$iter - params$lookbackema2 - 1
      
      ema1<-last(EMA(store$cl[startIndexema1:store$iter,i],n=params$lookbackema1))
      ema2<-last(EMA(store$cl[startIndexema2:store$iter,i],n=params$lookbackema2))
      
      indicator<-ema1-ema2
      
      #buy when shorter one crosses above the longer one and crosses below for sell
      if (indicator>0){
        pos[i] <-positionSizes[i]
      } 
      else if(indicator<0){
        pos[i] <- -positionSizes[i]
      }
    }
  }
  
  #bbands strategy with stoploss for series 4 
  if(i==4){
    
    if (store$iter > params$lookbackbbandsstop1) {
      startIndexbbandsstop1 <-  store$iter - params$lookbackbbandsstop1    
      cl <- store$cl[store$iter,i]     
      # set trading rule for entry point
      bbands <- last(BBands(store$cl[startIndexbbandsstop1:store$iter,i],
                            n=params$lookbackbbandsstop1,sd=params$sdParam))
      
      #buy when price crosses below the lower bbands
      if (cl < bbands["dn"]) {
        pos[i] <- positionSizes[i] 
      }
      #sell when price crosses above upper bbands 
      if (cl > bbands["up"]) {
        pos[i] <- -positionSizes[i]
      }      
      
      #record the entry point
      if (currentPos[i]==0) {     
        if (pos[i] != currentPos[i])
          store$entry[i] <- store$iter       
      }     
      else {
        # call getTradeReturn function to calculate the return
        ret <- getTradeReturn(store$cl[,i],store$entry[i],store$iter,isTRUE(pos[i]>0))        
        #remain the position if return is between -0.1 and 0.4
        #otherwise, exit the trade
        if ( ret<0.4 && ret>-0.1) 
          pos[i] <- currentPos[i]            
        else 
          pos[i] <- 0
      }
    }
  }
  
  #bbands strategy with stoploss for series 5
  if(i==5){
    
    if (store$iter > params$lookbackbbandsstop2) {
      startIndexbbandsstop2 <-  store$iter - params$lookbackbbandsstop2    
      cl <- store$cl[store$iter,i]     
      # set trading rule for entry point
      bbands <- last(BBands(store$cl[startIndexbbandsstop2:store$iter,i],
                            n=params$lookbackbbandsstop2,sd=params$sdParam))
      
      #buy when price crosses below the lower bbands
      if (cl < bbands["dn"]) {
        pos[i] <- positionSizes[i] 
      }
      #sell when price crosses above the upper bbands
      if (cl > bbands["up"]) {
        pos[i] <- -positionSizes[i]
      }  
      #remember entry point
      if (currentPos[i]==0) {     
        if (pos[i] != currentPos[i])
          store$entry[i] <- store$iter       
      }     
      else {
        #call getTradeReturn funtion to calculate simple return
        ret <- getTradeReturn(store$cl[,i],store$entry[i],store$iter,isTRUE(pos[i]>0))        
        #remain the position if return is between -0.1 and 0.4
        #otherwise, exit the trade
        if ( ret<0.4 && ret>-0.1) 
          pos[i] <- currentPos[i]            
        else 
          pos[i] <- 0
      }
    }
  } 
 
  #rsi strategy with stoploss for series 7
  if(i==7){
    if (store$iter > max(params$atr3,params$sl3,params$lookbackrsi)) {
        startIndexrsi <-  store$iter - params$lookbackrsi-1
        startIndexsl3 <-  store$iter - params$sl3-1
        startIndexatr3 <-  store$iter - params$atr3-1
      
        High<-store$high[startIndexatr3:store$iter,i]
        Low<-store$low[startIndexatr3:store$iter,i]
        Close<-store$cl[startIndexatr3:store$iter,i]
        HLC<-cbind(High,Low,Close)
        #set ATR for stoploss price setting
        atr<-last(as.matrix(ATR(HLC,n=params$atr3))[,'atr'])
        cl<-store$cl[store$iter,i]
        #set initial trading rule(without stoploss)
        rsi <- last(RSI(store$cl[startIndexrsi:store$iter,i],n=params$lookbackrsi))
        
        #set stoploss1 as stop buy price
        stoploss1<-min(store$high[startIndexsl3:(store$iter-1),i])-atr*params$p3
        #set stoploss2 as stop sell price
        stoploss2<-min(store$low[startIndexsl3:(store$iter-1),i])+atr*params$p3
         
        #buy if rsi touch the lower line and the higher line for sell
        if (rsi < (50 - params$threshold))
        {
          pos[i] <- positionSizes[i]
          #when the stoploss is touched, stop the trade
          if(cl>stoploss1){
            pos[i] <- 0}
        }
        else if (rsi > (50 + params$threshold))
        {
          pos[i] <- -positionSizes[i]
          if(cl<stoploss2){
            pos[i] <- 0}
        }    
      }
  }
  
  #limit order strategy for series 10
  if(i==10){
    if (store$iter > params$atr4) {
       startIndexatr4 <-  store$iter - params$atr4 - 1
       
       High<-store$high[startIndexatr4:store$iter,i]
       Low<-store$low[startIndexatr4:store$iter,i]
       Close<-store$cl[startIndexatr4:store$iter,i]
       HLC<-cbind(High,Low,Close)
      
       atr<-last(as.matrix(ATR(HLC,n=params$atr4))[,'atr'])
      
       #only trade on series 10
       limitOrders1  <- c(0,0,0,0,0,0,0,0,0,positionSizes[i])
       #use ATR to set the limit prices for series 10
       limitPrices1  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$High - atr*params$p4)
      
       limitOrders2  <- c(0,0,0,0,0,0,0,0,0,-positionSizes[i])
       limitPrices2  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$Low + atr*params$p4)     
       }
     }  
  }
 
  marketOrders <- marketOrders + pos + pos1
  
 return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}


getTradeReturn <- function(cl,entry,exit,short=FALSE) {
  
  prices <- as.numeric(cl)
  if (short)
    prices[entry]/prices[exit] - 1
  else
    prices[exit]/prices[entry] - 1
}

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}

updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

initOpenStore  <- function(newRowList,series) {
  openStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(openStore)
}

updateOpenStore <- function(openStore, newRowList, series, iter) {
  for (i in 1:length(series))
    openStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(openStore)
}

initHighStore  <- function(newRowList,series) {
  highStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(highStore)
}

updateHighStore <- function(highStore, newRowList, series, iter) {
  for (i in 1:length(series))
    highStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(highStore)
}

initLowStore  <- function(newRowList,series) {
  lowStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(lowStore)
}

updateLowStore <- function(lowStore, newRowList, series, iter) {
  for (i in 1:length(series))
    lowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(lowStore)
}

initVolumeStore  <- function(newRowList,series) {
  volumeStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(volumeStore)
}

updateVolumeStore <- function(volumeStore, newRowList, series, iter) {
  for (i in 1:length(series))
    volumeStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(volumeStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              open=initOpenStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series),
              volume=initVolumeStore(newRowList,series),
              entry=rep(1,10)))
}

updateStore <- function(store, newRowList, series) {
  
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$open<-updateOpenStore(store$open,newRowList,series,store$iter) 
  store$high<-updateHighStore(store$high,newRowList,series,store$iter) 
  store$low<-updateLowStore(store$low,newRowList,series,store$iter) 
  store$volume<-updateVolumeStore(store$volume,newRowList,series,store$iter)
  store$entry <- store$entry
  return(store)
}
