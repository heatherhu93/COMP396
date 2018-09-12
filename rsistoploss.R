#params <- list(lookback=25,threshold=15,sl=30,atr=30,       
#               series=1:10)

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)

  positionSizes<-rep(1,length(newRowList))
  if(store$iter>2){
  openDiffs <- apply(store$open[1:store$iter,],2,diff)
  absOpenDiffs <- apply(openDiffs,2,abs)
  avgAbsDiffs <- apply(absOpenDiffs,2,mean,na.rm=TRUE)
  largestAvgAbsDiffs <- max(avgAbsDiffs)
  positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)

  }  

  marketOrders <- -currentPos; 
  pos <- allzero
  for (i in 1:length(params$series)) {
if(i==7){
  if (store$iter > max(params$atr,params$sl,params$lookback)) {
    startIndex <-  store$iter - params$lookback-1
    startIndexsl <-  store$iter - params$sl-1
    startIndexatr <-  store$iter - params$atr-1
    

      High<-store$high[startIndexatr:store$iter,i]
      Low<-store$low[startIndexatr:store$iter,i]
      Close<-store$cl[startIndexatr:store$iter,i]
      HLC<-cbind(High,Low,Close)
      
      atr<-last(as.matrix(ATR(HLC,n=params$atr))[,'atr'])
      #print(atr)
      
      stoploss1<-min(store$low[startIndexsl:(store$iter-1),i])+atr
      stoploss2<-min(store$high[startIndexsl:(store$iter-1),i])-atr
      cl<-store$cl[store$iter,i]
           
      rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$lookback))
      
      if (rsi < (50 - params$threshold))
      {
        pos[i] <- positionSizes[i]
        if(cl>stoploss2){
          pos[i] <- 0}
      }
      else if (rsi > (50 + params$threshold))
      {
        pos[i] <- -positionSizes[i]
        if(cl<stoploss1){
          pos[i] <- 0}
      }
      }
    }
  }
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              #limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              #limitOrders2=limitOrders2,limitPrices2=limitPrices2))
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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
              volume=initVolumeStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$open<-updateOpenStore(store$open,newRowList,series,store$iter) 
  store$high<-updateHighStore(store$high,newRowList,series,store$iter) 
  store$low<-updateLowStore(store$low,newRowList,series,store$iter) 
  store$volume<-updateVolumeStore(store$volume,newRowList,series,store$iter)
  return(store)
}