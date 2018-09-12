#params <- list(lookbackema1=15,lookbackema2=35,series=1:10)
maxRows <- 1100

getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  positionSizes<-rep(1,length(newRowList))
  if(store$iter>2){
  openDiffs <- apply(store$open[1:store$iter,],2,diff)
  absOpenDiffs <- apply(openDiffs,2,abs)
  avgAbsDiffs <- apply(absOpenDiffs,2,mean,na.rm=TRUE)
  largestAvgAbsDiffs <- max(avgAbsDiffs)
  positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
  for(i in 1:10){
    if (positionSizes[i]>params$limit)
      positionSizes[i]<-params$limit
  }
  }

  allzero  <- rep(0,length(newRowList))
  # check if current inventory is above a limit and if so exit completely
  # with a market order
  #marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
  
  marketOrders <- -currentPos 
  pos <- allzero
  
  #marketOrders <- rep(0,length(newRowList))
  
  for (i in 1:length(params$series)) {
if(i==3||i==9){
  if (store$iter > params$lookbackema2) {
    #marketOrders   <- sapply(1:length(newRowList),
    #                        function(x) lgStFt(store$cl,x,store$iter))
    startIndexema1 <- store$iter - params$lookbackema1 - 1
    startIndexema2 <- store$iter - params$lookbackema2 - 1
    
    #for (i in 1:length(params$series)) {
      #sma<-last(SMA(store$cl[startIndex1:store$iter,i],n=params$lookback1))
      #sma1<-last(SMA(store$cl[startIndex2:store$iter,i],n=params$lookback2))
      ema<-last(EMA(store$cl[startIndexema1:store$iter,i],n=params$lookbackema1))
      ema1<-last(EMA(store$cl[startIndexema2:store$iter,i],n=params$lookbackema2))
      #cl<-newRowList[[params$series[i]]]$Close
      #indicator<-sma-sma1
      indicator<-ema-ema1
      #indicator<-ema-sma
      #indicator<-cl-ema
      if (indicator>0){
        #pos[i] <-params$posSizes[i]
        pos[params$series[i]] <-params$posSizes[params$series[i]]
      
      } 
      else if(indicator<0){
        #pos[i] <--params$posSizes[i]
        pos[params$series[i]] <--params$posSizes[params$series[i]]
  
      }
    }
}
  }
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
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

