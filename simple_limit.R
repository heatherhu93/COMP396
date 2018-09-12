#params  <- list(atr=30,p=0.89,series=10)

maxRows <- 1100
getOrders <- function(store, newRowList, currentPos, params) {

    #cat("currentPos", formatC(currentPos,3),"\n")

    # check if current inventory is above a limit and if so exit completely
    # with a market order
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  marketOrders <- rep(0,length(newRowList))
  limitOrders1  <- rep(0,length(newRowList))
  limitOrders2  <- rep(0,length(newRowList))
  limitPrices1  <- rep(0,length(newRowList))
  limitPrices2  <- rep(0,length(newRowList))
  
    # use the range (High-Low) as a indicator for a reasonable "spread" for
    # this pseudo market making strategy
  if (store$iter > params$atr) {
    startIndexatr <-  store$iter - params$atr - 1
    
    for (i in 1:length(params$series)) {
      High<-store$high[startIndexatr:store$iter,i]
      Low<-store$low[startIndexatr:store$iter,i]
      Close<-store$cl[startIndexatr:store$iter,i]
      HLC<-cbind(High,Low,Close)
      
      atr<-last(as.matrix(ATR(HLC,n=params$atr))[,'atr'])
      
      limitOrders1  <- c(0,0,0,0,0,0,0,0,0,params$posSizes[i])
     #limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
      limitPrices1  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$High - atr*params$p)
      
      limitOrders2  <- c(0,0,0,0,0,0,0,0,0,-params$posSizes[i]) 
      #limitOrders2  <-rep(-1,length(newRowList)) # SELL LIMIT ORDERS
      limitPrices2  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$Low + atr*params$p)
    }
  }

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=limitOrders1,
	                        limitPrices1=limitPrices1,
	                        limitOrders2=limitOrders2,
	                        limitPrices2=limitPrices2))
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


