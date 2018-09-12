#params <- list(lookback=30,threshold=15, series=c(7),posSizes=rep(1,10))

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  
  
  marketOrders <- -currentPos; 
  pos <- allzero
  
  if (store$iter > params$lookbackRsi) {
    startIndexRsi <-  store$iter - params$lookbackRsi-1
    
    for (i in 1:length(params$series)) {
      
      rsi <- last(RSI(store$cl[startIndexRsi:store$iter,i],n=params$lookbackRsi))
      
      if (rsi < (50 - params$threshold)) 
      {
        pos[i] <- params$posSizes[i]
      }
      else if (rsi > (50 + params$threshold))
      {
        pos[i] <- -params$posSizes[i]
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
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}

