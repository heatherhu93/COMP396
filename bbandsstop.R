#params <- list(lookbackbbandsstop=30,sdParam=2.5,posSizes=rep(1,10))

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$lookbackbbandsstop) {
    #decide when to start
    startIndexbbandsstop <-  store$iter - params$lookbackbbandsstop
    #for loop for series that determined in parameter
    for (i in 1:length(params$series)) {
      
      #define close price
      cl <- store$cl[store$iter,i]
      
      # using bbands as an example
      bbands <- last(BBands(store$cl[startIndexbbandsstop:store$iter,i],
                            n=params$lookbackbbandsstop,sd=params$sdParam))
      
      # if price touches lower band, buy
      if (cl < bbands["dn"]) {
        pos[i] <- params$posSizes[i]
        
      }
      #if price touches upper band, sell
      if (cl > bbands["up"]) {
        pos[i] <- -params$posSizes[i]
      }
      
      # check whether the former day trade or not
      if (currentPos[i]==0) {
        
        # if today does trading unlike yesterday, mark today as entry point
        if (pos[i] != currentPos[i])
          store$entry[i] <- store$iter
        
      }
      
      else {
        # call tht function to calculate return
        ret <- getTradeReturn(store$cl[,i],store$entry[i],store$iter,isTRUE(pos[i]>0))
        
        # if return is not less than stop points, stay in trade
        if ( ret<0.3&&ret>-0.1) 
          pos[i] <- currentPos[i]            
        # export return which sets exit as stop day.
        else 
          pos[i] <- 0
      }
    }
  }
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
  
  
} 

getTradeReturn <- function(cl,entry,exit,short=FALSE) {
  # set prices as number
  prices <- as.numeric(cl)
  # in my opinion, we set dafault value of short as false
  # so,this means either long or flat
  if (short)
    # calculate return
    prices[entry]/prices[exit] - 1
  else
    # calculate return
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


initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),entry=rep(1,10)))
  
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$entry <- store$entry
  return(store)
}



