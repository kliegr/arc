library(discretization)
# heavily modified version of function mdlp from the discretize package:
# handles missing values,allows to output data with labels, allows to automatically skip numeric columns, class does not need to be the last attribute
mdlp2 <-  function(data,handle_missing=FALSE,labels=FALSE,skip_nonnumeric=FALSE,class=NULL, infinite_bounds=FALSE){
  if (is.null(class))
  {
    class<-length(data[1,])
  }
  
  if (!handle_missing)
  {
    y <- data[,class] 
  }
  
  xd <- data
  cutp <- list()
  for (i in 1:length(data[1,])){
    if (i==class) next
    if (!is.numeric(data[[i]]) & skip_nonnumeric)
    {
      next
    }
    

    if (handle_missing)
    {
      data_<-na.omit(data[c(i,class)])
      x <- data_[,1]
      y <- data_[,2]
      
    }
    else{
      x <- data[,i]
    }
    # prevents the  'breaks' are not unique error
    if (length(unique(x))<2) 
    {
      if (labels)
      {
        #if labels is set to true the result should be all factors
        #in this case there is no discretization, so we need to convert the number to factor explicitly
        xd[,i] <- as.factor(data[[i]])
      }
      next
    }
    
    
    cuts1 <- cutPoints(x,y)
    if (infinite_bounds)
    {
      cuts <- c(-Inf,cuts1,+Inf)
    }
    else
    {
      cuts <- c(min(x),cuts1,max(x))
    }
      
    cutp[[i]] <- cuts1
    if(length(cutp[[i]])==0) cutp[[i]] <- "All"
    if (labels)
    {
      #the first arg in cuts cannot be x as in the original version
      #because x may be a shorter vector due to omission of missing observations
      
      #invoking cut.semicolon instead of cut  ensures that numbers in intervals are separated with semicolon not colon: "(0,1.05]" -> "(0;1.05]"
      #the reason is that in the arules format colon would be overloaded: it is also used to separate items in a rule
      #dig.lab=12 ensures that cut should not perform  rounding of interval boundaries (unless there is more than 12 digits, which is maximum in cut)
      #if rounding is in place it can cause the interval not to match any instance
      xd[,i] <- cut.semicolon(data[[i]],cuts,dig.lab=12,include.lowest = TRUE)
    }
    else
    {
      xd[,i] <- as.integer(cut(data[[i]],cuts, labels=FALSE, include.lowest = TRUE))
    }
    
  }
  return (list(cutp=cutp,Disc.data=xd))
}

#cut function from R.base with one modification
#numbers are separated with semicolon instead of comma
cut.semicolon <-
  function (x, breaks, labels = NULL, include.lowest = FALSE,
            right = TRUE, dig.lab = 3L, ordered_result = FALSE, ...)
  {
    numsep<-";"
    if (!is.numeric(x)) stop("'x' must be numeric")
    if (length(breaks) == 1L) {
      if (is.na(breaks) || breaks < 2L)
        stop("invalid number of intervals")
      nb <- as.integer(breaks + 1) # one more than #{intervals}
      dx <- diff(rx <- range(x, na.rm = TRUE))
      if(dx == 0) {
        dx <- abs(rx[1L])
        breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
                          length.out = nb)
      } else {
        breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
        breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + dx/1000)
      }
    } else nb <- length(breaks <- sort.int(as.double(breaks)))
    if (anyDuplicated(breaks)) stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {#- try to construct nice ones ..
      for(dig in dig.lab:max(12L, dig.lab)) {
        ## 0+ avoids printing signed zeros as "-0"
        ch.br <- formatC(0+breaks, digits = dig, width = 1L)
        if(ok <- all(ch.br[-1L] != ch.br[-nb])) break
      }
      labels <-
        if(ok) paste0(if(right)"(" else "[",
                      ch.br[-nb], numsep, ch.br[-1L],
                      if(right)"]" else ")")
      else paste("Range", seq_len(nb - 1L), sep="_")
      if (ok && include.lowest) {
        if (right)
          substr(labels[1L], 1L, 1L) <- "[" # was "("
        else
          substring(labels[nb-1L],
                    nchar(labels[nb-1L], "c")) <- "]" # was ")"
      }
    } else if (is.logical(labels) && !labels)
      codes.only <- TRUE
    else if (length(labels) != nb - 1L)
      stop("lengths of 'breaks' and 'labels' differ")
    code <- .bincode(x, breaks, right, include.lowest)
    if(codes.only) code
    else factor(code, seq_along(labels), labels, ordered = ordered_result)
  }

#mdlp2(iris[c(5,2,3)],skip_nonnumeric=TRUE,labels=TRUE,handle_missing=TRUE,class=1)
