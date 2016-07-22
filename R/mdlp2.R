#' @import discretization
library(discretization)



#'

#' Method that matches rule model against test data.
#'
#' @param df a data frame with data.
#' @param classatt name the class attribute in df
#' @param min_distinct_values the minimum number of unique values a column needs to have to be subject to supervised discretization.
#' @param unsupervised_bins  number of target bins for  discretizing the class attribute. Ignored when the class attribute is not numeric or when discretize_class is set to FALSE.
#' @param discretize_class logical value indicating whether the class attribute should be discretized. Ignored when the class attribute is not numeric.
#'
#' @return list with two slots: $cutp with cutpoints and $Disc.data with discretization results
#'
#' @examples
#' \code{
#'   data(iris)
#'   discrNumeric(iris,"Species")
#' }
#'
#'
#' @export
discrNumeric <- function(df,classatt,min_distinct_values=3,unsupervised_bins=3, discretize_class=FALSE)
{
  classatt_col<-which(colnames(df)==classatt)
  if(!is.factor(df[[classatt_col]]))
  {
    if (discretize_class)
    {
      class_discr<-discretizeUnsupervised(df[[classatt]],labels=TRUE,infinite_bounds=TRUE,categories=unsupervised_bins)
      df[[classatt_col]] <-applyCut(df[[classatt_col]],class_discr$cutp,infinite_bounds=TRUE,labels=TRUE)
    }
    else
    {
      df[[classatt_col]] <- factor(df[[classatt_col]])
    }
  }
  discr<-mdlp2(df,class=classatt_col,skip_nonnumeric=TRUE,labels=TRUE,handle_missing=TRUE,infinite_bounds=TRUE,min_distinct_values)
  if (exists("class_discr")){
    discr$cutp[[classatt_col]]<-class_discr$cutp
  }
  return (discr)

}
#' Function that applies cut points on input data.
#'
#' @param df input data frame.
#' @param cutp a list of vectors with cutpoints @seealso \code{\link{applyCut}}.
#' @param infinite_bounds a logical indicating how the bounds on the extremes should look like.
#' @param labels a logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#' @return discretized data. If there was no discretization specified for some columns, these are returned as is.
#' @export
#'
#' @examples
#' \code{
#'
#'   applyCuts(iris,list(c(5,6),c(2,3),"All","NULL","NULL"),TRUE,TRUE)
#'
#' }
#' @seealso{apply_cut}
#'

applyCuts <-function(df,cutp,infinite_bounds,labels)
{
  xd <- df
  for (i in 1:length(cutp)){

      xd[,i] <- applyCut(df[[i]],cutp[[i]],infinite_bounds,labels)
    }

  return (xd)
}

#' Function that applies cut points on input column.
#'
#' @param col input vector with data.
#' @param cuts1 vector with cutpoints.
#' There are several special values defined:
#' \itemize{
#' \item NULL indicates that no discretization will be performed,
#' \item All indicates all values will be merged into one.
#' }

#' @param infinite_bounds a logical indicating how the bounds on the extremes should look like.
#'  If set to FALSE, the leftmost/rightmost intervals will be bounded by the minimum and maximum in the respective column.
#'  If set to TRUE, the leftmost/rightmost intervals will be bounded by negative and positive infinity.
#' @param labels a logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#'
#' @return Vector with discretized data.
#' @export
#'
#' @examples
#' \code{
#'   applyCut(iris[[1]],c(3,6),TRUE,TRUE)
#'
#' }
#' @seealso \code{\link{applyCuts}}
#'
applyCut <- function(col,cuts,infinite_bounds,labels)
{
  cuts1<-unlist(cuts)
  if (cuts1[1]=="NULL")
  {
    return(col)
  }
  else if (cuts1[1]=="All")
  {
    #String "All" indicates that all values should be covered by one interval
    cuts1 <- c()
  }
  if (infinite_bounds)
  {
    cuts1 <- c(-Inf,cuts1,+Inf)
  }
  else
  {
    cuts1 <- c(min(col),cuts1,max(col))
  }
  if (labels)
  {
    # the first arg in cuts cannot be x as in the original version
    # because x may be a shorter vector due to omission of missing observations

    # invoking cutSemicolon instead of cut  ensures that numbers in intervals are separated with semicolon not colon: "(0,1.05]" -> "(0;1.05]"
    # the reason is that in the arules format colon would be overloaded: it is also used to separate items in a rule
    # dig.lab=12 ensures that cut should not perform  rounding of interval boundaries (unless there is more than 12 digits, which is maximum in cut)
    # if rounding is in place it can cause the interval not to match any instance

    out <- cutSemicolon(col,cuts1,dig.lab=12,include.lowest = TRUE)
  }
  else
  {
    out <- as.integer(cut(col,cuts1, labels=FALSE, include.lowest = TRUE))
  }

  return(out)
}

#' Performs unsupervised discretization.
#'
#' @param categories number of categories (bins) to produce.
#' @param data input numeric vector.
#' @param infinite_bounds a logical indicating how the bounds on the extremes should look like.
#' @param labels a logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#' @return Discretized data. If there was no discretization specified for some columns, these are returned as is.
#' @export
#'
#' @examples
#' \code{
#'   cba_csv("iris.csv","iris-rules.csv")
#'
#' }
#'

discretizeUnsupervised <- function(data, labels=FALSE, infinite_bounds=FALSE,categories=3)
{
  if (is.factor(data))
  {
    cutp <- "NULL"
    xd<-data
  }
  else if (length(unique(data))<=categories)
  {
    cutp <- "NULL"
    xd <- factor(data)
  } else {
    cutp<-discretize(data,  "frequency", categories=categories,onlycuts=TRUE)
    #remove lower and upper bounds, so that they can be replaced by +-infinite
    cutp <- cutp[-c(1,length(cutp))]
    xd <-apply_cut(data,cutp,infinite_bounds=infinite_bounds,labels=labels)
  }
  return (list(cutp=cutp,Disc.data=xd))
}
#' Performs supervised discretization.
#'
#' @param df input data frame.
#' @param handle_missing Setting to TRUE activates the following behaviour: if there are any missing observations in the column processed,
#'  the input for discretization is a subset of data containing this column and target with rows containing missing values excuded.
#' @param infinite_bounds A logical indicating how the bounds on the extremes should look like.
#' @param skip_nonnumeric If set to TRUE, any non-numeric columns will be skipped.
#' @param min_distinct_values If a column contains less than specified number of distinct values, it is not discretized.
#' @param class_index index of the class variable. If not specified, the last column is used as the class variable.
#' @param labels A logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#'
#' @return Discretized data. If there were any non-numeric input columns they are returned as is. All returned columns except class are factors.
#' @export
#'
#' @examples
#' \code{
#'   mdlp2(iris) #gives the same result as mdlp(iris) from discretize package
#'   #uses Sepal.Length as target variable
#'   mdlp2(iris,handle_missing=TRUE,labels=TRUE,skip_nonnumeric=TRUE,infinite_bounds=TRUE,min_distinct_values=30,class=1)
#' }
#'

mdlp2 <-  function(df,class_index=NULL,handle_missing=FALSE,labels=FALSE,skip_nonnumeric=FALSE, infinite_bounds=FALSE,min_distinct_values=3){
  if (is.null(class_index))
  {
    class_index<-length(df[1,])
  }

  if (!handle_missing)
  {
    y <- df[,class_index]
  }
  xd <- df
  cutp <- list()
  for (i in 1:length(df[1,])){
    print(i)
    if (i==class_index) next
    if (!is.numeric(df[[i]]) & skip_nonnumeric)
    {
      cutp[[i]] <- "NULL"
      next
    }
    if (length(unique(df[[i]]))<min_distinct_values)
    {
      xd[,i] <- as.factor(df[[i]])
      next
    }

    if (handle_missing)
    {
      data_<-na.omit(df[c(i,class_index)])
      x <- data_[,1]
      y <- data_[,2]
    }
    else
    {
      x <- df[[i]]
    }
    # prevents the  'breaks' are not unique error
    if (length(unique(x))<2)
    {
      if (labels)
      {
        cutp[[i]] <- "NULL"
        # if labels is set to true the result should be all factors
        # in this case there is no discretization, so we need to convert the number to factor explicitly
        xd[,i] <- as.factor(df[[i]])
      }
      next

    }

    cutp[[i]] <-  cutPoints(x,y)
    if(length(cutp[[i]])==0) cutp[[i]] <- "All"
    xd[,i] <- applyCut(df[[i]],cutp[[i]],infinite_bounds,labels)

  }
  return (list(cutp=cutp,Disc.data=xd))
}

#cut function from R.base with one modification
#numbers are separated with semicolon instead of comma
cutSemicolon <-
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
