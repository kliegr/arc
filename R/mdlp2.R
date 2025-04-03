#' @importFrom stats na.omit
#' @import discretization
library(discretization)

#' Discretize Numeric Columns In Data frame
#' @description Can discretize both predictor columns in  data frame -- using supervised algorithm MDLP (Fayyad & Irani, 1993) -- and the target class -- using unsupervised algorithm (k-Means).
#' This R file contains fragments of code from the GPL-licensed R discretization package by HyunJi Kim.
#' @references Fayyad, U. M. and Irani, K. B. (1993). Multi-interval discretization of continuous-valued attributes for classification learning, Artificial intelligence 13, 1022–1027
#' @param df a data frame with data.
#' @param classAtt name the class attribute in df
#' @param min_distinct_values the minimum number of unique values a column needs to have to be subject to supervised discretization.
#' @param unsupervised_bins  number of target bins for  discretizing the class attribute. Ignored when the class attribute is not numeric or when \code{discretize_class} is set to FALSE.
#' @param discretize_class logical value indicating whether the class attribute should be discretized. Ignored when the class attribute is not numeric.
#'
#' @return list with two slots: \code{$cutp} with cutpoints and \code{$Disc.data} with discretization results
#'
#' @examples
#'   discrNumeric(datasets::iris, "Species")
#'
#' @export
discrNumeric <- function(df, classAtt, min_distinct_values = 3, unsupervised_bins = 3, discretize_class = FALSE)
{
  cl_index <- which(colnames(df) ==classAtt)
  if(!is.factor(df[[cl_index]]))
  {
    if (discretize_class)
    {
      class_discr <- discretizeUnsupervised(df[[classAtt]], labels= TRUE, infinite_bounds = TRUE, categories = unsupervised_bins)
      df[[cl_index]] <- applyCut(df[[cl_index]], class_discr$cutp, infinite_bounds=TRUE, labels=TRUE)
    }
    else
    {
      df[[cl_index]] <- factor(df[[cl_index]])
    }
  }
  discr <- mdlp2(df,cl_index=cl_index, skip_nonnumeric = TRUE, labels = TRUE,
                 handle_missing = TRUE, infinite_bounds = TRUE, min_distinct_values)
  if (exists("class_discr")){
    discr$cutp[[cl_index]] <- class_discr$cutp
  }
  else{
    # associate class with NULL which indicates it should not be discretized but turned to factor
    discr$cutp[cl_index] <- list(NULL)
  }
  return (discr)

}
#' Apply Cut Points to Data Frame
#' @description Applies cut points to input data frame.
#'
#' @param df input data frame.
#' @param cutp a list of vectors with cutpoints (for more information see \code{\link{applyCut}}).
#' @param infinite_bounds a logical indicating how the bounds on the extremes should look like (for more information see \code{\link{applyCut}})
#' @param labels a logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#' @return discretized data. If there was no discretization specified for some columns, these are returned as is.
#' @export
#'
#' @examples
#'   applyCuts(datasets::iris, list(c(5,6), c(2,3), "All", NULL, NULL), TRUE, TRUE)
#'
#' @seealso{applyCut}
#'

applyCuts <-function(df,cutp,infinite_bounds,labels)
{
  xd <- df
  for (i in 1:length(cutp)){

      xd[,i] <- applyCut(df[[i]], cutp[[i]], infinite_bounds, labels)
    }

  return (xd)
}

#' Apply Cut Points to Vector
#' @description  Applies cut points to vector.
#' @param col input vector with data.
#' @param cuts vector with cutpoints.
#' There are several special values defined:
#' \code{NULL} indicates that no discretization will be performed, but the value will be converted to factor
#'  \code{"All"} indicates all values will be merged into one.

#' @param infinite_bounds a logical indicating how the bounds on the extremes should look like.
#'  If set to \code{FALSE}, the leftmost/rightmost intervals will be bounded by the minimum and maximum in the respective column.
#'  If set to \code{TRUE}, the leftmost/rightmost intervals will be bounded by negative and positive infinity.
#' @param labels a logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#'
#' @return Vector with discretized data.
#' @export
#'
#' @examples
#'   applyCut(datasets::iris[[1]], c(3,6), TRUE, TRUE)
#' @seealso \code{\link{applyCuts}}
#'
applyCut <- function(col, cuts, infinite_bounds, labels)
{
  cuts1 <- unlist(cuts)
  if (is.null(cuts1[1]))
  {
    return(as.factor(col))
  }
  else if (cuts1[1] == "All")
  {
    #String "All" indicates that all values should be covered by one interval
    cuts1 <- c()
  }
  if (infinite_bounds)
  {
    cuts1 <- c(-Inf, cuts1, +Inf)
  }
  else
  {
    cuts1 <- c(min(col), cuts1, max(col))
  }
  if (labels)
  {
    # the first arg in cuts cannot be x as in the original version
    # because x may be a shorter vector due to omission of missing observations

    # invoking cutSemicolon instead of cut  ensures that numbers in intervals are separated with semicolon not colon: "(0,1.05]" -> "(0;1.05]"
    # the reason is that in the arules format colon would be overloaded: it is also used to separate items in a rule
    # dig.lab=12 ensures that cut should not perform  rounding of interval boundaries (unless there is more than 12 digits, which is maximum in cut)
    # if rounding is in place it can cause the interval not to match any instance

    out <- cutSemicolon(col, cuts1, dig.lab=12, include.lowest = TRUE)
  }
  else
  {
    out <- as.integer(cut(col, cuts1, labels=FALSE, include.lowest = TRUE))
  }

  return(out)
}

#' Unsupervised Discretization
#' @description Discretizes provided numeric vector.
#' @param categories number of categories (bins) to produce.
#' @param data input numeric vector.
#' @param infinite_bounds a logical indicating how the bounds on the extremes should look like.
#' @param labels a logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#' @param method clustering method, one of "interval" (equal interval width), "frequency" (equal frequency), "cluster" (k-means clustering). See also documentation of the \code{\link[arules]{discretize}} function from the arules package.
#' @return Discretized data. If there was no discretization specified for some columns, these are returned as is.
#' @export
#'
#' @examples
#'   discretizeUnsupervised(datasets::iris[[1]])
#'

discretizeUnsupervised <- function(data, labels=FALSE, infinite_bounds=FALSE,categories=3,method="cluster")
{
  if (is.factor(data))
  {
    cutp <- NULL
    xd<-data
  }
  else if (length(unique(data))<=categories)
  {
    cutp <- NULL
    xd <- factor(data)
  } else {
    cutp <- discretize(data,  method, categories = categories, onlycuts=TRUE)
    #remove lower and upper bounds, so that they can be replaced by +-infinite
    cutp <- cutp[-c(1, length(cutp))]
    xd <- applyCut(data, cutp, infinite_bounds, labels)
  }
  return (list(cutp = cutp,Disc.data = xd))
}

#' Supervised Discretization
#' @description Performs supervised discretization of numeric columns, except class, on the provided data frame. Uses the Minimum Description Length Principle algorithm (Fayyed and Irani, 1993) as implemented in the discretization package.
#' @references Fayyad, U. M. and Irani, K. B. (1993). Multi-interval discretization of continuous-valued attributes for classification learning, Artificial intelligence 13, 1022–1027
#' @param df input data frame.
#' @param handle_missing Setting to TRUE activates the following behaviour: if there are any missing observations in the column processed,
#'  the input for discretization is a subset of data containing this column and target with rows containing missing values excuded.
#' @param infinite_bounds A logical indicating how the bounds on the extremes should look like.
#' @param skip_nonnumeric If set to TRUE, any non-numeric columns will be skipped.
#' @param min_distinct_values If a column contains less than specified number of distinct values, it is not discretized.
#' @param cl_index index of the class variable. If not specified, the last column is used as the class variable.
#' @param labels A logical indicating whether the bins of the discretized data should be represented by integer codes or as interval notation using (a;b] when set to TRUE.
#'
#' @return Discretized data. If there were any non-numeric input columns they are returned as is. All returned columns except class are factors.
#' @export
#'
#' @examples
#'   mdlp2(datasets::iris) #gives the same result as mdlp(datasets::iris) from discretize package
#'   #uses Sepal.Length as target variable
#'   mdlp2(df=datasets::iris, cl_index = 1,handle_missing = TRUE, labels = TRUE,
#'   skip_nonnumeric = TRUE, infinite_bounds = TRUE, min_distinct_values = 30)
#'

mdlp2 <-  function(df, cl_index = NULL, handle_missing = FALSE, labels = FALSE,
  skip_nonnumeric = FALSE, infinite_bounds = FALSE,min_distinct_values = 3){
  if (is.null(cl_index))
  {
    cl_index <- length(df[1,])
  }

  if (!handle_missing)
  {
    y <- df[,cl_index]
  }
  xd <- df
  cutp <- list()
  for (i in 1:length(df[1,])){
    if (i == cl_index) next
    if (!is.numeric(df[[i]]) & skip_nonnumeric)
    {
      cutp[[i]] <- NULL
      next
    }
    if (length(unique(df[[i]])) < min_distinct_values)
    {
      xd[,i] <- as.factor(df[[i]])
      next
    }

    if (handle_missing)
    {
      data_ <- stats::na.omit(df[c(i,cl_index)])
      x <- data_[,1]
      y <- data_[,2]
    }
    else
    {
      x <- df[[i]]
    }
    # prevents the  'breaks' are not unique error
    if (length(unique(x)) < 2)
    {
      if (labels)
      {
        cutp[[i]] <- NULL
        # if labels is set to true the result should be all factors
        # in this case there is no discretization, so we need to convert the number to factor explicitly
        xd[,i] <- as.factor(df[[i]])
      }
      next
    }
    cutp[[i]] <-  cutPoints(x,y)
    if(length(cutp[[i]]) == 0) cutp[[i]] <- "All"
    xd[,i] <- applyCut(df[[i]], cutp[[i]], infinite_bounds, labels)

  }
  return (list(cutp = cutp, Disc.data = xd))
}
