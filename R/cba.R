#' @importFrom utils read.csv write.csv
library(arules)



#' CBARuleModel
#'
#' @description  This class represents a rule-based classifier.

#' @name CBARuleModel-class
#' @rdname CBARuleModel-class
#' @exportClass CBARuleModel
#' @slot rules an object of class rules from arules package
#' @slot cutp list of cutpoints
#' @slot classAtt name of the target class attribute
#' @slot attTypes attribute types
CBARuleModel <- setClass("CBARuleModel",
  slots = c(
    rules = "rules",
    cutp = "list",
    classAtt ="character",
    attTypes = "vector"
  )
)



#' Apply Rule Model
#' @description Method that matches rule model against test data.
#'
#' @param object a \link{CBARuleModel} class instance
#' @param newdata a data frame with data
#' @param ... other arguments (currently not used)
#' @return A vector with predictions.
#' @export
#' @method predict CBARuleModel
#' @examples
#'   allData<-datasets::iris[sample(nrow(datasets::iris)),]
#'   trainFold <- allData[1:100,]
#'   testFold <- allData[101:nrow(allData),]
#'   #increase for more accurate results in longer time
#'   target_rule_count <- 1000
#'   classAtt <- "Species"
#'   rm <- cba(trainFold, classAtt, list(target_rule_count = target_rule_count))
#'   prediction <- predict(rm, testFold)
#'   acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
#'   message(acc)
#' @seealso \link{cbaIris}
#'
predict.CBARuleModel <- function(object, newdata,...) {
  start.time <- Sys.time()
  rule_model <- object
  # apply any discretization that was applied on train data also on test data
  test_txns <- as(applyCuts(newdata, rule_model@cutp, infinite_bounds=TRUE, labels=TRUE), "transactions")
  # t is logical matrix with |rules| rows |test instances| columns
  # the unname function is not strictly necessary, but it may save memory for larger data:
  #  as the is.subset function returns concatenated attribute  values as the name for each column (test instance)
  t <- unname(is.subset(rule_model@rules@lhs, test_txns))
  # get row index of first rule matching each transaction
  matches <- apply(t, 2, function(x) min(which(x==TRUE)))
  # for each element in the matches vector (i.e. index of first matching rule)
  # get the index of the item on the right hand side of this rule which is true
  # and lookup the name of this item in iteminfo by this index
  result <- droplevels(unlist(lapply(matches, function(match) rule_model@rules@rhs@itemInfo[which(rule_model@rules@rhs[match]@data == TRUE),][1,3])))
  
  end.time <- Sys.time()
  message (paste("Prediction (CBA model application) took:", round(end.time - start.time, 2), " seconds"))  
  return(result)
}

#' Prediction Accuracy
#' @description Compares predictions with groundtruth and outputs accuracy.
#'
#' @param prediction a vector with predictions
#' @param groundtruth a vector with groundtruth
#'
#' @return Accuracy
#' @export
CBARuleModelAccuracy<- function(prediction, groundtruth)
{
  prediction <- as.factor(prediction)
  groundtruth <- as.factor(groundtruth)
  both <- union(levels(groundtruth), levels(prediction))
  accuracy <- mean(factor(groundtruth, levels = both) == factor(prediction, levels = both))
  return(accuracy)
}


#' Method that generates items for values in given data frame column.
#'
#' @param df a data frame contain column \code{classAtt}.
#' @param classAtt name of the column in \code{df} to generate items for.
#'
#' @return appearance object for mining classification rules
#' @export
#'
#' @examples
#' getAppearance(datasets::iris,"Species")
#'
getAppearance <- function(df, classAtt){
  classes <- as.character(unname(unique(unlist(df[classAtt]))))
  classitems <- paste(classAtt, "=", classes, sep="")
  appearance <- list(rhs =  classitems, default="lhs")
  return(appearance)
}

#' @title  Example CBA Workflow with CSV Input
#' @description Learns a CBA rule set and saves the resulting rule set back to csv.
#'
#' @param path path to csv file with data.
#' @param outpath path to write the rule set to.
#' @param classAtt the name of the class attribute.
#' @param idcolumn the name of the id column in the dataf ile.
#' @param rulelearning_options custom options for the rule learning algorithm overriding the default values.
#' @param pruning_options custom options for the pruning algorithm overriding the default values.

#'
#' @return Object of class \link{CBARuleModel}
#' @export
#'
#' @examples
#'  # cbaCSV("path-to-.csv")
#'
#'
cbaCSV <- function(path, outpath = NULL, classAtt = NULL, idcolumn = NULL, rulelearning_options = NULL, pruning_options = NULL)
{
  train <- utils::read.csv(path, header  =TRUE, check.names = FALSE)
  if (!is.null(idcolumn))
  {
    train <- subset( train, select = -c (idcolumn) )
  }

  if (is.null(classAtt))
  {
    classAtt<-colnames(train)[ncol(train)]
  }
  rm<-cba(train, classAtt, rulelearning_options, pruning_options)
  if (!is.null(outpath))
  {
    utils::write.csv(as(rm@rules, "data.frame"), outpath, row.names=TRUE, quote = TRUE)
  }
  return(rm)

}
#' @title Test CBA Workflow on Iris Dataset
#' @description Test workflow on iris dataset: learns a cba classifier on one "train set" fold , and applies it to the second  "test set" fold.
#'
#' @return Accuracy.
#' @export
#'
#'
cbaIris <- function()
{
  classAtt <- "Species"
  set.seed(111)
  allData <- datasets::iris[sample(nrow(datasets::iris)),]
  trainFold <- allData[1:100,]
  testFold <- allData[101:nrow(allData),]
  # increase for more accurate results in longer time
  target_rule_count <- 1000
  rm <- cba(trainFold, classAtt = classAtt, rulelearning_options = list(target_rule_count = target_rule_count))
  prediction <- predict(rm, testFold)
  acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
  return (acc)
}

#' @title Test CBA Workflow on Iris Dataset with numeric target
#' @description Test workflow on iris dataset: learns a cba classifier on one "train set" fold, and applies it to the second  "test set" fold.
#'
#' @return Accuracy.
#' @export
#'
#'
cbaIrisNumeric <- function()
{
  classAtt <- "Species"
  set.seed(111)
  allData <- datasets::iris[sample(nrow(datasets::iris)),]

  #map target to numeric codes
  x <- vector(mode="numeric", length=nrow(allData))
  x[allData[5] == "setosa"] <- 1
  x[allData[5] == "virginica"] <- 2
  x[allData[5] == "versicolor"] <- 3
  allData[5] <- x

  trainFold <- allData[1:100,]
  testFold <- allData[101:nrow(allData),]
  # increase for more accurate results in longer time
  target_rule_count <- 1000
  rm <- cba(trainFold, classAtt = classAtt, rulelearning_options = list(target_rule_count = target_rule_count))
  prediction <- predict(rm, testFold)
  acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
  return (acc)
}

#' @title CBA Classifier
#' @description Learns a CBA rule set from supplied dataframe.
#' @export
#' @param train a data frame with data.
#' @param classAtt the name of the class attribute.
#' @param rulelearning_options custom options for the rule learning algorithm overriding the default values.
#' @param pruning_options custom options for the pruning algorithm overriding the default values.
#'
#' @return Object of class \link{CBARuleModel}.
#'
#' @examples
#'   cba(datasets::iris, "Species", rulelearning_options = list(target_rule_count = 50000))

cba <- function(train, classAtt, rulelearning_options=NULL, pruning_options=NULL){

  discr <- discrNumeric(train, classAtt)

  txns <- as(discr$Disc.data, "transactions")

  appearance <- getAppearance(train, classAtt)

  start.time <- Sys.time()


  rules <- do.call("topRules", appendToList(list(txns = txns, appearance = appearance), rulelearning_options))

  end.time <- Sys.time()
  message (paste("Rule learning (incl. automatic threshold detection) took:", round(end.time - start.time, 2), " seconds"))

  start.time <- Sys.time()
  rules <- do.call("prune", appendToList(list(rules = rules,txns = txns,classitems = appearance$rhs), pruning_options))

  #rules <-prune(rules, txns,classitems,pruning_options)
  end.time <- Sys.time()
  message (paste("Pruning took:", round(end.time - start.time,2), " seconds"))

  #bundle cutpoints with rule set into one object
  rm <- CBARuleModel()
  rm@rules <- rules
  rm@cutp <- discr$cutp
  rm@classAtt <- classAtt
  rm@attTypes <- sapply(train, class)
  return(rm)
}



appendToList <- function(list1,list2){
  # even if length==0, the for cycle would be run once without this condition
  if (length(list2) == 0) return(list1)
  for (i in 1:length(list2))
  {
    list1[[names(list2)[i]]] <- list2[[i]]
  }
  return(list1)
}
