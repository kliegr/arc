#' @importFrom utils read.csv write.csv data
library(arules)



#' RuleModel
#'
#' @description  This class represents a rule-based classifier.

#' @name RuleModel-class
#' @rdname RuleModel-class
#' @exportClass RuleModel
#' @slot rules an object of class rules from arules package
#' @slot cutp list of cutpoints
#' @slot classatt name of the target class attribute
RuleModel <- setClass("RuleModel",
  slots = c(
    rules = "rules",
    cutp = "list",
    classatt ="character"
  )
)



#' Apply Rule Model
#' @description Method that matches rule model against test data.
#'
#' @param object a \link{RuleModel} class instance
#' @param newdata a data frame with data
#' @param ... other arguments (currently not used)
#' @return A vector with predictions.
#' @export
#' @method predict RuleModel
#' @examples
#'   utils::data(iris)
#'   train <- iris[1:100,]
#'   test <- iris[101:length(iris),]
#'   #increase for more accurate results in longer time
#'   target_rule_count <- 1000
#'   classatt <- "Species"
#'   rm <- cba(train, classatt, list(target_rule_count = target_rule_count))
#'   prediction <- predict(rm, test)
#'   acc <- rulemodelAccuracy(prediction, test[[classatt]])
#'   message(acc)
#' @seealso \link{cbaIris}
#'
predict.RuleModel <- function(object, newdata,...) {
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
rulemodelAccuracy<- function(prediction, groundtruth)
{
  both <- union(levels(groundtruth), levels(prediction))
  accuracy <- mean(factor(groundtruth, levels = both) == factor(prediction, levels = both))
  return(accuracy)
}


#' Method that generates items for values in given data frame column.
#'
#' @param df a data frame contain column \code{classatt}.
#' @param classatt name of the column in \code{df} to generate items for.
#'
#' @return appearance object for mining classification rules
#' @export
#'
#' @examples
#' utils::data(iris)
#' getAppearance(iris,"Species")
#'
getAppearance <- function(df, classatt){
  classes <- as.character(unname(unique(unlist(df[classatt]))))
  classitems <- paste(classatt, "=", classes, sep="")
  appearance <- list(rhs =  classitems, default="lhs")
  return(appearance)
}

#' @title  Example CBA Workflow with CSV Input
#' @description Learns a CBA rule set and saves the resulting rule set back to csv.
#'
#' @param path path to csv file with data.
#' @param outpath path to write the rule set to.
#' @param classatt the name of the class attribute.
#' @param idcolumn the name of the id column in the dataf ile.
#' @param rulelearning_options custom options for the rule learning algorithm overriding the default values.
#' @param pruning_options custom options for the pruning algorithm overriding the default values.

#'
#' @return Object of class \link{RuleModel}
#' @export
#'
#' @examples
#'  # cbaCSV("path-to-.csv")
#'
#'
cbaCSV <- function(path, outpath = NULL, classatt = NULL, idcolumn = NULL, rulelearning_options = NULL, pruning_options = NULL)
{
  train <- utils::read.csv(path, header  =TRUE, check.names = FALSE)
  if (!is.null(idcolumn))
  {
    train <- subset( train, select = -c (idcolumn) )
  }

  if (is.null(classatt))
  {
    classatt<-colnames(train)[ncol(train)]
  }
  rm<-cba(train, classatt, rulelearning_options, pruning_options)
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
  utils::data("iris")
  classatt <- "Species"
  train <- iris[1:100,]
  test <- iris[101:length(iris),]
  # increase for more accurate results in longer time
  target_rule_count <- 1000
  rm <- cba(test <- iris[101:length(iris),], classatt = classatt, rulelearning_options = list(target_rule_count = target_rule_count))
  prediction <- predict(rm, test)
  acc <- rulemodelAccuracy(prediction, test[[classatt]])
  return (acc)
}

#' @title CBA Classifier
#' @description Learns a cba rule set from supplied dataframe.
#' @export
#' @param train a data frame with data.
#' @param classatt the name of the class attribute.
#' @param rulelearning_options custom options for the rule learning algorithm overriding the default values.
#' @param pruning_options custom options for the pruning algorithm overriding the default values.
#'
#' @return Object of class \link{RuleModel}.
#'
#' @examples
#'   utils::data(iris)
#'   cba(iris, "Species", rulelearning_options = list(target_rule_count = 50000))

cba <- function(train, classatt, rulelearning_options=NULL, pruning_options=NULL){

  discr <- discrNumeric(train, classatt)

  txns <- as(discr$Disc.data, "transactions")

  appearance <- getAppearance(train, classatt)

  start.time <- Sys.time()


  rules <- do.call("topRules", appendToList(list(txns = txns, appearance = appearance), rulelearning_options))

  end.time <- Sys.time()
  message (paste("Rule learning took:", time.taken <- end.time - start.time))

  start.time <- Sys.time()
  rules <- do.call("prune", appendToList(list(rules = rules,txns = txns,classitems = appearance$rhs), pruning_options))

  #rules <-prune(rules, txns,classitems,pruning_options)
  end.time <- Sys.time()
  message (paste("Pruning took:", time.taken <- end.time - start.time))

  #bundle cutpoints with rule set into one object
  rm <- RuleModel()
  rm@rules <- rules
  rm@cutp <- discr$cutp
  rm@classatt <- classatt

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
