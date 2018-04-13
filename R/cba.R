#' @importFrom utils read.csv write.csv
library(arules)



#' CBARuleModel
#'
#' @description  This class represents a rule-based classifier.

#' @name CBARuleModel-class
#' @rdname CBARuleModel-class
#' @export CBARuleModel
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
#' @param data a data frame with data
#' @param discretize boolean indicating whether the passed data should be discretized
#' using information in the passed @cutp slot of the ruleModel argument.
#' @param ... other arguments (currently not used)
#' @return A vector with predictions.
#' @export
#' @method predict CBARuleModel
#' @examples
#'   allData <- datasets::iris[sample(nrow(datasets::iris)),]
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
predict.CBARuleModel <- function(object, data, discretize=TRUE,...) {
  # start.time <- Sys.time()
  # apply any discretization that was applied on train data also on test data

  if (discretize && length(object@cutp)>0)
  {
    data <- applyCuts(data, object@cutp, infinite_bounds=TRUE, labels=TRUE)
  }
  test_txns <- as(data, "transactions")
  # t is logical matrix with |rules| rows |test instances| columns
  # the unname function is not strictly necessary, but it may save memory for larger data:
  #  as the is.subset function returns concatenated attribute  values as the name for each column (test instance)
  t <- unname(is.subset(object@rules@lhs, test_txns))
  # get row index of first rule matching each transaction
  # the suppressWarnings is there because of "no non-missing arguments to min; returning Inf" produced by Min
  # the returned inf denotes that the instance is not classified, which is handled below
  matches <- suppressWarnings(apply(t, 2, function(x) min(which(x==TRUE))))


  # check if all instances are classified
  first_unclassified_instance <- match(Inf,matches)
  if (!is.na(first_unclassified_instance))
  {
    # the is.subset function does not mark default (with empty lhs) rule as applicable for all instances,
    # we need to do this manually.

    first_rules_with_empty_lhs <- min(which(apply(object@rules@lhs@data, 2, function(x) sum(x))==0))
    if (!is.na(first_rules_with_empty_lhs))
    {
      # the default rule will be used only for instances unclassified by any of the other rules
      matches[matches==Inf] <- first_rules_with_empty_lhs
    }
    else
    {
      stop(paste("There were unclassified instances, the first one has index: ", first_unclassified_instance, " and there is no default rule in the classifier"))
    }

  }
  # for each element in the matches vector (i.e. index of first matching rule)
  # get the index of the item on the right hand side of this rule which is true
  # and lookup the name of this item in iteminfo by this index

  result <- droplevels(unlist(lapply(matches, function(match) object@rules@rhs@itemInfo[which(object@rules@rhs[match]@data == TRUE),][1,3])))

  # end.time <- Sys.time()
  # message (paste("Prediction (CBA model application) took:", round(end.time - start.time, 2), " seconds"))
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
#' If not specified, the the   \link{topRules} function is called and defaults specified there are used\cr
#' \code{target_rule_count} (int) mining stops when the resulting rule set contains this number of rules; \cr
#' \code{trim} (boolean) if set to TRUE and more than \code{target_rule_count} is discovered, only first \code{target_rule_count} rules will be returned. \cr
#' \code{minsupp} (float)  minimum support threshold  \cr
#' \code{minconf} (float) minimum confidence threshold \cr
#' \code{minlen} (int) minimum length of rules, minlen=1 corresponds to rule with empty antecedent and one item in consequent. In general, rules with empty antecedent are not desirable for the subsequent pruning algorithm, therefore the value of this parameter should be set at least to value 2. \cr
#' \code{maxlen}  (int) maximum length of rules, should be equal or higher than minlen. A higher value may decrease the number of iterations to obtain target_rule_count rules, but it also increases the risk of initial combinatorial explosion and subsequent memory crash of the apriori rule learner. \cr
#' \code{maxtime} (int) maximum number of seconds it should take `apriori` to obtain rules. \cr
#' \code{find_conf_supp_thresholds} (boolean) whether to use automatic threshold detection or not. \cr
#' @param pruning_options custom options for the pruning algorithm overriding the default values. \cr
#'
#' @return Object of class \link{CBARuleModel}.
#'
#' @examples
#'  # Example using automatic threshold detection
#'   cba(datasets::iris, "Species", rulelearning_options = list(target_rule_count = 50000))
#'  # Example using manually set confidence and support thresholds
#'   rm <- cba(datasets::iris, "Species", rulelearning_options = list(minsupp=0.01,
#'    minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE,
#'    find_conf_supp_thresholds=FALSE))
#'   inspect(rm@rules)

cba <- function(train, classAtt, rulelearning_options=NULL, pruning_options=NULL){

  discr <- discrNumeric(train, classAtt)

  txns <- as(discr$Disc.data, "transactions")

  appearance <- getAppearance(train, classAtt)

  start.time <- Sys.time()
  if (is.null(rulelearning_options) || is.null(rulelearning_options$find_conf_supp_thresholds) || rulelearning_options$find_conf_supp_thresholds==TRUE)
  {
    message (paste("Using automatic threshold detection"))
    rules <- do.call("topRules", appendToList(list(txns = txns, appearance = appearance), rulelearning_options))
  }
  else
  {
    message (paste("Using manually set thresholds"))

    rules <- apriori(txns, parameter =
              list(confidence = rulelearning_options$minconf, support = rulelearning_options$minsupp, minlen = rulelearning_options$minlen, maxlen = rulelearning_options$maxlen,maxtime=rulelearning_options$maxtime),
            appearance = appearance, control = list(verbose=FALSE))
    if(rulelearning_options$trim & length(rules) > rulelearning_options$target_rule_count)
    {
      message("Removing excess discovered rules")
      rules <- rules[1:rulelearning_options$target_rule_count]
    }
  }


  end.time <- Sys.time()
  message (paste("Rule learning took:", round(end.time - start.time, 2), " seconds"))

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




#' @title CBA Classifier from provided rules
#' @description Learns a CBA rule set from supplied rules
#' @export
#' @param train_raw a data frame with raw data (numeric attributes are not discretized).
#' @param rules Rules class instance  output by the apriori package
#' @param txns Transactions class instance  passed to  the arules method invocation. Transactions are created over discretized data frame  - numeric values are replaced  with intervals such as "(13;45]".
#' @param rhs character vectors giving the labels of the items which can appear in the RHS
#' ($rhs element of the APappearance class instance passed to the arules call)
#' @param cutp list of cutpoints used to discretize data (required for application of the model on continuous data)
#' @param classAtt the name of the class attribute.
#' @param pruning_options custom options for the pruning algorithm overriding the default values.
#'
#' @return Object of class \link{CBARuleModel}.
#'
#' @examples
#'   data(humtemp)
#'   data_raw<-humtemp
#'   data_discr <- humtemp
#'
#'   #custom discretization
#'   data_discr[,1]<-cut(humtemp[,1],breaks=seq(from=15,to=45,by=5))
#'   data_discr[,2]<-cut(humtemp[,2],breaks=c(0,40,60,80,100))
#'
#'   #change interval syntax from (15,20] to (15;20], which is required by MARC
#'   data_discr[,1]<-as.factor(unlist(lapply(data_discr[,1], function(x) {gsub(",", ";", x)})))
#'   data_discr[,2]<-as.factor(unlist(lapply(data_discr[,2], function(x) {gsub(",", ";", x)})))
#'   data_discr[,3] <- as.factor(humtemp[,3])
#'
#'   #mine rules
#'   classAtt="Class"
#'   appearance <- getAppearance(data_discr, classAtt)
#'   txns_discr <- as(data_discr, "transactions")
#'   rules <- apriori(txns_discr, parameter =
#'    list(confidence = 0.5, support= 3/nrow(data_discr), minlen=1, maxlen=5), appearance=appearance)
#'   inspect(rules)
#'
#'
#'   rmCBA <- cba_manual(data_raw,  rules, txns_discr, appearance$rhs,
#'    classAtt, cutp= list(), pruning_options=NULL)
#'   inspect (rmCBA@rules)
#'   # prediction <- predict(rmCBA,data_discr,discretize=FALSE)
#'   # acc <- CBARuleModelAccuracy(prediction, data_discr[[classAtt]])
#'   # print(paste("Accuracy:",acc))

cba_manual <- function(train_raw,  rules, txns, rhs, classAtt, cutp, pruning_options=list(input_list_sorted_by_length=FALSE)){
  start.time <- Sys.time()
  rules <- do.call("prune", appendToList(list(rules = rules,txns = txns,classitems = rhs), pruning_options))

  #rules <-prune(rules, txns,classitems,pruning_options)
  end.time <- Sys.time()
  message (paste("Pruning took:", round(end.time - start.time,2), " seconds"))

  #bundle cutpoints with rule set into one object
  rm <- CBARuleModel()
  rm@rules <- rules
  rm@cutp <- cutp
  rm@classAtt <- classAtt
  rm@attTypes <- sapply(train_raw, class)
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
