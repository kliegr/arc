library(arules)


#' Default parameters for rule learning.
#'
#'@export
#'@return A list with default parameter valuees
#'
default_rule_learning_setup <- function()
{
return (list(target_rule_count=1000,init_support=0.00,init_conf=0.5,conf_step=0.05,supp_step=0.05,minlen=2,init_maxlen=3,iteration_timeout=2,total_timeout=100.0,max_iterations=30)  )
}

#' Default parameters for pruning
#'
#'@export
#'@return A list with default parameter valuees
default_pruning_setup <- function()
{
  return (list(default_rule_pruning=TRUE, rule_window=100))
}
#' Sample dataset
#'
#' A sample credit scoring dataset
#'
#'
#' @docType data
#' @keywords datasets
#' @name KO_Bank_all
#' @usage data(KO_Bank_all)
#' @format A comma separated csv file
NULL



#' RuleModel
#'
#' This class represents a rule-based classifier.
#'
#' \describe{
#'    \item{rules}{An object of class rules}
#'
#'    \item{cutp}{A list of cutpoints defining required discretization for the rule set to be applicable}
#'
#'    \item{classatt}{The name of the class attribute}
#'  }
#' @name RuleModel
#' @rdname RuleModel
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

#' method that matches rule model against test data
#'
#' @param rule_model a \link{RuleModel} class instance
#' @param test a data frame with test data, the data frame may optionally contain the target class attribute - if it does, the method also computes and <i>prints</i> accuracy.
#'
#' @return factor with predictions for input instances
#' @export
#'
#' @examples
#'   #data(iris)
#'   train<-iris[1:100,]
#'   test<-iris[101:length(iris),]
#'   #increase for more accurate results in longer time
#'   target_rule_count<-1000
#'   rm<-cba(train,"Species",list(target_rule_count=target_rule_count))
#'   ruleMatch(rm,test)
#'
#'
setGeneric("ruleMatch", function(rule_model,test) {
  standardGeneric("ruleMatch")
})

#' Title
#'
#' @param rule_model RuleModel.
#' @param test data.frame.
#'
#' @return A vector with predictions.
#' @export
#'
#' @seealso \link{cbaIris}
setMethod("ruleMatch", signature(rule_model = "RuleModel", test = "data.frame"), function(rule_model,test) {
  # apply any discretization that was applied on train data also on test data
  test_txns <- as(applyCuts(test,rule_model@cutp,infinite_bounds=TRUE,labels=TRUE), "transactions")
  # t is logical matrix with |rules| rows |test instances| columns
  # the unname function is not strictly necessary, but it may save memory for larger data:
  #  as the is.subset function returns concatenated attribute  values as the name for each column (test instance)
  t<-unname(is.subset(rule_model@rules@lhs,test_txns))
  # get row index of first rule matching each transaction
  matches<-apply(t, 2, function(x) min(which(x==TRUE)))
  # for each element in the matches vector (i.e. index of first matching rule)
  # get the index of the item on the right hand side of this rule which is true
  # and lookup the name of this item in iteminfo by this index
  result<-droplevels(unlist(lapply(matches, function(match) rule_model@rules@rhs@itemInfo[which(rule_model@rules@rhs[match]@data==TRUE),][1,3])))
  return(result)
})

rulemodelAccuracy<- function(prediction,groundtruth)
{
  both<-union(levels(groundtruth),levels(prediction))
  accuracy<-mean(factor(groundtruth,levels=both)==factor(prediction,levels=both))
  return(accuracy)
}


#' Method that generates items for values in given data frame column.
#'
#' @param df a data frame contain column \code{classatt}.
#' @param classatt name of the column in df to generate items for.
#'
#' @return a list of items.
#' @export
#'
#' @examples
#' #For input column \code{Species} with distinct values \code{versicolor, setosa},
#' #the function returns a list of two items  \code{Species=versicolor, Species=setosa}.
#' #data(iris)
#'   getItems(iris,"Species")
#'
#'
getItems <- function(df,classatt){
  classes<-as.character(unname(unique(unlist(df[classatt]))))
  classitems <-paste(classatt,"=",classes, sep="")
  return(classitems)
}


#' Example workflow that reads a data file from csv, learns a cba rule set and saves the resulting rule set back to csv.
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
#'  # cbaCSV("KO_Bank_all.csv")
#'
#'
cbaCSV<- function(path,outpath=NULL,classatt=NULL,idcolumn=NULL,rulelearning_options=NULL,pruning_options=NULL)
{
  train<-read.csv(path,header=TRUE, check.names=FALSE)
  if (!is.null(idcolumn))
  {
    train<-subset( train, select = -c (idcolumn) )
  }

  if (is.null(classatt))
  {
    classatt<-colnames(train)[ncol(train)]
  }
  rm<-cba(train,classatt,rulelearning_options,pruning_options)
  if (!is.null(outpath))
  {
    write.csv(as(rm@rules,"data.frame"), outpath, row.names=TRUE,quote = TRUE)
  }
  return(rm)

}
#' Test workflow on iris dataset, learns a cba classifier on one "train set" part , and applies it to the second  "test set" part.
#'
#' @return Accuracy.
#' @export
#'
#'
cbaIris<- function()
{
  #data(iris)
  classatt<-"Species"
  train<-iris[1:100,]
  test<-iris[101:length(iris),]
  # increase for more accurate results in longer time
  target_rule_count<-1000
  rm<-cba(test<-iris[101:length(iris),],classatt=classatt,rulelearning_options=list(target_rule_count=target_rule_count))
  prediction<-ruleMatch(rm,test)
  acc<-rulemodelAccuracy(prediction,test[[classatt]])
  return (acc)
}

#' example workflow that reads a data file from csv, learns a cba rule set and saves the resulting rule set back to csv.
#'
#' @export
#' @param train a data frame with data.
#' @param classatt the name of the class attribute.
#' @param rulelearning_options custom options for the rule learning algorithm overriding the default values.
#' @param pruning_options custom options for the pruning algorithm overriding the default values.
#'
#' @return Object of class \link{RuleModel}.
#'
#' @examples
#'   cba(iris,"Species",rulelearning_options=list(target_rule_count=50000))
#'   data(iris)

cba <- function(train,classatt,rulelearning_options=NULL,pruning_options=NULL){

  discr<-discrNumeric(train,classatt)

  txns<-as(discr$Disc.data,"transactions")

  classitems<-getItems(train,classatt)

  start.time <- Sys.time()

  rules<-topRules(txns,classitems,rulelearning_options)

  end.time <- Sys.time()
  print (paste("Rule learning took:",time.taken <- end.time - start.time))

  start.time <- Sys.time()
  rules <-prune(rules, txns,classitems,pruning_options)
  end.time <- Sys.time()
  print (paste("Pruning took:",time.taken <- end.time - start.time))

  #bundle cutpoints with rule set into one object
  rm <- RuleModel()
  rm@rules<-rules
  rm@cutp <-discr$cutp
  rm@classatt <-classatt

  return(rm)
}
