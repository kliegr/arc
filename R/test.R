library(arules)




RuleModel <- setClass("RuleModel",
  slots = c(
    rules = "rules",
    cutp = "list",
    classatt ="character"
  )
)

setGeneric("rulematch", function(rulemodel,test) {
  standardGeneric("rulematch")
})

setMethod("rulematch", signature(rulemodel = "RuleModel", test = "data.frame"), function(rulemodel,test) {

  gtruth<-NULL
  #check if test data contain also class attribute
  if (rulemodel@classatt %in% colnames(test))
  {
    #if so remove it separate it from the dest dataset into gtruth 
    gtruth<-test[[rulemodel@classatt]]
    test<-within(test, rm(list=(rulemodel@classatt)))
  }
  # apply any discretization that was applied on the train data also on test data
  test_discr<-apply_cuts(rulemodel@cutp,test)
  test_txns <- as(test_discr, "transactions")
  
  # t is logical matrix with |rules| rows |test instances| columns
  # the unname function is not strictly necessary, but it may save memory for larger data:
  #  as the is.subset function returns concatenated attribute  values as the name for each column (test instance)
  t<-unname(is.subset(rulemodel@rules@lhs,test_txns))
  # get row index of first rule matching each transaction
  matches<-apply(t, 2, function(x) min(which(x==TRUE)))
  # for each element in the matches vector (i.e. index of first matching rule) 
  # get the index of the item on the right hand side of this rule which is true
  # and lookup the name of this item in iteminfo by this index
  result<-droplevels(unlist(lapply(matches, function(match) rulemodel@rules@rhs@itemInfo[which(rulemodel@rules@rhs[match]@data==TRUE),][1,3])))
  if (!is.null(gtruth))
  {
    #test and train data may not have the same set of distinct class values, which would result into an error caused by comparing factors with different levels
    both<-union(levels(gtruth),levels(result))
    accuracy<-mean(factor(gtruth,levels=both)==factor(result,levels=both))
    
    #accuracy<-mean(droplevels(droplevels(gtruth))==droplevels(droplevels(result)))
    print(paste("Accuracy:",accuracy))
  }
  return(result)
})

  
discr_if_needed <- function(train,classatt)
{
  discretize_class<-FALSE
  classatt_col<-which( colnames(train)==classatt )
  if(!is.factor(train[[classatt_col]]))
  {
    if (length(unique(train[[classatt_col]]))>5 & discretize_class)
    {
      train[[classatt_col]] <- discretize(train[[classatt]],  "frequency", categories=3)
    }
    else
    {
      train[[classatt_col]] <- factor(train[[classatt_col]])
    }
  }
  discr<-mdlp2(train,skip_nonnumeric=TRUE,labels=TRUE,handle_missing=TRUE,class=classatt_col,infinite_bounds=TRUE)
  
  return (discr)
  
}

getclassitems <- function(train,classatt){
  classes<-as.character(unname(unique(unlist(train[classatt]))))
  classitems <-paste(classatt,"=",classes, sep="")
  return(classitems)
}

#learnprune_csv("/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train/car5.csv",,,,"/home/big/car5.csv")
#learnprune_csv("/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train",,,,"/home/big/heart-h6.csv.arules")
#path="/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train/credit-a6.csv"
learnprune_csv<- function(path,classatt=NULL,idcolumn=NULL,target_rule_count=50000,pruning_type="CBA",outpath=NULL)
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
  rules<-example_learnprune(train,classatt,target_rule_count,pruning_type)
  if (!is.null(outpath))
  {
    write.csv(as(rules,"data.frame"), outpath, row.names=TRUE,quote = TRUE)
  }
  
}
learnprune_iris<- function()
{
  data(iris)
  train<-iris[1:100,]
  test<-iris[101:length(iris),]
  # increase for more accurate results in longer time
  target_rule_count<-1000
  rm<-example_learnprune(test<-iris[101:length(iris),],target_rule_count=1000,classatt="Species")
  rulematch(rm,test)
}

example_learnprune <- function(train,classatt,target_rule_count,pruning_type="CBA"){
  discr<-discr_if_needed(train,classatt)
  
  txns<-as(discr$Disc.data,"transactions")
  classitems<-getclassitems(train,classatt)
  
  start.time <- Sys.time()
  if (TRUE)
  {
    rules<-firstkrules(txns,classitems,target_rule_count=target_rule_count,total_timeout=100)
  }
  else
  {
    rules <- apriori(txns, parameter = list(confidence = 0.01, support= 0.01, minlen= 1, maxlen= 4),appearance = list(rhs = classitems,default="lhs"))
  }
  end.time <- Sys.time()
  print (time.taken <- end.time - start.time)
  
  start.time <- Sys.time()
  default_rule_pruning<-FALSE
  if (pruning_type=="CBA_NO_DEFAULT")
  {
    default_rule_pruning<-FALSE
  }
  else
  {
    default_rule_pruning<-TRUE
  }
  rules <-prunerules(rules, txns,classitems,100,default_rule_pruning)
  end.time <- Sys.time()
  print (time.taken <- end.time - start.time)
  
  #bundle cutpoints with rule set into one object
  rm <- RuleModel()
  rm@rules<-rules
  rm@cutp <-discr$cutp
  rm@classatt <-classatt
  return(rm)
}