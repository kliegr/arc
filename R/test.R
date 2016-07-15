library(arules)




RuleModel <- setClass("RuleModel",
  slots = c(
    rules = "rules",
    cutp = "list"
  )
)

setGeneric("rulematch", function(rm,test) {
  standardGeneric("rulematch")
})

setMethod("rulematch", signature(rm = "RuleModel", test = "data.frame"), function(rm,test) {
  # implement a method taking on input also data frame with test data
  # apply rm@cutp on test data using "cut"
  # then create a one dimensional matrix, considering only the subset of items in rules@lhs@itemInfo (! which contains also class items ! )
  # multiply the one dimensional vector with each rule i, check if length matches rules[i]@quality$length
  ## multiply 
  ## 
  #get logical matrix showing which rules (rows) match which transactions (columns)
  
  #apply rm@cutp on train
  test<-apply_cuts(rm@cutp,test)
  txns <- as(test, "transactions")
  t<-is.subset(rules@lhs,txns)
  #get row index of first rule matching each transaction
  matches<-apply(t, 2, function(x) min(which(t[,x]==TRUE)))
  # for each elemenent in the matches vector (i.e. index of first matching rule) 
  # get the index of the item on the right hand side of this rule which is true
  # and lookup the name of this item in iteminfo by this index
  result<-lapply(matches, function(match) rules@rhs@itemInfo[which(rules@rhs[match]@data==TRUE),1] )
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
learnprune_iris<- function(auto=TRUE)
{
  data(iris)
  return(example_learnprune(iris,"Species",1000,"CBA"))
  
}

example_learnprune <- function(train,classatt,target_rule_count,pruning_type){
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
  return(rm)
}