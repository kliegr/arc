library(arules)
#if(exists("prune", mode = "function"))
#  source("m1prune.R")

#if(exists("nclassrules", mode = "function"))
#  source("cbam1_auto.R")


#train <- read.csv("/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train/heart-h6.csv",header=TRUE, check.names=FALSE, sep = ",") # load csv +
#path<-"/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train/heart-h6.csv"
#classitems=c("class=+", "class=-")


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
  
  return (mdlp2(train,skip_nonnumeric=TRUE,labels=TRUE,handle_missing=TRUE,class=classatt_col,infinite_bounds=TRUE)$Disc.data)
  
}

getclassitems <- function(train,classatt){
  classes<-as.character(unname(unique(unlist(train[classatt]))))
  classitems <-paste(classatt,"=",classes, sep="")
  return(classitems)
}

#learnprune_csv("/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train/car5.csv",,,,"/home/big/car5.csv")
#learnprune_csv("/home/tomas/Dropbox/Projekty/MARC/experiments/dev/train",,,,"/home/big/heart-h6.csv.arules")
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
  train<-discr_if_needed(train,classatt)
  
  txns<-as(train,"transactions")
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
  return(rules)
}