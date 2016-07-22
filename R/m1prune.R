library(arules)
#' @title Default rule pruning parameters
#' @import Matrix methods arules

#' @name DEFAULT_PRUNING_SETUP
setOption("DEFAULT_PRUNING_SETUP",list(default_rule_pruning=TRUE, rule_window=100))

#' An implementation of the CBA-CB M1 algorithm adapted for R and arules package apriori implementation in place of CBA-RG.
#'
#' @param data input dataframe or transactions.
#' @param classitems a list of items to appear in the consequent (rhs) of the rules.
#' @param custom_alg_options a list of custom options, any options specified will override those defined by the \link{DEFAULT_RULE_LEARNING_SETUP} variable. These options are
#'  \itemize{
#'   \item \strong{default_rule_pruning}: boolean indicating whether default pruning should be performed. If set to TRUE, default pruning is performed as in the CBA algorithm.
#'   If set to FALSE, default pruning is not performed i.e. all rules surviving data coverage pruning are kept. In either case, a default rule is added to the end of the classifier.
#'   \item \strong{rule_window}: the number of rules to precompute for CBA data coverage pruning. The default value can be adjusted to decrease runtime.
#'   }
#' @param debug output debug messages.
#'
#' @return Returns an object of class \link{rules}.
#' @export
#'
#' @examples
#' \code{
#'   data(iris)
#'   txns <- as(discrNumeric(iris, "Species")$Disc.data,"transactions")
#'   classitems <- getItems(iris,"Species")
#'   rules <- apriori(txns, parameter = list(confidence = 0.5, support= 0.01, minlen= 2, maxlen= 4),appearance = list(rhs = classitems,default="lhs"))
#'   prune(rules,txns, classitems)
#'   inspect(rules)
#' }
#' @seealso \code{\link{topRules}}
#'
prune <- function  (rules, txns, classitems,custom_alg_options=NULL,debug=FALSE){
  alg_options<-replace(getOption("DEFAULT_PRUNING_SETUP"),names(custom_alg_options),custom_alg_options)

  # compute rule length
  tryCatch(
    {
      rules@quality$lhs_length<-colSums(rules@lhs@data)
    }, error= function(err)
    {
      print(err)
      print("Is there at least one rule with non empty antecedent?")
      return(rules)
    })
  # sort rules
  rules<-sort(rules,by = c("confidence","support","lhs_length"))

  # obtain item ids in dataframe for class items
  classitemspositions<-vector(length = length(classitems))

  for (i in 1:length(classitems))
  {
    classitemspositions[i]<-which(txns@itemInfo$labels==classitems[i])
  }
  # compute class frequencies
  # this is neeeded to determine support of default rule during default rule pruning
  alldata_classfrequencies<-rowSums(txns@data[classitemspositions,])
  orig_transaction_count<-length(txns)
  distinct_items<-ncol(txns)



  rules_to_remove=c()

  rule_count<-length(rules)
  RULEWINDOW<-alg_options$rule_window
  if (RULEWINDOW>rule_count)
  {
    RULEWINDOW<-rule_count
  }

  total_errors <- c(0,length(rules))
  default_classes <- c(0,length(rules))
  last_total_error <- 0


  for(r in 1:length(rules))
  {
    # the purpose of using rule windows is following:
    # bulk operations on multiple rules at a time may be cheaper than many iterative multiplications of single rule vectors with all transactions
    # however, when the number of input rules is large, this may result in many unneccesary operations
    # the RULEWINDOW parameter controls how many rules are processed at a time
    # there are two possible savings:
    ## in some cases, all transactions can be covered by first N rules, remaining rules thus may not need to be processed at all
    ## as the pruning proceeds through the rule set, the number of remaining transactions decreases, which makes subsequent multiplications cheaper


    # RT holds data precomputed for current window size
    if ((r-1)%%RULEWINDOW==0)
    {
      ws<- (r %/% RULEWINDOW)*RULEWINDOW +1
      we<- ws + RULEWINDOW-1
      # the result of matrix mutliplication is a matrix for which elemenent M[r,t] corresponds to how many items in rule r are matched by an item in transaction t
      # the conversion from ngCMatrix to dgCMatrix since the definition of matrix multiplication operations on ngCMatrix would mean different result than outlined above

      RT.lhs<-t(as(rules@lhs@data[,ws:we,drop = FALSE],"dgCMatrix")) %*% as(txns@data,"dgCMatrix")
      # this operation would also work without the subsetting by classitemspositions
      # now do the same thing for rhs
      # Performance tip: ngCMatrix can be cast to dgCMatrix just once, outside the loop for the entire matrix
      RT.rhs<-t(as(rules@rhs@data[classitemspositions,ws:we,drop = FALSE],"dgCMatrix")) %*% as(txns@data[classitemspositions,,drop = FALSE],"dgCMatrix")

      # thus the rule r's lhs matches the transaction t iff M[r,t] == number of items in t is the same as number of items in lhs of  rule r,
      RT.matches_lhs<-(rules[ws:we]@quality$lhs_length==RT.lhs)
      # the rule r correctly covers the transaction t if the number of items in t matched by the LHS and RHS equals the length of the LHS plus 1 (RHS must always have length 1)
      RT.matches_lhs_and_rhs<-(rules[ws:we]@quality$lhs_length+1==RT.lhs+RT.rhs)
      # performance tip: now the RT.lhs and RT.rhs can be freed
    }
    # the index of currently processed rule r is recomputed to relate to current window
    sr<- r-ws +1
    if (debug) print(paste("processing rule ",r, " sr = ", sr))

    # cur_rule holds data for current rule r
    cur_rule.matches_lhs<-RT.matches_lhs[sr,]
    if (TRUE %in% cur_rule.matches_lhs)
    {
      if (debug) print(paste("Antecedent of rule ",r, " matches at least 1 transaction"))
      # check if rule completely matches  at least one transaction
      R.matches_lhs_and_rhs<-RT.matches_lhs_and_rhs[sr,]
      if (TRUE %in% R.matches_lhs_and_rhs)
      {
        if (debug) print(paste("Rule ",r, " correctly covers at least 1 transaction"))
        # remove transactions matching lhs
        RT.matches_lhs<-RT.matches_lhs[,!cur_rule.matches_lhs,drop=FALSE]
        RT.matches_lhs_and_rhs<-RT.matches_lhs_and_rhs[,!cur_rule.matches_lhs,drop=FALSE]
        # remove covered instances from data to allow default rule computation

        # drop=FALSE ensures that when txns is subset so that it contains only one transaction the result is not converted to a vector, which would result into an error during assignment
        txns@data<-txns@data[,!cur_rule.matches_lhs,drop = FALSE]
        # total errors are computed and set only for rules which will not be pruned
        total_errors[r]<-last_total_error+sum(cur_rule.matches_lhs-R.matches_lhs_and_rhs)
        # last_total_error should exclude default rule error, which is added to total_errors[r] later
        last_total_error<-total_errors[r]

        #check if there are any more transactions to process
        if (ncol(txns@data)==0)
        {
          if (debug) print(paste("rule ",r, " No transactions to process"))
          rules_to_remove <- c(rules_to_remove,r+1:rule_count)
          # there are no transactions left from which to compute the default class for this rule
          # use default class from the last iteration
          default_classes[r]<-default_class
          break
        }
        else{
          #compute default rule error only if there any transactions left
          classfrequencies<-rowSums(txns@data[classitemspositions,,drop=FALSE])
          default_class<-which.max(classfrequencies)
          majority_class_tran_count<-classfrequencies[default_class]
          default_err_count<- length(txns) - majority_class_tran_count
          total_errors[r]<-total_errors[r]+default_err_count
          default_classes[r]<-default_class
        }
      }
      else
      {
        if (debug) print(paste("Rule ",r, " does not match at least 1 transaction correctly"))
        rules_to_remove <- c(rules_to_remove,r)
      }
    }
    else{
      if (debug) print(paste("Antecedent of rule ",r, " does not match at least 1 transaction"))
      rules_to_remove <- c(rules_to_remove,r)
    }

  }
  print(paste("Original rules: ",rule_count))
  rules <- rules[-rules_to_remove]
  print(paste("Rules after data coverage pruning:",length(rules)))
  total_errors <- total_errors[-rules_to_remove]
  default_classes <- default_classes[-rules_to_remove]
  # perform default rule pruning
  if (alg_options$default_rule_pruning)
  {
    print("Performing default rule pruning.")
    # find "last rule" the first rule with the smallest number of errors
    last_rule_pos <- which.min(total_errors)
    # keep only the top rules until "last rule"(inclusive)
    rules <- rules[1:last_rule_pos]

  }
  else{
    # in case there is no default rule pruning, the "last rule" is actually the last rule among those surviving data coverage pruning
    last_rule_pos<-length(rules)
  }
  # add default rule to the end
  ## the lhs of the default rule hasno items (all item positions to 0 in the item matrix)
  rules@lhs@data<-cbind(rules@lhs@data, as(matrix(0, distinct_items,1),"ngCMatrix"))

  # now prepare the rhs of the default rule
  # first prepare empty vector
  default_rhs<-as(matrix(0, distinct_items,1),"ngCMatrix")
  # the class associated with the default rule is the class that has been precomputed as part of evaluation of the "last rule"
  default_rhs[classitemspositions[default_classes[last_rule_pos]]]<-TRUE
  # finally append the default rule rhs to the rules rhs  matrix
  rules@rhs@data<-cbind(rules@rhs@data, default_rhs)
  # compute the support and confidence (will be the same) of the default rule
  default_rule_support_confidence<-alldata_classfrequencies[default_classes[last_rule_pos]]/orig_transaction_count
  # append the result
  rules@quality<-rbind(rules@quality,c(default_rule_support_confidence,default_rule_support_confidence,1,0))
  # set row name on the newly added default rule to "0"
  #  so that it does not clash with any of the row names of the original rule set
  row.names(rules@quality)[nrow(rules@quality)]<-0

  print(paste("Final rule set size: ",length(rules)))
  return(rules)
}


