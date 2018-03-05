#' @import Matrix methods arules
#'
library(arules)
library(R.utils)

#' @title Classifier Builder
#' @description  An implementation of the CBA-CB M1 algorithm (Liu et al, 1998) adapted for R and arules package apriori implementation in place of CBA-RG.
#' @references Ma, Bing Liu Wynne Hsu Yiming. Integrating classification and association rule mining. Proceedings of the fourth international conference on knowledge discovery and data mining. 1998.
#' @param rules object of class rules from arules package
#' @param txns input object with transactions.
#' @param classitems a list of items to appear in the consequent (rhs) of the rules.
#' @param default_rule_pruning boolean indicating whether default pruning should be performed. If set to TRUE, default pruning is performed as in the CBA algorithm.
#'   If set to FALSE, default pruning is not performed i.e. all rules surviving data coverage pruning are kept. In either case, a default rule is added to the end of the classifier.
#' @param rule_window the number of rules to precompute for CBA data coverage pruning. The default value can be adjusted to decrease runtime.
#' @param debug output debug messages.
#' @param greedy_pruning setting to TRUE activates early stopping condition: pruning will be stopped on first rule on which total error increases.
#'
#' @return Returns an object of class \link{rules}.
#' @export
#' @seealso \code{\link{topRules}}
#' @examples
#'  #Example 1
#'   txns <- as(discrNumeric(datasets::iris, "Species")$Disc.data,"transactions")
#'   appearance <- getAppearance(datasets::iris,"Species")
#'   rules <- apriori(txns, parameter = list(confidence = 0.5,
#'   support= 0.01, minlen= 2, maxlen= 4),appearance = appearance)
#'   prune(rules,txns, appearance$rhs)
#'   inspect(rules)
#'
#' #Example 2
#'  utils::data(Adult) # this dataset comes with the arules package
#'  classitems <- c("income=small","income=large")
#'  rules <- apriori(Adult, parameter = list(supp = 0.3, conf = 0.5,
#'  target = "rules"), appearance=list(rhs=classitems, default="lhs"))
#'  # produces 1266 rules
#'  rules <- prune(rules,Adult,classitems)
#'  # Rules after data coverage pruning: 198
#'  # Performing default rule pruning.
#'  # Final rule list size:  174


prune <- function  (rules, txns, classitems,default_rule_pruning=TRUE, rule_window=50000,greedy_pruning=FALSE,debug=FALSE){
  if (!default_rule_pruning & greedy_pruning)
  {
    stop("When greedy_pruning is enabled, default_rule_pruning must be enabled too")
  }
  if (length(rules)==0)
  {
    stop("Cannot prune empty rule list")
  }
  # compute rule length
  tryCatch(
    {
      rules@quality$lhs_length <- colSums(rules@lhs@data)
    }, error= function(err)
    {
      warning(err)
      warning("Is there at least one rule with non empty antecedent?")
      return(rules)
    })
  # sort rules
  rules <- sort(rules,by = c("confidence","support","lhs_length"))


  # obtain item ids in dataframe for class items
  classitemspositions <- vector(length = length(classitems))

  for (i in 1:length(classitems))
  {
    classitemspositions[i] <- which(txns@itemInfo$labels==classitems[i])
  }
  # compute class frequencies
  # this is neeeded to determine support of default rule during default rule pruning
  alldata_classfrequencies <- rowSums(txns@data[classitemspositions,])
  #set default class based on all transactions
  #this default will be used only in the rare situation when the first rule matches all transactions,
  #these will be removed, and thus no transactions will be left to compute default class
  default_class <- which.max(alldata_classfrequencies)
  orig_transaction_count <- length(txns)
  distinct_items <- ncol(txns)

  rules_to_remove <- c()

  rule_count <- length(rules)
  RULEWINDOW <- rule_window
  if (RULEWINDOW > rule_count)
  {
    RULEWINDOW <- rule_count
  }

  total_errors <- c(0,rule_count)
  default_classes <- c(0,rule_count)
  last_total_error_without_default <- 0
  last_total_error_with_default <- Inf


  for(r in 1:rule_count)
  {
    # the purpose of using rule windows is following:
    # bulk operations on multiple rules at a time may be cheaper than many iterative multiplications of single rule vectors with all transactions
    # however, when the number of input rules is large, this may result in many unneccesary operations
    # the RULEWINDOW parameter controls how many rules are processed at a time
    # there are two possible savings:
    ## in some cases, all transactions can be covered by first N rules, remaining rules thus may not need to be processed at all
    ## as the pruning proceeds through the rule list, the number of remaining transactions decreases, which makes subsequent multiplications cheaper

    # RT holds data precomputed for current window size
    if ((r-1)%%RULEWINDOW==0)
    {
      if (RULEWINDOW==1)
      {
        ws <- r
        we <- r
      }
      else
      {
        ws <- ((r %/% RULEWINDOW)*RULEWINDOW)+1
        we <- ws + RULEWINDOW-1
        if (we > rule_count)
        {
          we <- rule_count
        }
      }
      # the result of matrix mutliplication is a matrix for which element M[r,t] corresponds to how many items in rule r are matched by an item in transaction t
      # the conversion from ngCMatrix to dgCMatrix since the definition of matrix multiplication operations on ngCMatrix would mean different result than outlined above
      RT.lhs <- t(as(rules@lhs@data[,ws:we,drop = FALSE],"dgCMatrix")) %*% as(txns@data,"dgCMatrix")

      # this operation would also work without the subsetting by classitemspositions
      # now do the same thing for rhs
      # Performance tip: ngCMatrix can be cast to dgCMatrix just once, outside the loop for the entire matrix
      RT.rhs <- t(as(rules@rhs@data[classitemspositions,ws:we,drop = FALSE],
        "dgCMatrix")) %*% as(txns@data[classitemspositions,,drop = FALSE],"dgCMatrix")

      # thus the rule r's lhs matches the transaction t iff M[r,t] == number of items in t is the same as number of items in lhs of  rule r,
      RT.matches_lhs <- (rules[ws:we,drop = FALSE]@quality$lhs_length==RT.lhs)
      # the rule r correctly covers the transaction t if the number of items in t matched by the LHS and RHS equals the length of the LHS plus 1 (RHS must always have length 1)
      RT.matches_lhs_and_rhs <- (rules[ws:we,drop = FALSE]@quality$lhs_length+1==RT.lhs+RT.rhs)
      # performance tip: now the RT.lhs and RT.rhs can be freed
    }
    # the index of currently processed rule r is recomputed to relate to current window
    sr <- r - ws +1
    if (debug) message(paste("processing rule ",r, " sr = ", sr))

    # cur_rule holds data for current rule r
    cur_rule.matches_lhs <- RT.matches_lhs[sr,]
    if (TRUE %in% cur_rule.matches_lhs)
    {
      if (debug) message(paste("Antecedent of rule ",r , " matches at least 1 transaction"))
      # check if rule completely matches  at least one transaction
      R.matches_lhs_and_rhs <- RT.matches_lhs_and_rhs[sr,]
      if (TRUE %in% R.matches_lhs_and_rhs)
      {
        if (debug) message(paste("Rule ",r, " correctly covers at least 1 transaction"))
        # remove transactions matching lhs
        RT.matches_lhs <- RT.matches_lhs[,!cur_rule.matches_lhs,drop=FALSE]
        RT.matches_lhs_and_rhs <- RT.matches_lhs_and_rhs[,!cur_rule.matches_lhs,drop=FALSE]
        # remove covered instances from data to allow default rule computation

        # drop=FALSE ensures that when txns is subset so that it contains only one transaction the result is not converted to a vector, which would result into an error during assignment
        txns@data <- txns@data[,!cur_rule.matches_lhs,drop = FALSE]
        # total errors are computed and set only for rules which will not be pruned
        total_errors[r] <- last_total_error_without_default+sum(cur_rule.matches_lhs-R.matches_lhs_and_rhs)
        # last_total_error should exclude default rule error, which is added to total_errors[r] later
        last_total_error_without_default <- total_errors[r]

        #check if there are any more transactions to process
        if (ncol(txns@data)==0)
        {
          if (debug) message(paste("rule ",r, " No transactions to process"))
          rules_to_remove <- c(rules_to_remove,r+1:rule_count)
          # there are no transactions left from which to compute the default class for this rule
          # use default class from the last iteration
          default_classes[r] <- default_class
          break
        }
        else{
          #compute default rule error only if there any transactions left
          classfrequencies <- rowSums(txns@data[classitemspositions,,drop=FALSE])
          default_class <- which.max(classfrequencies)
          majority_class_tran_count <- classfrequencies[default_class]
          default_err_count <- length(txns) - majority_class_tran_count
          total_errors[r] <- total_errors[r] + default_err_count
          default_classes[r] <- default_class

          if (greedy_pruning)
          {
            if (last_total_error_with_default<total_errors[r])
            {
              if (debug) message(paste("Total error including default increase on rule  ",r))
              rules_to_remove <- c(rules_to_remove,r+1:rule_count)
              break;
            }
            last_total_error_with_default <- total_errors[r]
          }
        }
      }
      else
      {
        if (debug) message(paste("Rule ",r, " does not match at least 1 transaction correctly"))
        rules_to_remove <- c(rules_to_remove,r)
      }
    }
    else{
      if (debug) message(paste("Antecedent of rule ",r, " does not match at least 1 transaction"))
      rules_to_remove <- c(rules_to_remove,r)
    }
  }

  message(paste("Original rules: ",rule_count))
  if (length(rules_to_remove)>0)
  {
    rules <- rules[-rules_to_remove]
    total_errors <- total_errors[-rules_to_remove]
    default_classes <- default_classes[-rules_to_remove]
  }

  message(paste("Rules after data coverage pruning:",length(rules)))
  # perform default rule pruning
  if (default_rule_pruning)
  {
    message("Performing default rule pruning.")
    # find "last rule" the first rule with the smallest number of errors
    if (debug)
    {
      message("total errors by rule")
      print (total_errors)
    }

    last_rule_pos <- which.min(total_errors)
    # keep only the top rules until "last rule"(inclusive)
    rules <- rules[1:last_rule_pos]
  }
  else {
    last_rule_pos <- length(rules)
  }
  # add default rule to the end
  ## the lhs of the default rule has no items (all item positions to 0 in the item matrix)
  rules@lhs@data <- cbind(rules@lhs@data, as(matrix(0, distinct_items,1),"ngCMatrix"))

  # now prepare the rhs of the default rule
  # first prepare empty vector
  default_rhs <- as(matrix(0, distinct_items,1),"ngCMatrix")
  # the class associated with the default rule is the class that has been precomputed as part of evaluation of the "last rule"
  default_rhs[classitemspositions[default_classes[last_rule_pos]]] <- TRUE
  # finally append the default rule rhs to the rules rhs  matrix
  rules@rhs@data <- cbind(rules@rhs@data, default_rhs)
  # compute the support and confidence (will be the same) of the default rule
  default_rule_support_confidence <- alldata_classfrequencies[default_classes[last_rule_pos]]/orig_transaction_count
  # append the result
  rules@quality <- rbind(rules@quality,c(default_rule_support_confidence,default_rule_support_confidence,1,0))
  # set row name on the newly added default rule to "0"
  # so that it does not clash with any of the row names of the original rule list
  row.names(rules@quality)[nrow(rules@quality)] <- 0
  message(paste("Final rule list size: ",length(rules)))
  return(rules)
}


