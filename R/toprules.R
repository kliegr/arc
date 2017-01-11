#' @importFrom R.utils evalWithTimeout
#' @import arules
library(R.utils)



#' @title Rule Generation
#' @description  A wrapper for the arules method from the apriori package that iteratively changes mining parameters until a desired number of rules is obtained, all options are exhausted or a preset time limit is reached.
#' Within the arc package, this function serves as a replacement for the CBA Rule Generation algorithm (Liu et al, 1998) -- without pessimistic pruning -- with general apriori implementation provided by existing fast R package \strong{arules}.
#' @references Ma, Bing Liu Wynne Hsu Yiming. Integrating classification and association rule mining. Proceedings of the fourth international conference on knowledge discovery and data mining. 1998.

#' @param txns input transactions.
#' @param appearance object named list or APappearance object (refer to arules package)
#' @param target_rule_count the main stopping criterion, mining stops when the resulting rule set contains this number of rules.
#' @param init_support initial support.
#' @param init_conf initial confidence.
#' @param conf_step confidence will be changed by steps defined by this parameter.
#' @param supp_step support will be changed by steps defined by this parameter.
#' @param minlen minimum length of rules, minlen=1 corresponds to rule with empty antecedent and one item in consequent. In general, rules with empty antecedent are not desirable for the subsequent pruning algorithm, therefore the value of this parameter should be set at least to value 2.
#' @param init_maxlen maximum length of rules, should be equal or higher than minlen. A higher value may decrease the number of iterations to obtain target_rule_count rules, but it also increases the risk of initial combinatorial explosion and subsequent memory crash of the apriori rule learner.
#' @param iteration_timeout maximum number of seconds it should take apriori to obtain rules with current configuration/
#' @param total_timeout maximum number of seconds the mining should take.
#' @param max_iterations maximum number of iterations.
#' @param trim if set to TRUE and more than \code{target_rule_count} is discovered, only first \code{target_rule_count} rules will be returned.
#' @param debug boolean indicating whether to output debug messages.
#'
#' @return Returns an object of class rules.
#' @export
#' @seealso \code{\link{prune}}
#' @examples
#' # Example 1
#'  utils::data(Adult)
#'  rules <- topRules(Adult, appearance = list(), target_rule_count = 100,
#'   init_support = 0.5,init_conf = 0.9, minlen = 1, init_maxlen = 10)
#'
#' # Example 2
#'   rules <- topRules(as(discrNumeric(datasets::iris, "Species")$Disc.data,"transactions"),
#'   getAppearance(datasets::iris,"Species"))
#'
#' # Example 3
#'   utils::data(datasets::iris)
#'   appearance <- list(rhs =  c("Species=setosa", "Species=versicolor",
#'    "Species=virginica"), default="lhs")
#'   data <- sapply(datasets::iris,as.factor)
#'   data <- data.frame(data, check.names=FALSE)
#'   txns <- as(data,"transactions")
#'   rules <- topRules(txns, appearance)
#'

topRules <- function(txns, appearance=list(), target_rule_count = 1000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05,
                     supp_step = 0.05, minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0,
                     max_iterations = 30, trim=TRUE, debug = FALSE)
{
  starttime <- proc.time()
  if (missing(txns))
  {
    stop("txns cannot be null")
  }


  MAX_RULE_LEN <- length(unique(txns@itemInfo$variables))
  support <- init_support
  conf <- init_conf
  #maxlen=1 corresponds to rule with empty antecedent and one item in consequent
  maxlen <- init_maxlen


  iteration_time_limit_exceeded <- 0
  flag <- TRUE
  lastrulecount <- -1
  maxlendecreased_dueTIMEOUT <- FALSE
  iterations <- 0
  while(flag)
  {
    iterations <- iterations+1
    if (iterations == max_iterations) {
      message(paste("Max iterations reached"))
      break
    }
    new_values<-tryCatch(
      {
        #invisible(capture.output captures remaining progress output
        message(paste("Running apriori with SETTING: confidence = ", conf,", support = ", support, ", minlen = ", minlen, ", maxlen = ", maxlen, ", MAX_RULE_LEN = ",  MAX_RULE_LEN))
        rules <- evalWithTimeout({rules <- suppressWarnings(apriori(txns, parameter =
                  list(confidence = conf, support = support, minlen = minlen, maxlen = maxlen),
                  appearance = appearance, control = list(verbose=FALSE)));},
                 timeout = iteration_timeout, onTimeout="error");
                rulecount <- length(rules)
        message(paste("Rule count: ",rulecount, " Iteration: ",iterations))
        if (rulecount >= target_rule_count)
        {
          flag <- FALSE
          message(paste("Target rule count satisfied:  ", target_rule_count))
        }
        else{
          exectime = proc.time() - starttime
          if(debug)
          {
            message(maxlen < MAX_RULE_LEN)
            message(lastrulecount != rulecount)
            message(!maxlendecreased_dueTIMEOUT)
          }
          if (exectime[3] > total_timeout)
          {
            message(paste("Max execution time exceeded:  ", total_timeout))
            flag <- FALSE
          }
          # increase max len if maximum maxlen has not yet been achieved and the number of rules increased during the last optimization step
          # if the number of rules did not increase further increase of maxlen will not bring more rules (?) or at least it is unlikely to
          else if (maxlen < MAX_RULE_LEN & lastrulecount != rulecount & !maxlendecreased_dueTIMEOUT)
          {
            maxlen <- maxlen+1
            lastrulecount  <- rulecount
            message(paste("Increasing maxlen to:  ", maxlen))
          }
          # if maxlen has been previously decreased, it means it resulted in combinatorial explosion
          # this can be hopefully prevented if it is increased with simultaneous increase of minsup
          # this can be performed only if there is space for increasing support
          else if (maxlen < MAX_RULE_LEN & maxlendecreased_dueTIMEOUT & support <= (1-supp_step))
          {
            support <- support + supp_step
            maxlen <- maxlen + 1
            lastrulecount  <- rulecount
            message(paste("Increasing maxlen to:  ", maxlen))
            #TODO check if this is a good design option
            maxlendecreased_dueTIMEOUT <- FALSE
          }
          # try decreasing confidence if other options in the previous iteration did not increase the number of rules
          else if (conf > conf_step){
            conf <- conf - conf_step
            message(paste("Decreasing confidence to:  ", conf))
          }
          else{
            message("All options exhausted")
            flag <- FALSE
          }
        }
      }, error= function(err)
      {
        if (!"TimeoutException"  %in% class(err))
        {
          stop(paste("Unexpecterd error",err))
        }
        message("Iteration timeout")
        message(paste("Maxlen:",maxlen))
        iteration_time_limit_exceeded <- iteration_time_limit_exceeded + 1
        exectime <- proc.time() - starttime
        if (exectime[3] > total_timeout)
        {
          message("Max execution time exceeded")
          flag<-FALSE
        }
        else if (maxlen > minlen)
        {
          maxlen <- maxlen - 1
          maxlendecreased_dueTIMEOUT <- TRUE
          message(paste("Decreasing maxlen to:  ", maxlen, ", current support:", support))
        }
        else{
          message("All options exhausted")
          flag <- FALSE
        }
        return (list(iteration_time_limit_exceeded = iteration_time_limit_exceeded,
                     maxlen = maxlen, maxlendecreased_dueTIMEOUT = maxlendecreased_dueTIMEOUT, flag = flag))
      })

    if (is.list(new_values))
    {
      iteration_time_limit_exceeded <- new_values$iteration_time_limit_exceeded
      maxlen <- new_values$maxlen
      maxlendecreased_dueTIMEOUT <- new_values$maxlendecreased_dueTIMEOUT
      flag <- new_values$flag
    }
  }
  if (!exists("rules"))
  {
    message("Returning no rules")
    return(NULL)
  }

  if(trim & length(rules) > target_rule_count)
  {
    message("Removing excess discovered rules")
    #TODO rules are removed using the order in which they appear in the rules object
    rules <- rules[1:target_rule_count]
  }
  return (rules)

}
