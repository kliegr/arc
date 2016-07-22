#' @importFrom R.utils evalWithTimeout
#' @import arules
library(R.utils)


#' A wrapper for the arules method from the apriori package that iteratively changes mining parameters until a desired number of rules is obtained, all options are exhausted or a preset time limit is reached.
#' Within the arc package, this function serves as a replacement for the CBA-RG algorithm (without pessimistic pruning) with general apriori implementation provided by existing fast R package \strong{arules}.
#'
#' @param txns input transactions
#' @param classitems a list of items to appear in the consequent (rhs) of the rules
#' @param custom_alg_options a list of custom options, any options specified will override those defined by the DEFAULT_RULE_LEARNING_SETUP variable
#'  \itemize{
#'   \item \strong{target_rule_count}: the main stopping criterion, mining stops when the resulting rule set contains this number of rules,
#'   \item \strong{init_support}: initial support,
#'   \item \strong{init_conf}: initial confidence,
#'   \item \strong{conf_step}: confidence will be changed by steps defined by this parameter,
#'   \item \strong{supp_step}: support will be changed by steps defined by this parameter,
#'   \item \strong{minlen}: minimum length of rules, minlen=1 corresponds to rule with empty antecedent and one item in consequent. In general, rules with empty antecedent are not desirable for the subsequent pruning algorithm, therefore the value of this parameter should be set at least to value 2,
#'   \item \strong{init_maxlen}: maximum length of rules, should be equal or higher than minlen. A higher value may decrease the number of iterations to obtain target_rule_count rules, but it also increases the risk of initial combinatorial explosion and subsequent memory crash of the apriori rule learner,
#'   \item \strong{iteration_timeout}: maximum number of seconds it should take apriori to obtain rules with current configuration,
#'   \item \strong{total_timeout}: maximum number of seconds the mining should take,
#'   \item \strong{max_iterations}: maximum number of iterations,
#'   }
#' @param debug boolean indicating whether to output debug messages.
#'
#' @return Returns an object of class rules.
#' @export
#'
#' @examples
#'   topRules(as(discrNumeric(iris, "Species")$Disc.data,"transactions"), getItems(iris,"Species"))
#'
#' @seealso \code{\link{prune}}
topRules <- function(txns,classitems,custom_alg_options=NULL,debug=FALSE)
{
  starttime<-proc.time()
  if (missing(classitems))
  {
    stop("classitems cannot be null")
  }
  if (missing(txns))
  {
    stop("txns cannot be null")
  }

  alg_options<-replace(default_rule_learning_setup(),names(custom_alg_options),custom_alg_options)


  MAX_RULE_LEN<-ncol(txns)-length(classitems)+1
  support <- alg_options$init_support
  conf <- alg_options$init_conf
  #maxlen=1 corresponds to rule with empty antecedent and one item in consequent
  maxlen<-alg_options$init_maxlen


  iteration_time_limit_exceeded<-0
  flag<-TRUE
  lastrulecount <- -1
  maxlendecreased_dueTIMEOUT<-FALSE
  iterations<-0
  test<-"AAA"
  while(flag)
  {
    iterations<-iterations+1
    if (iterations==alg_options$max_iterations) {
      print(paste("Max iterations reached"))
      break
    }
    new_values<-tryCatch(
      {
        rules <- evalWithTimeout({rules <- apriori(txns, parameter =
          list(confidence = conf, support= support, minlen= alg_options$minlen, maxlen= maxlen),
          appearance = list(rhs = classitems,default="lhs"));},
          timeout=alg_options$iteration_timeout, onTimeout="error");

        rulecount<-length(rules)
        print(paste("Rule count:  ",rulecount))
        if (rulecount >= alg_options$target_rule_count)
        {
          flag<-FALSE
          print(paste("Target rule count satisfied:  ",alg_options$target_rule_count))
        }
        else{
          exectime = proc.time() -starttime
          if(debug)
          {
            print(maxlen < MAX_RULE_LEN)
            print(lastrulecount != rulecount)
            print(!maxlendecreased_dueTIMEOUT)
          }
          if (exectime[3] > alg_options$total_timeout)
          {
            print(paste("Max execution time exceeded:  ",alg_options$total_timeout))
            flag<-FALSE
          }
          # increase max len if maximum maxlen has not yet been achieved and the number of rules increased during the last optimization step
          # if the number of rules did not increase further increase of maxlen will not bring more rules (or at least it is unlikely to)
          else if (maxlen < MAX_RULE_LEN & lastrulecount != rulecount & !maxlendecreased_dueTIMEOUT)
          {
            maxlen <- maxlen+1
            lastrulecount  <- rulecount
            print(paste("Increasing maxlen to:  ",maxlen))
          }
          # if maxlen has been previously decreased, it means it resulted in combinatorial explosion
          # this can be hopefully prevented if it is increased with simultaneous increase of minsup
          # this can be performed only if there is space for increasing support
          else if (maxlen < MAX_RULE_LEN & maxlendecreased_dueTIMEOUT & support<=(1-alg_options$supp_step))
          {
            support<-support+alg_options$supp_step
            maxlen <- maxlen+1
            lastrulecount  <- rulecount
            print(paste("Increasing maxlen to:  ",maxlen))
            #TODO check if this is a good design option
            maxlendecreased_dueTIMEOUT<-FALSE
          }
          # try decreasing confidence if other options in the previous iteration did not increase the number of rules
          else if (conf > alg_options$conf_step){
            conf <- conf-alg_options$conf_step
            print(paste("Decreasing confidence to:  ",conf))
            #lastrulecount  <-  -1
          }
          else{
            print("All options exhausted")
            flag<-FALSE
          }
        }

      }, error= function(err)
      {
        if (!"TimeoutException"  %in% class(err))
        {
          print(paste("Unexpecterd error",err))
          stop(err)
        }
        print("Iteration timeout")
        print(paste("Maxlen:",maxlen))
        iteration_time_limit_exceeded <- iteration_time_limit_exceeded+1
        exectime <- proc.time() -starttime
        if (exectime[3] > alg_options$total_timeout)
        {
          print("Max execution time exceeded")
          flag<-FALSE
        }
        else if (maxlen > alg_options$minlen)
        {
          maxlen<-maxlen-1
          maxlendecreased_dueTIMEOUT<-TRUE
          print(paste("Decreasing maxlen to:  ",maxlen, ", current support:", support))
        }
        else{
          print("All options exhausted")
          flag<-FALSE
        }
        return (list(iteration_time_limit_exceeded=iteration_time_limit_exceeded,maxlen=maxlen,maxlendecreased_dueTIMEOUT=maxlendecreased_dueTIMEOUT,flag=flag))
      })

    if (is.list(new_values))
    {
      iteration_time_limit_exceeded<-new_values$iteration_time_limit_exceeded
      maxlen<-new_values$maxlen
      maxlendecreased_dueTIMEOUT<-new_values$maxlendecreased_dueTIMEOUT
      flag<-new_values$flag
    }
  }
  if (!exists("rules"))
  {
    print("Returning no rules")
    return(NULL)
  }

  if(length(rules)>alg_options$target_rule_count)
  {
    print("Removing excess discovered rules")
    #TODO rules are removed simply in the order in which they appear in the rules object
    rules <- rules[1:alg_options$target_rule_count]
  }
  return (rules)

}
