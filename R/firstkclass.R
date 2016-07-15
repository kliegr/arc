library(R.utils)

firstkrules <- function(data,classitems,target_rule_count=100,support=0.00,init_conf=0.5,conf_step=0.05,supp_step=0.05,minlen=2,init_maxlen=3,iteration_timeout=2.0,total_timeout=10.0,max_iterations=30)
{
  debug<-FALSE
  starttime<-proc.time()
  if (missing(classitems))
  {
    stop("classitems cannot be null")
  }
  if (missing(data))
  {
    stop("data cannot be null")
  }
  
  #if input is already in transaction format, this line adds only a little overhead
  txns <- as(data,"transactions")
  #maxlen=1 corresponds to rule with empty antecedent and one item in consequent
  MAX_RULE_LEN<-ncol(txns)-length(classitems)+1
  conf<-init_conf
  maxlen<-init_maxlen
  
  iteration_time_limit_exceeded<-0
  flag<-TRUE
  lastrulecount <- -1
  maxlendecreased_dueTIMEOUT<-FALSE
  iterations<-0
  while(flag)
  {
    iterations<-iterations+1
    if (iterations==max_iterations) {
      print(paste("Max iterations reached"))
      break
    }
    tryCatch(
      {
        print(minlen)
        print(maxlen)
        rules <- evalWithTimeout({rules <- apriori(txns, parameter = list(confidence = conf, support= support, minlen= minlen, maxlen= maxlen),appearance = list(rhs = classitems,default="lhs"));},timeout=iteration_timeout, onTimeout="error");
        rulecount<-length(rules)
        print(paste("Rule count:  ",rulecount))
        if (rulecount >= target_rule_count)
        {
          flag<-FALSE
          print(paste("Target rule count satisfied:  ",target_rule_count))
        }
        else{
          exectime = proc.time() -starttime 
          if(debug)
          {
            print(maxlen < MAX_RULE_LEN)
            print(lastrulecount != rulecount)
            print(!maxlendecreased_dueTIMEOUT)
          }
          if (exectime[3] > total_timeout)
          {
            print(paste("Max execution time exceeded:  ",maxlen))
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
          else if (maxlen < MAX_RULE_LEN & maxlendecreased_dueTIMEOUT & support<=(1-supp_step))
          {   
            support<-support+supp_step
            maxlen <- maxlen+1
            lastrulecount  <- rulecount
            print(paste("Increasing maxlen to:  ",maxlen))
            #TODO check if this is a good design option
            maxlendecreased_dueTIMEOUT<-FALSE
          }
          # try decreasing confidence if other options in the previous iteration did not increase the number of rules
          else if (conf > conf_step){                
            conf <- conf-conf_step
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
        print(paste("Individual exec time exceeded:  ",err))
        iteration_time_limit_exceeded <- iteration_time_limit_exceeded+1
        exectime <- proc.time() -starttime 
        if (exectime[3] > total_timeout)
        {
          print(paste("Max execution time exceeded:  ",maxlen))
          flag=FALSE
        } 
        else if (maxlen > 1)
        {
          maxlen<-maxlen-1
          maxlendecreased_dueTIMEOUT<-TRUE
          print(paste("Decreasing maxlen to:  ",maxlen))
        }
        else{
          print("All options exhausted - returning no rules")
          flag<-FALSE
        }        
      })
  }
  if(length(rules)>target_rule_count)
  {
    print("Removing excess discovered rules")
    #TODO rules are removed simply in the order in which they appear in the rules object
    rules <- rules[1:target_rule_count]
  }
  return (rules)
  
}
