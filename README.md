#  Association Rule Classification (arc)

[![Travis-CI Build Status](https://travis-ci.org/kliegr/arc.svg?branch=master)](https://travis-ci.org/kliegr/arc)
[![](http://cranlogs.r-pkg.org/badges/arc)](http://cran.rstudio.com/web/packages/qCBA/index.html)
[![](https://www.r-pkg.org/badges/version/arc)](https://cran.r-project.org/web/packages/arc/index.html)



This package for R implements the Classification based on Associations algorithm (CBA):

 ```Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. Proceedings KDD-98, New York, 27-31 August. AAAI. pp 80-86.```

The [arules](https://github.com/mhahsler/arules/) package is used for the rule generation step.

The package is also availabe in **R CRAN repository** as [Association Rule Classification (arc)](https://cran.r-project.org/web/packages/arc/index.html) package.
## Features 
- Pure R implementation* 
- Supports numerical predictor attributes (via parameter-free supervised  discretization)
- Supports numerical target attribute (k-means)
- No meta parameters with automatic tuning of support and confidence thresholds (optional)**

NOTES: * Requires the arules package for the rule generation step ** There are some metaparameters for automatic tuning, but in most cases the default values are OK.

## Other use cases
- `prune` function can be used to reduce the size of a rule set learnt by the apriori function from the  arules package
- `topRules` function can be used as a wrapper for apriori allowing to mine for a user specified number of rules.

## Installation
The package can be installed directly from CRAN using the following command executed from the R environment:
```R
install.packages("arc")
```
Development version can be installed from github from the R environment using the devtools package.
```R
devtools::install_github("kliegr/arc")
```

## Examples

### Complete classification workflow
```R
library(arc)
# note that the iris dataset contains numerical features
data(iris)
train <- iris[1:100,]
test <- iris[101:length(iris),]
classatt <- "Species"
# learn the classifier
rm <- cba(train, classatt)
prediction <- predict(rm, test)
acc <- rulemodelAccuracy(prediction, test[[classatt]])
print(acc)
```

### Prune rules
This shows how to apply arc data coverage pruning to reduce the size of the rule set. A prerequisite is a rule learning task with one attribute on the right hand side.
```R
library(arc)
data(Adult)
classitems <- c("income=small","income=large")
rules <- apriori(Adult, parameter = list(supp = 0.05, conf = 0.5, target = "rules"), appearance=list(rhs=classitems, default="lhs"))
# now we have 1266 rules
pr_rules <- prune(rules,Adult,classitems)
# only 174 after pruning
```

Additional reduction of the size of the rule set can be achieved by setting `greedy_pruning=TRUE`.
```R
pr_rules <- prune(rules, Adult, classitems, greedy_pruning=TRUE)
# produces 141 rules
```
Pruning by default consists of two steps, data coverage pruning and default rule pruning, which replaces part of the rules surviving data coverage pruning with a new default rule (rule with empty LHS). Default rule pruning can be turned off:
```R
pr_rules <- prune(rules, Adult, classitems, default_rule_pruning = FALSE)
# produces 198 rules
```


### Mine predefined number of rules with apriori
The [arules documentation](https://cran.r-project.org/web/packages/arules/arules.pdf) gives the following example:
```R
data("Adult")
## Mine association rules.
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)
```
This returns 52 rules. The default value for the minlen and maxlen parameters unspecified by the user was 1 and 10. 
Assuming that the user wishes to obtain 100 rules, this can be achieved with the arc package as follows:

```R
data("Adult")
rules <- topRules(Adult, target_rule_count = 100, init_support = 0.5, init_conf = 0.9, minlen = 1, init_maxlen = 10)
summary(rules)
```
This will return 100 rules. The mechanics behind are  iterative step-wise changes to the initial values of the provided thresholds. In this case, there will be nine iterations, the minimum confidence threshold will be lowered to 0.65 and the final rule set will be trimmed.

### Performance optimization
#### Rule learning
* In order to keep the number of iterations and thus run time low, it might be a good idea to set the `init_maxlen` parameter to a low value:
```R
data("Adult")
rules <- topRules(Adult, target_rule_count = 100, init_support = 0.5, init_conf = 0.9, minlen = 1, init_maxlen = 2)
summary(rules)
```
#### Rule pruning
* Experiment with the value of the `rule_window` parameter. This has no effect on the quality of the classifier. 
* Set `greedy_pruning` to TRUE. This will have generally adverse impact on the quality of the classifier, but it will decrease the size of the rule set and reduce the time required for pruning. Greedy pruning is not part of the CBA algorithm as published by Liu et al. 

