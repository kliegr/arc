# arc
Association Rule Classification

This package for R implements the Classification based on Associations algorithm (CBA):

 ```Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. Proceedings KDD-98, New York, 27-31 August. AAAI. pp 80-86.```


## Features 
- Pure R implementation* 
- Supports numerical attributes
- No meta parameters with automatic tuning of support and confidence thresholds (optional)**

NOTES: * Requires the arules package for the rule generation step ** There are some metaparameters for automatic tuning, but in most cases the default values are OK.

## Other use cases
- `prune` function can be used to reduce the size of a rule set learnt by the apriori function from the  arules package
- `topRules` function can be used as a wrapper for apriori allowing to mine for a user specified number of rules.

## Examples

### Complete classification workflow
```R
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

### Reduce size of a rule set
