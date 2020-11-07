install.packages("class")
library(class)
install.packages("gmodels")
library(gmodels)


# Step 1 – collecting data
## South German Credit (UPDATE) Data Set from http://archive.ics.uci.edu/ml/datasets/South+German+Credit+%28UPDATE%29. 


# Step 2. exploring and preparing the data
sgc <- read.csv('german.data-numeric.csv', stringsAsFactors = FALSE)

table(sgc$Credit.result)
sgc$Credit.result <- factor(sgc$Credit.result, levels = c("1", "2"), labels = c("Good", "Bad"))
round(prop.table(table(sgc$Credit.result))*100, digits = 1)

summary(sgc[c("Status.of.existing.checking.account", "Duration.in.month", "Credit.history", "Credit.amount")])

# normalizing numeric data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

sgc_n <- as.data.frame(lapply(sgc[1:24], normalize))
summary(sgc_n$Duration.in.month)

# creating training and test datasets
sgc_train <- sgc_n[1:800, ]
sgc_test <- sgc_n[801:1000, ]
sgc_train_labels <- sgc[1:800, 25]
sgc_test_labels <- sgc[801:1000, 25]


# Step 3 – training a model on the data
sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                      cl = sgc_train_labels, k = 28)


# Step 4 – evaluating model performance
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq=FALSE)


# Step 5 – improving model performance
## Transformation – z-score standardization
sgc_z <- as.data.frame(scale(sgc[-25]))
summary(sgc_z$Duration.in.month)

sgc_train <- sgc_n[1:800, ]
sgc_test <- sgc_n[801:1000, ]
sgc_train_labels <- sgc[1:800, 25]
sgc_test_labels <- sgc[801:1000, 25]

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                      cl = sgc_train_labels, k = 28)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

## Testing alternative values of k
sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 1)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 5)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 10)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 15)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 20)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 25)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 30)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 35)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 40)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)

sgc_test_pred <- knn(train = sgc_train, test = sgc_test,
                     cl = sgc_train_labels, k = 45)
CrossTable(x = sgc_test_labels, y = sgc_test_pred,
           prop.chisq = FALSE)





