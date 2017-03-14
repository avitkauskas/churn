### set working directory
setwd("~/churn")

### read initial data
init <- read.csv("Churn prediction data.csv")

### split by months
init6 <- init[init$month == 6, ]
init7 <- init[init$month == 7, ]
init8 <- init[init$month == 8, ]

### remove unneeded colums
init6 <- init6[,3:65]
init7 <- init7[,3:65]
init8 <- init8[,3:65]

### read churn data
churn <- read.csv("Customer churners.csv")
churn <- churn[,3:4]

### combine all data together making one line per user_account_id
combi <- merge(churn, init6, by = "user_account_id", all.x = TRUE)
combi <- merge(combi, init7, by = "user_account_id", suffixes = c("_6", ""), all.x = TRUE)
combi <- merge(combi, init8, by = "user_account_id", suffixes = c("_7", "_8"), all.x = TRUE)

### remove unneeded data frames
remove(init, init6, init7, init8, churn)

### --- cleaning the data ---

### -- adjusting user_lifetime data --

### for users with 3 months of data:

### adjust the flow of user_lifetime (should be +31 each month)
ids <- which(!is.na(combi$user_lifetime_6))
combi$user_lifetime_7[ids] <- combi$user_lifetime_6[ids] + 31
combi$user_lifetime_8[ids] <- combi$user_lifetime_7[ids] + 31

### if user_intake_6 == 1 & user_lifetime_6 > 31
###   set user_lifetime_6 to median lifetime of correct new users
###   adjust user_lifetime_7 and user_lifetime_8 accordingly: +31

ids <- which(!is.na(combi$user_lifetime_6) & combi$user_lifetime_6 > 31 & combi$user_intake_6 == 1)
med <- median(combi$user_lifetime_6[combi$user_lifetime_6 < 31 & combi$user_intake_6 == 1], na.rm = TRUE)
combi$user_lifetime_6[ids] <- med
combi$user_lifetime_7[ids] <- combi$user_lifetime_6[ids] + 31
combi$user_lifetime_8[ids] <- combi$user_lifetime_7[ids] + 31

### if user_intake_6 == 0 & user_lifetime_6 > 15000
###   set user_lifetime_6 to max lifetime of correct old users
###   adjust user_lifetime_7 and user_lifetime_8 accordingly: +31

ids <- which(!is.na(combi$user_lifetime_6) & combi$user_lifetime_6 > 15000 & combi$user_intake_6 == 0)
max <- max(combi$user_lifetime_6[combi$user_lifetime_6 < 15000 & combi$user_intake_6 == 0], na.rm = TRUE)
combi$user_lifetime_6[ids] <- max
combi$user_lifetime_7[ids] <- combi$user_lifetime_6[ids] + 31
combi$user_lifetime_8[ids] <- combi$user_lifetime_7[ids] + 31

### for users with 2 months of data:

### adjust the flow of user_lifetime (should be +31 each month)
ids <- which(is.na(combi$user_lifetime_6) & !is.na(combi$user_lifetime_7))
combi$user_lifetime_8[ids] <- combi$user_lifetime_7[ids] + 31

### treat all these users as new in month 7
### if user_lifetime_7 > 31
###   set user_lifetime_7 to median lifetime of correct new users
###   adjust user_lifetime_8 accordingly: +31

ids <- which(is.na(combi$user_lifetime_6) & !is.na(combi$user_lifetime_7) & combi$user_lifetime_7 > 31)
med <- median(combi$user_lifetime_7[combi$user_lifetime_7 < 31 & combi$user_intake_7 == 1], na.rm = TRUE)
combi$user_lifetime_7[ids] <- med
combi$user_lifetime_8[ids] <- combi$user_lifetime_7[ids] + 31

### set user_intake_7 = 1 for all users with 2 months of data
ids <- which(is.na(combi$user_lifetime_6) & !is.na(combi$user_lifetime_7))
combi$user_intake_7[ids] <- 1

### for users with 1 month of data:

### treat all these users as new in month 8
### if user_lifetime_8 > 31
###   set user_lifetime_8 to median lifetime of correct new users
ids <- which(is.na(combi$user_lifetime_6) & is.na(combi$user_lifetime_7) & combi$user_lifetime_8 > 31)
med <- median(combi$user_lifetime_8[combi$user_lifetime_8 < 31 & combi$user_intake_8 == 1], na.rm = TRUE)
combi$user_lifetime_8[ids] <- med

### set user_intake_8 = 1 for all users with 1 month of data
ids <- which(is.na(combi$user_lifetime_6) & is.na(combi$user_lifetime_7))
combi$user_intake_8[ids] <- 1

remove(ids, max, med)

### -- end of adjusting user_lifetime data --

### -- adjusting inactivity counters --

### inactivity cannot be longer than lifetime
### adjust inactivity counters for month 6
inactivity_colums_6 <- grep("days_6", names(combi))
for (i in inactivity_colums_6) {
  ids <- which(combi[i] > combi$user_lifetime_6)
  if (length(ids) > 0) {
    combi[ids, i] <- combi$user_lifetime_6[ids]
  }
}

### month-to-month inactivity change cannot be bigger than 31 days
### adjust inactivity counters for month 7
inactivity_colums_7 <- grep("days_7", names(combi))
for (i in 1:length(inactivity_colums_6)) {
  ids <- which(combi[inactivity_colums_7[i]] - combi[inactivity_colums_6[i]] > 31)
  if (length(ids) > 0) {
    combi[ids, inactivity_colums_7[i]] <- combi[ids, inactivity_colums_6[i]] + 31
  }
}

### month-to-month inactivity change cannot be bigger than 31 days
### adjust inactivity counters for month 8
inactivity_colums_8 <- grep("days_8", names(combi))
for (i in 1:length(inactivity_colums_7)) {
  ids <- which(combi[inactivity_colums_8[i]] - combi[inactivity_colums_7[i]] > 31)
  if (length(ids) > 0) {
    combi[ids, inactivity_colums_8[i]] <- combi[ids, inactivity_colums_7[i]] + 31
  }
}

remove(i, ids, inactivity_colums_6, inactivity_colums_7, inactivity_colums_8)

### -- end of adjusting inactivity counters --

### -- adjusting activity counts --
### activity counts cannot be 0 if inactive_days < 31
### refused to implement: correction would be arbitrary
### should think of removing these rows if important
### -- end of adjusting activity counts --

### --- end of data cleaning ---

### quarterly usage aggregates
### using averages as some rows have only 1 or 2 months data
combi$calls_count <- rowMeans(combi[grep("^calls_outgoing_count_[678]", names(combi))], na.rm = TRUE)
combi$calls_duration <- rowMeans(combi[grep("^calls_outgoing_duration_[678]", names(combi))], na.rm = TRUE)
combi$sms_count <- rowMeans(combi[grep("^sms_outgoing_count_[678]", names(combi))], na.rm = TRUE)
combi$gprs_sessions <- rowMeans(combi[grep("^gprs_session_count_[678]", names(combi))], na.rm = TRUE)
combi$gprs_usage <- rowMeans(combi[grep("^gprs_usage_[678]", names(combi))], na.rm = TRUE)
combi$spendings <- rowMeans(combi[grep("^user_spendings_[678]", names(combi))], na.rm = TRUE)

### segmenting by usage profile
### we need a separate data frame for kmeans
usage <- data.frame(combi$calls_count, combi$calls_duration, combi$sms_count, combi$gprs_sessions, combi$gprs_usage)
### need scaling for better clustering as columns are in different scales
usage <- scale(usage)
### set seed for run-by-run clusters stability
set.seed(1234)
cl <- kmeans(usage, 5, iter.max = 20, nstart = 5)
combi$cluster <- cl$cluster
remove(usage)

### aggregates by clusters
agg_mean <- aggregate(cbind(calls_duration, sms_count, gprs_usage, spendings) ~ cluster, combi, mean)
agg_sum <- aggregate(cbind(calls_duration, sms_count, gprs_usage, spendings) ~ cluster, combi, sum)

### plot user cluster profiles
barplot(as.matrix(t(agg_mean[,2:5])), beside=TRUE)
agg_sum$size <- cl$size
barplot(prop.table(as.matrix(agg_sum[,2:6]),2))

### segmenting by user lifecycle
combi$user_lifecycle_group <- 1 # new users
combi$user_lifecycle_group[combi$user_lifetime_8 > 90] <- 2 # growing users (3-12 months)
combi$user_lifecycle_group[combi$user_lifetime_8 > 365] <- 3 # mature users (12-24 months)
combi$user_lifecycle_group[combi$user_lifetime_8 > 365*2] <- 4 # declining users (24+ months)

### segmenting by user value
quant <- quantile(combi$spendings, c(.25, .75))
combi$user_value_group <- 2  # middle 50% group
combi$user_value_group[combi$spendings <= quant[1]] <- 1 # bottom 25%
combi$user_value_group[combi$spendings >= quant[2]] <- 3 # top 25%

### segmenting by lifecycle and value
quant <- quantile(combi$spendings[combi$user_lifetime_8 > 90], c(.25, .75))
combi$user_lifecycle_value_group <- 1 # new users
combi$user_lifecycle_value_group[combi$user_lifetime_8 > 90] <- 3 # middle 50% (3+ months)
combi$user_lifecycle_value_group[combi$user_lifetime_8 > 90 & combi$spendings <= quant[1]] <- 2 # bottom 25% (3+ months)
combi$user_lifecycle_value_group[combi$user_lifetime_8 > 90 & combi$spendings >= quant[2]] <- 4 # top 25% (3+ months)

### RFM segmentation
#rfm.table <- data.frame(combi$reloads_inactive_days_8, combi$reloads_count_8, combi$reloads_sum_8)
#rfm.table <- scale(rfm.table)
#rfm <- kmeans(rfm.table, 3, iter.max = 20, nstart = 5)
#combi$rfm <- rfm$cluster
#rfm_mean <- aggregate(cbind(reloads_inactive_days_8, reloads_count_8, reloads_sum_8) ~ rfm, combi, mean)

aggregate(reloads_count_8 ~ cluster, combi, mean)
aggregate(reloads_sum_8 ~ cluster, combi, mean)
aggregate(reloads_sum_8 ~ cluster, combi, sum)

### segmenting customers according to migration trend
### segmenting by value in month 7
quant7 <- quantile(combi$user_spendings_7, c(.25, .75), na.rm = TRUE)
combi$user_value_group_7 <- 2  # middle 50% group
combi$user_value_group_7[combi$user_spendings_7 <= quant7[1]] <- 1 # bottom 25%
combi$user_value_group_7[combi$user_spendings_7 >= quant7[2]] <- 3 # top 25%
### segmenting by value in month 8
quant8 <- quantile(combi$user_spendings_8, c(.25, .75), na.rm = TRUE)
combi$user_value_group_8 <- 2  # middle 50% group
combi$user_value_group_8[combi$user_spendings_8 <= quant8[1]] <- 1 # bottom 25%
combi$user_value_group_8[combi$user_spendings_8 >= quant8[2]] <- 3 # top 25%
### show movement
prop.table(table(combi$user_value_group_7, combi$user_value_group_8), 1)

remove(quant, quant7, quant8)

### -- output to console for graphs in presentation --

### ARPU for different segmentations
aggregate(spendings ~ user_lifecycle_group, combi, mean)
aggregate(spendings ~ user_lifecycle_group, combi[combi$spendings != 0,], mean)
aggregate(spendings ~ user_value_group, combi, mean)

### churning revenue composition
aggregate(spendings ~ churn, combi, sum)
aggregate(spendings ~ user_lifecycle_group + churn, combi, sum)
aggregate(spendings ~ user_value_group + churn, combi, sum)
aggregate(spendings ~ cluster + churn, combi, sum)

### usage profiles composition
agg_mean
agg_sum
cl$size
prop.table(table(combi$churn, combi$cluster), 2)
prop.table(table(combi$churn, combi$cluster), 1)

remove(agg_mean, agg_sum, cl)

### lifecycle groups composition
table(combi$user_lifecycle_group)
prop.table(table(combi$user_lifecycle_group, combi$cluster), 2)
prop.table(table(combi$churn, combi$user_lifecycle_group), 2)
prop.table(table(combi$churn, combi$user_lifecycle_group), 1)

### value groups composition
table(combi$user_value_group)
prop.table(table(combi$churn, combi$user_value_group), 2)
prop.table(table(combi$churn, combi$user_value_group), 1)

### lifecycle-value groups composition
table(combi$user_lifecycle_value_group)
prop.table(table(combi$churn, combi$user_lifecycle_value_group), 2)
prop.table(table(combi$churn, combi$user_lifecycle_value_group), 1)


### --- churn modeling ---

### -- modeling for new user segment

### separate new user data of 8th month
cols <- grep("_8", names(combi))  # take all columns of 8th month
cols <- cols[-2]  # delete user_intake column as it is 1 for all new users
cols <- append(cols, 2) # add churn column
### make a new user data frame
newusers <- combi[combi$user_lifecycle_group == 1, cols]

### preparing for cross-validation
n.fold <- 5   # folds of cross-validation
n.sample <- nrow(newusers) / n.fold   # number of samples per fold
s <- sample(1:n.fold, size=n.sample, replace=T)

### cross-validation for random forest model
library(party)
acc.cf <- true.p.cf <- false.p.cf <- 0
for (i in 1:n.fold){
  ### create training data and validation data for each fold
  trn <- newusers[s!=i,]
  val <- newusers[s==i,]
  ### fit the model to training set
  cf <- cforest(as.factor(churn) ~ ., data = trn, control = cforest_unbiased(ntree = 50, trace = TRUE))
  ### make prediction on valitadion set
  pr <- predict(cf, newdata = val, OOB = TRUE)
  ### show confusion matrix
  t <- table(pr, val$churn)
  print(t)
  ### calculate accuracy, false positives and true positives
  ### for this cross-validation fold
  acc <- (t[1,1]+t[2,2])/sum(t)
  false.p <- t[2,1]/(t[1,1]+t[2,1])
  true.p <- t[2,2]/(t[1,2]+t[2,2])
  ### accumulate accuracy, false positives and true positives
  ### for all cross-validation folds
  acc.cf <- acc.cf + acc
  true.p.cf <- true.p.cf + true.p
  false.p.cf <- false.p.cf + false.p
  print(acc)
  print(true.p)
  print(false.p)
}
### calculate average accuracy, false positives and true positives
acc.cf <- acc.cf / n.fold
true.p.cf <- true.p.cf / n.fold
false.p.cf <- false.p.cf / n.fold
print(acc.cf)
print(true.p.cf)
print(false.p.cf)


#cols <- cols[-grep("spendings", names(combi)[cols])]  # delete spendings columns
# make a new data frame
#newusers <- combi[combi$user_lifecycle_group == 1, cols]

### cross-validation for logistic regression model
acc.lr <- true.p.lr <- false.p.lr <- 0
for (i in 1:n.fold){
  ### create training data and validation data for each fold
  trn <- newusers[s!=i,]
  val <- newusers[s==i,]
  ### fit the model to training set
  lr <- glm("churn ~ .", data = trn, family="binomial"("logit"))
  ### make prediction on valitadion set
  val$fitted <- predict(lr, newdata=val, type="response")
  val$prediction <- ifelse(val$fitted > 0.5, 1, 0)
  ### show confusion matrix
  t <- table(val$prediction, val$churn)
  print(t)
  ### calculate accuracy, false positives and true positives
  ### for this cross-validation fold
  acc <- (t[1,1]+t[2,2])/sum(t)
  false.p <- t[2,1]/(t[1,1]+t[2,1])
  true.p <- t[2,2]/(t[1,2]+t[2,2])
  ### accumulate accuracy, false positives and true positives
  ### for all cross-validation folds
  acc.lr <- acc.lr + acc
  true.p.lr <- true.p.lr + true.p
  false.p.lr <- false.p.lr + false.p
  print(acc)
  print(true.p)
  print(false.p)
}
### calculate average accuracy, false positives and true positives
acc.lr <- acc.lr / n.fold
true.p.lr <- true.p.lr / n.fold
false.p.lr <- false.p.lr / n.fold
print(acc.lr)
print(true.p.lr)
print(false.p.lr)

### final models
lr <- glm("churn ~ .", data = newusers, family="binomial"("logit"))
cf <- cforest(as.factor(churn) ~ ., data = newusers, control = cforest_unbiased(ntree = 50, trace = TRUE))

### predictions by logistic model
newusers$fitted <- predict(lr, newdata=newusers, type="response")
newusers$pr.lr <- ifelse(newusers$fitted > 0.5, 1, 0)

### predictions by random forest
newusers$pr.cf <- predict(cf, newdata = newusers, OOB = TRUE)

### print ROC curves
library(ROSE)
roc.curve(newusers$churn, newusers$pr.cf)
roc.curve(newusers$churn, newusers$pr.lr, add.roc = TRUE, col=2, lwd=2, lty=2)

remove(trn, val)
#remove(newusers)

### -- modeling for "pragmatic" usage profile segment

### separate "pragmatic" user data
pragmatic <- combi[combi$cluster == 4, -c(1, 189:200)]
### as churn is very unbalanced, let's make a balanced churn/non-churn 50/50 sample
s <- sample(nrow(pragmatic[pragmatic$churn == 0,]), nrow(pragmatic[pragmatic$churn == 1,]))
pragma <- rbind(pragmatic[pragmatic$churn == 1,], pragmatic[s,])

### fit model to the balanced sample
cf <- cforest(as.factor(churn) ~ ., data = pragma, control = cforest_unbiased(ntree = 100, trace = TRUE))
### make predictions on full data
pr.cf <- predict(cf, newdata = pragmatic)
### show confusion matrix
t <- table(pr.cf, pragmatic$churn)
print(t)
### calculate accuracy, false positives and true positives
acc <- (t[1,1]+t[2,2])/sum(t)
false.p <- t[2,1]/(t[1,1]+t[2,1])
true.p <- t[2,2]/(t[1,2]+t[2,2])
print(acc)
print(true.p)
print(false.p)

### select users for insentives campaign
campaign <- pragmatic[pr.cf == 1,]
saved <- campaign[campaign$churn == 1,]
saved$spendings <- rowMeans(saved[grep("^user_spendings_[678]", names(saved))], na.rm = TRUE)
### saved monthly revenue
sum(saved$spendings)
### total churning monthly revenue
sum(combi$spendings[combi$churn == 1])
