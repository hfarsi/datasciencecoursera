run_analysis <- function() {

library(plyr)
library(reshap2)

### Read the data files

xtest <- read.table("UCI_HAR_Dataset/test/X_test.txt", header=F)
ytest <- read.table("UCI_HAR_Dataset/test/y_test.txt", header=F, col.names="ActivityCode")
xtrain <- read.table("UCI_HAR_Dataset/train/x_train.txt", header=F)
ytrain <- read.table("UCI_HAR_Dataset/train/y_train.txt", header=F, col.names="ActivityCode") 
subjtest <- read.table("UCI_HAR_Dataset/test/subject_test.txt", header=F, col.names="Subject")
subjtrain <- read.table("UCI_HAR_Dataset/train/subject_train.txt", header=F, col.names="Subject")
features <- read.table("UCI_HAR_Dataset/features.txt", header=F)

### Question 1: Merge the training and the test sets to create one data set.

### Set column names and subject IDs, and merge the test and training sets. 
### 
### Output: test_train 

names(xtest) <- features[,2]
names(xtrain) <- features[,2]
xytest <- cbind(subjtest,ytest,xtest)
xytrain <- cbind(subjtrain,ytrain,xtrain)
test_train <- rbind(xytest,xytrain)

### Question 2: Extract only the measurements on the mean and standard deviation for each measurement. 

### select column names containing the words mean and std and use the list to extract the relevant columns from
### the dataset.
### Input:  test_train
### Output: mean_std

meanstd <- sort(c(grep("mean", colnames(test_train)),grep("std", colnames(test_train))))
mean_std <- test_train[,meanstd]

### Question 3: Use descriptive activity names to name the activities in the data set
### This requires a join between test_train and activities data frames.
### Input:  test_train
### Output: tidy_data

activities <- read.table("UCI_HAR_Dataset/activity_labels.txt", header=F, col.names=c("ActivityCode", "ActivityLabel"))
tidy_data <- merge(activities, test_train, by = "ActivityCode")

### Question 4: Appropriately label the data set with descriptive variable names. 
### Use full words (starting with capital letter) instead of short hand words.
### Input:  tidy_data
### Output: tidy_data

x <- colnames(tidy_data)
x <- sub("^t","Time", x)
x <- sub("^f","Frequency", x)
x <- sub("Acc","Acceleration", x)
x <- sub("Gyro","Gyroscope", x)
x <- sub("Mag","Magnitude", x)
x <- sub("tBody","TimeBody", x)

### Turn first letter of functions to uper case and remove

x <- sub("-m","M", x)
x <- sub("-s","S", x)
x <- sub("-e","E", x)
x <- sub("-i","I", x)
x <- sub("-k","K", x)
x <- sub("-a","A", x)
x <- sub("-b","B", x)
x <- sub("-c","C", x)

### remove special characters: (, ), ., ",", -

x <- gsub("\\(","", x)
x <- gsub("\\)","", x)
x <- gsub("\\.","", x)
x <- gsub(",","", x)
x <- gsub("-","", x)
x <- gsub("-","", x)

x[1:3] <- c("ActivityCode", "ActivityLabel", "Subject")
colnames(tidy_data) <- x

  
### Question 5: Create a second, independent tidy data set with the average of each 
###             variable for each activity and each subject. 
### Input:  tidy_data
### Output: new_tidy_data

TD <- tidy_data
TD$ActivityLabel <- NULL
new_tidy <- ddply(TD, c("Subject","ActivityCode"), colMeans)
new_tidy_data <- merge(activities, new_tidy, by = "ActivityCode")

### Tidy up the dataset: using the combination of Subject and Activity as grouping factor
### aggregate each subset with stats functions (mean, std, min, max, and med) for each variable (column).
### Input: tidy_data
### Output: tidy

TD <- tidy_data
TD$ActivityLabel <- NULL
attach(TD)
aggmean <-aggregate(TD, by=list(ActivityCode, Subject), FUN=mean, na.rm=TRUE)
aggmean$statfnc <- rep("Average", nrow(aggmean))
aggstd <-aggregate(TD, by=list(ActivityCode, Subject), FUN=sd, na.rm=TRUE)
aggstd$statfnc <- rep("Std", nrow(aggstd)); 
aggstd$ActivityCode <- aggstd$Group.1; aggstd$Subject <- aggstd$Group.2
aggmin <-aggregate(TD, by=list(ActivityCode, Subject), FUN=min, na.rm=TRUE)
aggmin$statfnc <- rep("Min", nrow(aggmin))
aggmax <-aggregate(TD, by=list(ActivityCode, Subject), FUN=max, na.rm=TRUE)
aggmax$statfnc <- rep("Max", nrow(aggmax))
aggmed <-aggregate(TD, by=list(ActivityCode, Subject), FUN=median, na.rm=TRUE)
aggmed$statfnc <- rep("Median", nrow(aggmed))
detach(TD)

tidy_stats <- rbind(aggmean,aggstd,aggmin,aggmax,aggmed)
tidy_stats <- merge(activities, tidy_stats, by = "ActivityCode")

temp <- tidy_stats[,1:5]
temp <- temp[,-c(3,4)]
temp$statfnc <- tidy_stats$statfnc; tidy_stats$statfnc <- NULL
tidy <- cbind(temp,tidy_stats[,-c(1,2,3,4,5)])
tidy <- arrange(tidy, statfnc, ActivityLabel, Subject)

write.table(tidy, file="Data.text", row.name= T, col.name=T)
### getback <- read.table("Data.text", header=T) # To check if the file is ok
}