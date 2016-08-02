
##Q1: Merges the training and the test sets to create one data set.

## Retrieved data source :https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## R-codes detail

# 1. Merging the training and the test sets to get one data set.

data1 <- read.table("train/X_train.txt")
data2 <- read.table("test/X_test.txt")
X <- rbind(data1, data2)

data1 <- read.table("train/subject_train.txt")
data2 <- read.table("test/subject_test.txt")
S <- rbind(data1, data2)

data1 <- read.table("train/y_train.txt")
data2 <- read.table("test/y_test.txt")
Y <- rbind(data1, data2)

##############
##data1
##data2

##Q2:Extracts only the measurements on the mean and standard deviation for each measurement.

##2. To extract only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_good_features]
names(X) <- features[indices_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))


## Q3: Uses descriptive activity names to name the activities in the data set

## 3. To use descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"



##Q4: Appropriately labels the data set with descriptive variable names.

## 4. To appropriately label the data set with descriptive activity names.

names(S) <- "subject"
Cleaned <- cbind(S, Y, X)
write.table(Cleaned, "Clean_Merged_data.txt")



##Q5: From the data set in step 4, creates a second, independent tidy data set with the average of each

##variable for each activity and each subject.

# 5. To create  independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(Cleaned)[2]
result = Cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- Cleaned[Cleaned$subject==s & Cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "tidy_data.txt")








