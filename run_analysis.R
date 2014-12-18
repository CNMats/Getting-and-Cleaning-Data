## This R script performs the following:
## 0.Get the data from online source
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## 0.Get the data
## create data folder for downloaded data
if(!file.exists("./data"))
{dir.create("./data")}

## download data and unzip the file
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/SamsungDataSet.zip")
unzip("./data/SamsungDataSet.zip")


## obtain test data and read into R
test.raw <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
## combine with subject from subject_test file & test lables from y_test file
tester.id <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
test.id <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
test.data <- cbind(tester.id, test.id) #tester - test
test.data <- cbind(test.data, test.raw) #tester - test - data
## change column names
names(test.data)[1] <- "subject_id"
names(test.data)[2] <- "activity_id"

## obtain training data and read into R
train.raw <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
## combine with subject from subject_train file & training tables from y_train file
trainee.id <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
train.id <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
train.data <- cbind(trainee.id, train.id)
train.data <- cbind(train.data, train.raw)
## change column names
names(train.data)[1] <- "subject_id"
names(train.data)[2] <- "activity_id"


## 1.Merges the training and the test sets to create one data set.
mergedData = merge(test.data, train.data,all=TRUE)

## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
features.label <- read.table("./data/UCI HAR Dataset/features.txt")
col <- grep("mean|std",features.label[,2])
data.work <- mergedData[,c(1,2,col+2)]  #offset by 2 for subject & activity ids

## 3.Uses descriptive activity names to name the activities in the data set
activity.id <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

activityNameFct <- function(x){activity.id[x,2]}
data.work$activityLabel <- sapply(data.work[2],activityNameFct)
data.work <- cbind(data.work[1],data.work[2],data.work[82],data.work[3:81])

## 4.Appropriately labels the data set with descriptive variable names. 
names(data.work)[3:81] <- c(as.character(features.label[col,2]))

## 5.From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
work.melt <- melt(data.work, id=c("subject_id","activity_id","activityLabel"))
work.mean <- dcast(work.melt, subject_id + activityLabel ~ variable, mean)

## export as txt file for submission
write.table(work.mean,file="tidydata.txt", sep=",",col.names=TRUE, row.name=FALSE)
