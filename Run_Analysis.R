library(reshape2)
library(data.table)
## 1. Merges the training and the test sets to create one data set.
# store file url 
UCIHarUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# provide the file path for the dataset
filepath <- file.path(getwd(),"UCIHARDataset")
#download the file 
download.file(UCIHarUrl,filepath)
# place the unzip folder in getwd() directory
# give path to this directory
ZipfilePath <- file.path(getwd(),"UCI HAR Dataset")
# list files to confirm file exists
list.files(ZipfilePath,recursive = TRUE)
# Read Files
SubjectTrain <- read.table(file.path(ZipfilePath,"train","subject_train.txt"))
SubjectTest <- read.table(file.path(ZipfilePath,"test","subject_test.txt"))
Ytest <- read.table(file.path(ZipfilePath,"test","y_test.txt"))
Ytrain <- read.table(file.path(ZipfilePath,"train","y_train.txt"))
Xtrain <- read.table(file.path(ZipfilePath,"train","X_train.txt"))
Xtest <- read.table(file.path(ZipfilePath,"test","X_test.txt"))

# add column name for subject files
names(SubjectTrain) <- "subjectID"
names(SubjectTest) <- "subjectID"

# add column names for measurement files
featureNames <- read.table(file.path(ZipfilePath,"features.txt"))
names(Xtrain) <- featureNames$V2
names(Xtest) <- featureNames$V2

# add column name for label files
names(Ytrain) <- "activity"
names(Ytest) <- "activity"

# combine data sets
Test <- cbind(SubjectTest,Ytest,Xtest)
Train <- cbind(SubjectTrain,Ytrain,Xtrain)
Mergeddatset <- rbind(Train,Test)

# 2.  Mean & SD COlumn for the Merged data set
MeanSDColumns <- grepl("mean\\(\\)", names(Mergeddatset)) | grepl("std\\(\\)", names(Mergeddatset))
MeanSDColumns[1:2] <- TRUE
Mergeddatset <- Mergeddatset[, MeanSDColumns]


#3. Uses descriptive activity names to name the activities in the data set
Mergeddatset$activity <- factor(Mergeddatset$activity, labels=c("Walking","Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##STEP 5: Creates a second, independent tidy data set with the
## take the average of each variable for each activity and each subject.

melt <- melt(Mergeddatset, id=c("subjectID","activity"))
tidy <- dcast(melt, subjectID+activity ~ variable, mean)

# export to a file
write.csv(tidy, "tidy.csv", row.names=FALSE)
write.table(tidy, "tidy.txt", row.names=FALSE)

#generate codebookx
x <- summary(Mergeddatset)
capture.output(x,file = "codebook.txt")
