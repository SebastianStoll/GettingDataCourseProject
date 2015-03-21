##############################################################################
# This R-script is the solution for course project 1 of the coursera course
# "Getting and Cleaning Data".
#
##############################################################################

#Load required libraries
library(dplyr)

# For reading the data it is assumed than a sub directory called "UCI HAR Dataset"
# exists containing the provided input data.

# For both test and training data set the subject mappings, 
# measurements and activity labels are combined into one data set
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE, col.names = c("activity_label_key","activity_label")) 

# feature labels are loaded and filtered down to the required labels/column positions
featureLabels <- read.table("./UCI HAR Dataset/features.txt", sep = " ", header = FALSE, col.names = c("feature_key","feature_label")) 
requiredFeatureLables <- grep("mean\\(\\)|std\\(\\)",featureLabels$feature_label)
featureLabels <- featureLabels[requiredFeatureLables,]
featureLabels$feature_label <- sub("-mean\\(\\)-","Mean",featureLabels$feature_label)
featureLabels$feature_label <- sub("-mean\\(\\)","Mean",featureLabels$feature_label)
featureLabels$feature_label <- sub("-std\\(\\)-","Std",featureLabels$feature_label)
featureLabels$feature_label <- sub("-std\\(\\)","Std",featureLabels$feature_label)

# Loading and filtering of the test data. The results are merged with the loaded subject ids
# and augmented by the activity labels. The input was checked for having the same size
# and as ordering of the samples is given, columns were simply added.
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = c("subject_key"))
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
testSet <- testSet[,featureLabels$feature_key]
colnames(testSet) <- featureLabels$feature_label
testSet$subject_key <- testSubjects$subject_key

testActivities <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = c("activity_label_key"))
testActivities <- inner_join(activityLabels, testActivities, by = "activity_label_key")
testSet$activity_label <- testActivities$activity_label

# Repeating the same steps for the training data
trainingSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = c("subject_key"))
trainingSet <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
trainingSet <- trainingSet[,featureLabels$feature_key]
colnames(trainingSet) <- featureLabels$feature_label
trainingSet$subject_key <- trainingSubjects$subject_key

trainActivities <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = c("activity_label_key"))
trainActivities <- inner_join(activityLabels, trainActivities, by = "activity_label_key")
trainingSet$activity_label <- trainActivities$activity_label

# Test and training data frames have the same columns and are unioned
runAnalysisData <- union(testSet, trainingSet)
runAnalysisData$subject_key <- factor(runAnalysisData$subject_key)

# Measurements on the mean and standard deviation of each variable 
# for each activity and each subject. The column names of the
# resulting data frame are updated to be more telling.
subjectActivityAnalysis <- runAnalysisData %>% group_by(activity_label, subject_key) %>% summarise_each(funs(mean),1:66)
colnames(subjectActivityAnalysis)[3:68] <- lapply(colnames(subjectActivityAnalysis)[3:68], function(x) paste("Mean",x, sep=""))

# Writing out the results
write.table(subjectActivityAnalysis, file = "./subjectActivityAnalysis.txt", row.name=FALSE)
