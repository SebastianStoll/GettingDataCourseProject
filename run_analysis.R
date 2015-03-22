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
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE, col.names = c("activityLabelKey","activityLabel")) 

# feature labels are loaded and filtered down to the required labels/column positions
featureLabels <- read.table("./UCI HAR Dataset/features.txt", sep = " ", header = FALSE, col.names = c("featureKey","featureLabel")) 
requiredFeatureLables <- grep("mean\\(\\)|std\\(\\)",featureLabels$featureLabel)
featureLabels <- featureLabels[requiredFeatureLables,]
featureLabels$featureLabel <- sub("-mean\\(\\)-","Mean",featureLabels$featureLabel)
featureLabels$featureLabel <- sub("-mean\\(\\)","Mean",featureLabels$featureLabel)
featureLabels$featureLabel <- sub("-std\\(\\)-","Std",featureLabels$featureLabel)
featureLabels$featureLabel <- sub("-std\\(\\)","Std",featureLabels$featureLabel)

# Loading and filtering of the test data. The results are merged with the loaded subject ids
# and augmented by the activity labels.
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = c("subjectKey"))
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
testSet <- testSet[,featureLabels$featureKey]
colnames(testSet) <- featureLabels$featureLabel
testSet$subjectKey <- testSubjects$subjectKey

testActivities <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = c("activityLabelKey"))
testSet$activityLabelKey <- testActivities$activityLabelKey
testSet <- inner_join(testSet, activityLabels, by = "activityLabelKey")

# Repeating the same steps for the training data
trainingSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = c("subjectKey"))
trainingSet <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
trainingSet <- trainingSet[,featureLabels$featureKey]
colnames(trainingSet) <- featureLabels$featureLabel
trainingSet$subjectKey <- trainingSubjects$subjectKey

trainActivities <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = c("activityLabelKey"))
trainingSet$activityLabelKey <- trainActivities$activityLabelKey
trainingSet <- inner_join(trainingSet, activityLabels, by = "activityLabelKey")


# Test and training data frames have the same column layout and are unioned
runAnalysisData <- union(testSet, trainingSet)
runAnalysisData$subjectKey <- factor(runAnalysisData$subjectKey)

# Measurements on the mean and standard deviation of each variable 
# for each activity and each subject. The column names of the
# resulting data frame are updated to be more telling.
subjectActivityAnalysis <- runAnalysisData %>% group_by(activityLabel, subjectKey) %>% summarise_each(funs(mean),1:66)
colUpdate <- function(colName) {
  colName <- paste(toupper(substr(colName, 1, 1)), substr(colName, 2, nchar(colName)), sep="")
  paste("mean",colName, sep="")
}  
colnames(subjectActivityAnalysis)[3:68] <- lapply(colnames(subjectActivityAnalysis)[3:68], colUpdate)

# Writing out the results
write.table(subjectActivityAnalysis, file = "./subjectActivityAnalysis.txt", row.name=FALSE)