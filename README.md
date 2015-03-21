# Getting Data Course Project

## Overview
This repository contains the results for the Getting and Cleaning Data Course Project.

The following sections describe how the course project script works and provide a code book describing the variables of the output data.

## Script Implementation
The implementation of run_analysis.R uses the dplyr library along with base R functionality to manipulate the input data.

A base assumption made by the script is that the data contains no missing values and that the number of records in the measurements, label and subject files fit to each other. This was verified using R and the source data

### Reading data
Data is read using the *read.table* function. Column labels for the measurements are generated by filtering the *features.txt* file for all columns having *mean()* or *std()* in them. Additional substitions normalize the feature column names into a CamelCase style.

Descriptive labels based on the *activity_labels.txt* are joined to each loaded data set. The subject keys are read from the respective files and are appended to the data sets.

### Merging the test and training data sets
Both test and training data sets have the same column layout and can be combined into one data frame using the *union()* function of R.

### Generating the subject activity analysis data
To generate the output from the tidied data set dplyr in combination with pipes is used.

Together with grouping by subject key and activity the summarise_each function is used to apply the mean function to all feature measurements columns. A subsequent update of the column names indicate that they are mean values.

### Writing the results
The results are written out by the *write.table(..)* function to a file called *subjectActivityAnalysis.txt*

## Code book

The output data file is named *subjectActivityAnalysis.txt*

The file contains 40 rows and 68 columns.

The first two columns are group columns over which the means were created. All remaining columns contain means for the features in question.

Each record contains the means for all features in question and a specific activity done by an individual subject.

The following table provides a list of all columns in the data file.

| Column Name  | Type | Description |
|---|---|---|
| activity_label   | group column   | The label of an activity that was done by a subject |
| subject_key | group column  | The id of a subject |
|MeantBodyAccMeanX| summary column |  mean of tBodyAccMeanX feature |
|MeantBodyAccMeanY| summary column |  mean of tBodyAccMeanY feature |
|MeantBodyAccMeanZ| summary column |  mean of tBodyAccMeanZ feature |
|MeantBodyAccStdX| summary column |  mean of tBodyAccStdX feature |
|MeantBodyAccStdY| summary column |  mean of tBodyAccStdY feature |
|MeantBodyAccStdZ| summary column |  mean of tBodyAccStdZ feature |
|MeantGravityAccMeanX| summary column |  mean of tGravityAccMeanX feature |
|MeantGravityAccMeanY| summary column |  mean of tGravityAccMeanY feature |
|MeantGravityAccMeanZ| summary column |  mean of tGravityAccMeanZ feature |
|MeantGravityAccStdX| summary column |  mean of tGravityAccStdX feature |
|MeantGravityAccStdY| summary column |  mean of tGravityAccStdY feature |
|MeantGravityAccStdZ| summary column |  mean of tGravityAccStdZ feature |
|MeantBodyAccJerkMeanX| summary column |  mean of tBodyAccJerkMeanX feature |
|MeantBodyAccJerkMeanY| summary column |  mean of tBodyAccJerkMeanY feature |
|MeantBodyAccJerkMeanZ| summary column |  mean of tBodyAccJerkMeanZ feature |
|MeantBodyAccJerkStdX| summary column |  mean of tBodyAccJerkStdX feature |
|MeantBodyAccJerkStdY| summary column |  mean of tBodyAccJerkStdY feature |
|MeantBodyAccJerkStdZ| summary column |  mean of tBodyAccJerkStdZ feature |
|MeantBodyGyroMeanX| summary column |  mean of tBodyGyroMeanX feature |
|MeantBodyGyroMeanY| summary column |  mean of tBodyGyroMeanY feature |
|MeantBodyGyroMeanZ| summary column |  mean of tBodyGyroMeanZ feature |
|MeantBodyGyroStdX| summary column |  mean of tBodyGyroStdX feature |
|MeantBodyGyroStdY| summary column |  mean of tBodyGyroStdY feature |
|MeantBodyGyroStdZ| summary column |  mean of tBodyGyroStdZ feature |
|MeantBodyGyroJerkMeanX| summary column |  mean of tBodyGyroJerkMeanX feature |
|MeantBodyGyroJerkMeanY| summary column |  mean of tBodyGyroJerkMeanY feature |
|MeantBodyGyroJerkMeanZ| summary column |  mean of tBodyGyroJerkMeanZ feature |
|MeantBodyGyroJerkStdX| summary column |  mean of tBodyGyroJerkStdX feature |
|MeantBodyGyroJerkStdY| summary column |  mean of tBodyGyroJerkStdY feature |
|MeantBodyGyroJerkStdZ| summary column |  mean of tBodyGyroJerkStdZ feature |
|MeantBodyAccMagMean| summary column |  mean of tBodyAccMagMean feature |
|MeantBodyAccMagStd| summary column |  mean of tBodyAccMagStd feature |
|MeantGravityAccMagMean| summary column |  mean of tGravityAccMagMean feature |
|MeantGravityAccMagStd| summary column |  mean of tGravityAccMagStd feature |
|MeantBodyAccJerkMagMean| summary column |  mean of tBodyAccJerkMagMean feature |
|MeantBodyAccJerkMagStd| summary column |  mean of tBodyAccJerkMagStd feature |
|MeantBodyGyroMagMean| summary column |  mean of tBodyGyroMagMean feature |
|MeantBodyGyroMagStd| summary column |  mean of tBodyGyroMagStd feature |
|MeantBodyGyroJerkMagMean| summary column |  mean of tBodyGyroJerkMagMean feature |
|MeantBodyGyroJerkMagStd| summary column |  mean of tBodyGyroJerkMagStd feature |
|MeanfBodyAccMeanX| summary column |  mean of fBodyAccMeanX feature |
|MeanfBodyAccMeanY| summary column |  mean of fBodyAccMeanY feature |
|MeanfBodyAccMeanZ| summary column |  mean of fBodyAccMeanZ feature |
|MeanfBodyAccStdX| summary column |  mean of fBodyAccStdX feature |
|MeanfBodyAccStdY| summary column |  mean of fBodyAccStdY feature |
|MeanfBodyAccStdZ| summary column |  mean of fBodyAccStdZ feature |
|MeanfBodyAccJerkMeanX| summary column |  mean of fBodyAccJerkMeanX feature |
|MeanfBodyAccJerkMeanY| summary column |  mean of fBodyAccJerkMeanY feature |
|MeanfBodyAccJerkMeanZ| summary column |  mean of fBodyAccJerkMeanZ feature |
|MeanfBodyAccJerkStdX| summary column |  mean of fBodyAccJerkStdX feature |
|MeanfBodyAccJerkStdY| summary column |  mean of fBodyAccJerkStdY feature |
|MeanfBodyAccJerkStdZ| summary column |  mean of fBodyAccJerkStdZ feature |
|MeanfBodyGyroMeanX| summary column |  mean of fBodyGyroMeanX feature |
|MeanfBodyGyroMeanY| summary column |  mean of fBodyGyroMeanY feature |
|MeanfBodyGyroMeanZ| summary column |  mean of fBodyGyroMeanZ feature |
|MeanfBodyGyroStdX| summary column |  mean of fBodyGyroStdX feature |
|MeanfBodyGyroStdY| summary column |  mean of fBodyGyroStdY feature |
|MeanfBodyGyroStdZ| summary column |  mean of fBodyGyroStdZ feature |
|MeanfBodyAccMagMean| summary column |  mean of fBodyAccMagMean feature |
|MeanfBodyAccMagStd| summary column |  mean of fBodyAccMagStd feature |
|MeanfBodyBodyAccJerkMagMean| summary column |  mean of fBodyBodyAccJerkMagMean feature |
|MeanfBodyBodyAccJerkMagStd| summary column |  mean of fBodyBodyAccJerkMagStd feature |
|MeanfBodyBodyGyroMagMean| summary column |  mean of fBodyBodyGyroMagMean feature |
|MeanfBodyBodyGyroMagStd| summary column |  mean of fBodyBodyGyroMagStd feature |
|MeanfBodyBodyGyroJerkMagMean| summary column |  mean of fBodyBodyGyroJerkMagMean feature |
|MeanfBodyBodyGyroJerkMagStd| summary column |  mean of fBodyBodyGyroJerkMagStd feature |