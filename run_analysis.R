######################################################################

## Step 0.
## Manual download and unzipping of datafile https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## to datafolder './data' resulting in subfolder './data/dataset' with several data files.

# clean all
rm(list=ls())

# Required libraries
library(LaF)
library(plyr)
library(reshape2)
#library(data.table)

######################################################################

## Step 1.
## Merge the training and the test sets to create one data set.

# Get training data.
# Using the LaF package to load large fixed width file. 

# load X_train data
# create file connection
laf <- laf_open_fwf (filename="./data/dataset/train/X_train.txt"
                  , column_types=rep("numeric",561)
                  , column_widths=rep(16,561))

# load standard data frame with x_train data
traindat <- laf[,]

# set traindat columnnames
colnames <- read.table("./data/dataset/features.txt", sep = ""
                       , header = F, colClasses = c("integer","character")
                       , stringsAsFactors = F)
names(traindat) <- colnames[[2]] # assuming columns are read in order

# load y_train data
ytrain <- read.table("./data/dataset/train/y_train.txt", sep = ""
                   , header = F, colClasses = "integer"
                   , stringsAsFactors = F)


# add label column
traindat[, c("labelid")] <- ytrain # this adds column as vector (and not as a dataframe)
                                 # which would cause a problem with
                                 # rbind of the test en train data frames

# load subject data
subtrain <- read.table("./data/dataset/train/subject_train.txt", sep = ""
                       , header = F, colClasses = "integer"
                       , stringsAsFactors = F)

# add subject column
traindat[, c("subjectid")] <- subtrain


# Get test data.

# load X_test data
# create file connection
laf <- laf_open_fwf (filename="./data/dataset/test/X_test.txt"
                     , column_types=rep("numeric",561)
                     , column_widths=rep(16,561))

# load standard data frame with x_train data
testdat <- laf[,]

# set testdat columnnames
names(testdat) <- colnames[[2]] # assuming columns are read in order

# load y_test data
ytest <- read.table("./data/dataset/test/y_test.txt", sep = ""
                     , header = F, colClasses = "integer"
                     , stringsAsFactors = F)

# add label column
testdat[, c("labelid")] <- ytest

# load subject data
subtest <- read.table("./data/dataset/test/subject_test.txt", sep = ""
                       , header = F, colClasses = "integer"
                       , stringsAsFactors = F)

# add subject column
testdat[, c("subjectid")] <- subtest

# merge the training data and test data set
dataset.step1 <- rbind(traindat, testdat)

# intermediate clean up
rm(ytest,ytrain, subtest, subtrain, testdat, traindat, laf)


######################################################################

## Step 2.
## Extract only the measurements on the mean and standard deviation 
## for each measurement. 

# Guessing that the frequency domain variables are not actual
# measurements.. so extract only the time domain variables on the
# mean and std measurements.

# regex pattern for selecting the time domain column names
pattern <- "^t.*(mean|std)"

# grep the values using pattern
reqcols <- grep("^t.*(mean|std)", colnames[[2]], value=T)

# we still want the subject and label columns 
reqcols <- c(reqcols, c("subjectid", "labelid"))

# subsetting the dataset from step 1.
dataset.step2 <- dataset.step1[,reqcols]

# intermediate clean up
rm(colnames, pattern, reqcols, dataset.step1)


######################################################################

## Step 3.
## Use descriptive activity names to name the activities in 
## the data set.

# This means joining the dataset from step 2 to the 'activity labels'
# table in file './data/dataset/activity_labels.txt'.

# Load the activity_labels.txt file 
actlabls <- read.table("./data/dataset/activity_labels.txt", sep = ""
                        , header = F, colClasses = c("integer", "character")
                        , col.names = c("labelid", "label")
                        , stringsAsFactors = F)

# merge using (default left join) join function in plyr package
# (plyr join preserves original ordering as opposed to base merge; 
# might be useful for time series data?)
dataset.step3 <- join(dataset.step2,actlabls)
# NOTE : this command will print a 'Joining by: labelid' message.


# column labelid is now redundant, dropping.. 
dataset.step3$labelid <- NULL

# intermediate clean up
rm(actlabls, dataset.step2)


######################################################################

## Step 4.
## Appropriately label the data set with descriptive variable names. 

# The columns names already contain descriptive names (from the features.txt file).
# Only removing the parentheses..
pattern <- "\\(\\)" # escaping the metacharacters ( ).

# removing parantheses
colnames <- sub("\\(\\)", "", names(dataset.step3))

# rename label column to activity
colnames <- sub("label", "activity", colnames)

# setting the new names vector
names(dataset.step3) <- colnames

# 'renaming' the dataset variable ..
dataset.step4 <- dataset.step3

# intermediate clean up
rm(pattern, dataset.step3, colnames)


######################################################################

## Step 5.
## From the data set in step 4, create a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

# first tidy the data set by melting the measurement columns..
result <- melt(dataset.step4, id=c("subjectid","activity")
               , variable.name="signal", value.name="value")


# summarize the data by variable group subjectid, activity, signal
result <- ddply(result, .(subjectid, activity, signal), summarize, avg = mean(value))

# write out to file
write.table(result, file="./result_step5.txt", row.name=F)






