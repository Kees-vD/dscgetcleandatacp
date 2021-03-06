---
title: "CodeBook"
author: "Kees"
date: "Sunday, November 23, 2014"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

# Description of the script output data file

## Columns

The resulting data set is built up as follows:
```
'data.frame':      7200 obs. of  4 variables:  
 $ subjectid: int  1 1 1 1 1 1 1 1 1 1 ...  
 $ activity : chr  "LAYING" "LAYING" "LAYING" "LAYING" ...  
 $ signal   : Factor w/ 40 levels "tBodyAcc-mean-X",..: 1 2 3 4 5 6 7 8 9 10 ...  
 $ avg      : num  0.2216 -0.0405 -0.1132 -0.9281 -0.8368 ...  
```

Possible values for the variable 'activity' are:
```
"LAYING"             "SITTING"            "STANDING"           "WALKING"           
"WALKING_DOWNSTAIRS" "WALKING_UPSTAIRS"
```

The variable 'signal' is a factor with the following levels:
```
 [1] "tBodyAcc-mean-X"       "tBodyAcc-mean-Y"       "tBodyAcc-mean-Z"      
 [4] "tBodyAcc-std-X"        "tBodyAcc-std-Y"        "tBodyAcc-std-Z"       
 [7] "tGravityAcc-mean-X"    "tGravityAcc-mean-Y"    "tGravityAcc-mean-Z"   
[10] "tGravityAcc-std-X"     "tGravityAcc-std-Y"     "tGravityAcc-std-Z"    
[13] "tBodyAccJerk-mean-X"   "tBodyAccJerk-mean-Y"   "tBodyAccJerk-mean-Z"  
[16] "tBodyAccJerk-std-X"    "tBodyAccJerk-std-Y"    "tBodyAccJerk-std-Z"   
[19] "tBodyGyro-mean-X"      "tBodyGyro-mean-Y"      "tBodyGyro-mean-Z"     
[22] "tBodyGyro-std-X"       "tBodyGyro-std-Y"       "tBodyGyro-std-Z"      
[25] "tBodyGyroJerk-mean-X"  "tBodyGyroJerk-mean-Y"  "tBodyGyroJerk-mean-Z" 
[28] "tBodyGyroJerk-std-X"   "tBodyGyroJerk-std-Y"   "tBodyGyroJerk-std-Z"  
[31] "tBodyAccMag-mean"      "tBodyAccMag-std"       "tGravityAccMag-mean"  
[34] "tGravityAccMag-std"    "tBodyAccJerkMag-mean"  "tBodyAccJerkMag-std"  
[37] "tBodyGyroMag-mean"     "tBodyGyroMag-std"      "tBodyGyroJerkMag-mean"
[40] "tBodyGyroJerkMag-std" 
```

The column 'avg' is the average of each variable for each activity and each subject.

## Signal variable
The signal data was generated by experiments that have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity is captured. The 


* Signal type ..Acc.. : accelaration in X/Y/Z axis (standard gravity units 'g').
* Signal type ..Gyro..: angular velocity in X/Y/Z axis (radians/second).

For each type of measurement the mean and standard deviation (std) are available.
