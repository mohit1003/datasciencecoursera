########################################################################

# Using data obtained from the accelelerometers of Samsung Galaxy S smartphone, 
# Working with the data and making a tidy data set
# produced in "tidy_data.txt" 
# See README.md for more details

########################################################################

library(dplyr)

# Step 0.1: Get data

#Download the zip file if not already exists
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFileName <- "UCI HAR Dataset.zip"

if (!file.exists(zipFileName)) {
  download.file(zipUrl, zipFileName, mode = "wb")
}

# unzip the file containing the data

dataPath <- "UCI HAR Dataset"

if(!file.exists(dataPath)){
  unzip(zipFileName)
}

# Step 0.2: Read Data

# Reading training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# Reading Test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# Read features. (no labels to factors conversion)
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# Read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "Label")


##############################################################################
# Step 1: Merge the training and the test sets to create one data set
##############################################################################

# concatinate individual data tables to make a table
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# saving memory :)
rm(trainingSubjects, trainingValues, trainingActivity, testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[,2], "activity")
colnames(humanActivity)

###################################################################################################
#step 2: Extract only the measurements on the mean and standard deviation for each measurement.
###################################################################################################

# Determine the columns of the data set to keep based on column name
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]
#colnames(humanActivity)

###################################################################################################
#step 3: Use descriptive activity names to name the activities in the data set
###################################################################################################
#View(head(humanActivity$activity))
humanActivity$activity <- factor(humanActivity$activity, levels = activities[, 1], labels = activities[,2])
#View(head(humanActivity$activity))


###################################################################################################
#step 4: Appropriately label the data set with descriptive variable names
###################################################################################################


# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up the names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# put right typos
humanActivityCols <- gsub("BodyBody", "body", humanActivityCols)

# update colnames
colnames(humanActivity) <- humanActivityCols


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
 #     variable for each activity and each subject
##############################################################################

# group by subjects and activity and summarise using mean
humanActivityMean <- humanActivity %>% group_by(subject, activity) %>%
                      summarise_all(mean)

# Output into "tidy_data.txt"
write.table(humanActivityMean, "tidy_data.txt", row.names = FALSE, quote = FALSE)