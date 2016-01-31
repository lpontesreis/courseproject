#read files to data set

#creates path to the files
path <- "C:/DataScience"

#first, create a vector with the names of the files

trainFiles <- c("body_acc_x_train", "body_acc_y_train", "body_acc_z_train", 
                "body_gyro_x_train", "body_gyro_y_train", "body_gyro_z_train", 
                "total_acc_x_train", "total_acc_y_train", "total_acc_y_train" )


testFiles <- c("body_acc_x_test", "body_acc_y_test", "body_acc_z_test", 
               "body_gyro_x_test", "body_gyro_y_test", "body_gyro_z_test", 
               "total_acc_x_test", "total_acc_y_test", "total_acc_y_test" )


#read file from the train set
#first, subject, then labels, and then the file

#subject
filePath <- paste(path, "/UCI HAR Dataset/train/subject_train.txt", sep = "")
dataset_train <- read.table(filePath)

#labels
filePath <- paste(path, "/UCI HAR Dataset/train/y_train.txt", sep = "")
temp_dataset <- read.table(filePath)
dataset_train <- cbind(dataset_train, temp_dataset) #merge with subject
rm(temp_dataset)

#file
filePath <- paste(path,"/UCI HAR Dataset/train/X_train.txt", sep = "")
temp_dataset <- read.table(filePath)
dataset_train <- cbind(dataset_train, temp_dataset) #merge with subject and labels
rm(temp_dataset)


#read files from the train set Inertial Signals

for (fileName in trainFiles) {
  filePath <- paste(path, "/UCI HAR Dataset/train/Inertial Signals/", fileName, ".txt", sep ="")
  if (!exists("dataset_train")){
    dataset_train <- read.table(filePath)
  }
  else {
    temp_dataset <- read.table(filePath)
    dataset_train <- cbind(dataset_train, temp_dataset)
    rm(temp_dataset)
  }
}

#reads from the test set

#subject
filePath <- paste(path, "/UCI HAR Dataset/test/subject_test.txt", sep = "")
dataset_test <- read.table(filePath)

#labels
filePath <- paste(path, "/UCI HAR Dataset/test/y_test.txt", sep = "")
temp_dataset <- read.table(filePath)
dataset_test <- cbind(dataset_test, temp_dataset) #merge with subject
rm(temp_dataset)

#file
filePath <- paste(path,"/UCI HAR Dataset/test/X_test.txt", sep = "")
temp_dataset <- read.table(filePath)
dataset_test <- cbind(dataset_test, temp_dataset) #merge with subject and labels
rm(temp_dataset)



#read files from the test set to a matrix
for (fileName in testFiles) {
  filePath <- paste(path, "/UCI HAR Dataset/test/Inertial Signals/", fileName, ".txt", sep ="")
  if (!exists("dataset_test")){
    dataset_test <- read.table(filePath)
  }
  else {
    temp_dataset <- read.table(filePath)
    dataset_test <- cbind(dataset_test, temp_dataset)
    rm(temp_dataset)
  }
}

#converts matrix to data frame
dataset <- as.data.frame(rbind(dataset_train, dataset_test))

#removes the other matrices
rm(dataset_train)
rm(dataset_test)

#names the data frame columns
filePath <- paste(path, "/UCI HAR Dataset/features.txt", sep = "")
features <- read.table(filePath, stringsAsFactors = FALSE)
featuresNames <- features[,2]

names <- c("body_acc_x", "body_acc_y", "body_acc_z", 
                       "body_gyro_x", "body_gyro_y", "body_gyro_z", 
                       "total_acc_x", "total_acc_y", "total_acc_y")
freqReadings <- c()

for (name in names) {
  for (i in 1:128) #128 readings for each one feature
    freqReadings <- c(freqReadings, paste(name, "_", i, sep = ""))
}

colnames(dataset) <- c("subject", "activity", featuresNames, freqReadings)

#Replace the activities for the names
filePath <- paste(path, "/UCI HAR Dataset/activity_labels.txt", sep = "")
activities <- read.table(filePath)
activities <- activities[,2]
dataset$activity <-activities[as.factor(dataset$activity)]

rm(features)

#now we have a very big dataset with 10,299 observations and 1,715 variables; let's trim it down
#let's keep the first and second columns, and the ones with "mean()" and "std()"
dataset <- cbind(dataset[,1:2], dataset[,grepl("mean()", names(dataset))|grepl("std()", names(dataset))])

#create an independent tidy dataset with the average of each variable for each activity and each subject
for (sub in 1:30) {
  for (act in activities) {
    dataset_new_temp <- subset(dataset, activity == act & subject == sub)
    dataset_new_temp <- cbind(data.frame(subject = sub, activity = act) , as.data.frame.list(colMeans(dataset_new_temp[,3:81])))
    
    if(!exists("dataset_new")) {
      dataset_new <- dataset_new_temp
    }
    else {
      dataset_new <- rbind(dataset_new, dataset_new_temp)
      rm(dataset_new_temp)
    }
  }
}

write.table(dataset_new, "tidydataset.txt", row.name = FALSE)