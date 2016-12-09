# This script reads in datafiles that were downloaded from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# The zipfile should be extracted into a directory
# The run_analysis function can be used to read in the downloaded datafiles contained in the zip,
# and obtain a tidy dataset from this data by merging and subsetting different files.

run_analysis <- function(){  
  #### part 1: Merge the training and the test sets to create one data set ####################################################    
  # Get features and activities  
  features <- read.table("UCI HAR Dataset/features.txt")  
  activities <- read.table("UCI HAR Dataset/activity_labels.txt")    
  # get test data and combine  
  test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")  
  test_x <- read.table("UCI HAR Dataset/test/X_test.txt")  
  test_y <- read.table("UCI HAR Dataset/test/y_test.txt")  
  test_data <- cbind(test_subjects,test_y,test_x)   
  # get training data and combine   
  train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")  
  train_x <- read.table("UCI HAR Dataset/train/X_train.txt") 
  train_y <- read.table("UCI HAR Dataset/train/y_train.txt")  
  train_data <- cbind(train_subjects,train_y,train_x)    
  #remove unused data  
  rm(test_x,test_y,test_subjects,train_x,train_y,train_subjects)    
  # merge datasets, set column names  
  allData <- rbind(train_data,test_data)  
  rm(train_data,test_data) 
  ncol(allData)  
  colnames(allData)[1] <- "subject"  
  colnames(allData)[2] <- "activity"  
  
  #### part 3: use activity names to label the activities #####################################################################  
  colnames(allData)[3:ncol(allData)] <- as.character(features[,2])     
  
  #### part 2: Extract only the measurements on the mean and standard deviation for each measurement ##########################  
  # get the wanted measurements/features (mean and sd of each measurement)  
  x <- grep("-(mean|std)\\(\\)",features[,2]) 
  w_features <- as.character(features[x,2])   
  #subset the data  
  sub <- subset(allData,select=c("subject","activity",w_features))  
  rm(allData)      
  
  #### part 4: appropriately label the data set with descriptive variable names #############################################  
  # set column names of activities  
  colnames(activities) <- c("activity","activity_name")    
  # replace activity numbers with activity name in all data  
  # merge the data  
  merged_dat <- merge(sub,activities, by.x="activity",by.y="activity")  
  rm(sub)  
  # replace the activity number column with descripition  
  merged_dat$activity <- merged_dat$activity_name  
  merged_dat$activity_name <- NULL       
  
  #### part 5. create a second, independent tidy data set with the average of each variable for each activity and subject ######  
  # Get vector of activities  
  activity_vec <- as.character(activities$activity_name)  
  # Get vector of subjects  
  subject_vec <- sort(unique(merged_dat$subject))    
  # Create (empty) data frame with same columns  
  tidy_set <- NULL      
  # for each activity and subject get the mean for each variable  
  for (act in activity_vec){    
    for(sub in subject_vec){      
      # create subset of activity and subject      
      x <- merged_dat[merged_dat$activity == act & merged_dat$subject == sub,]            
      # calculate the mean of all variables (use apply )      
      meandat <- apply(x[,3:ncol(x)],2,mean)      
      # add the activity and subject      
      meandat <- c(act,sub,meandat)           
      # add the vector to the new data frame      
      tidy_set <- rbind(tidy_set, meandat)      
    }  
  }   
  # write the tidy dataset to the work directory  
  write.table(tidy_set,"tidy_dataset.txt")
} 
