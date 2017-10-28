# This program "run_analysis.R" tasked with getting and cleaning the dataset shared for this assignment
# There are two datasets shared for this assignment 1) test dataset 2) train dataset
# Objective is to clean and load the test and train datasets and combine them into a single dataset
# From the dataset, extract only the measurements on the mean and standard deviation for each measurement
# And Evaluate average of each variable for each activity and each subject and write into a new file

#loading library required for this program
library(dplyr)

#Read test dataset
#get working directory of my machine
wd <- "/Users/RajesSahaa/Documents/Learn R/Practice R"
if(file.exists(paste0(wd,"/UCI HAR Dataset/test"))) {
        setwd(paste0(wd,"/UCI HAR Dataset/test"))
        num_vec <- numeric(length=561)
        subject_test <- read.csv("subject_test.txt",sep="",header=FALSE)
        num_vec <- c(-1, rep(c(15,-1),times=561))
        test_data <- read.fwf("X_test.txt",num_vec,header=FALSE)
        test_labels <- read.csv("y_test.txt",sep="",header=FALSE)
}

#resetting working directory
setwd(wd)

#Read train dataset
if(file.exists(paste0(wd,"/UCI HAR Dataset/train"))) {
        setwd(paste0(wd,"/UCI HAR Dataset/train"))
        num_vec <- numeric(length=561)
        subject_train <- read.csv("subject_train.txt",sep="",header=FALSE)
        num_vec <- c(-1, rep(c(15,-1),times=561))
        train_data <- read.fwf("X_train.txt",num_vec,header=FALSE)
        train_labels <- read.csv("y_train.txt",sep="",header=FALSE)
}

#resetting working directory
setwd(wd)

#Read activity labels
activity_labels <- read.csv(paste0(wd,"/UCI HAR Dataset/activity_labels.txt"),sep="",header=FALSE,stringsAsFactors = FALSE)

#Read features - column names - various types of measurements made using the device
column_names <- read.csv(paste0(wd,"/UCI HAR Dataset/features.txt"),sep="",header=FALSE,stringsAsFactors = FALSE)

#test dataset - Assign column name "Subject" who did the activity
names(subject_test) <- c("Subject")

#test dataset - assign column name "Acivity" for various activities performed by the subject
names(test_labels) <- c("Activity")

#test dataset - create a copy of the "Activity" to capture the description
test_labels$Activity_Description <- test_labels$Activity

#train dataset - Assign column name "Subjects" who did the activity
names(subject_train) <- c("Subject")

#train dataset - assign column name "Acivity" for various activities performed by the subject
names(train_labels) <- c("Activity")

#train dataset - create a copy of the "Activity" to capture the description
train_labels$Activity_Description <- train_labels$Activity

#test dataset - combing subject, activity and test data into single dataframe
df_test <- cbind(subject_test,test_labels,test_data)

#train dataset - combing subject, activity and test data into single dataframe
df_train <- cbind(subject_train,train_labels,train_data)

#combing consolidated test and train dataset into one single dataset
df_full <- rbind(df_test,df_train)

#cleanup unused varibles to free up memory
rm(df_test,df_train,train_labels,test_labels,wd,subject_test,subject_train,num_vec,test_data,train_data)

#update Activity names to the combined dataset
df_full[,"Activity_Description"] <- activity_labels[df_full[, "Activity_Description"], 2]

#assign column names to combined dataset
names(df_full) <- c("Subject","Activity","Activity_Description",column_names$V2)

mean_std_variables <- grep("mean|std",names(df_full))
df_full <- df_full[,c(1,2,3,mean_std_variables)]

#cleanup unused varibles to free up memory
rm(activity_labels,mean_std_variables,column_names)


#group by Subject, Activity and Activity Description and summarise the results and writing the result set to the new CSV file
df_full %>% arrange(order(Subject,Activity)) %>% group_by(Subject,Activity,Activity_Description) %>% summarise_all(funs(mean),cols=names(df_full)[4:length(names(df_full))]) %>% ungroup() %>% select(1:length(names(df_full)),-Activity) %>% write.csv(file="tidy_data.csv")

#cleanup unused variables to free up memory
rm(df_full)
