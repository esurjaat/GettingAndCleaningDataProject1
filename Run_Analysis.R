#Steps 1-4

#Columns of measurement variables
##Reading in column names and ommiting unwanted variables, keeping only those 
##that have a mean and standard deviation measurement
ColumnNames <- read.table("~/Desktop/UCI HAR Dataset/features.txt")[,-1]
ColumnNames <- ColumnNames[c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,
                             227:228,240:241,253:254,266:271,345:350,424:429,
                             503:504,516:517,529:530,542:543)]
##Renaming the column names
ColumnNames <- c("tBodyAcc_XMean", "tBodyAcc_YMean", "tBodyAcc_ZMean", 
                 "tBodyAcc_XStd", "tBodyAcc_YStd", "tBodyAcc_ZStd",
                 "tGravityAcc_XMean", "tGravityAcc_YMean", "tGravityAcc_ZMean",
                 "tGravityAcc_XStd", "tGravityAcc_YStd", "tGravityAcc_ZStd", 
                 "tBodyAccJerk_XMean", "tBodyAccJerk_YMean", 
                 "tBodyAccJerk_ZMean", "tBodyAccJerk_XStd", "tBodyAccJerk_YStd",
                 "tBodyAccJerk_ZStd", "tBodyGyro_XMean", "tBodyGyro_YMean", 
                 "tBodyGyro_ZMean", "tBodyGyro_XStd", "tBodyGyro_YStd", 
                 "tBodyGyro_ZStd", "tBodyGyroJerk_XMean", "tBodyGyroJerk_YMean",
                 "tBodyGyroJerk_ZMean", "tBodyGyroJerk_XStd", 
                 "tBodyGyroJerk_YStd", "tBodyGyroJerk_ZStd", 
                 "tBodyAccMag_Mean", "tBodyAccMag_Std", "tGravityAccMag_Mean",
                 "tGravityAccMag_Std", "tBodyAccJerkMag_Mean", 
                 "tBodyAccJerkMag_Std", "tBodyGyroMag_Mean", "tBodyGyroMag_Std",
                 "tBodyGyroJerkMag_Mean", "tBodyGyroJerkMag_Std", 
                 "fBodyAcc_XMean", "fBodyAcc_YMean", "fBodyAcc_ZMean",
                 "fBodyAcc_XStd", "fBodyAcc_YStd", "fBodyAcc_ZStd",
                 "fBodyAccJerk_XMean", "fBodyAccJerk_YMean", 
                 "fBodyAccJerk_ZMean", "fBodyAccJerk_XStd", "fBodyAccJerk_YStd",
                 "fBodyAccJerk_ZStd", "fBodyGyro_XMean", "fBodyGyro_YMean", 
                 "fBodyGyro_ZMean", "fBodyGyro_XStd", "fBodyGyro_YStd", 
                 "fBodyGyro_ZStd", "fBodyAccMag_Mean", "fBodyAccMag_Std", 
                 "fBodyAccJerkMag_Mean", "fBodyAccJerkMag_Std", 
                 "fBodyGyroMag_Mean", "fBodyGyroMag_Std", 
                 "fBodyGyroJerkMag_Mean", "fBodyGyroJerkMag_Std")

#Test Dataset
##Reading in ID Numbers
TestSubjects <- read.table("~/Desktop/UCI HAR Dataset/test/subject_test.txt")
##Reading in test observations and ommitting unwanted variables, keeping only
##those that have a mean and standard deviation measurement
Test <- read.table("~/Desktop/UCI HAR Dataset/test/X_test.txt")
Test <- Test[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,
                  240:241,253:254,266:271,345:350,424:429,503:504,516:517,
                  529:530,542:543)]
##Reading in Activity Descriptions and changing it to be descriptive
TestActivity <- read.table("~/Desktop/UCI HAR Dataset/test/y_test.txt")
TestExercise <- TestActivity[,1]
TestExercise <- factor(TestExercise)
levels(TestExercise) <- c("Walking", "Walking_Upstairs", "Walking_Downstairs", 
                          "Sitting", "Standing", "Laying")
TestExercise <- as.data.frame(TestExercise)
names(TestExercise) <- "Exercise"
##Naming the columns
colnames(Test) <- ColumnNames


#Train Dataset
##Reading in ID Numbers
TrainSubjects <- read.table("~/Desktop/UCI HAR Dataset/train/subject_train.txt")
##Reading in train observations and ommitting unwanted variables, keeping only
##those that have a mean and standard deviation measurement
Train <- read.table("~/Desktop/UCI HAR Dataset/train/X_train.txt")
Train <- Train[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,
                  240:241,253:254,266:271,345:350,424:429,503:504,516:517,
                  529:530,542:543)]
##Reading in Activity Descriptions and changing it to be descriptive
TrainActivity <- read.table("~/Desktop/UCI HAR Dataset/train/y_train.txt")
TrainExercise <- TrainActivity[,1]
TrainExercise <- factor(TrainExercise)
levels(TrainExercise) <- c("Walking", "Walking_Upstairs", "Walking_Downstairs",
                           "Sitting", "Standing", "Laying")
TrainExercise <- as.data.frame(TrainExercise)
names(TrainExercise) <- "Exercise"
##Naming the columns
colnames(Train) <- ColumnNames


#Putting it all together
##Adding in the corresponding exercises for each row for Test and Train Datasets
##(separately)
Test <- cbind(TestExercise, Test)
Train <- cbind(TrainExercise,Train)
##Adding the ID numbers as a column for Test and Train Datasets (separately)
Test <- cbind(TestSubjects, Test)
Train <- cbind(TrainSubjects, Train)
##Combining the Test and Training Dataset
Data <- rbind(Test,Train)
##Naming the ID and Exercise Columns
colnames(Data)[1] <- "ID"
##Print sample of results
print(Data[1:100,1:10])


#Step 5.

#Creating a Separate tidy data set with the average of each variable for each
#activity and each subject.

##Creating an empty data frame to bind on mean values for all measurement
##columns to bind onto. Sorted by Exercises then ID
Tidy <- data.frame()
for(i in 1:30){
       Tidy <- rbind(Tidy,summarise_each(Data[Data$ID==i&Data$Exercise=="Walking",],funs(mean)))
}
for(i in 1:30){
        Tidy <- rbind(Tidy,summarise_each(Data[Data$ID==i&Data$Exercise=="Walking_Upstairs",],funs(mean)))
}
for(i in 1:30){
        Tidy <- rbind(Tidy,summarise_each(Data[Data$ID==i&Data$Exercise=="Walking_Downstairs",],funs(mean)))
}
for(i in 1:30){
        Tidy <- rbind(Tidy,summarise_each(Data[Data$ID==i&Data$Exercise=="Sitting",],funs(mean)))
}
for(i in 1:30){
        Tidy <- rbind(Tidy,summarise_each(Data[Data$ID==i&Data$Exercise=="Standing",],funs(mean)))
}
for(i in 1:30){
        Tidy <- rbind(Tidy,summarise_each(Data[Data$ID==i&Data$Exercise=="Laying",],funs(mean)))
}
##Renaming the Exercises in the Exercise Column
Tidy[[2]] <- c(rep("Walking", 30), rep("Walking_Upstairs", 30), 
               rep("Walking_Downstairs", 30), rep("Sitting", 30), 
               rep("Standing", 30), rep("Laying", 30))
##Print sample of results
print(Tidy[1:100,1:10])