
#(1) Merges the training and the test sets to create one data set.
setwd("~/GitProjects/GettingAndCleaningData/UCI HAR Dataset");

# Importing training data from sourcefiles 
# Naming the columns :training data
features <- read.table('./features.txt',header=FALSE);
activity_labels <- read.table('./activity_labels.txt',header=FALSE); 
colnames(activity_labels) <- c("activity_id","activity_type");
subject_train <- read.table('./train/subject_train.txt',header=FALSE); 
colnames(subject_train) <- "subject_id";
X_train <- read.table('./train/x_train.txt',header=FALSE); 
colnames(X_train) <- features[,2];
Y_train <- read.table('./train/y_train.txt',header=FALSE); 
colnames(Y_train) <- "activity_id";

# Merging data to training_set
training_set = cbind(Y_train,subject_train,X_train);

# Importing test data from sourcefiles
# Naming the columns: test data
subject_test <- read.table('./test/subject_test.txt',header=FALSE); 
colnames(subject_test) <- "subject_id";
x_test <- read.table('./test/x_test.txt',header=FALSE); 
colnames(x_test) <- features[,2];
y_test <- read.table('./test/y_test.txt',header=FALSE); 
colnames(y_test) <- "activity_id";

# Merging data to test_set
test_set = cbind(y_test,subject_test,x_test);

# Merging training_set and test_set
merged_data = rbind(training_set,test_set);


columns <- colnames(merged_data);

##########################################################################



#(2) Extracts only the measurements on the mean and standard deviation for each measurement.

# Vector for the ID, mean and standard deviation columns as TRUE
vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) & !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | grepl("-std..",columns) & !grepl("-std()..-",columns));

# Updating merged_data 
merged_data <- merged_data[vector==TRUE];

########################################################################

#(3) Uses descriptive activity names to name the activities in the data set

# Adding the  descriptive activity names to merged_data 
# Update columns vector
merged_data <- merge(merged_data,activity_labels,by='activity_id',all.x=TRUE);
merged_data$activity_id <-activity_labels[,2][match(merged_data$activity_id, activity_labels[,1])] 

columns <- colnames(merged_data);

####################################################################################	
# (4) Appropriately label the data set with descriptive activity names.

# Tidy Data Set columns
for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
};

# Updating merged_data 
colnames(merged_data) <- columns;


merged_data <- merged_data[,names(merged_data) != 'activity_type'];
######################################################################################	
#(5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Creating the tidy_data_set file
tidy_data_set <- aggregate(merged_data[,names(merged_data) != c('activity_id','subject_id')],by=list (activity_id=merged_data$activity_id, subject_id=merged_data$subject_id),mean);


write.table(tidy_data_set, '../TidyData.txt',row.names=FALSE,sep='\t')


