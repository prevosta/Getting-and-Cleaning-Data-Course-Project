extractDatasetNames<-function()
{
	path <- ".\\UCI HAR Dataset"
	my_name <- read.csv(file = paste(path, "features.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_name) <- c("ID","NAME") # Appropriately labels the data set with descriptive variable names.
	my_name
}		
	
extractTestDataset<-function(my_name)
{
	path <- ".\\UCI HAR Dataset\\test"
	list.files(path)
	
	my_data <- read.fwf(file = paste(path, "X_test.txt", sep = "\\"), widths = rep(c(15,-1), 4), head = FALSE )
	colnames(my_data) <- my_name$NAME[1:4] # Appropriately labels the data set with descriptive variable names.
	
	my_data_subject <- read.csv(file = paste(path, "subject_test.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_subject) <- c("subject") # Uses descriptive activity names to name the activities in the data set
	my_data <- cbind(my_data, my_data_subject)
	
	my_data_activity <- read.csv(file = paste(path, "y_test.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_activity) <- c("activity") # Uses descriptive activity names to name the activities in the data set
	my_data <- cbind(my_data, my_data_activity)
}

extractTrainingDataset<-function(my_name)
{
	path <- ".\\UCI HAR Dataset\\train"
	list.files(path)
	
	my_data <- read.fwf(file = paste(path, "X_train.txt", sep = "\\"), widths = rep(c(15,-1), 4), head = FALSE ) #561
	colnames(my_data) <- my_name$NAME[1:4] # Appropriately labels the data set with descriptive variable names.
	
	my_data_subject <- read.csv(file = paste(path, "subject_train.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_subject) <- c("subject") # Uses descriptive activity names to name the activities in the data set
	my_data <- cbind(my_data, my_data_subject)
	
	my_data_activity <- read.csv(file = paste(path, "y_train.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_activity) <- c("activity") # Uses descriptive activity names to name the activities in the data set
	my_data <- cbind(my_data, my_data_activity)
}

# Appropriately labels the data set with descriptive variable names.
my_name <- extractDatasetNames()

# Merges the training and the test sets to create one data set.
test_dataset <- extractTestDataset(my_name)
training_dataset <- extractTrainingDataset(my_name)
dataset <- rbind(test_dataset, training_dataset)

# Extracts only the measurements on the mean and standard deviation for each measurement.
std_name_col <- unlist( mapply(grep, "std()", my_name, fixed = TRUE) )
mean_name_col <- unlist( mapply(grep, "mean()", my_name, fixed = TRUE) )
good_name_col <- c( std_name_col, mean_name_col, 5, 6 )
mean_std_dataset <- dataset[,good_name_col[as.numeric(good_name_col)<5]]

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
dataset$activity <- as.factor(as.numeric(dataset$activity))
dataset$subject <- as.factor(as.numeric(dataset$subject))
tidy_dataset <- aggregate(mean_std_dataset, by=list(activity = dataset$activity, subject = dataset$subject), FUN=mean, na.rm=TRUE)


write.table(tidy_dataset, file = "tidy_dataset.txt", row.name = FALSE)
