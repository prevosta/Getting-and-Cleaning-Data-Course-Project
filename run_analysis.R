extractName<-function()
{
	path <- ".\\UCI HAR Dataset"
	my_name <- read.csv(file = paste(path, "features.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_name) <- c("ID","NAME")
	my_name
}		
	
extractTest<-function(my_name)
{
	path <- ".\\UCI HAR Dataset\\test"
	list.files(path)
	
	my_data <- read.fwf(file = paste(path, "X_test.txt", sep = "\\"), widths = rep(c(15,-1), 4), head = FALSE )
	colnames(my_data) <- my_name$NAME[1:4]
	
	my_data_subject <- read.csv(file = paste(path, "subject_test.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_subject) <- c("subject")
	my_data <- cbind(my_data, my_data_subject)
	
	my_data_y <- read.csv(file = paste(path, "y_test.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_y) <- c("y")
	my_data <- cbind(my_data, my_data_y)
}

extractTraining<-function(my_name)
{
	path <- ".\\UCI HAR Dataset\\train"
	list.files(path)
	
	my_data <- read.fwf(file = paste(path, "X_train.txt", sep = "\\"), widths = rep(c(15,-1), 561), head = FALSE )
	colnames(my_data) <- my_name$NAME[1:561]
	
	my_data_subject <- read.csv(file = paste(path, "subject_train.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_subject) <- c("subject")
	my_data <- cbind(my_data, my_data_subject)
	
	my_data_y <- read.csv(file = paste(path, "y_train.txt", sep = "\\"), sep = " ", head=FALSE)
	colnames(my_data_y) <- c("y")
	my_data <- cbind(my_data, my_data_y)
}

my_name <- extractName()

# Merges the training and the test sets to create one data set.
test_dataset <- extractTest(my_name)
training_dataset <- extractTraining(my_name)
dataset <- rbind(test_dataset, training_dataset)

# Extracts only the measurements on the mean and standard deviation for each measurement.
std_name_col <- unlist( mapply(grep, "std()", my_name, fixed = TRUE) )
mean_name_col <- unlist( mapply(grep, "mean()", my_name, fixed = TRUE) )
good_name_col <- c( std_name_col, mean_name_col, "subject", "y" )
dataset <- dataset[,good_name_col]

# Uses descriptive activity names to name the activities in the data set
# [DONE]

# Appropriately labels the data set with descriptive variable names.
# [DONE]

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- 



dataset[,1] <- as.factor(dataset[,1])

aggregate(dataset[,1], by=list(y = dataset[,5]), FUN=mean, na.rm=TRUE)

	

View(dataset)
