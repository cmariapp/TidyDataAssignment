run_analysis<- function(){

	library("reshape2")

	##set the working directory
	setwd("UCI HAR Dataset")

	##read all the data files for activities, subject and measurements from training and test data files. Also read the description of features 

	##read the feature names
	features <- read.table("features.txt")
	feature_names <-  features[,2]
	feature_names<- gsub("\\(\\)", "", feature_names)

	##read the activity_lables
	activity_labels <- read.table("activity_labels.txt",col.names=c("activity_id","activity_name"))


	##read training data files from train folder
	
	#list.files()

	x_train<- read.table("./train/X_train.txt")
	#head(x_train)
	#str(x_train)
	#tail(x_train)

	y_train<- read.table("./train/y_train.txt")
	#str(y_train)
 
	subject_train<- read.table("./train/subject_train.txt")
	#str(subject_train)

	#read test data files from test folder

	x_test<- read.table("./test/X_test.txt")
	#str(x_test)
 
	y_test<- read.table("./test/y_test.txt")
	#str(y_test)
 
	subject_test<- read.table("./test/subject_test.txt")
 
	#combine all columns from the training data frames for activity Id, subject Id and data
	train_data <- cbind(y_train, subject_train, x_train)
	#str(train_data)

	#combine all columns from the test data frames for activity Id, subject Id and data
	test_data <- cbind(y_test, subject_test, x_test)
	#str(test_data)

	#combine all rows from training and test data frames 
	combined <- rbind(train_data, test_data)
	#str(combined)

	#'data.frame':   10299 obs. of  563 variables:
 
	#set the readable column names 
	colnames(combined) <- c("activity_id", "subject_id", as.character(feature_names))

	#get the subset of mean and std devaitaion related columns 
	mean_col_idx <- grep("mean",names(combined),ignore.case=TRUE)
	mean_col_names <- names(combined)[mean_col_idx]
         
	std_col_idx <- grep("std",names(combined),ignore.case=TRUE)
	std_col_names <- names(combined)[std_col_idx]
         
	meanstddata <- combined[,c("activity_id","subject_id",mean_col_names,std_col_names)]
         
	#change the activity ids with factor names from activity table
	meanstddata$activity_id <- factor(meanstddata$activity_id, labels=as.character(activity_labels[,2]))  
	colnames(meanstddata)[1] <- c("activity_name")

	write.table(meanstddata,"../UCI_HAR_meanstd_data.txt", row.name=FALSE)

	#melt the data by having activity name and subject id. 	
	data_melt <- melt(meanstddata,id=c("activity_name","subject_id"))
	write.table(data_melt,"../UCI_HAR_melted_data.txt", row.name=FALSE)
         
	##Cast the melted dataset as per the average of each variable for each activity and each subject
	mean_data <- dcast(data_melt,activity_name + subject_id ~ variable,mean)
        
	## Create a file with the new tidy dataset
	write.table(mean_data,"../UCI_HAR_tidy_data.txt",row.name=FALSE)
	View(read.table("../UCI_HAR_tidy_data.txt"))
	
	#reset working directory
	setwd("..")

}