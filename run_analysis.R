
run_analysis <- function() {
require("dplyr")


#get the descriptive column names
features <- read.table("features.txt")
colnames(features) <- c("colnum", "colname")

X_test <- read.table("test/X_test.txt") #test set
#make the column name meaningful (this is for step 4)
colnames(X_test) <- features[,2]

Y_test <- read.table("test/y_test.txt") #test labels
#label the one column here actId
colnames(Y_test) <- "actId"

subject_test <- read.table("test/subject_test.txt")
#label the one column here Subject
colnames(subject_test) <- "Subject"

#Combine the columns in these 3 together
test_result <- cbind(subject_test, Y_test)
test_result <- cbind(test_result, X_test)


########################################################
X_train <- read.table("train/X_train.txt") # training set
#make the column name meaningful (this is for step 4)
colnames(X_train) <- features[,2]

Y_train <- read.table("train/y_train.txt") # training labels
#label the one column here actId
colnames(Y_train) <- "actId"

subject_train <- read.table("train/subject_train.txt")
#label the one column here Subject
colnames(subject_train) <- "Subject"

#Combine the columns in these 3 together
train_result <- cbind(subject_train, Y_train)
train_result <- cbind(train_result, X_train)


#####################################################
#STEP 1: merge two sets
result <- rbind(train_result, test_result) #10299 rows

###############################################################
#STEP 2:  Extracts only the measurements on the mean 
# and standard deviation for each measurement. 
result1 <- result[,c("Subject", "actId", colnames(result)[grep("-mean()|-std()", colnames(result))])]

##################################################################
#STEP 3:  Uses descriptive activity names to name the activities in the data set
# merge with activity
activity_labels <- read.table("activity_labels.txt")
colnames(activity_labels) <- c("actId", "activity")

result1 <- merge(result1, activity_labels, by="actId")
##################################################################

# STEP 5:  From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.


##################################################################

result2 <- result1 %>% 
group_by(Subject, activity) %>% 
summarise_each(funs(mean))

##write to file with row.names=FALSE
write.table(result2, file="HNguyen_wearable_Coursera_getdata-014.txt", row.names=FALSE)

return(result2)
}

