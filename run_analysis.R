# Step1. Merges the training and the test sets to create one data set.
setwd("D:/Downloads/Downloads Old/Downloads/Data Science Specialization/Getting and Cleaning Data/Quiz/peer/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
train.Data <- read.table("train/X_train.txt")
dim(trainData) 
# The dimension of traindata is 7352*561
train.Label <- read.table("train/y_train.txt")
table(train.Label)
train.Subject <- read.table("train/subject_train.txt")
test.Data <- read.table("test/X_test.txt")
dim(test.Data) 
# Dimension of dataset is 2947 by 561
test.Label <- read.table("test/y_test.txt")
table(test.Label)
test.Subject <- read.table("test/subject_test.txt")
join.Data <- rbind(train.Data, test.Data)
dim(join.Data) 
# Dimension of dataset is 10299 by 561
join.Label <- rbind(train.Label, test.Label)
dim(join.Label) 
# Dimension of dataset is 10299 by 1
bind.Subject <- rbind(train.Subject, test.Subject)
dim(bind.Subject)

# Step2. Extracts only the measurements on the mean and standard
# deviation for each measurement.
features <- read.table("features.txt")
dim(features) 
# Dimension of dataset is 561 by 2
mean.Std <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(mean.Std) # 66
join.Data <- join.Data[, meanStd]
dim(join.Data) # 10299*66
names(join.Data) <- gsub("\\(\\)", "", features[meanStd, 2]) # remove "()"
names(join.Data) <- gsub("mean", "Mean", names(join.Data)) # capitalize M
names(join.Data) <- gsub("std", "Std", names(join.Data)) # capitalize S
names(join.Data) <- gsub("-", "", names(join.Data)) # remove "-" in column names

# Step3. Using descriptive activity names for naming the data set
activity <- read.table("activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activity.Label <- activity[join.Label[, 1], 2]
join.Label[, 1] <- activity.Label
names(join.Label) <- "activity"

# Now we label the data set with activity names.
names(bind.Subject) <- "subject"
modified.Data <- cbind(bind.Subject, join.Label, join.Data)

# Step5. Creates a tidy data set with the average of each variable for each activity and each subject.
subject.Len<-length(table(bind.Subject))
activity.Len<-dim(activity)[1]
column.Len<-dim(modified.Data)[2]
final<-as.data.frame(matrix(NA, nrow=subject.Len*activity.Len, ncol=column.Len))
colnames(final)<-colnames(modified.Data)
row <- 1
for(i in 1:subject.Len) {
  for(j in 1:activity.Len) {
    final[row, 1] <- sort(unique(bind.Subject)[, 1])[i]
    final[row, 2] <- activity[j, 2]
    indicator1 <- i == modified.Data$subject
    indicator2 <- activity[j, 2] == modified.Data$activity
    final[row, 3:column.Len] <- colMeans(modified.Data[indicator1 & indicator2, 3:column.Len])
    row <- row + 1
  }
}

# Creates the tidy dataset
write.table(final, "tidy_data.txt") 
