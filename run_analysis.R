
#1 Merges the training and the test sets to create one data set.
train1<-read.table("data/train/X_train.txt")
test1<-read.table("data/test/X_test.txt")
data1 <- rbind(train1, test1)


train2<-read.table("data/train/y_train.txt")
test2<-read.table("data/test/y_test.txt")
data2 <- rbind(train2, test2)

subj1<-read.table("data/train/subject_train.txt")
subj2<-read.table("data/test/subject_test.txt")
subject<-rbind(subj1,subj2)

#2 Extracts only the measurements on the mean and standard deviation for each measurement
features<-read.table("data/features.txt")
Ifeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
data1<-data1[,Ifeatures]
names(data1) <- features[Ifeatures, 2]
names(data1) <- gsub("\\(|\\)", "", names(data1))
names(data1) <- tolower(names(data1))

#3 Uses descriptive activity names to name the activities in the data set
activities <- read.table("data/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
data2[,1] = activities[data2[,1], 2]
names(data2) <- "activity"

#4 Appropriately labels the data set with descriptive variable names. 
names(subject) <- "subject"
clean <- cbind(subject, data2, data1)
write.table(clean, "data/merged_clean_data.txt")

#5 From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
USubjects = unique(subject)[,1]
NoSubjects = length(unique(subject)[,1])
NoActivities = length(activities[,1])
numCols = dim(clean)[2]
result = clean[1:(NoSubjects*NoActivities), ]

row = 1
for (s in 1:NoSubjects) {
        for (a in 1:NoActivities) {
                result[row, 1] = USubjects[s]
                result[row, 2] = activities[a, 2]
                tmp <- clean[clean$subject==s & clean$activity==activities[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, "data_with_the_averages.txt")
