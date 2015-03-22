run_analysis <- function()
{
library(plyr)
library(dplyr)
#read data for training set
xdata <- read.table("./UCI HAR Dataset/train/x_train.txt")
ydata <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjectdata <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(ydata) <- c("activity_labels")
names(subjectdata) <- c("subject")
xdata$id <- seq(along=xdata$V1)
ydata$id <- seq(along=ydata$activity_labels)
subjectdata$id <- seq(along=subjectdata$subject)
dfl = list(subjectdata,ydata,xdata)
mtrain <- join_all(dfl)
mtrain$Category <- "Train"

#read data for test set
xdata <- read.table("./UCI HAR Dataset/test/x_test.txt")
ydata <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjectdata <- read.table("./UCI HAR Dataset/test/subject_test.txt")


names(ydata) <- c("activity_labels")
names(subjectdata) <- c("subject")
xdata$id <- seq(along=xdata$V1)
ydata$id <- seq(along=ydata$activity_labels)
subjectdata$id <- seq(along=subjectdata$subject)
dfl = list(subjectdata,ydata,xdata)
mtest <- join_all(dfl)
mtest$Category <- "Test"

#Combine both data sets from train and test

tidyData <- rbind(mtrain,mtest)
rm(xdata)
rm(ydata)
rm(mtest)
rm(mtrain)

#tidyData
features <- read.table("./UCI HAR Dataset/features.txt")
valid_features <- features[grepl("(mean\\(\\)|std\\(\\))",features$V2),]
collist <- paste("V" ,valid_features$V1,sep="")
tidyData2 <- tidyData[c("subject","activity_labels","Category",collist)]
names(tidyData2) <- c("subject","activity","Category",collist)
colnames <- as.character(valid_features$V2)
names(tidyData2) <- c("subject","activity","Category",colnames)
tidyData4 <- group_by(tidyData2,subject,activity)
#x<- paste('\"',names(tidyData4),'\"','=',"mean(tidyData4$)\"",names(tidyData4),")",sep="")
#paste(x,collapse=',')

finaldata <- summarize(tidyData4,"tBodyAcc-mean()-X"=mean(tidyData4$"tBodyAcc-mean()-X"),"tBodyAcc-mean()-Y"=mean(tidyData4$"tBodyAcc-mean()-Y"),"tBodyAcc-mean()-Z"=mean(tidyData4$"tBodyAcc-mean()-Z"),"tBodyAcc-std()-X"=mean(tidyData4$"tBodyAcc-std()-X"),"tBodyAcc-std()-Y"=mean(tidyData4$"tBodyAcc-std()-Y"),"tBodyAcc-std()-Z"=mean(tidyData4$"tBodyAcc-std()-Z"),"tGravityAcc-mean()-X"=mean(tidyData4$"tGravityAcc-mean()-X"),"tGravityAcc-mean()-Y"=mean(tidyData4$"tGravityAcc-mean()-Y"),"tGravityAcc-mean()-Z"=mean(tidyData4$"tGravityAcc-mean()-Z"),"tGravityAcc-std()-X"=mean(tidyData4$"tGravityAcc-std()-X"),"tGravityAcc-std()-Y"=mean(tidyData4$"tGravityAcc-std()-Y"),"tGravityAcc-std()-Z"=mean(tidyData4$"tGravityAcc-std()-Z"),"tBodyAccJerk-mean()-X"=mean(tidyData4$"tBodyAccJerk-mean()-X"),"tBodyAccJerk-mean()-Y"=mean(tidyData4$"tBodyAccJerk-mean()-Y"),"tBodyAccJerk-mean()-Z"=mean(tidyData4$"tBodyAccJerk-mean()-Z"),"tBodyAccJerk-std()-X"=mean(tidyData4$"tBodyAccJerk-std()-X"),"tBodyAccJerk-std()-Y"=mean(tidyData4$"tBodyAccJerk-std()-Y"),"tBodyAccJerk-std()-Z"=mean(tidyData4$"tBodyAccJerk-std()-Z"),"tBodyGyro-mean()-X"=mean(tidyData4$"tBodyGyro-mean()-X"),"tBodyGyro-mean()-Y"=mean(tidyData4$"tBodyGyro-mean()-Y"),"tBodyGyro-mean()-Z"=mean(tidyData4$"tBodyGyro-mean()-Z"),"tBodyGyro-std()-X"=mean(tidyData4$"tBodyGyro-std()-X"),"tBodyGyro-std()-Y"=mean(tidyData4$"tBodyGyro-std()-Y"),"tBodyGyro-std()-Z"=mean(tidyData4$"tBodyGyro-std()-Z"),"tBodyGyroJerk-mean()-X"=mean(tidyData4$"tBodyGyroJerk-mean()-X"),"tBodyGyroJerk-mean()-Y"=mean(tidyData4$"tBodyGyroJerk-mean()-Y"),"tBodyGyroJerk-mean()-Z"=mean(tidyData4$"tBodyGyroJerk-mean()-Z"),"tBodyGyroJerk-std()-X"=mean(tidyData4$"tBodyGyroJerk-std()-X"),"tBodyGyroJerk-std()-Y"=mean(tidyData4$"tBodyGyroJerk-std()-Y"),"tBodyGyroJerk-std()-Z"=mean(tidyData4$"tBodyGyroJerk-std()-Z"),"tBodyAccMag-mean()"=mean(tidyData4$"tBodyAccMag-mean()"),"tBodyAccMag-std()"=mean(tidyData4$"tBodyAccMag-std()"),"tGravityAccMag-mean()"=mean(tidyData4$"tGravityAccMag-mean()"),"tGravityAccMag-std()"=mean(tidyData4$"tGravityAccMag-std()"),"tBodyAccJerkMag-mean()"=mean(tidyData4$"tBodyAccJerkMag-mean()"),"tBodyAccJerkMag-std()"=mean(tidyData4$"tBodyAccJerkMag-std()"),"tBodyGyroMag-mean()"=mean(tidyData4$"tBodyGyroMag-mean()"),"tBodyGyroMag-std()"=mean(tidyData4$"tBodyGyroMag-std()"),"tBodyGyroJerkMag-mean()"=mean(tidyData4$"tBodyGyroJerkMag-mean()"),"tBodyGyroJerkMag-std()"=mean(tidyData4$"tBodyGyroJerkMag-std()"),"fBodyAcc-mean()-X"=mean(tidyData4$"fBodyAcc-mean()-X"),"fBodyAcc-mean()-Y"=mean(tidyData4$"fBodyAcc-mean()-Y"),"fBodyAcc-mean()-Z"=mean(tidyData4$"fBodyAcc-mean()-Z"),"fBodyAcc-std()-X"=mean(tidyData4$"fBodyAcc-std()-X"),"fBodyAcc-std()-Y"=mean(tidyData4$"fBodyAcc-std()-Y"),"fBodyAcc-std()-Z"=mean(tidyData4$"fBodyAcc-std()-Z"),"fBodyAccJerk-mean()-X"=mean(tidyData4$"fBodyAccJerk-mean()-X"),"fBodyAccJerk-mean()-Y"=mean(tidyData4$"fBodyAccJerk-mean()-Y"),"fBodyAccJerk-mean()-Z"=mean(tidyData4$"fBodyAccJerk-mean()-Z"),"fBodyAccJerk-std()-X"=mean(tidyData4$"fBodyAccJerk-std()-X"),"fBodyAccJerk-std()-Y"=mean(tidyData4$"fBodyAccJerk-std()-Y"),"fBodyAccJerk-std()-Z"=mean(tidyData4$"fBodyAccJerk-std()-Z"),"fBodyGyro-mean()-X"=mean(tidyData4$"fBodyGyro-mean()-X"),"fBodyGyro-mean()-Y"=mean(tidyData4$"fBodyGyro-mean()-Y"),"fBodyGyro-mean()-Z"=mean(tidyData4$"fBodyGyro-mean()-Z"),"fBodyGyro-std()-X"=mean(tidyData4$"fBodyGyro-std()-X"),"fBodyGyro-std()-Y"=mean(tidyData4$"fBodyGyro-std()-Y"),"fBodyGyro-std()-Z"=mean(tidyData4$"fBodyGyro-std()-Z"),"fBodyAccMag-mean()"=mean(tidyData4$"fBodyAccMag-mean()"),"fBodyAccMag-std()"=mean(tidyData4$"fBodyAccMag-std()"),"fBodyBodyAccJerkMag-mean()"=mean(tidyData4$"fBodyBodyAccJerkMag-mean()"),"fBodyBodyAccJerkMag-std()"=mean(tidyData4$"fBodyBodyAccJerkMag-std()"),"fBodyBodyGyroMag-mean()"=mean(tidyData4$"fBodyBodyGyroMag-mean()"),"fBodyBodyGyroMag-std()"=mean(tidyData4$"fBodyBodyGyroMag-std()"),"fBodyBodyGyroJerkMag-mean()"=mean(tidyData4$"fBodyBodyGyroJerkMag-mean()"),"fBodyBodyGyroJerkMag-std()"=mean(tidyData4$"fBodyBodyGyroJerkMag-std()"))

write.table(finaldata,"tidyDataAssignment.txt",row.names=FALSE)

finaldata

}