activity_lables <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_lables) <-c("activityid","activityname")
features <- read.table("./UCI HAR Dataset/features.txt") 

#read test dataset
subject_test <-read.table("./UCI HAR Dataset/test/subject_test.txt" )
X_test       <-read.table("./UCI HAR Dataset/test/X_test.txt"       )
y_test       <-read.table("./UCI HAR Dataset/test/y_test.txt"       )

#read train dataset
subject_train <-read.table("./UCI HAR Dataset/train/subject_train.txt" )
X_train       <-read.table("./UCI HAR Dataset/train/X_train.txt"       )
y_train       <-read.table("./UCI HAR Dataset/train/y_train.txt"       )

#1.Merges the training and the test sets to create one data set.
x<-rbind(X_test,X_train)  

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
colnames(x) <- c(as.character(features[,2]))  
Mean<-grep("mean()",colnames(x),fixed=TRUE)  
Sd<-grep("std()",colnames(x),fixed=TRUE)  
MeanSd<-x[,c(Mean,Sd)]  

#3.Uses descriptive activity names to name the activities in the data set 
y <-rbind(y_train,y_test)   
xy<-cbind(y,x)  
colnames(xy)[1] <- "activityid"  
xy<-merge(activity_lables,xy,by.x="activityid",by.y="activityid")
xy<-xy[,-1] 
  
#4.Appropriately labels the data set with descriptive variable names. 
#have done above 

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subject<-rbind(subject_train,subject_test)
alldata<-cbind(subject,xy)  
colnames(alldata)[1] <- "subjectid"  

tidydata <- aggregate( alldata[,3] ~ subjectid+activityname, data = alldata, FUN= "mean" )  

for(i in 4:ncol(alldata)){ 
  tidydata[,i] <- aggregate( alldata[,i] ~ subjectid+activityname, data = alldata, FUN= "mean" )[,3] 
} 
colnames(tidydata)[3:ncol(tidydata)] <- colnames(MeanSd)
write.table(tidydata, file = "FinalData.txt",row.name=FALSE)   