## MEREDEL ABABA

# You should create one script called that does the following.
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of 
#    each variable for each activity and each subject.

##Setting a path to the unzip file
path <- "~/Desktop/UCI HAR Dataset1"

##Working Directory
setwd(path)
getwd()

##Uploading train and test set
x_test=read.table("./test/X_test.txt", sep = "")
y_test=read.table("./test/y_test.txt", sep = "")
x_train=read.table("./train/X_train.txt")
y_train=read.table("./train/y_train.txt")

##import subject train and test
subject_test <- read.table("./test/subject_test.txt")
colnames(subject_test) <- "SubjectID"
subject_test$dataset="TEST"
subject_train <- read.table("./train/subject_train.txt")
colnames(subject_train) <- "SubjectID"
subject_train$dataset="TRAIN"


#Import features.txt which contains the labels, removed first column. 
features = read.table("./features.txt") 
features$V1 = NULL

###combining tables
subject = rbind(subject_train, subject_test)
total = rbind(x_test, x_train)
y_total = rbind(y_test, y_train)


##rename column name for y
colnames(y_total) <- "activity"
#change the response to the corresponding activity
y_total$activity_label <- factor(y_total$activity,
                           levels = c(1,2,3,4,5,6),
                           labels = c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing","Laying"))

##label column names for dataset total
colnames(total) <- features$V2

#create new df with extracted columns using regular expression
total1 = total[grepl("mean\\(\\)", names(total))]
total2 = total[grepl("std\\(\\)", names(total))]

#combined total1, total2 and activity to create one final table
final_table = cbind(total1, total2, y_total, subject)
#remove special characters
names(final_table)<-tolower(gsub("[^0-9A-Za-z]","",names(final_table)))

#factor the dataset
final_table$dataset<-as.factor(final_table$dataset)

#prepping for sum table by removing activity label
forsumtable <- final_table
forsumtable$activitylabel<-NULL

##removing activity
final_table1 <- final_table
final_table1$activity <- NULL

##STEP 5
#independent tidy data set with the average of each variable for each activity and each subject.
sumtable = aggregate(forsumtable[,names(forsumtable) != c('subjectid','activity','dataset')],
                     by=list(subjectid=forsumtable$subjectid,activity=forsumtable$activity),mean,na.rm=TRUE)
sumtable$activity <- NULL
sumtable$subjectid <- NULL
sumtable$activity <- factor(sumtable$activity,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing","Laying"))


write.csv(final_table1,"~/Desktop/final_table1.csv")
write.table(sumtable,"~/Desktop/sumtable.txt,row.name=FALSE")




