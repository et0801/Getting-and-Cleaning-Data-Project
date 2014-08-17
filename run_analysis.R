#run_analysis.R
#The data is collected from the accelerometers from the Samsung Galaxy S smartphone.
#The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
#Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 
# run_analysis.R does the following
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# 1. Read the training and the test data sets:

# Read all the data after change directory to the location of your files in R
features = read.table('./features.txt',header=FALSE)
Activity_Type = read.table('./activity_labels.txt',header=FALSE)
subject_Train = read.table('./train/subject_train.txt',header=FALSE)
x_Train = read.table('./train/x_train.txt',header=FALSE)
y_Train = read.table('./train/y_train.txt',header=FALSE)
subject_Test = read.table('./test/subject_test.txt',header=FALSE)
x_Test = read.table('./test/x_test.txt',header=FALSE)
y_Test = read.table('./test/y_test.txt',header=FALSE)


# 2. Assigin column names to the data imported above
colnames(Activity_Type) = c("Activity_ID","Activity_Type")
colnames(subject_Train)= "Subject_ID"
colnames(x_Train)= features[,2]
colnames(y_Train)= "Activity_ID"
Training_Data = cbind(y_Train,subject_Train,x_Train)
colnames(subject_Test)= "Subject_ID"
colnames(x_Test)= features[,2]
colnames(y_Test)= "Activity_ID"
Test_Data = cbind(y_Test,subject_Test,x_Test)

# 3. Merge the training and the test sets to create one data set
Final_Data = rbind(Training_Data,Test_Data)

# 4. Only include the mean and standard deviation for each measurement for the daata (Excludes meanfeq(), angle.., max(), min() etc...) 
col_name = colnames(Final_Data)
ColVector = (grepl("Activity..",col_name) | grepl("Subject..",col_name) | grepl("-mean..",col_name) & !grepl("-meanFreq..",col_name) | grepl("-std..",col_name))
Final_Data = Final_Data[ColVector==TRUE]
Final_Data = merge(Final_Data,Activity_Type,by="Activity_ID",all.x=TRUE)

# 5. Label the data set with descriptive activity names 
col_name  = colnames(Final_Data)
for (i in 1:length(col_name)) 
{
  col_name[i] = gsub("\\()","",col_name[i])
  col_name[i] = gsub("-std$","Stand Dev ",col_name[i])
  col_name[i] = gsub("-mean","Mean ",col_name[i])
  col_name[i] = gsub("^(t)","Time ",col_name[i])
  col_name[i] = gsub("^(f)","Frequency ",col_name[i])
  col_name[i] = gsub("([Gg]ravity)","Gravity ",col_name[i])
  col_name[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body ",col_name[i])
  col_name[i] = gsub("[Gg]yro","Gyroscope",col_name[i])
  col_name[i] = gsub("AccMag","Acceleration Magnitude",col_name[i])
  col_name[i] = gsub("([Bb]odyaccjerkmag)","Body Acceleration Jerk Magnitude",col_name[i])
  col_name[i] = gsub("JerkMag","Jerk Magnitude",col_name[i])
  col_name[i] = gsub("GyroMag","Gyroscope Magnitude",col_name[i])
}
colnames(Final_Data) = col_name

# 6. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
Final_Data_No_Activity = Final_Data[,names(Final_Data) != "Activity_Type"]
Tidy_Dataset = aggregate(Final_Data_No_Activity[,names(Final_Data_No_Activity) != c("Activity_ID","Subject_ID")],by=list(Activity_ID=Final_Data_No_Activity$Activity_ID,Subject_ID = Final_Data_No_Activity$Subject_ID),mean)
Tidy_Dataset = merge(Tidy_Dataset,Activity_Type,by="Activity_ID",all.x=TRUE)
Tidy_Dataset = Tidy_Dataset[order(Tidy_Dataset$Subject_ID),]
Tidy_Dataset [,1] <- Tidy_Dataset[,69]
colnames(Tidy_Dataset)[1] <- colnames(Tidy_Dataset)[69]
Tidy_Dataset[,69] <- NULL
write.table(Tidy_Dataset, './Tidy_Data.txt',row.names=FALSE,sep='\t') 