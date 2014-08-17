## 1. Read the training and the test data sets:
After setting the directory for the files, read the data from the files:
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

## 2. Assigin column names to the data imported above
Training_Data: combines x_train and y_train
Test_Data: combines x_test and y_test

## 3. Merge the training and the test sets to create one data set
Final_Data: Merge Test_Data and Training_Data

## 4. Only include the mean and standard deviation for each measurement for the data (Excludes meanfeq(), angle.., max(), min() etc...) 
ColVector: Use TRUE/FALSE to identify the mean and standard deviation column and excludes others
Regenerated Final_Data to only include those selected columns at this point 

## 5. Label the data set with descriptive activity names
Use a for-loop to replace the names with descriptive activity names

## 6. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
Tidy_Dataset: only includes the average of each variable for each activity and each subject
Create a Tidy_Data.txt with write.table() using row.name=FALSE