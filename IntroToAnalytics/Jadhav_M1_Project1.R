#Print your name at the top of the script
Name <- "Rahul Avinash Jadhav"
Name

#install the vcd package
install.packages("vcd")

#import the vcd package
library(vcd)

#Plot a sales ~ temp scatter plot using the data provided
Sales_data <- c(8,11,15,20,21,11,18,10,6,22)
Temperature_data <- c(69,80,77,84,80,77,87,70,65,90)
plot(Sales_data,Temperature_data)

#Find the mean temperature
mean(Temperature_data)

#Delete the 3rd element from the sales vector
Sales_data <- Sales_data[-3]
Sales_data

#Insert 16 as the 3rd element into the sales vector
Sales_data<- append(Sales_data,16,2)  
Sales_data

#Create a vector with elements provided
names <- c("Tom","Dick","Harry")
names

#Create a 5 row and 2 column matrix of 10 integers
x <- matrix(1:10, nrow=5, ncol=2)
x

#Create a data frame with sales and temp attributes
icSales <- data.frame(Sales_data , Temperature_data)
icSales

#Display the data frame structure
str(icSales)

#Display the data frame summary
summary(icSales)

#import csv dataset
Students <- read.csv2("Student.csv",header=TRUE,sep=",")

#Display variable names of the dataset
colnames(Students)
