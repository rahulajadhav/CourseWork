#Print your name at the top of the script
Prefix <- "Plotting Basics:"
name <- readline("Enter your last name : ")
print(paste(Prefix,name))

#checking version
R.Version()

#Installing and Import libraries 
  #a)for installing single packages including dependencies
install.packages("FSA")

  #b)For installing multiple packages including dependencies at once
install.packages(c("FSA","FSAdata","ggplot2","moments"))

  #importing single library
library(FSAdata)

  #importing multiple libraries at once
lapply(c("FSA","magrittr","dplyr","plotrix","ggplot2","moments"),require, character.only = TRUE)


#Load the BullTroutRML2 dataset
BullTroutRMS2 <- BullTroutRML2
BullTroutRMS2

#Print the first and last 3 records from the BullTroutRMS2 dataset
  #if dataset is loaded 

headtail(BullTroutRMS2,n=3)

  #if dataset is imported to the project
headtail(BullTroutRML2,n=3)

#Remove all records except those from Harrison Lake
Harrison_dataset <- filterD(BullTroutRMS2,lake=="Harrison")
Harrison_dataset

#Display the first and last 5 records from the filtered BullTroutRML2 dataset
headtail(Harrison_dataset,n=5)

#Display the structure of the filtered BullTroutRML2 dataset
str(Harrison_dataset)

#Display the summary of the filtered BullTroutRML2dataset
summary(Harrison_dataset)

#Create a scatterplot for "age" (y variable) and "fl" (x variable) with the provided specifications
plot(Harrison_dataset$fl,
     Harrison_dataset$age,
     xlab ="Fork Length (mm)",
     ylab = "Age (yrs)",
     xlim=c(0,500),
     ylim=c(0,15),
     pch=20,
     title("Plot 1: Harrison Lake Trout"))


#Plot an "Age" histogram with the provided specifications
attach(Harrison_dataset)
hist(age,
     xlab="Age (yrs)",
     ylab = "Frequency",
     main="Plot 2: Harrison Fish Age Distribution", col.main = "cadetblue",
     xlim = c(0,15),
     ylim = c(0,15),
     col = "cadetblue",
     )

#creating the density plot 
cols <-c("lightgreen","darkgreen")
attach(Harrison_dataset)
cols
plot(fl,
     age,
     xlab ="Fork Length (mm)",
     ylab = "Age (yrs)",
     xlim=c(0,500),
     ylim=c(0,15),
     pch=20,
     col=cols[era])
legend(x="topleft",legend = paste(levels(era)),col=cols,pch=20)


#Create a new object called "tmp" that includes the first 3 and last 3 records of the BullTroutRML2 data set
tmp <- headtail(BullTroutRML2,n=3)
tmp

#Display the "era" column (variable) in the new "tmp" object
tmp$era

#Create a pchs vector with the argument values for + and x
pchs <- c("+","x")
pchs

# Create a cols vector with the two elements "red" and "gray60"
cols <- c("red","gray60")
cols

# Convert the tmp era values to numeric values.
#before
tmp$era

#after
tmp$era <- as.numeric(tmp$era)
tmp$era

#Initialize the cols vector with the tmp era values
initialize(cols,tmp$era)

#Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) with the following specifications:
plot(Harrison_dataset$fl,
     Harrison_dataset$age,
     xlab ="Fork Length (mm)",
     ylab = "Age (yrs)",
     xlim=c(0,500),
     ylim=c(0,15),
     pch=pchs,
     col=cols[as.numeric(tmp$era)],
     title("Plot 4: Symbol & Color by Era"))

#Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression Overlay"
abline(lm(Harrison_dataset$age ~ Harrison_dataset$fl, data = Harrison_dataset), col = "blue")

# Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay
legend(x="topleft",legend = c("1977-80","1997-01"),col=cols,pch=20)

