#Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr, dplyr, tidyr plyr and tidyverse
Name <- "Rahul Avinash Jadhav"
Name
packages<-(c("FSA","FSAdata","magrittr","dplyr","tidyr","plyr","tidyverse"))
package.check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
      }
    }
)

lapply(c("FSA","FSAdata","magrittr","dplyr","tidyr","plyr","tidyverse"),require, character.only = TRUE)

#Import the inchBio.csv and name the table <bio>
bio <- read.csv2("inchBio.csv",header=TRUE,sep=",")
bio

#Display the head, tail and structure of <bio>
headtail(bio)
str(bio)


#Create an object, <counts>, that counts and lists all the species records
counts <- count(bio,"species")
counts

#Display just the 8 levels (names) of the species
as.data.frame(counts$species)

#Create a <tmp> object that displays the different species and the number of record of each species in the dataset. Include this information in your report.-
tmp <- table(bio$species)
tmp

#Create a subset, <tmp2>, of just the species variable and display the first five records
tmp2 <- subset(bio,select=species)
head(tmp2, n=5)

#Create a table, <w>, of the species variable. Display the class of w
w <- table(bio$species)
w
class(w)

#Convert <w> to a data frame named <t> and display the results
t <- as.data.frame(w)
t
class(t)

#Extract and display the frequency values from the <t> data frame
t$Freq


#Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio>
cSpec <- table(bio$species)
cSpec
class(cSpec)


#Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class
cSpecPct <- prop.table(table(bio$species))*100
cSpecPct
class(cSpecPct)

#Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u <- as.data.frame(cSpecPct)
u
class(u)

#Create a barplot of <cSpec> with the following: titled Fish Count with the given specifications:
barplot(cSpec,
        main ="Fish Count",
        ylab = "COUNTS",
        col = "lightgreen",
        horiz = TRUE,
        cex.names = 0.6,
        xlim = c(0,250)
       )



#Create a barplot of <cSpecPct>, with the given specifications:
#left to color the ylabel
barplot(cSpecPct,
        ylim =c(0,40),
        ylab = "Frequency in Percentage",
        col.lab = "LightBlue",
        main = "Fish Relative Frequency",
        cex.names = 0.55)

#Rearrange the <u> cSpecPct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d>
d <- u[order(-u$Freq),]
d

#Rename the <d> columns Var 1 to Species, and Freq to RelFreq

d <- rename(d, replace = c("Var1" = "Species", "Freq" = "RelFreq"))
d

#Add new variables to <d> and call them cumfreq, counts, and cumcounts
d <- d %>%
  add_column(cumfreq=cumsum(d$RelFreq),counts=(d$RelFreq*length(bio$species))/100,cumcounts=cumsum(counts))
d

#. Create a parameter variable <def_par> to store parameter variables
def_par <- par(mar=c(10,5,5,8))
def_par

#Create a barplot, <pc>, with the given specifications
pc <- barplot(d$counts,
              width = 1,
              space = 0.15,
              border = NA,
              axes = F,
              ylim = c(0,3.05*max(d$counts,na.rm = TRUE)),
              ylab = "Cummulative Counts",
              cex.axis = 0.70,
              names.arg = d$Species,
              cex.names=.55,
              main = "Species Pareto",
              las=2
)


#Add a cumulative counts line to the <pc> plot with given instruction:
lines(pc,d$cumcounts,
     type="b",
     cex=0.70,
     pch=19,
     col="cyan4"
     )

#Place a grey box around the pareto plot
box(col="grey")

#Add a left side axis with the following specifications
axis(2,at=c(0,d$cumcounts),
     col.ticks = "grey62",
     col.axis="grey62",
     cex.axis=0.8)

#Add axis details on right side of box with the specifications:
axis(4,at=c(0,round(d$cumcounts)),
     labels = paste(c(0,round(d$cumfreq)),"%"),
     col.axis="cyan4",
     col = "cyan4",
     cex.axis=0.80)

#Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot
mtext("Jadhav", side=3)


#data analysis

analysis1 <- aggregate(bio$tl~bio$species, bio, max)
analysis1 <- rename(analysis1, replace = c("bio$species"="species", "bio$tl" = "total_length"))
analysis1


barplot(analysis1$total_length,
  main ="Total length of fish",
  ylab = "LENGTH",
  cex.names = 0.6,
  names.arg = analysis1$species,
  ylim = c(0,500)
  )
box(col = "grey")

Largemouth <- filterD(bio,species=="Largemouth Bass")
summary(Largemouth)
plot(Largemouth$tl,Largemouth$w,
     xlab = "Lengths",
     ylab = "Weights",
     main = "Largemouth fish",
     xlim = c(0,500),
     ylim = c(0,1150)
     )

hist(Largemouth$tl,
     xlab = "Length",
     main = "Largemouth Bass length distribution",
     xlim = c(0,500),
     ylim = c(0,150))

