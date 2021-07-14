library(ISLR)
library(ggplot2)
library(GGally)
library(gridExtra)
getwd()
setwd("C:\\Users\\seokho\\Documents")
heart<-read.table("Heart.csv",sep=",",header= T)
head(heart)

plot(subset(heart,select = c(X,Age,ChestPain,Chol,MaxHR,Ca,Slope)),col=as.character(heart$Sex+2))

palette()
plot(1:8, 1:8, col=1:8, pch=19, cex=3, xlab="", ylab="")
colors()
plot(1:8, 1:8, col=colors()[1:8], pch=19, cex=3, xlab="", ylab="")

?subset
subset(heart, subset = heart$Sex == 1)
subset(heart, subset = heart$Sex == 1, select = c(1,2,3))

ggpairs(subset(heart,select = c(X,Age,ChestPain,Chol,MaxHR,Ca,Slope)),aes(colour = as.factor(heart$Sex)))
warnings()
a<-ggpairs(subset(heart,select = c(X,Age,ChestPain,Chol,MaxHR,Ca,Slope)),aes(colour = as.factor(heart$Sex),alpha=0.4))
a

grid.arrange(a[1,1],a[2,1],a[1,2],a[2,2],nrow=2)
#########Data Sets ?Ұ?
data(Wage)
data(Smarket)
data(NCI60)

head(Wage)
head(Smarket)
str(NCI60)


##############strings

# empty string
empty_str = ""
# display
empty_str

# class
class(empty_str)


# empty character vector
empty_chr = character(0)
# display
empty_chr

character(0)
character(1)
character(2)

# class
class(empty_chr)

# character vector with 5 empty strings
char_vector = character(5)

# display
char_vector

# add first element
example=character(0)
example[1] = "first"
example

# check its length again
length(example)

example[4] = "fourth"
example

example=character(0)
example[4] = "fourth"
example

example=c()
str(example)
example[4] = "fourth"
example
str(example)

##############Input
# read 'ktop100.txt' file
top105 = readLines("http://www.textfiles.com/music/ktop100.txt")

length(top105)

# inspecting first elements
head(top105)

# top 5 songs
top105[11:15]
top105[11]
substr(top105[11],5,6)


###################
# paste
PI = paste("The life of", pi)
PI
IloveR = paste("I", "love", "R", sep = "-")
IloveR

#More info on https://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf

#https://www.tidytextmining.com/



