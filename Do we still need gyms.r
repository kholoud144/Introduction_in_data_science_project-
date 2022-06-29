#Install packages —————————————————-
install.packages("RCurl")
install.packages("Hmisc")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("ggcorrplot")

#libraries ————————————————————–
library(skimr)
library(Hmisc)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(corrplot)
library(ggcorrplot)

#set the working directory 
setwd("C:/Users/dell/Desktop/ÏæÑÉ R 2022/ãÔÑæÚ ÏæÑÉ ÇáR")
# Reading data and call it "df"
df <- read.csv("survey.csv.csv",sep = ";")

# Know the dimensions of the data (row, column)
dim(df)

# Get the first sex rows
head(df)

# Get the last sex rows
tail(df)

#Column Names: to check if it is need modification :
colnames(df)

#modify values name in the Gender column (F to Female ,M to male):
df$Gender = gsub("F","Female",df$Gender)
df$Gender = gsub("M","Male",df$Gender)

#modify values name in the SideEffects column (N to NO ,Y to Yes):
df$SideEffects = gsub("Y","Yes",df$SideEffects)
df$SideEffects = gsub("N","No",df$SideEffects)

#checks if any of columns in the data have null values - should print False :
any((is.na(df)))

#print number of duplicates in the data :
sum(duplicated(df))

#discovering the dataset more (type of variables and some statistic measures)
skim(df)

#and by - Statistic Measures:
describe(df)

#The dataset contains numbers of people by gender :
df_Gender<-df%>%
  group_by(Gender)%>%
  summarize(number_of_people= n())
df_Gender

#Basic barplot for Gender :
ggplot(data=df_Gender, aes(x=Gender, y=number_of_people)) +
  geom_bar(stat="identity",fill=c("coral","deepskyblue4"))+
  labs(y = "Number of people")+
  ggtitle("Numbers of people by gender :")+
  theme(plot.title = element_text(face="bold", size=14))

#Number of people using each of three types of medication by gender:
#Medicine1 with other Medicine 
counts1 <- table(df$Gender, df$Medicine1)
#Medicine2 with other Medicine 
counts2 <- table(df$Gender, df$Medicine2)
#Medicine3 with other Medicine 
counts3 <- table(df$Gender, df$Medicine3)

# create grouped bar chart 
## Create a 1 x 3 plotting matrix
par(mfrow=c(1,3))
#Medicine1 with other Medicine:
barplot(counts1, main="Grouped Bar Chart of Medicine1 \n by Gender",
        xlab="Gender", legend = T,ylim = c(0,500),col=c("coral","deepskyblue4"), beside=TRUE)
#Medicine2 with other Medicine:
barplot(counts2, main="Grouped Bar Chart of Medicine2 \n by Gender",
        xlab="Gender", legend = T,ylim = c(0,500),col=c("coral","deepskyblue4"), beside=TRUE)
#Medicine3 with other Medicine:
barplot(counts3, main="Grouped Bar Chart of Medicine3 \n by Gender",
        xlab="Gender", legend = T,ylim = c(0,500),col=c("coral","deepskyblue4"), beside=TRUE)

#Type of Medicines by Side Effects:
#Number of people using each of three types of medication by Side Effects:
df1<-df %>%
  filter(df$Medicine1 == "Yes")%>%
  select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter,Medicine2,Medicine3))

 
df2<-df %>%
    filter(df$Medicine2 == "Yes")%>%
    select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter,Medicine1,Medicine3))
  
df3<-df %>%
  filter(df$Medicine3 == "Yes")%>%
  select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter,Medicine1,Medicine2))

## Create a 1 x 3 plotting matrix
par(mfrow=c(1,3))

barplot(table(df1$Gender,df1$SideEffects),main="Side Effects of used medicine 1 \n by Gender",
          xlab="Side Effects",ylim = c(0,300),col=c("coral","deepskyblue4"), beside=TRUE)

barplot(table(df2$Gender,df2$SideEffects),main="Side Effects of used medicine 2\n by Gender",
          xlab="Side Effects",ylim = c(0,300),col=c("coral","deepskyblue4"), beside=TRUE)


barplot(table(df3$Gender,df3$SideEffects),main="Side Effects of used medicine 3 \n by Gender",
        xlab="Side Effects",ylim = c(0,300),col=c("coral","deepskyblue4"), beside=TRUE)  


#I focused on the second  Medicine 2 because it is the most used from male and female:

#create of new data that contain: people whose used Medicine 1 with Medicine 2 just 
df_M1_M2<- df %>%
  filter(df$Medicine1 == "Yes",df$Medicine2 == "Yes",df$Medicine3 == "No")%>%
  select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter))

#create of new data that contain Medicine : people whose used Medicine 2 with Medicine 3 just 
df_M2_M3<- df %>%
  filter(df$Medicine1 == "No",df$Medicine2 == "Yes",df$Medicine3 == "Yes")%>%
  select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter))

#create of new data that contain Medicine :people whose used Medicine 1, Medicine2 and Medicine 3  
df_M1_M2_M3<- df %>%
  filter(df$Medicine1 == "Yes",df$Medicine2 == "Yes",df$Medicine3 == "Yes")%>%
  select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter))

#create of new data that contain Medicine : people whose used Medicine 2 just 
df_M2<- df %>%
  filter(df$Medicine1 == "No",df$Medicine2 == "Yes",df$Medicine3 == "No")%>%
  select(-c(Age,KmBefore,KgBefore,TimeBefore,KmAfter,KgAfter,TimeAfter))

## Create a 2 x 2 plotting matrix
par(mfrow=c(2,2))

#Now , determine which of the people who use medicine 2 highest 
barplot(table(df_M1_M2$Gender),main="Grouped Bar Chart of\n Medicine1 with Medicine2 ",
        xlab="Gender",ylim = c(0,150),col=c("coral","deepskyblue4"), beside=TRUE)

barplot(table(df_M2_M3$Gender),main="Grouped Bar Chart of\n Medicine2 with Medicine3",
        xlab="Gender",ylim = c(0,150),col=c("coral","deepskyblue4"), beside=TRUE)

barplot(table(df_M1_M2_M3$Gender),main="Grouped Bar Chart of \n Medicine1, Medicine2 ,Medicine3 ",
        xlab="Gender",ylim = c(0,150),col=c("coral","deepskyblue4"), beside=TRUE)

barplot(table(df_M2$Gender),main="Grouped Bar Chart of Medicine2 just",
        xlab="Gender",ylim = c(0,150),col=c("coral","deepskyblue4"), beside=TRUE)


## Create a 2 x 2 plotting matrix
par(mfrow=c(2,2))

# table for SideEffects : people whose used Medicine 1 with Medicine 2 just
fs_M1_M2 <- table(df_M1_M2$SideEffects)
# calculate a percenage of each level 
pct1 <- round(100*prop.table(fs_M1_M2 ), 1) 
# to add labels for the plot 
lbls1 <- paste(names(fs_M1_M2 ), "\n", pct1, "%", sep = "") 
# plot pie chart 
pie(fs_M1_M2, col = brewer.pal(3,"Blues"), labels = lbls1, main=("SideEffects M1 & M2"))


# table for SideEffects: people whose used Medicine 2 with Medicine 3 just 
fs_M2_M3 <- table(df_M2_M3$SideEffects)
# calculate a percenage of each level 
pct2 <- round(100*prop.table(fs_M2_M3 ), 1) 
# to add labels for the plot 
lbls2 <- paste(names(fs_M2_M3 ), "\n", pct2, "%", sep = "") 
# plot pie chart 
pie(fs_M2_M3, col = brewer.pal(3,"Blues"), labels = lbls2, main=("SideEffects M2 & M3"))


# table for SideEffects: people whose used Medicine 1, Medicine2 and Medicine 3 
fs_M1_M2_M3 <- table(df_M1_M2_M3$SideEffects)
# calculate a percenage of each level 
pct3 <- round(100*prop.table(fs_M1_M2_M3  ), 1) 
# to add labels for the plot 
lbls3 <- paste(names(fs_M1_M2_M3  ), "\n", pct3, "%", sep = "") 
# plot pie chart 
pie(fs_M1_M2_M3 , col = brewer.pal(3,"Blues"), labels = lbls3, main=("SideEffects M1, M2 and M3"))


# table for SideEffects : people whose used Medicine 2 just 
fs_M2 <- table(df_M2$SideEffects)
# calculate a percenage of each level 
pct4 <- round(100*prop.table(fs_M2  ), 1) 
# to add labels for the plot 
lbls4 <- paste(names(fs_M2  ), "\n", pct4, "%", sep = "") 
# plot pie chart 
pie(fs_M2 , col = brewer.pal(3,"Blues"), labels = lbls4, main=("SideEffects M2"))


df_M2_just<- df %>%
  filter(df$Medicine1 == "No",df$Medicine2 == "Yes",df$Medicine3 == "No")
#discovering the dataset:
dim(df_M2_just)
skim(df_M2_just)
#checks if any of columns in the data have null values - should print False :
any((is.na(df_M2_just)))
#print number of duplicates in the dataset :
sum(duplicated(df_M2_just))
# Get the first sex rows
head(df_M2_just)

#delete the unused columns (Medicine1+Medicine2+Medicine3)â€”â€“
#dataest "df_M " : The data is for the people who used " Medicine 2 "  :  
df_M2_just <- df_M2_just %>%
  select(-c(`Medicine1`, `Medicine3`,`Medicine2`))
dim(df_M2_just)


# first, we should determine the breaks (intervals), 
# and R will do that for us
breaks1 = hist(df_M2_just$Age,xlab="Age" , ylab="" , main="Distribution of Ages who use the \n medicines2 ")[[1]]
#Frequancy table :
# then assign the suitable interval for each value in the variable "age"
class1 <- cut(df_M2_just $Age, breaks1, right = F)
# then we follow the same steps as before (3rd method) 
ff<-data.frame(table(class1))
mutate(ff, relFreq = round(prop.table(Freq),4)*100, cumFreq = cumsum(Freq), cumRelFreq = cumsum(relFreq))


#Distribution of Male/Female Ages who use the medicines2 :
#hist for males and females separately:
par(mfrow=c(1,2))
#male:
df_M2_male<- df_M2_just %>%
  filter( df_M2_just$Gender == "Male")
head(df_M2_male) 
dim(df_M2_male)
hist(df_M2_male$Age,xlab="Age", ylab="", main="Distribution of male Ages ")
#female:
df_M2_female<- df_M2_just %>%
  filter( df_M2_just$Gender == "Female")
head(df_M2_female)
dim(df_M2_female)
hist(df_M2_female$Age,xlab="Age", ylab="", main="Distribution of female Ages ")


#Is there a relationship between weights ,Running Kilometers and Running time before and after using medicines 2?
df_M2_num<-df_M2_just %>%
  select(-c(Gender,SideEffects))

dim(df_M2_num)

res <- round(cor(df_M2_num) ,2)
res
ggcorrplot(res, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
          colors = c("coral", "white", "deepskyblue4"))


## Create a 3 x 2 plotting matrix
par(mfrow=c(3,2))

#boxplot of kg ,km ,time  by gender :(find outlier )

#boxplot of kg 
boxplot( KmBefore~ Gender, data = df_M2_just,col = c("coral", "deepskyblue4"))$out  
boxplot( KmAfter~ Gender, data = df_M2_just,col = c("coral", "deepskyblue4"))$out   

#boxplot of km
boxplot( KgBefore~ Gender, data = df_M2_just,col = c("#FFE0B2", "#FFA726"))$out
boxplot( KgAfter~ Gender, data = df_M2_just,col = c("#FFE0B2", "#FFA726"))$out   

#boxplot of time
boxplot( TimeBefore~ Gender, data = df_M2_just,col = c("#FFE0B2", "#FFA726"))$out
boxplot( TimeAfter~ Gender, data = df_M2_just,col = c("#FFE0B2", "#FFA726"))$out





#difference:
difference=df_M2_just$KgBefore - df_M2_just$KgAfter
#Shapiro-Wilk test :
shapiro.test(difference)

#boxplot:
boxplot(difference)

#paired t-test
t.test(df_M2_just$KgBefore,df_M2_just$KgAfter,paired=TRUE)


mean(df_M2_just$KgBefore) 
mean(df_M2_just$KgAfter)


#modify values name in the SideEffects column (N to 0 ,Y to 1):
df_M2_just$SideEffects= gsub("Yes", 1 ,df_M2_just$SideEffects)
df_M2_just$SideEffects = gsub("No",0, df_M2_just$SideEffects)


head(df_M2_just)
tail(df_M2_just)

str(df_M2_just)

df_M2_just$SideEffects <- as.numeric(df_M2_just$SideEffects)
class(df_M2_just$SideEffects)


#Do age and gender have a role in the side effects of people who use the Medicine 2?
model<-glm(SideEffects~Age+Gender ,family = binomial(link = "logit"),data=df_M2_just )
summary(model)




