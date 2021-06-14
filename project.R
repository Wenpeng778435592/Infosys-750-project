project<-read.csv(file.choose(),header=TRUE)
head(project)

#Histogram for original data
hist(project$forks)
hist(project$newCommits)
hist(project$newOwnerFollower)

#qq plot
qqnorm(project$forks)

library("nortest")
lillie.test(project$forks)

#Linearity
library("car")
library("RColorBrewer")
scatterplotMatrix(~forks +as.factor(OwnerType)+ 
                    newCommits + OwnerFollower,transform=TRUE,data=project)

#ORG = 1, USR = 0
dummyOwnerType<-NULL
dummyOwnerType<-(project$OwnerType=="ORG")*1
project1<-cbind(dummyOwnerType, project)

normalizedCommits <- NULL
normalizedCommits<-(project$newCommits / project$members)
project2<-cbind(normalizedCommits, project1)

#replace inf with 0
is.na(project2)<-sapply(project2, is.infinite)
project2[is.na(project2)]<-0

#remove the first record for each project
project3<-project2[project2$Time != 1, ] 
project3
head(project3)

#Histogram after transformation
#log10(max(x+1) - x)
hist(log(project3$forks + 1) )
hist(log(log(project3$forks+1) ))
hist(log(project3$newOwnerFollower +1))

#check normality - won't allow to using the number without + 1
#ks test 
library("nortest")
lillie.test(log(project3$forks +1))

#extract columns into a new array 
projectData<-project3[,c("normalizedCommits","dummyOwnerType","prjId","forks","newCommits","OwnerFollower")]
head(projectData)

# Random select 10 sample from the new array
projectRandom=projectData[projectData$prjId %in% sample(unique(projectData$prjId),10),]
head(projectRandom)

#Model A
library(nlme)
model.a <- lme(log(forks + 1) ~ 1, project3, random= ~1 |prjId)
summary(model.a)
VarCorr(model.a)
#Varcorr - for variance - between inline one, within in line 2 
m<- VarCorr(model.a)
#between variance / within variance + between 
icc.a<-as.numeric(m[1,1]) / (as.numeric(m[1,1]) + as.numeric(m[2,1]))
icc.a

#Model B
model.b <- lme(log(forks + 1) ~ Time , data=project3, random= ~ Time | prjId, method="ML")
summary(model.b)
m2<-VarCorr(model.b)
m2
icc.b<-as.numeric(m2[1,1]) / (as.numeric(m2[1,1]) + as.numeric(m2[2,1]))
icc.b

#Model c1
model.c1 <- lme(log(forks + 1)  ~  dummyOwnerType * Time ,
                data=project2, random= ~ Time | prjId, method="ML")
summary(model.c1)
## Interaction plots
interaction.plot(project3$Time,as.factor(project3$dummyOwnerType),log(project3$forks+1))

#Model c2
model.c2<- lme(log(forks + 1)  ~  dummyOwnerType * Time  + log(newOwnerFollower + 1) * Time ,
               data=project2, random= ~ Time | prjId, method="ML")
summary(model.c2)

#Model D
model.d <- lme(log(forks + 1)  ~ normalizedCommits * Time +dummyOwnerType*Time + log(newOwnerFollower +1) * Time ,
               data=project3, random= ~ Time | prjId, method="ML")
summary(model.d)

#Model e
model.e <- lme(log(forks + 1)  ~ normalizedCommits +dummyOwnerType*Time ,
               data=project3, random= ~ Time | prjId, method="ML")
summary(model.e)