require(xlsx)
require(xtable)

#chunk 1

x<-citation()
x1<-citation(package="xlsx")
toBibtex(x)

sessionInfo()
packages_in_use <- c( sessionInfo()$basePkgs, names( sessionInfo()$loadedOnly ) )
the_citations_list <- lapply( X=packages_in_use, FUN=citation)
the_citations_list
#chunk3
setwd("/Users/pcru/SizeDoesMatter1")
table1<-read.xlsx2("1_R Wkshp_dummy data_OTU table.xlsx", sheetName = "Sheet1",header=FALSE,rowNames=FALSE,transpose=TRUE,endRow=18)

#chunk6
table1t=setNames(data.frame(t(table1[,-1])),table1[,1])
ctridx<-which(table1t$Group=="Control")
table1t$Group[1:48]<-"Contaminated"
table1t$Group[(ctridx+1):48]<-"Control"

#chunk6ii
ttt<-table1t$Site
for(i in c(2:length(table1t$Site)))
{
temp<-as.character(table1t$Site[i])
tempb<-as.character(ttt[i-1])
#print(i)
#print(temp)
#print(tempb)
if(table1t$Site[i]=="")
{
#print("a")
 ttt[i]<-tempb
 }
if(!table1t$Site[(i)]=="")
{
#print("b")
ttt[i]<-temp
}
print(ttt[i])
}
table1t$Site<-ttt

#chunk6iii
require(stringr)
table1t$Rep<-str_replace(table1t$Rep,"[rep]{3}?","\\1")
table1t$Rep<-str_replace(table1t$Rep,"A","1")
table1t$Rep<-str_replace(table1t$Rep,"B","2")
table1t$Rep<-str_replace(table1t$Rep,"C","3")
table1t$Rep<-as.factor(table1t$Rep)

#chunk7
setwd("/Users/pcru/SizeDoesMatter1")
table2<-read.xlsx2("2_R Wkshp_dummy data_Env Data_incl2outliersMK.xlsx", sheetName ="Sheet2",header=TRUE,rowNames=FALSE)

#chunk8ii
table2<-read.xlsx2("2_R Wkshp_dummy data_Env Data_incl2outliersMK.xlsx", sheetName = "Sheet2",header=TRUE,rowNames=FALSE,as.Data.frame=FALSE,colIndex=c(1:5),stringsAsFactors=FALSE,colClasses=c("character","numeric","numeric",rep("character",2)),endRow=4)
sapply(table2,mode)
sapply(table2,class)
table2<-read.xlsx2("2_R Wkshp_dummy data_Env Data_incl2outliersMK.xlsx", sheetName = "Sheet2",header=TRUE,rowNames=FALSE,as.Data.frame=FALSE,colIndex=c(1:11),stringsAsFactors=FALSE,colClasses=c("character",rep("numeric",3),rep("character",2),rep("numeric",6)),endRow=4)

#chunk8iii
table2f<-table2
table2f$Spill.date<-as.Date(table2f$Spill.date,"%d-%b-%y")
table2f$Sample.collection.date<-as.Date(table2f$Sample.collection.date,"%d.%m.%y")
sapply(table2f,mode)
sapply(table2f,class)
table2b<-read.xlsx2("2_R Wkshp_dummy data_Env Data_incl2outliersMK.xlsx", sheetName = "Sheet2",header=TRUE,rowNames=FALSE,as.Data.frame=FALSE,colIndex=c(1:11),stringsAsFactors=FALSE,colClasses=c("character",rep("numeric",2),"character",rep("character",2),rep("numeric",6)))
table2bf<-table2b
table2bf$Spill.date<-as.Date(table2bf$Spill.date,"%d-%b-%y")
#table2bf$Sample.collection.date<-as.Date(table2bf$Sample.collection.date,"%d.%m.%y")
cdate1<-as.Date(table2bf$Sample.collection.date,"%d.%m.%y")
cdate2<-as.Date(table2bf$Sample.collection.date,"%d/%m/%y")
table2bf$Sample.collection.date<-as.Date(ifelse(!is.na(cdate1),as.Date(cdate1),as.Date(cdate2)), origin="1970-01-01")
table2bf$Group<-as.factor(table2bf$Group)
table2bf$Rep<-as.factor(table2bf$Rep)
na_count <-sapply(table2bf, function(y) sum(length(which(is.na(y)))))
na_count
dated<-table2bf$Sample.collection.date-table2bf$Spill.date

#chunk8iv
require(stringr)
table2bf$Rep<-str_replace(table2bf$Rep,"[rep]{3}?","\\1")
table2bf$Rep<-str_replace(table2bf$Rep,"A","1")
table2bf$Rep<-str_replace(table2bf$Rep,"B","2")
table2bf$Rep<-str_replace(table2bf$Rep,"C","3")
table2bf$Rep<-as.factor(table2bf$Rep)
str(table2bf)
table3<-read.xlsx2("3_Follow up data from contaminated site_MK.xlsx", sheetName = "Sheet1",header=TRUE,rowNames=FALSE,colClasses=c(rep("character",3),rep("character",2),rep("numeric",18)),endRow=4)

table3f<-table3
table3f$Spill.date<-as.Date(table3f$Spill.date,"%d.%m.%y")
table3f$Sample.collection.date<-as.Date(table3f$Sample.collection.date,"%d.%m.%y")
sapply(table3f,mode)
sapply(table3f,class)


tab1c<-table1t[1:9,]
tab2c<-table2b[1:9,]
m1<-merge(tab1c,tab2c,by.x="Sample ID",by.y="Sample.ID")
#m2<-merge(table1t,table2b,by.x=c("Group","Site","Sample ID"),by.y=c("Group","Site","Sample.ID"))
m3<-merge(table1t,table2bf,by.x=c("Group","Site","Sample ID","Rep"),by.y=c("Group","Site","Sample.ID","Rep"))

Sample.ID<-rep(20000,3)
table3fi<-cbind(table3f,Sample.ID)
#how many columns I can't count
ncol(table3fi)
ncol(m3)
#now get the cols all right
table3fii<-table3fi[c(1,2,24,3,4:23)]
m3i<-m3[c(1:4,19:20,5:18,21:26)]
setdiff(names(m3i),names(table3fii))
m3ii<-rename(m3i,c("Sample ID"="Sample.ID"))

corynebacteriaceae<-rep(NA,nrow(table3fii))
porphyromondaceae<-rep(NA,nrow(table3fii))


table3fiii<-cbind(table3fii, corynebacteriaceae, porphyromondaceae)
setdiff(names(m3ii),names(table3fiii))


m3ii[,c(7:24)] <- sapply(m3ii[,c(7:24)],as.numeric)
m3ii[,c(1:4)] <-sapply(m3ii[,c(1:4)],as.character)
#m3ii[,c("Site")] <-sapply(m3ii[,c("Site")],as.character)

table3fiii[,c(1:4)] <- sapply(table3fiii[,c(1:4)],as.character)
table3fiii[,c(7:24)] <- sapply(table3fiii[,c(7:24)],as.numeric)
table4<-rbind(m3ii,table3fiii)
table4[,1] <- sapply(table4[,1],as.factor)



#chunk9
tab1c<-table1t[1:9,]

library(DBI)
require(RSQLite)
require(gsubfn)
require(chron)
#require(tcltk)
library(sqldf)
#RSQLite.extfuns
db <- dbConnect(SQLite(), dbname="Test.sqlite")
#getConfig()$staged.queries
# sqldf(attach "Test1.sqlite" as new)
dbBegin(db)
dbWriteTable(db,"table1",table1t,overwrite=TRUE)
dbReadTable(db,"table1")
dbListFields(db,"table1")
dbListTables(db)
dbGetQuery(db, "SELECT * from table1")
dbRollback(db)
dbDisconnect(db)

#chunk 10
numNAs_inData4_rows <- apply(rawData4, 1, function(z) sum(is.na(z)))
numNAs_inData4_col <- apply(table4, 2, function(z) sum(is.na(z))) # count NAs in Data4
lessThan20 <- table4[!(numNAs_inData4_rows > 20),]	   #only select the rows contain less Than 20 NAs
lessThan20col <- table4[,!(numNAs_inData4_col > 20)]

dates4<-table4[,c(5,6)]
abundance<-table4[,c(7:25)]
days<-dates4[,2]-dates4[,1]

sweepOutContinu<-sweep(abundance,2,apply(abundance,2,min,na.rm=TRUE))	
 afterSweepContinu<-sweep(sweepOutContinu,2,apply(sweepOutContinu,2,max,na.rm=TRUE),"/") 
 table5<-cbind(table4[,c(1:6)],afterSweepContinu,days)
 
 numNAs_inData5_col <- apply(table5, 2, function(z) sum(is.na(z)))
 t5lessThan20col <- table5[,!(numNAs_inData5_col > 20)]
 ncol(t5lessThan20col)
 abuncor<-cor(t5lessThan20col[,c(6:22)])
 

