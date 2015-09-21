require(xlsx)
require(xtable)

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
able1t$Rep<-str_replace(table1t$Rep,"[rep]{3}?","\\1")
table1t$Rep<-str_replace(table1t$Rep,"A","1")
table1t$Rep<-str_replace(table1t$Rep,"B","2")
table1t$Rep<-str_replace(table1t$Rep,"C","3")
table1t$Rep<-as.factor(table1t$Rep)

#chunk7
table2<-read.xlsx2("2_R Wkshp_dummy data_Env Data.xlsx", sheetName ="Sheet2",header=TRUE,rowNames=FALSE)

#chunk8ii
table2<-read.xlsx2("2_R Wkshp_dummy data_Env Data.xlsx", sheetName = "Sheet2",header=TRUE,rowNames=FALSE,as.Data.frame=FALSE,colIndex=c(1:5),stringsAsFactors=FALSE,colClasses=c("character","numeric","numeric",rep("character",2)),endRow=4)
sapply(table2,mode)
sapply(table2,class)
table2<-read.xlsx2("2_R Wkshp_dummy data_Env Data.xlsx", sheetName = "Sheet2",header=TRUE,rowNames=FALSE,as.Data.frame=FALSE,colIndex=c(1:11),stringsAsFactors=FALSE,colClasses=c("character",rep("numeric",3),rep("character",2),rep("numeric",6)),endRow=4)

#chunk8iii
table2f<-table2
table2f$Spill.date<-as.Date(table2f$Spill.date,"%d-%b-%y")
table2f$Sample.collection.date<-as.Date(table2f$Sample.collection.date,"%d.%m.%y")
sapply(table2f,mode)
sapply(table2f,class)
table2b<-read.xlsx2("2_R Wkshp_dummy data_Env Data.xlsx", sheetName = "Sheet2",header=TRUE,rowNames=FALSE,as.Data.frame=FALSE,colIndex=c(1:11),stringsAsFactors=FALSE,colClasses=c("character",rep("numeric",2),"character",rep("character",2),rep("numeric",6)))
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


#chunk9
tab1c<-table1t[1:9,]

library(DBI)
require(RSQLite)
require(gsubfn)
require(chron)
require(tcltk)
library(sqldf)
%RSQLite.extfuns
 db <- dbConnect(SQLite(), dbname="Test.sqlite")
# sqldf(attach "Test1.sqlite" as new)
dbWriteTable(db,"table1",table1t)
dbReadTable(db,"table1")
dbListFields(db,"table1")
dbListTables(db)
dbGetQuery(db, "SELECT * from table1")
#dbDisconnect(db)


