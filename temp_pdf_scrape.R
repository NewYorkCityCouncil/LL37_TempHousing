library(data.table)
library(pdftools)
library(stringr)
library(zoo)

#link=c("https://www1.nyc.gov/assets/operations/downloads/pdf/temporary_housing_report.pdf")
#link no longer has historical data
link=c("/Users/romartinez/Desktop/Data_Projects/Homelessness/sources/temporary_housing_report_aug19.pdf")
files<-pdf_text(link)

#create a vector that makes a list for the text sepeparted by \n, which means a new line.
s <-strsplit(files,"\n")


############################
HPD 
############################
#pull out the dates you want from the HPD pages. 
#I grabbed the dates first to see if had all the months we wanted.
#There's two spelling of 'HPD - Monthly Shelter Census Report', 'HPD- Monthly Shelter Census Report'
# the first one is for historical data not found on the open data portal
# no point in getting the second spelling/date since this is avaiable easily in open data portal
date<-list()

for(i in 1:length(s))
{
  date[[i]] <- s[[i]][grep("HPD - Monthly Shelter Census Report",s[[i]])+1]

}

W <- grep("   nyc.gov/hpd",date)


for(i in 1:length(W))
{
  date[W[i]] <- s[[W[i]]][grep("HPD - Monthly Shelter Census Report",s[[W[i]]])+2]	
}

date[grep("character",date)] <- "NA"

date <- unlist(date)
date=date[which(date!="NA")]
date=str_trim(date,side = c("both"))
date=gsub("For the month of: ", "", date)
fdate=as.Date(paste('01', date), format='%d %b %Y')

setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/HPD")
write.csv(date, "hpd_dates.csv")

#there's always more than one way to scrape a pdf.
#For this one I decided to do it by the type of HPD facility bc the types seem to unique and occured only once on each HPD page.
#make a list of the rows on each page where facilty type "Administered Fac" occurs.
pull1 <-list()
for(i in 1:length(s))
{
  pull1[[i]] <- s[[i]][grep("Administered Fac",s[[i]])]
}

pull=pull1[pull1!='character(0)']

t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]],"  ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}


#make some vector where you'll store data you want for the facilities.
v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <-c()
v9 <- c()
v10 <-c()
v11 <-c()
v12 <-c()
v13 <- c()
v14 <-c()
v15 <-c()
v16 <-c()
v17 <-c()
v18 <-c()

for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
  v9[i] <- t[[i]][9]
  v10[i] <- t[[i]][10]
  v11[i] <- t[[i]][11]
  v12[i] <- t[[i]][12]
  v13[i] <- t[[i]][13]
  v14[i] <- t[[i]][14]
  v15[i] <- t[[i]][15]
  v16[i] <- t[[i]][16]
  v17[i] <- t[[i]][17]
  v18[i] <- t[[i]][18]
}

HPD_Admin_Fac <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18))

# #ignore the first 6 rows, which are the most recent and available on open data
HPD_Admin_Fac=HPD_Admin_Fac[-c(1:6),]

HPD_Admin_Fac=HPD_Admin_Fac[-grep('American Red Cross',HPD_Admin_Fac$v1),]
#line up rows that have missing LOS for single adults
HPD_Admin_Fac[is.na(HPD_Admin_Fac$v17)==TRUE,17]<-HPD_Admin_Fac[is.na(HPD_Admin_Fac$v17)==TRUE,14]
HPD_Admin_Fac[is.na(HPD_Admin_Fac$v18)==TRUE,18]<-HPD_Admin_Fac[is.na(HPD_Admin_Fac$v18)==TRUE,15]
HPD_Admin_Fac[is.na(HPD_Admin_Fac$v16)==TRUE,14]<-0
HPD_Admin_Fac[is.na(HPD_Admin_Fac$v16)==TRUE,15]<-0
HPD_Admin_Fac[is.na(HPD_Admin_Fac$v16)==TRUE,16]<-0


write.csv(HPD_Admin_Fac,"HPD_Admin_Fac.csv", row.names = FALSE)


#do the same thing we did above, but for the "Emerg. Facilities Type"

pull1 <-list()
for(i in 1:length(s))
{
  pull1[[i]] <- s[[i]][grep("Emerg. Facilities",s[[i]])]
}

pull=pull1[pull1!='character(0)']

t <-list()
for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]],"  ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <-c()
v9 <- c()
v10 <-c()
v11 <-c()
v12 <-c()
v13 <- c()
v14 <-c()
v15 <-c()
v16 <-c()



for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
  v9[i] <- t[[i]][9]
  v10[i] <- t[[i]][10]
  v11[i] <- t[[i]][11]
  v12[i] <- t[[i]][12]
  v13[i] <- t[[i]][13]
  v14[i] <- t[[i]][14]
  v15[i] <- t[[i]][15]
  v16[i] <- t[[i]][16]
}

HPD_Emerg_Fac <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16))

#add missing info from facility type
HPD_Emerg_Fac[is.na(HPD_Emerg_Fac$v9)==TRUE,9]<-HPD_Emerg_Fac[is.na(HPD_Emerg_Fac$v9)==TRUE,4]
HPD_Emerg_Fac[is.na(HPD_Emerg_Fac$v10)==TRUE,10]<-HPD_Emerg_Fac[is.na(HPD_Emerg_Fac$v10)==TRUE,8]
#fixing smushed entries
HPD_Emerg_Fac[which(nchar(HPD_Emerg_Fac$v9)>6),9]<-HPD_Emerg_Fac[which(nchar(HPD_Emerg_Fac$v9)>6),4]
HPD_Emerg_Fac$c17=rep(NA,nrow(HPD_Emerg_Fac))
HPD_Emerg_Fac$c18=rep(NA,nrow(HPD_Emerg_Fac))

#redorder to merge later on with all fac types
HPD_Emerg_Fac=HPD_Emerg_Fac[,c(1:3,11:12,4:8,13:18,9:10)]
write.csv(HPD_Emerg_Fac,"HPD_Emerg_Fac.csv", row.names = FALSE)


#################################

#Do the the sam as we did above but for the "Emeg. Hotels" facilities.

pull1 <-list()


for(i in 1:length(s))
{
  pull1[[i]] <- s[[i]][grep("Emerg. Hotels",s[[i]])]
}

pull=pull1[pull1!='character(0)']

t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]],"  ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <-c()
v9 <- c()
v10 <-c()
v11 <-c()
v12 <-c()
v13 <- c()
v14 <-c()
v15 <-c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
  v9[i] <- t[[i]][9]
  v10[i] <- t[[i]][10]
  v11[i] <- t[[i]][11]
  v12[i] <- t[[i]][12]
  v13[i] <- t[[i]][13]
  v14[i] <- t[[i]][14]
  v15[i] <- t[[i]][15]
  v16[i] <- t[[i]][16]
  
}

HPD_Emerg_Hotel <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16))

#fix the SRO shifted rows
which(HPD_Emerg_Hotel$v2==" (SRO)")
sub=HPD_Emerg_Hotel[which(HPD_Emerg_Hotel$v2==" (SRO)"),]
sub=sub[,-2]
sub$v17=rep(NA,nrow(sub))
HPD_Emerg_Hotel[which(HPD_Emerg_Hotel$v2==" (SRO)"),]<-sub
#add missing values
HPD_Emerg_Hotel$v4=as.numeric(HPD_Emerg_Hotel$v4)
HPD_Emerg_Hotel$v12=as.numeric(HPD_Emerg_Hotel$v12)
HPD_Emerg_Hotel$v7=as.numeric(HPD_Emerg_Hotel$v7)
HPD_Emerg_Hotel[is.na(HPD_Emerg_Hotel$v14)==TRUE,14]<-rowSums(HPD_Emerg_Hotel[is.na(HPD_Emerg_Hotel$v14)==TRUE,c(4,12)])
HPD_Emerg_Hotel[is.na(HPD_Emerg_Hotel$v15)==TRUE,15]<-rowSums(HPD_Emerg_Hotel[is.na(HPD_Emerg_Hotel$v15)==TRUE,c(7,12)])
HPD_Emerg_Hotel=HPD_Emerg_Hotel[,-16]

#adding columns needed to rbind with the other facility types
HPD_Emerg_Hotel$c1=rep(NA,nrow(HPD_Emerg_Hotel))
HPD_Emerg_Hotel$c2=rep(NA,nrow(HPD_Emerg_Hotel))
HPD_Emerg_Hotel$c8=rep(NA,nrow(HPD_Emerg_Hotel))

#reorder to merge later on
HPD_Emerg_Hotel=HPD_Emerg_Hotel[,c(1,16:17,2:6,18,7:15)]
write.csv(HPD_Emerg_Hotel,"HPD_Emerg_Hotel.csv")

#merge all columns
###rbind all facilities & cbind to dates & then rename columns
HPD_All_Fac=rbind(HPD_Admin_Fac,HPD_Emerg_Fac,HPD_Emerg_Hotel,use.names=FALSE)
HPD_All_Fac=data.frame(cbind(fdate, date, HPD_All_Fac))
names(HPD_All_Fac)<-c("Fixed_Date", "Date", "Facility_Type", "NumberOfFamiliesWithChildren", "LOS_FamiliesWithChildren", "NumberOfAdultFamilies", "LOS_AdultFamilies", "NumberofTotalFamilies",
                      "LOS_TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies","TotalIndividualsinFamilies","NumberOfSingleMen","LOS_SingleMen","NumberOfSingleWomen",
                      "LOS_SingleWomen","NumberofTotalSingleAdults","LOS_SingleAdults","FacilityFamilyCount", "FacilityHeadCount")

write.csv(HPD_All_Fac, "HPD_All_facilities.csv", row.names = FALSE)


####################################################
DHS
#################################################
##starts on open data on 201901
#date
#######
setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS")
date<-list()

for(i in 1:length(s))
{
  date[[i]] <- s[[i]][grep("DHS Local Law 37 report",s[[i]])]
  
}

date[grep("character",date)] <- "NA"

date <- unlist(date)
date=date[which(date!="NA")]
date=str_trim(date,side = c('both'))
date=gsub("DHS Local Law 37 report for the Month of ", "", date)
fdate=as.Date(paste('01', date), format='%d %b %Y')
write.csv(date, "dhs_dates.csv")


#######
dhs<-list()
for(i in 1:length(s))
{
  dhs[[i]] <- grep("stabilization beds",s[[i]])
  #dhs[[i]] <- grep("veterans shelters",s[[i]])
  #dhs[[i]] <- grep("safe havens",s[[i]])
}

dhs=as.character(dhs)
ind=c()
ind=which(dhs!="integer(0)")
ind=s[ind]

##### checK
l=c()
for (i in 1:length(ind)){
  l[i]=length(unlist(ind[i]))
}
#### check

###### 
#pulling interested columns

#getting the first row on the number of DHS-administered facilites occurs.
#first pull gets the row info when the info is within the same character string value
pull1 <-list()
#second pull gets the row info when the numbers are in the next charcter string value
pull2<-list()
#third pull helps distinguish where the term 'DHS administered facilties is mentioned the second time
pull3<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- ind[[i]][grep("DHS-administered facilities   ",ind[[i]])]
  pull2[[i]] <- ind[[i]][grep("DHS-administered facilities",ind[[i]])+1]
  pull3[[i]] <- ind[[i]][grep("                                                               2",ind[[i]])]
}
#second pull gets the row info when the numbers are in the next charcter string value
pull1[which(lengths(pull1)==0)]<-pull2[which(lengths(pull1)==0)]

#third pull helps distinguish where the term 'DHS administered facilties is mentioned the second time
pull1[which(lengths(pull3)!=0)]<-"NA"

#we only need the first element of each list value 
pull=c()
for (i in 1:length(pull1)){
  pull[i]=pull1[[i]][1]
}

pull=pull[which(pull!="NA")]
pull=gsub("DHS-administered facilities", "", pull)

#make some vector where you'll store data you want for the facilities.

t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#remove the first element of that has #1 
t[7]<-lapply(t[7], function(x) x[-1])

#add number that pdftool didn't pick up
lengths(t)
t[2]<-list(unlist(append(t[2],'73', after = 0)))
t[4]<-list(unlist(append(t[2],'72', after = 0)))


v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <-c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

Num_DHS_Shelters <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
Num_DHS_Shelters <- data.table(cbind(date,Num_DHS_Shelters))
Num_DHS_Shelters <- data.table(cbind(fdate,Num_DHS_Shelters)) 
names(Num_DHS_Shelters )<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                     "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
write.csv(Num_DHS_Shelters,'Num_DHS_Shelters.csv', row.names = FALSE)

######
##getting the second row on the average length of stay.

pull1<-list()
#second pull gets the row info when the numbers are in the next charcter string value
pull2<-list()
#thirs pull gets the row info when the numbers are in the next next charcter string value
pull3<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- ind[[i]][grep("Average Length of Stay",ind[[i]])]
  pull2[[i]] <- ind[[i]][grep("Average Length of Stay",ind[[i]])+1]
  pull3[[i]] <- ind[[i]][grep("Average Length of Stay",ind[[i]])+2]
}

#second pull gets the row info when the numbers are in the next charcter string value
#to help put pull2 into pull1
n=c()
for (i in 1:length(pull1)){
  n[i]=nchar(pull1[i])
}

unique(n)
pull1[which(nchar(pull1)<=33)]<-pull2[which(nchar(pull1)<=33)]

#to help put pull3 into pull1
n=c()
for (i in 1:length(pull1)){
  n[i]=nchar(pull1[i])
}

unique(n)
pull1[which(nchar(pull1)<=44)]<-pull3[which(nchar(pull1)<=44)]

pull1=pull1[lengths(pull1)==1]

pull=gsub("[^.0-9[:space:]]", "", pull1)


#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()



for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
}

DHS_Avg_LOS <- data.table(cbind(v1,v2,v3,v4,v5))
DHS_Avg_LOS <- data.table(cbind(fdate,date,DHS_Avg_LOS,rep(0,nrow(DHS_Avg_LOS)),rep(0,nrow(DHS_Avg_LOS)),
                                rep(0,nrow(DHS_Avg_LOS))))
#checked dates, the data and dates are off starting June 2016
DHS_Avg_LOS[35:92,3:7]<-shift(DHS_Avg_LOS[35:92,3:7],n=1, fill=NA, type = c('lag'))
DHS_Avg_LOS[35,3:7]<-list("375", "357","370","423","565")
DHS_Avg_LOS[40,3:7]<-list("", "","","434","566")

names(DHS_Avg_LOS)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                             "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
write.csv(DHS_Avg_LOS,'DHS_Avg_LOS.csv',row.names = FALSE)


##### 
#####next row: unduplicated persons in dhs admin facilities

#first pull gets #Number of unduplicated persons: DHS-
pull1<-list()
#second pull gets 
#15] "DHS-administered facilities"                                                                                         
#[16] "Number of unduplicated persons:   524      62      586" 
pull2<-list()
pull3<-list()
pull4<-list()
pull5<-list()

for(i in 1:length(ind))
{
   pull1[[i]] <- ind[[i]][grep("Number of unduplicated persons: DHS-",ind[[i]])+0:1][1]
   pull2[[i]] <- ind[[i]][grep("  Number of unduplicated persons:",ind[[i]])+0:2][2]
   pull3[[i]] <- ind[[i]][grep("Number of unduplicated persons:",ind[[i]])+0:2]
   pull4[[i]] <- ind[[i]][grep("DHS-administered facilities",ind[[i]])-1][2]
   pull5[[i]] <- ind[[i]][grep("Number of unduplicated persons: DHS-",ind[[i]])+0:1][2]
}

w1=nchar(pull5)>90
pull1[is.na(pull1)==TRUE]<-pull2[is.na(pull1)==TRUE]
pull1[35:110]<-pull4[35:110]
pull1[53]<-pull3[53]
pull1[which(w1==TRUE)]<-pull5[which(w1==TRUE)]


pull=pull1[is.na(pull1)==FALSE]
pull=gsub("[^.0-9[:space:]]", "", pull)

#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

DHS_AdminFac_Undup_Cts <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
DHS_AdminFac_Undup_Cts <- data.table(cbind(fdate,date,DHS_AdminFac_Undup_Cts))
names(DHS_AdminFac_Undup_Cts)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                      "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
setwd('~/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS')

#check dataset
#found extra 0
DHS_AdminFac_Undup_Cts[which(DHS_AdminFac_Undup_Cts$NumberofAdultsinFamilies==186360),9]<-18636

#january 2012 & December 2011 numbers are blank in the pdf, therefore NA in the dataset

write.csv(DHS_AdminFac_Undup_Cts, "DHS_AdminFac_Undup_Counts.csv", row.names = FALSE)



##### 
#####next row: unduplicated persons in dhs safe havens

#first pull gets "  safe havens"  
pull1<-list()
#second pull gets 
# "DHS safe havens"                                                                                         
pull2<-list()
pull3<-list()
pull4<-list()
pull5<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- ind[[i]][grep("  safe havens",ind[[i]])-1]
  pull2[[i]] <- ind[[i]][grep("  safe havens",ind[[i]])]
  pull3[[i]] <- ind[[i]][grep("DHS safe havens",ind[[i]])-1][2]
  pull4[[i]] <- ind[[i]][grep("safe havens",ind[[i]])][2]
  pull5[[i]] <- ind[[i]][grep("DHS safe havens   ",ind[[i]])]
}

w1=nchar(pull2)>40
pull3[is.na(pull3)==TRUE]<-pull1[is.na(pull3)==TRUE]
pull3[which(w1==TRUE)]<-pull2[which(w1==TRUE)]
pull3[lengths(pull3)==0]<-pull4[lengths(pull3)==0]
pull3[35]<-pull5[35]

pull=pull3[is.na(pull3)==FALSE]
pull=gsub("[^.0-9[:space:]]", "", pull)

#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

DHS_SafeHavens_Undup_Cts <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
DHS_SafeHavens_Undup_Cts <- data.table(cbind(fdate,date,DHS_SafeHavens_Undup_Cts))
names(DHS_SafeHavens_Undup_Cts)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                                   "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")

#check for errors
#Feburary 2013 numbers are NA
DHS_SafeHavens_Undup_Cts[DHS_SafeHavens_Undup_Cts$Date=='February 2013',3]<-NA
DHS_SafeHavens_Undup_Cts[DHS_SafeHavens_Undup_Cts$Date=='February 2013',4]<-NA
set("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS")

write.csv(DHS_SafeHavens_Undup_Cts, "DHS_SafeHavens_Undup_Cts.csv", row.names = FALSE)


#####
#####next row: unduplicated persons in dhs stabilization beds
#######
dhs<-list()
for(i in 1:length(s))
{
  dhs[[i]] <- grep("stabilization beds",s[[i]])
  #dhs[[i]] <- grep("veterans shelters",s[[i]])
  #dhs[[i]] <- grep("safe havens",s[[i]])
}

dhs=as.character(dhs)
ind=c()
ind=which(dhs!="integer(0)")
ind=s[ind]

#first pull gets "  stabilization beds"  
pull1<-list()
#second pull gets 
# "DHS stabilization beds"                                                                                         
pull2<-list()
pull3<-list()
pull4<-list()
pull5<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- ind[[i]][grep("  stabilization beds",ind[[i]])-1]
  pull2[[i]] <- ind[[i]][grep("  stabilization beds",ind[[i]])]
  pull3[[i]] <- ind[[i]][grep("DHS stabilization beds" ,ind[[i]])-1][2]
  pull4[[i]] <- ind[[i]][grep("stabilization beds",ind[[i]])]
  pull5[[i]] <- ind[[i]][grep("DHS stabilization beds    ",ind[[i]])]
}

w1=nchar(pull2)>30
pull3[is.na(pull3)==TRUE]<-pull1[is.na(pull3)==TRUE]
pull3[which(w1==TRUE)]<-pull2[which(w1==TRUE)]
pull3[which(grepl("^stabilization", pull4)==TRUE)]<-pull4[which(grepl("^stabilization", pull4)==TRUE)]
pull3[36]<-pull5[36]
#missing number
pull3[8]<-list(paste0(unlist(pull3[8]), unlist(ind[[8]][3])))

pull=pull3[lengths(pull3)==1]
pull=gsub("[^.0-9[:space:]]", "", pull)

#make some vector where you'll store data you want 
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

DHS_StabilizationBeds_Undup_Cts <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
DHS_StabilizationBeds_Undup_Cts <- data.table(cbind(fdate,date,DHS_StabilizationBeds_Undup_Cts))
names(DHS_StabilizationBeds_Undup_Cts)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                                               "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
#visual check for errors
setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS")
write.csv(DHS_StabilizationBeds_Undup_Cts, "DHS_StabilizationBeds_Undup_Cts.csv", row.names = FALSE)


#####
#####next row: unduplicated persons in dhs veterans shelters

dhs<-list()
for(i in 1:length(s))
{
  #dhs[[i]] <- grep("stabilization beds",s[[i]])
  dhs[[i]] <- grep("veterans shelters",s[[i]])
  #dhs[[i]] <- grep("safe havens",s[[i]])
}

dhs=as.character(dhs)
ind=c()
ind=which(dhs!="integer(0)")
ind=s[ind]

#first pull gets "  veterans shelters"  
pull1<-list()
#second pull gets 
# "DHS veterans shelters"                                                                                         
pull2<-list()
pull3<-list()
pull4<-list()
pull5<-list()
pull6<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- ind[[i]][grep("  veterans shelters",ind[[i]])-1]
  pull2[[i]] <- ind[[i]][grep("  veterans shelters",ind[[i]])]
  pull3[[i]] <- ind[[i]][grep("DHS veterans shelters" ,ind[[i]])-1][2]
  pull4[[i]] <- ind[[i]][grep("veterans shelters",ind[[i]])][1]
  pull5[[i]] <- ind[[i]][grep("veterans shelters",ind[[i]])][2]
  pull6[[i]] <- ind[[i]][grep("DHS veterans shelters    ",ind[[i]])]
}

w1=nchar(pull2)>50
pull3[is.na(pull3)==TRUE]<-pull1[is.na(pull3)==TRUE]
pull3[which(w1==TRUE)]<-pull2[which(w1==TRUE)]

pull3[which(grepl("^stabilization", pull4)==TRUE)]<-pull4[which(grepl("^stabilization", pull4)==TRUE)]
pull3[c(16,18,20,22,24,26,36)]<-pull4[c(16,18,20,22,24,26,36)]
pull3[37:84]<-pull5[37:84]
pull3[54]<-ind[[54]][grep("DHS veterans shelters" ,ind[[54]])-1]
pull3[88]<-gsub("DNA", 0,pull3[88])

pull=pull3[lengths(pull3)==1]
pull=pull[is.na(pull)==FALSE]
pull=gsub("[^.0-9[:space:]]", "", pull)

#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

DHS_Vets_Undup_Cts <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
DHS_Vets_Undup_Cts <- data.table(cbind(fdate,date,DHS_Vets_Undup_Cts))
names(DHS_Vets_Undup_Cts)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                                             "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")

#visual check for errors
#NAs are blank in pdf for january 2012 and december 2011
setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS")
write.csv(DHS_Vets_Undup_Cts, "DHS_Vets_Undup_Cts.csv", row.names = FALSE)



#####
#####next row: unduplicated persons in dhs cj shelters

#first pull gets "  veterans shelters"  
pull1<-list()
#second pull gets 
# "DHS veterans shelters"                                                                                         
pull2<-list()
pull3<-list()
pull4<-list()
pull5<-list()
pull6<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- ind[[i]][grep("  CJ shelters",ind[[i]])-1]
  pull2[[i]] <- ind[[i]][grep("  CJ shelters",ind[[i]])]
  pull3[[i]] <- ind[[i]][grep("DHS CJ shelters" ,ind[[i]])-1][2]
  pull4[[i]] <- ind[[i]][grep("CJ shelters",ind[[i]])][1]
  pull5[[i]] <- ind[[i]][grep("CJ shelters",ind[[i]])][2]
  pull6[[i]] <- ind[[i]][grep("DHS CJ shelters    ",ind[[i]])]
}

w1=nchar(pull2)>50
pull1[which(w1==TRUE)]<-pull2[which(w1==TRUE)]
pull1[which(nchar(pull4)==55)]<-pull4[which(nchar(pull4)==55)]
pull1[36]<-pull6[36]

pull=pull1[lengths(pull1)==1]
pull=gsub("[^.0-9[:space:]]", "", pull)

#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()
v6 <-c()
v7 <-c()
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

DHS_CJ_Undup_Cts <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
#CJ beds start on December 2017, susbet date to then
DHS_CJ_Undup_Cts <- data.table(cbind(fdate[1:18],date[1:18],DHS_CJ_Undup_Cts))
names(DHS_CJ_Undup_Cts)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies", 
                                            "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")

#visual check for errors
setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS")
write.csv(DHS_CJ_Undup_Cts, "DHS_CJ_Undup_Cts.csv", row.names = FALSE)




#####
#next row: unduplicated persons in dhs entrants to dhs

dhs<-list()
for(i in 1:length(s))
{
  #dhs[[i]] <- grep("stabilization beds",s[[i]])
  dhs[[i]] <- grep("entrants",s[[i]])
  #dhs[[i]] <- grep("safe havens",s[[i]])
}

dhs=as.character(dhs)
ind=c()
ind=which(dhs!="integer(0)")
ind=s[ind]

#first pull gets "  Entrants to DHS-"  
pull1<-list()
pull2<-list()
pull3<-list()


for(i in 1:length(ind))
{
  pull1[[i]] <- str_trim(ind[[i]][grep("entrants to DHS",ind[[i]])],side = c("both"))
  pull2[[i]] <- str_trim(ind[[i]][grep("entrants to DHS",ind[[i]])+1],side = c("both"))
  pull3[[i]] <- str_trim(ind[[i]][grep("Entrants to DHS",ind[[i]])],side = c("both"))
}


n=c()
for (i in 1:length(pull2)){
  n[i]=nchar(pull2[i])
}

w1=nchar(pull2)==23
pull2[which(w1==TRUE)]<-pull1[which(w1==TRUE)]
pull2[66:92]<-pull1[66:92]
pull2[which(w1==TRUE)]<-pull3[which(w1==TRUE)]
pull2[3]<-pull1[3]

for (i in 1:length(pull2))
{
  pull2[i]<-pull2[[i]][1]
}

pull=str_squish(str_trim(gsub("[^.0-9[:space:]]", "", pull2), side = c('both')))

#make some vector where you'll store data you want for the entrants.
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]]
  }
}

#lengths(t)

v1 <- c()
v2 <-c()
v3 <-c()
v4 <-c()
v5 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
}

DHS_Entrants <- data.table(cbind(v1,v2,v3,v4,v5))
DHS_Entrants <- data.table(cbind(fdate,date,DHS_Entrants))
names(DHS_Entrants)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", "AdultFamilies")
#visual check for errors
#months feb-june 2012 have values shifted with NAs removed or due to blanks in the pdf
DHS_Entrants[83:87,6:7]<-DHS_Entrants[83:87,3:4]
DHS_Entrants[83:87,3:4]<-NA

setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/DHS")
write.csv(DHS_Entrants, "DHS_Entrants.csv", row.names = FALSE)


#######
## left off here!!
########

#######
#HRA DV 2018 backwards
##########

date<-list()

for(i in 1:length(s))
{
  date[[i]] <- s[[i]][grep("HRA Local Law 37 report",s[[i]], ignore.case = TRUE)]
  
}

date[grep("character",date)] <- "NA"

date <- unlist(date)
date=date[which(date!="NA")]
date=gsub("HRA Local Law 37 report for the Month of ", "", date, ignore.case = TRUE)
date=gsub("Febuary", "February", date, ignore.case = TRUE)
date=trimws(date, which = c("both"))
fdate=as.Date(paste('01', date), format='%d %b %Y')

setwd("/Users/romartinez/Desktop/Data_Projects/Homelessness/data/historical_LL37_temp_housing/HRA")
write.csv(date, "hra_dates.csv")

######
#HRA DV census
hra<-list()
for(i in 1:length(s))
{
  hra[[i]] <- grep("HRA domestic violence",s[[i]])
}

hra=as.character(hra)
ind=c()
ind=which(hra!="integer(0)")
ind=s[ind]

#first pull 
pull1<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- str_trim(ind[[i]][grep("Census: HRA domestic violence shelters*",ind[[i]])+1:9],side = c("both"))
}

#fwc
v1 <- c()
#tot adults in families
v2 <-c()
#tot children
v3 <-c()
#tot sa
v4 <-c()
#single women
v5 <- c()
#single men
v6 <-c()
#tot families
v7 <- c()
#adult families
v8 <- c()
#error
v9 <- c()

for(i in 1:length(pull1))
{
  v1[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][1]))
  v2[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][2]))
  v3[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][3]))
  v4[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][4]))
  v5[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][5]))
  v6[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][6]))
  v7[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][7]))
  v8[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][8]))
  v9[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][9]))
}

census_hra_dv=data.frame(cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9))
census_hra_dv <- data.table(cbind(fdate,date,census_hra_dv))
names(census_hra_dv)<-c("Fixed_Date", "Date", "FamiliesWithChildren","TotalAdultsinFamilies", 
                       "TotalChildren", "TotalSingleAdults","SingleWomen","SingleMen","TotalFamilies", "AdultFamilies", "Error")
#shift error
census_hra_dv[83:91 ,6:9]<-census_hra_dv[83:91 ,7:10]
census_hra_dv=census_hra_dv[,-c(10:11)]

write.csv(census_hra_dv,'HRA_DV_census.csv', row.names = FALSE)

##### 
# HRA DV unduplicated count
hra<-list()
for(i in 1:length(s))
{
  hra[[i]] <- grep("HRA domestic violence",s[[i]])
}

hra=as.character(hra)
ind=c()
ind=which(hra!="integer(0)")
ind=s[ind]

#first pull  
pull1<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- str_trim(ind[[i]][grep("Number of unduplicated persons: HRA domestic violence shelters*",ind[[i]], 
                                       ignore.case = TRUE)+1:7],side = c("both"))
}

#fwc
v1 <- c()
#tot adults in families
v2 <-c()
#tot children
v3 <-c()
#tot sa
v4 <-c()
#single women
v5 <- c()
#single men
v6 <-c()
#tot families
v7 <- c()

for(i in 1:length(pull1))
{
  v1[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][1]))
  v2[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][2]))
  v3[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][3]))
  v4[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][4]))
  v5[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][5]))
  v6[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][6]))
  v7[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull1[[i]][7]))
}

undup_hra_dv=data.frame(cbind(v1,v2,v3,v4,v5,v6,v7))
undup_hra_dv <- data.table(cbind(fdate,date,undup_hra_dv))
names(undup_hra_dv)<-c("Fixed_Date", "Date", "FamiliesWithChildren","TotalAdultsinFamilies", 
                        "TotalChildren", "TotalSingleAdults","SingleWomen","SingleMen","TotalFamilies")
write.csv(undup_hra_dv,'HRA_DV_undup.csv', row.names = FALSE)

######
#HRA HASA census transitional housing
hra<-list()
for(i in 1:length(s))
{
  hra[[i]] <- grep("(HASA)",s[[i]])
}

hra=as.character(hra)
ind=c()
ind=which(hra!="integer(0)")
ind=s[ind]


#first pull
pull1<-list()
pull2<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- str_trim(ind[[i]][grep("Census: HRA HASA transitional",ind[[i]], 
                                       ignore.case = TRUE)],side = c("both"))
  pull2[[i]] <- str_trim(ind[[i]][grep("Census: HRA HASA transitional",ind[[i]], 
                                       ignore.case = TRUE)+1],side = c("both"))
}

pull1[1:39]<-pull2[1:39]
pull1[83:91]<-pull3[83:91]

pull=pull1[is.na(pull1)==FALSE]
pull=gsub("[^.0-9[:space:]]", "", pull)

#make some vector where you'll store data 
t <-list()

for(i in 1:length(pull))
{
  t[[i]] <- strsplit(pull[[i]]," ")
  if(length(t[[i]])>0)
  {
    t[[i]] <- t[[i]][[1]][-which(t[[i]][[1]]=="")]	
  }
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

HASA_census_transitional <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))

#get remaining months earlier
ind2=list()
for (i in 1:length(ind)){
  ind2[[i]]=str_squish(ind[[i]])
}

pull3<-list()

for(i in 1:length(ind2))
  {
pull3[[i]] <- str_trim(ind2[[i]][grep("Census: HIV/ AIDS Emergency Placements*",
                                     ind2[[i]],ignore.case = TRUE)+2:9],side = c("both"))
}


#fwc
v1 <- c()
##tot adults in families
v2 <-c()
#tot children
v3 <-c()
#tot sa
v4 <-c()
#single women
v5 <- c()
#single men
v6 <-c()
#tot families
v7 <- c()
#adult families
v8 <- c()

for(i in 1:length(pull3))
{
  v1[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][1]))
  v2[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][2]))
  v3[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][3]))
  v4[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][4]))
  v5[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][5]))
  v6[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][6]))
  v7[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][7]))
  v8[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][8]))
}

#transitional is second

for(i in 1:length(v1))
{
  v1[[i]] <- strsplit(v1[[i]]," ")[[1]][2]
  v2[[i]] <- strsplit(v2[[i]]," ")[[1]][2]
  v3[[i]] <- strsplit(v3[[i]]," ")[[1]][2]
  v4[[i]] <- strsplit(v4[[i]]," ")[[1]][2]
  v5[[i]] <- strsplit(v5[[i]]," ")[[1]][2]
  v6[[i]] <- strsplit(v6[[i]]," ")[[1]][2]
  v7[[i]] <- strsplit(v7[[i]]," ")[[1]][2]
  v8[[i]] <- strsplit(v8[[i]]," ")[[1]][2]
}

rem=data.frame(cbind(v6,v5,v4,v1,v8,v7,v2,v3))
HASA_census_transitional[83:91,1:8]<-rem[83:91,1:8]

#missing info for months in rows 67 & 81


HASA_census_transitional <- data.table(cbind(fdate,date,HASA_census_transitional))
names(HASA_census_transitional)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                                   "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
write.csv(HASA_census_transitional,'HASA_census_transitional.csv', row.names = FALSE)

#get december 2018 onwards from open data, reporting is funny for 2019 in the pdf report

######
#HRA HASA census emergency housing
#fwc
v1 <- c()
##tot adults in families
v2 <-c()
#tot children
v3 <-c()
#tot sa
v4 <-c()
#single women
v5 <- c()
#single men
v6 <-c()
#tot families
v7 <- c()
#adult families
v8 <- c()

for(i in 1:length(pull3))
{
  v1[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][1]))
  v2[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][2]))
  v3[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][3]))
  v4[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][4]))
  v5[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][5]))
  v6[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][6]))
  v7[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][7]))
  v8[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][8]))
}

#emergency is first

for(i in 1:length(v1))
{
  v1[[i]] <- strsplit(v1[[i]]," ")[[1]][1]
  v2[[i]] <- strsplit(v2[[i]]," ")[[1]][1]
  v3[[i]] <- strsplit(v3[[i]]," ")[[1]][1]
  v4[[i]] <- strsplit(v4[[i]]," ")[[1]][1]
  v5[[i]] <- strsplit(v5[[i]]," ")[[1]][1]
  v6[[i]] <- strsplit(v6[[i]]," ")[[1]][1]
  v7[[i]] <- strsplit(v7[[i]]," ")[[1]][1]
  v8[[i]] <- strsplit(v8[[i]]," ")[[1]][1]
}

rem=data.frame(cbind(v6,v5,v4,v1,v8,v7,v2,v3))

#####
# left off here here

#first pull 
pull1<-list()
pull2<-list()

for(i in 1:length(ind2))
{
  pull1[[i]] <- str_trim(gsub("[^.0-9[:space:]]", "",ind2[[i]][grep("Census: HRA HASA emergency",ind2[[i]], 
                                       ignore.case = TRUE)+0]),side = c("both"))
  pull2[[i]] <- str_trim(gsub("[^.0-9[:space:]]", "",ind2[[i]][grep("Census: HRA HASA emergency",ind2[[i]], 
                                        ignore.case = TRUE)+1]),side = c("both"))
}

pull1[which(pull1=='')]<-pull2[which(pull1=='')]

for (i in 1:length(pull1))
  {if(nchar(pull1[[i]])<30){
    pull1[[i]]=paste(pull1[[i]],pull2[[i]])} else{pull1[[i]]=pull2[[i]]}
}


ll=c()
for (i in c(1:82,92:94)){
  ll[[i]]=nchar(pull1[[i]])
}

pull=gsub("[^.0-9[:space:]]", "", pull1)
#make some vector where you'll store data 
t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

HASA_census_emergency <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
HASA_census_emergency[83:91,1:8]<-rem[83:91,1:8]
HASA_census_emergency <- data.table(cbind(fdate,date,HASA_census_emergency))
names(HASA_census_emergency)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                                   "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
write.csv(HASA_census_emergency,'HASA_census_emergency.csv', row.names = FALSE)

######
#HRA HASA undup emergency housing

#first pull 
pull1<-list()
pull2<-list()


for(i in 1:length(ind2))
{
  pull1[[i]] <- str_trim(gsub("[^.0-9[:space:]]", "",ind2[[i]][grep("HASA emergency housing",ind2[[i]], 
                                                                    ignore.case = TRUE)-1]),side = c("both"))
  pull2[[i]] <- str_trim(ind2[[i]][grep("Number of unduplicated persons:",ind2[[i]], 
                                                                    ignore.case = TRUE)+1],side = c("both"))[1]
}

pull1[which(pull1=='')]<-pull2[which(pull1=='')]

pull=gsub("[^.0-9[:space:]]", "", pull1)
#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

HASA_undup_emergency <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))

#get missing earlier months
pull3<-list()

for(i in 1:length(ind2))
{
  pull3[[i]] <- str_trim(ind2[[i]][grep("Number of unduplicated persons:: HIV/ AIDS Emergency Placements",
                                        ind2[[i]],ignore.case = TRUE)+2:9],side = c("both"))
}


#fwc
v1 <- c()
##tot adults in families
v2 <-c()
#tot children
v3 <-c()
#tot sa
v4 <-c()
#single women
v5 <- c()
#single men
v6 <-c()
#tot families
v7 <- c()
#adult families
v8 <- c()

for(i in 1:length(pull3))
{
  v1[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][1]))
  v2[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][2]))
  v3[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][3]))
  v4[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][4]))
  v5[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][5]))
  v6[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][6]))
  v7[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][7]))
  v8[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][8]))
}

#emergency is first

for(i in 1:length(v1))
{
  v1[[i]] <- strsplit(v1[[i]]," ")[[1]][1]
  v2[[i]] <- strsplit(v2[[i]]," ")[[1]][1]
  v3[[i]] <- strsplit(v3[[i]]," ")[[1]][1]
  v4[[i]] <- strsplit(v4[[i]]," ")[[1]][1]
  v5[[i]] <- strsplit(v5[[i]]," ")[[1]][1]
  v6[[i]] <- strsplit(v6[[i]]," ")[[1]][1]
  v7[[i]] <- strsplit(v7[[i]]," ")[[1]][1]
  v8[[i]] <- strsplit(v8[[i]]," ")[[1]][1]
}

rem=data.frame(cbind(v6,v5,v4,v1,v8,v7,v2,v3))
HASA_undup_emergency[83:91,1:8]<-rem[83:91,1:8]

HASA_undup_emergency <- data.table(cbind(fdate,date,HASA_undup_emergency))
names(HASA_undup_emergency)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                                "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")

write.csv(HASA_undup_emergency,'HASA_undup_emergency.csv', row.names = FALSE)


######
#HRA HASA undup transitional housing

#first pull gets "  Entrants to DHS-"  
pull1<-list()
pull2<-list()


for(i in 1:length(ind2))
{
  pull1[[i]] <- str_trim(gsub("[^.0-9[:space:]]", "",ind2[[i]][grep("HASA transitional housing",ind2[[i]], 
                                                                    ignore.case = TRUE)-1]),side = c("both"))
  pull2[[i]] <- str_trim(gsub("[^.0-9[:space:]]", "",ind2[[i]][grep("Number of unduplicated persons:",ind2[[i]], 
                                                                    ignore.case = TRUE)+1]),side = c("both"))[2]
}

pull1[which(pull1=='')]<-pull2[which(pull1=='')]

pull=gsub("[^.0-9[:space:]]", "", pull1)
#make some vector where you'll store data you want for the avg length of stay.
t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

HASA_undup_transitional <- data.table(cbind(v1,v2,v3,v4,v5,v6,v7,v8))


###############3LEFT OFF HERE
#get missing earlier months
pull3<-list()

for(i in 1:length(ind2))
{
  pull3[[i]] <- str_trim(ind2[[i]][grep("Number of unduplicated persons:: HIV/ AIDS Emergency Placements",
                                        ind2[[i]],ignore.case = TRUE)+2:9],side = c("both"))
}


#fwc
v1 <- c()
##tot adults in families
v2 <-c()
#tot children
v3 <-c()
#tot sa
v4 <-c()
#single women
v5 <- c()
#single men
v6 <-c()
#tot families
v7 <- c()
#adult families
v8 <- c()

for(i in 1:length(pull3))
{
  v1[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][1]))
  v2[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][2]))
  v3[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][3]))
  v4[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][4]))
  v5[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][5]))
  v6[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][6]))
  v7[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][7]))
  v8[i] <- str_trim(gsub("[^.0-9[:space:]]", "",pull3[[i]][8]))
}

#emergency is first

for(i in 1:length(v1))
{
  v1[[i]] <- strsplit(v1[[i]]," ")[[1]][2]
  v2[[i]] <- strsplit(v2[[i]]," ")[[1]][2]
  v3[[i]] <- strsplit(v3[[i]]," ")[[1]][2]
  v4[[i]] <- strsplit(v4[[i]]," ")[[1]][2]
  v5[[i]] <- strsplit(v5[[i]]," ")[[1]][2]
  v6[[i]] <- strsplit(v6[[i]]," ")[[1]][2]
  v7[[i]] <- strsplit(v7[[i]]," ")[[1]][2]
  v8[[i]] <- strsplit(v8[[i]]," ")[[1]][2]
}

rem=data.frame(cbind(v6,v5,v4,v1,v8,v7,v2,v3))
HASA_undup_transitional[83:91,1:8]<-rem[83:91,1:8]

HASA_undup_transitional <- data.table(cbind(fdate,date,HASA_undup_transitional))
names(HASA_undup_transitional)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                               "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")

setwd('/Users/romartinez/Desktop/Data_Projects/Homelessness/HRA')
write.csv(HASA_undup_transitional,'HASA_undup_transitional.csv', row.names = FALSE)






#####
# DYCD
######
date<-list()

for(i in 1:length(s))
{
  date[[i]] <- s[[i]][grep("DYCD Local Law 37 report for the Month of",s[[i]], ignore.case = TRUE)]
  
}

date[grep("character",date)] <- "NA"

date <- unlist(date)
date=date[which(date!="NA")]
date=str_squish(gsub("DYCD Local Law 37 report for the Month of ", "", date, ignore.case = TRUE))
date=gsub(" men women adults children families families families children","",date)
date=gsub(" men women adults children families families children","",date)

date=gsub("Febuary", "February", date, ignore.case = TRUE)
date=trimws(date, which = c("both"))
fdate=as.Date(paste('01', date), format='%d %b %Y')
#write.csv(date, "dhs_dates.csv")

#subset to dycd pages only
dycd<-list()
for(i in 1:length(s))
{
  #dycd[[i]] <- grep("DYCD Local Law 37",s[[i]])
  dycd[[i]] <- grep("DYCD",s[[i]])
  #use this one for average length of stay
}

dycd=as.character(dycd)
ind=c()
ind=which(dycd!="integer(0)")
ind=s[ind]

ind2=list()
for (i in 1:length(ind)){
  ind2[[i]]=str_squish(ind[[i]])
}

######
#first pull gets "number of unduplicated persons - DYCD-administered facilities"  
pull1<-list()
pull2<-list()
pull3<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- str_trim(ind2[[i]][grep("- DYCD-administered facilities",ind2[[i]], 
                                       ignore.case = TRUE)-1],side = c("both"))[1]
  pull2[[i]] <- str_trim(ind2[[i]][grep("number of unduplicated persons - DYCD-administered facilities",ind2[[i]], 
                                       ignore.case = TRUE)+0],side = c("both"))
  pull3[[i]] <- str_trim(ind2[[i]][grep('number of unduplicated persons ‐ DYCD‐administered facilities',ind2[[i]], 
                                        ignore.case = TRUE)+0],side = c("both"))
}

pull1[which(pull2!='character(0)')]<-pull2[which(pull2!='character(0)')]
pull1[which(pull3!='character(0)')]<-pull3[which(pull3!='character(0)')]


pull=pull1[is.na(pull1)==FALSE]
pull=gsub("^\\D+", "", pull)
#pull=gsub("n/a", NA, pull)

t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

dycd_undup_adminFac=data.frame(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
dycd_undup_adminFac <- data.table(cbind(fdate,date,dycd_undup_adminFac))
names(dycd_undup_adminFac)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                                  "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
setwd('/Users/romartinez/Desktop/Data_Projects/Homelessness/DYCD')
write.csv(dycd_undup_adminFac,'dycd_undup_adminFac.csv',row.names = FALSE)


#####
#second pull gets "number of unduplicated persons - DYCD-administered facilities"  
pull1<-list()
pull2<-list()
pull3<-list()
pull4<-list()

for(i in 1:length(ind))
{
  pull1[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep("- DYCD-administered crisis",ind2[[i]], 
                                        ignore.case = TRUE)+0],side = c("both"))[1])
  pull2[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep("number of unduplicated persons - DYCD-administered crisis",ind2[[i]], 
                                        ignore.case = TRUE)+0],side = c("both")))
  pull3[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep('number of unduplicated persons ‐ DYCD‐administered crisis',ind2[[i]], 
                                        ignore.case = TRUE)+0],side = c("both")))
  pull4[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep("- DYCD-administered crisis",ind2[[i]], 
                                        ignore.case = TRUE)-1],side = c("both"))[1])
}

pull1[which(pull2!='character(0)')]<-pull2[which(pull2!='character(0)')]
pull1[which(pull3!='character(0)')]<-pull3[which(pull3!='character(0)')]
pull1[which(pull1=='')]<-pull4[which(pull1=='')]


pull=pull1[is.na(pull1)==FALSE]
pull=gsub("^\\D+", "", pull)
#pull=gsub("n/a", NA, pull)

t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}



#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

dycd_undup_crisis=data.frame(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
dycd_undup_crisis <- data.table(cbind(fdate,date,dycd_undup_crisis))
names(dycd_undup_crisis)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                              "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
setwd('/Users/romartinez/Desktop/Data_Projects/Homelessness/DYCD')
write.csv(dycd_undup_crisis,'dycd_undup_crisis.csv',row.names = FALSE)

#####
#third pull gets "number of unduplicated persons - drop-in centers"  
pull1<-list()
pull2<-list()
pull3<-list()
pull4<-list()

#remove gsub part to check outputs before

for(i in 1:length(ind2))
{
  pull1[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep("- DYCD-administered drop-in",ind2[[i]], 
                                                          ignore.case = TRUE)+0],side = c("both"))[1])
  pull2[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep("number of unduplicated persons - DYCD-administered drop-in centers",ind2[[i]], 
                                                          ignore.case = TRUE)+0],side = c("both")))
  pull3[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep('number of unduplicated persons ‐ DYCD‐administered drop‐in centers',ind2[[i]], 
                                                          ignore.case = TRUE)+0],side = c("both")))
  pull4[[i]] <- gsub("^\\D+", "", str_trim(ind2[[i]][grep("- DYCD-administered drop-in",ind2[[i]], 
                                                          ignore.case = TRUE)-1],side = c("both"))[1])
}

pull1[which(pull2!='character(0)')]<-pull2[which(pull2!='character(0)')]
pull1[which(pull3!='character(0)')]<-pull3[which(pull3!='character(0)')]
pull1[which(pull1=='')]<-pull4[which(pull1=='')]


pull=pull1[is.na(pull1)==FALSE]
pull=gsub("^\\D+", "", pull)
#pull=gsub("n/a", NA, pull)

t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

dycd_undup_dropin=data.frame(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
dycd_undup_dropin <- data.table(cbind(fdate,date,dycd_undup_dropin))
names(dycd_undup_dropin)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                            "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
setwd('/Users/romartinez/Desktop/Data_Projects/Homelessness/DYCD')
write.csv(dycd_undup_dropin,'dycd_undup_dropin.csv',row.names = FALSE)





#####
#third pull gets "number of unduplicated persons - transitional"  
pull1<-list()
pull2<-list()
pull3<-list()
pull4<-list()
pull5<-list()


#remove gsub("^\\D+", "",  part to check outputs before

for(i in 1:length(ind2))
{
  pull1[[i]] <- str_trim(ind2[[i]][grep('number of unduplicated persons ‐ DYCD‐administered transitional independent living',ind2[[i]], 
                                                          ignore.case = TRUE)+1],side = c("both"))
  pull2[[i]] <- str_trim(ind2[[i]][grep('number of unduplicated persons - DYCD-administered transitional independent living',ind2[[i]], 
                                        ignore.case = TRUE)+1],side = c("both"))
  pull3[[i]] <- str_trim(ind2[[i]][grep("transitional independent living",ind2[[i]], 
                                                          ignore.case = TRUE)-1],side = c("both"))[1]
  pull4[[i]] <- str_trim(ind2[[i]][grep("transitional independent living",ind2[[i]], 
                                                          ignore.case = TRUE)+0],side = c("both"))[1]
  pull5[[i]] <- str_trim(ind2[[i]][grep("transitional independent living",ind2[[i]], 
                                        ignore.case = TRUE)-2],side = c("both"))[1]
}


pull1[which(pull2!='character(0)')]<-pull2[which(pull2!='character(0)')]
pull1[which(pull1=='character(0)')]<-pull4[which(pull1=='character(0)')]
pull1[which(nchar(pull3)>30 & nchar(pull3)<60)]<-pull3[which(nchar(pull3)>30 & nchar(pull3)<60)]
pull1[which(nchar(pull5)>40 & nchar(pull5)<80)]<-pull5[which(nchar(pull5)>40 & nchar(pull5)<80)]

pull=gsub("^\\D+", "", pull1)
#exception: ind2[c(6,60)]
pull[60]<-paste(pull[60], gsub("^\\D+", "",pull3[60]))
pull[6]<-paste(pull[6], gsub("^\\D+", "",pull5[6]))


pull=pull[is.na(pull)==FALSE]
pull=pull[pull!='']

#pull=gsub("n/a", NA, pull)

t <-list()

for(i in 1:length(pull))
{
  t[i] <- strsplit(pull[i]," ")
  
}

#lengths(t)

#single men
v1 <- c()
#single women
v2 <-c()
#tot sa
v3 <-c()
#fwc
v4 <-c()
#adult families
v5 <- c()
#Total families
v6 <-c()
#tot adults in families
v7 <-c()
#tot children
v8 <- c()


for(i in 1:length(t))
{
  v1[i] <- t[[i]][1]
  v2[i] <- t[[i]][2]
  v3[i] <- t[[i]][3]
  v4[i] <- t[[i]][4]
  v5[i] <- t[[i]][5]
  v6[i] <- t[[i]][6]
  v7[i] <- t[[i]][7]
  v8[i] <- t[[i]][8]
}

dycd_undup_transitional=data.frame(cbind(v1,v2,v3,v4,v5,v6,v7,v8))
dycd_undup_transitional <- data.table(cbind(fdate,date,dycd_undup_transitional))
names(dycd_undup_transitional)<-c("Fixed_Date", "Date", "SingleMen","SingleWomen","TotalSingleAdults", "FamiliesWithChildren", 
                            "AdultFamilies", "TotalFamilies","NumberofAdultsinFamilies","NumberOfChildrenInFamilies")
setwd('/Users/romartinez/Desktop/Data_Projects/Homelessness/DYCD')
write.csv(dycd_undup_transitional,'dycd_undup_transitional.csv',row.names = FALSE)



