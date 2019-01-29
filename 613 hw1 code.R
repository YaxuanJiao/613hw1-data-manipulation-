#Exercise 1 Missing Data
#import datstu.csv
studentinfo <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)

#import datsss.csv
addressinfo <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)

#import datjss.csv
districtxy <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)

#check if the data has been imported correctly and report the number of students
studentinfo

#number of students
dim(studentinfo)

#combine 6 schoolcodes, delete duplications and report the number of schools
schoolcode <- c(studentinfo$schoolcode1,studentinfo$schoolcode2,studentinfo$schoolcode3,studentinfo$schoolcode4,studentinfo$schoolcode5,studentinfo$schoolcode6)
length(unique(schoolcode))

#combine 6 programcodes, delete duplications and report the number of programs
programcode <- c(studentinfo$choicepgm1,studentinfo$choicepgm2,studentinfo$choicepgm3,studentinfo$choicepgm4,studentinfo$choicepgm5,studentinfo$choicepgm6)
length(unique(programcode))-1

#number of choices (school,program) 
c1 <- data.frame(studentinfo$schoolcode1, studentinfo$choicepgm1)
c2 <- data.frame(studentinfo$schoolcode2, studentinfo$choicepgm2) 
c3 <- data.frame(studentinfo$schoolcode3, studentinfo$choicepgm3)
c4 <- data.frame(studentinfo$schoolcode4, studentinfo$choicepgm4)
c5 <- data.frame(studentinfo$schoolcode5, studentinfo$choicepgm5)
c6 <- data.frame(studentinfo$schoolcode6, studentinfo$choicepgm6)
names(c1) <- c("schoolcode","program name")
names(c2) <- c("schoolcode","program name")
names(c3) <- c("schoolcode","program name")
names(c4) <- c("schoolcode","program name")
names(c5) <- c("schoolcode","program name")
names(c6) <- c("schoolcode","program name")
choices <- rbind(c1,c2,c3,c4,c5,c6)
uchoices <- unique(choices)
uchoices[uchoices==""|uchoices =="NA"] <- NA
unchoices <- na.omit(uchoices)
write.csv(unchoices,file = "~/Desktop/number of choices.csv", row.names = FALSE)

#missing test score 
uscore <- unique(studentinfo)
sum(is.na(studentinfo$score))

#apply to the same school 
codefr <- table(studentinfo$X, studentinfo$schoolcode1==studentinfo$schoolcode2|studentinfo$schoolcode1==studentinfo$schoolcode3|studentinfo$schoolcode1==studentinfo$schoolcode4|studentinfo$schoolcode1==studentinfo$schoolcode5|studentinfo$schoolcode1==studentinfo$schoolcode6|studentinfo$schoolcode2==studentinfo$schoolcode3|studentinfo$schoolcode2==studentinfo$schoolcode4|studentinfo$schoolcode2==studentinfo$schoolcode5|studentinfo$schoolcode2==studentinfo$schoolcode6|studentinfo$schoolcode3==studentinfo$schoolcode4|studentinfo$schoolcode3==studentinfo$schoolcode5|studentinfo$schoolcode3==studentinfo$schoolcode6|studentinfo$schoolcode4==studentinfo$schoolcode5|studentinfo$schoolcode4==studentinfo$schoolcode6|studentinfo$schoolcode5==studentinfo$schoolcode6)
colnames(codefr) <- c("false","true")
codefr

#sum the columns of the table
margin.table(codefr,margin = 2) 

#apply to less than 6 choices
studentinfo$choicepgm1[studentinfo$choicepgm1==""|studentinfo$choicepgm1 =="NA"] <- NA
studentinfo$choicepgm2[studentinfo$choicepgm2==""|studentinfo$choicepgm2 =="NA"] <- NA
studentinfo$choicepgm3[studentinfo$choicepgm3==""|studentinfo$choicepgm3 =="NA"] <- NA
studentinfo$choicepgm4[studentinfo$choicepgm4==""|studentinfo$choicepgm4 =="NA"] <- NA
studentinfo$choicepgm5[studentinfo$choicepgm5==""|studentinfo$choicepgm5 =="NA"] <- NA
studentinfo$choicepgm6[studentinfo$choicepgm6==""|studentinfo$choicepgm6 =="NA"] <- NA
programfre <- table(studentinfo$X,is.na(studentinfo$choicepgm1)|is.na(studentinfo$choicepgm2)|is.na(studentinfo$choicepgm3)|is.na(studentinfo$choicepgm4)|is.na(studentinfo$choicepgm5)|is.na(studentinfo$choicepgm6))
programfre 
margin.table(programfre,margin=2)


#Exercise 2 Data 
#combine (school,program,score)
datainto <- addressinfo[c(2,3,4,5,6)]
udatainto <- unique(datainto)
s1 <- data.frame(studentinfo$schoolcode1, studentinfo$choicepgm1, studentinfo$score)
s2 <- data.frame(studentinfo$schoolcode2, studentinfo$choicepgm2, studentinfo$score) 
s3 <- data.frame(studentinfo$schoolcode3, studentinfo$choicepgm3, studentinfo$score)
s4 <- data.frame(studentinfo$schoolcode4, studentinfo$choicepgm4, studentinfo$score)
s5 <- data.frame(studentinfo$schoolcode5, studentinfo$choicepgm5, studentinfo$score)
s6 <- data.frame(studentinfo$schoolcode6, studentinfo$choicepgm6, studentinfo$score)
names(s1) <- c("schoolcode","program name","score")
names(s2) <- c("schoolcode","program name","score")
names(s3) <- c("schoolcode","program name","score")
names(s4) <- c("schoolcode","program name","score")
names(s5) <- c("schoolcode","program name","score")
names(s6) <- c("schoolcode","program name","score")
choices_s <- rbind(s1,s2,s3,s4,s5,s6)
choices_s[choices_s==""|choices_s =="NA"] <- NA
uchoices_s <- na.omit(choices_s)
unchoices_s <- unique(uchoices_s)

#merge address information into (school,program,score) combination 
mdata1 = merge(unchoices_s,udatainto,by = "schoolcode",all.x = TRUE) 
mdata1[mdata1==""|mdata1 =="NA"] <- NA
mdata2 <- na.omit(mdata1)
mdata2[!duplicated(mdata2[,c('schoolcode','program name')]),]
mdata2u <-mdata2[!duplicated(mdata2[,c('schoolcode','program name')]),]

#use package 
library(dplyr)

#group the data with same schoolcode and program, calculate the average admitted score
data_address_quality <- mdata2 %>%
  group_by(schoolcode, `program name`) %>%
  mutate(quality = mean(score))

#group the data with same schoolcode and program, compute the lowest score admitted
data_address_quality_cutoff <- data_address_quality %>%
  group_by(schoolcode) %>%
  mutate(cutoff = min(score))

#group the data with same schoolcode and program, count the number of students admitted by that school
data_address_quality_cutoff_size <- data_address_quality_cutoff %>%
  group_by(schoolcode) %>%
  mutate(size = length(score))
schoollevel_dataset <-data_address_quality_cutoff_size <-data_address_quality_cutoff_size[!duplicated(data_address_quality_cutoff_size[,c('schoolcode','program name')]),]
write.csv(schoollevel_dataset,file = "~/Desktop/school level dataset.csv", row.names = FALSE)

#Exercise 3 Distance
#create a new dataset contains all choices and jssdistrict
datainto <- addressinfo[c(2,3,4,5,6)]
udatainto <- unique(datainto)
d1 <- data.frame(studentinfo$schoolcode1, studentinfo$choicepgm1, studentinfo$jssdistrict)
d2 <- data.frame(studentinfo$schoolcode2, studentinfo$choicepgm2, studentinfo$jssdistrict) 
d3 <- data.frame(studentinfo$schoolcode3, studentinfo$choicepgm3, studentinfo$jssdistrict)
d4 <- data.frame(studentinfo$schoolcode4, studentinfo$choicepgm4, studentinfo$jssdistrict)
d5 <- data.frame(studentinfo$schoolcode5, studentinfo$choicepgm5, studentinfo$jssdistrict)
d6 <- data.frame(studentinfo$schoolcode6, studentinfo$choicepgm6, studentinfo$jssdistrict)
names(d1) <- c("schoolcode","program name","jssdistrict")
names(d2) <- c("schoolcode","program name","jssdistrict")
names(d3) <- c("schoolcode","program name","jssdistrict")
names(d4) <- c("schoolcode","program name","jssdistrict")
names(d5) <- c("schoolcode","program name","jssdistrict")
names(d6) <- c("schoolcode","program name","jssdistrict")
choices_j <- rbind(d1,d2,d3,d4,d5,d6)
uchoices_j <- unique(choices_j)
uchoices_j[uchoices_j==""|uchoices_j =="NA"] <- NA
unchoices_j <- na.omit(uchoices_j)

#merge datasets share the common column "schoolcode" to add sss address information 
mdata1 = merge(unchoices_j,udatainto,by = "schoolcode",all.x = TRUE) 
mdata1[mdata1==""|mdata1 =="NA"] <- NA
mdata2 <- na.omit(mdata1)

#merge datasets share the common column "jssdistrict" to add jss address information
mdata3 <- merge(mdata2,districtxy,by = "jssdistrict",all.x = TRUE)
mdata3[mdata3==""|mdata3 =="NA"] <- NA
mdata4 <- na.omit(mdata3)  
colnames(mdata4) [9] <- "jsslong"
colnames(mdata4) [10] <- "jsslat"

#calculate distance step by step in mdata4
#step1=mdata4$ssslong - mdata4$jsslong
#step2=cos(mdata4$jsslat/57.3)
#step3=69.172*(mdata4$ssslong - mdata4$jsslong)*cos(mdata4$jsslat/57.3)
#step4=(69.172*(mdata4$ssslong - mdata4$jsslong)*cos(mdata4$jsslat/57.3))^2
#step5=(mdata4$ssslat - mdata4$jsslat)
#step6=(69.172*(mdata4$ssslat - mdata4$jsslat))
#step7=((69.172*(mdata4$ssslat - mdata4$jsslat))^2)
#distance=sqrt(mdata4$step4+mdata4$step7)
mdata4$step1 <- (mdata4$ssslong - mdata4$jsslong)
mdata4$step2 <- cos(mdata4$jsslat/57.3)
mdata4$step3 <- (69.172*mdata4$step1*mdata4$step2)
mdata4$step4 <- (mdata4$step3)^2
mdata4$step5 <- (mdata4$ssslat - mdata4$jsslat)
mdata4$step6 <- (69.172*mdata4$step5)
mdata4$step7 <- ((mdata4$step6)^2)
mdata4$distance <- sqrt(mdata4$step4+mdata4$step7)
mdata4[,"distance"]
write.csv(mdata4,file = "~/Desktop/Distance.csv", row.names = FALSE)

#Exercise4 Descriptive Characteristics
#compute rank 1 
g1 <- select(studentinfo,2,5,11)
rank1 <- cbind(rank=1, g1) 
#compute rank 2
g2 <- select(studentinfo,2,6,12)
rank2 <- cbind(rank=2, g2)
#compute rank 3
g3 <- select(studentinfo,2,7,13)
rank3 <- cbind(rank=3, g3)
#compute rank 4
g4 <- select(studentinfo,2,8,14)
rank4 <- cbind(rank=4, g4)
#compute rank 5
g5 <- select(studentinfo,2,9,15)
rank5 <- cbind(rank=5, g5)
#compute rank 6
g6 <- select(studentinfo,2,10,16)
rank6 <- cbind(rank=6, g6)
#name columns
names(rank1) <- c('rank',"score","schoolcode","program name")
names(rank2) <- c('rank',"score","schoolcode","program name")
names(rank3) <- c('rank',"score","schoolcode","program name")
names(rank4) <- c('rank',"score","schoolcode","program name")
names(rank5) <- c('rank',"score","schoolcode","program name")
names(rank6) <- c('rank',"score","schoolcode","program name")
rank1_6 <- rbind(rank1,rank2,rank3,rank4,rank5,rank6)
rank1_6[rank1_6 == "99"|rank1_6 == "NA"|rank1_6 == ""] <- NA
urank1_6 <- na.omit(rank1_6)

#create new dataframe with schoolcode,program name,distance
dataspd <- select(mdata4,2,3,18)

#create new dataframe with schoolcode,program name,cutoff,quality 
dataspqc <- select(schoollevel_dataset,1,2,8,9)

#merge distance into urank1_6
mdata5 = inner_join(urank1_6,dataspd,by = c("schoolcode","program name"))

#merge quality and cutoff into urank1_6
mdata6 = inner_join(mdata5,dataspqc,by = c("schoolcode","program name")) 

#group the data with same rank, calculate the quality statistics
descrip_q <- mdata6 %>%
  group_by(rank) %>%
  mutate( quality_average = mean(quality))
descrip_qual <- descrip_q %>%
  group_by(rank) %>%
  mutate( quality_sd = sd(quality))

#report the average and sd of quality for each ranked choices
descrip_quality <- descrip_qual[!duplicated(descrip_qual[,c('quality_average','quality_sd')]),]
quality_and_ranks <- select(descrip_quality,3,8,9)

#group the data with same rankplace, calculate the cutoff statistics
descrip_c <- mdata6 %>%
  group_by(rank) %>%
  mutate( cutoff_average = mean(cutoff))
descrip_cut <- descrip_c %>%
  group_by(rank) %>%
  mutate( cutoff_sd = sd(cutoff))

#report the average and sd of cutoff for each ranked choices
descrip_cutoff <- descrip_cut[!duplicated(descrip_cut[,c('cutoff_average','cutoff_sd')]),]
cutoff_and_ranks <- select(descrip_cutoff,3,8,9)

#group the data with same rankplace, calculate the distance statistics
descrip_d <- mdata6 %>%
  group_by(rank) %>%
  mutate( distance_average = mean(distance))
descrip_dist <- descrip_d %>%
  group_by(rank) %>%
  mutate( distance_sd = sd(distance))

#report the average and sd of distance for each ranked choices
descrip_distance <- descrip_dist[!duplicated(descrip_dist[,c('distance_average','distance_sd')]),]
distance_and_ranks <- select(descrip_distance,3,8,9)


#Exercise 5 Diversification
#decile
paixu=data.frame(
  schoolcode=c(schoollevel_dataset$schoolcode),
  cutoff=c(schoollevel_dataset$cutoff)
)


paixu=paixu[order(paixu$cutoff),]



paixu=unique(paixu)
group1 = data.frame(
  schoolcode=(paixu$schoolcode[1:63]),
  cutoff=c(paixu$cutoff[1:63])
)

group2 = data.frame(
  schoolcode=(paixu$schoolcode[64:126]),
  cutoff=c(paixu$cutoff[64:126])
)

group3 = data.frame(
  schoolcode=(paixu$schoolcode[127:189]),
  cutoff=c(paixu$cutoff[127:189])
)

group4 = data.frame(
  schoolcode=(paixu$schoolcode[190:252]),
  cutoff=c(paixu$cutoff[190:252])
)

group5 = data.frame(
  schoolcode=(paixu$schoolcode[253:316]),
  cutoff=c(paixu$cutoff[253:316])
)

group6 = data.frame(
  schoolcode=(paixu$schoolcode[317:379]),
  cutoff=c(paixu$cutoff[317:379])
)

group7 = data.frame(
  schoolcode=(paixu$schoolcode[380:442]),
  cutoff=c(paixu$cutoff[380:442])
)

group8 = data.frame(
  schoolcode=(paixu$schoolcode[443:505]),
  cutoff=c(paixu$cutoff[443:505])
)

group9 = data.frame(
  schoolcode=(paixu$schoolcode[506:568]),
  cutoff=c(paixu$cutoff[506:568])
)

group10 = data.frame(
  schoolcode=(paixu$schoolcode[569:631]),
  cutoff=c(paixu$cutoff[569:631])
)


decile = data.frame(
  X=c(1:340823),
  groupsinapplication=c(1:340823)
  
)



ren=1
for (ren in (1:340823)){
  n=0
  m=0
  if (studentinfo$schoolcode1[ren] %in% group1$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group1$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group1$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group1$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group1$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group1$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group2$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group2$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group2$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group2$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group2$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group2$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group3$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group3$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group3$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group3$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group3$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group3$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group4$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group4$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group4$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group4$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group4$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group4$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group5$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group5$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group5$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group5$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group5$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group5$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group6$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group6$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group6$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group6$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group6$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group6$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group7$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group7$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group7$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group7$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group7$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group7$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group8$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group8$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group8$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group8$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group8$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group8$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group9$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group9$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group9$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group9$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group9$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group9$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[ren] %in% group10$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[ren] %in% group10$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[ren] %in% group10$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[ren] %in% group10$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[ren] %in% group10$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[ren] %in% group10$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  
  decile$groupsinapplication[ren]=m
  
}
write.csv(decile,file = "~/Desktop/Decile.csv", row.names = FALSE)

#quantile

diercipaixu=data.frame(
  schoolcode=c(schoollevel_dataset$schoolcode),
  quality=c(schoollevel_dataset$quality)
)


diercipaixu=diercipaixu[order(diercipaixu$quality),]



diercipaixu=unique(diercipaixu)


groupa = data.frame(
  schoolcode=(diercipaixu$schoolcode[1:656]),
  quality=c(diercipaixu$quality[1:656])
)

groupb = data.frame(
  schoolcode=(diercipaixu$schoolcode[657:1312]),
  quality=c(diercipaixu$quality[657:1312])
)

groupc = data.frame(
  schoolcode=(diercipaixu$schoolcode[1313:1967]),
  quality=c(diercipaixu$quality[1313:1967])
)

groupd = data.frame(
  schoolcode=(diercipaixu$schoolcode[1968:2623]),
  quality=c(diercipaixu$quality[1968:2623])
)



quantile = data.frame(
  X=c(1:340823),
  quantilescoregroups=c(1:340823)
  
)



xuesheng=1
for (xuesheng in (1:340823)){
  n=0
  m=0
  if (studentinfo$schoolcode1[xuesheng] %in% groupa$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[xuesheng] %in% groupa$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[xuesheng] %in% groupa$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[xuesheng] %in% groupa$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[xuesheng] %in% groupa$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[xuesheng] %in% groupa$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[xuesheng] %in% groupb$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[xuesheng] %in% groupb$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[xuesheng] %in% groupb$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[xuesheng] %in% groupb$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[xuesheng] %in% groupb$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[xuesheng] %in% groupb$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[xuesheng] %in% groupc$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[xuesheng] %in% groupc$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[xuesheng] %in% groupc$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[xuesheng] %in% groupc$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[xuesheng] %in% groupc$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[xuesheng] %in% groupc$schoolcode){n=n+1}
  if (n>0){m=m+1}
  n=0
  if (studentinfo$schoolcode1[xuesheng] %in% groupd$schoolcode){n=n+1}
  if (studentinfo$schoolcode2[xuesheng] %in% groupd$schoolcode){n=n+1}
  if (studentinfo$schoolcode3[xuesheng] %in% groupd$schoolcode){n=n+1}
  if (studentinfo$schoolcode4[xuesheng] %in% groupd$schoolcode){n=n+1}
  if (studentinfo$schoolcode5[xuesheng] %in% groupd$schoolcode){n=n+1}
  if (studentinfo$schoolcode6[xuesheng] %in% groupd$schoolcode){n=n+1}
  if (n>0){m=m+1}
  
  quantile$quantilescoregroups[xuesheng]=m
  
}

write.csv(quantile,file = "~/Desktop/Quantile.csv", row.names = FALSE)
