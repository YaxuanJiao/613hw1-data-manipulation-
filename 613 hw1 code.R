#Exercise 1 Missing Data
#import datasets
studentinfo <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)
addressinfo <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)
districtxy <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)
#check if the data has been imported correctly and report the number of students
studentinfo
#combine 6 schoolcodes, delete duplications and report the number of schools
schoolcode <- c(studentinfo$schoolcode1,studentinfo$schoolcode2,studentinfo$schoolcode3,studentinfo$schoolcode4,studentinfo$schoolcode5,studentinfo$schoolcode6)
length(unique(schoolcode))
#combine 6 programcodes, delete duplications and report the number of programs
programcode <- c(studentinfo$choicepgm1,studentinfo$choicepgm2,studentinfo$choicepgm3,studentinfo$choicepgm4,studentinfo$choicepgm5,studentinfo$choicepgm6)
length(unique(programcode))
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
choicefr<-data.frame(choices$xxx,choices$schoolcode)
choicefr <- table()

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
mdata1 = merge(unchoices_s,udatainto,by = "schoolcode",all.x = TRUE) 
mdata1[mdata1==""|mdata1 =="NA"] <- NA
mdata2 <- na.omit(mdata1)
mdata2[!duplicated(mdata2[,c('schoolcode','program name')]),]
mdata2u <-mdata2[!duplicated(mdata2[,c('schoolcode','program name')]),]
#group the data with same schoolcode and program, calculate the average admitted score
data_address_quality <- mdata2u %>%
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
schoollevel_dataset <- unique(data_address_quality_cutoff_size)

#Exercise 3 Distance
#create a new dataset contains all choices and jssdistrict
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
#distance=(mdata4$step4+mdata4$step7)
mdata4$step1 <- (mdata4$ssslong-mdata4$jsslong)
mdata4$step2 <- cos(mdata4$jsslat/57.3)
mdata4$step3 <- (69.172*mdata4$step1*mdata4$step2)
mdata4$step4 <- (mdata4$step3)^2
mdata4$step5 <- (mdata4$ssslat - mdata4$jsslat)
mdata4$step6 <- (69.172*(mdata4$ssslat - mdata4$jsslat))
mdata4$step7 <- ((69.172*(mdata4$ssslat - mdata4$jsslat))^2)
mdata4$distance <- (mdata4$step4+mdata4$step7)
mdata4[,"distance"]

#Exercise4 Descriptive Characteristics
e1 <- data.frame(studentinfo$schoolcode1, studentinfo$choicepgm1, studentinfo$rankplace)
e2 <- data.frame(studentinfo$schoolcode2, studentinfo$choicepgm2, studentinfo$rankplace)
e3 <- data.frame(studentinfo$schoolcode3, studentinfo$choicepgm3, studentinfo$rankplace)
e4 <- data.frame(studentinfo$schoolcode4, studentinfo$choicepgm4, studentinfo$rankplace)
e5 <- data.frame(studentinfo$schoolcode5, studentinfo$choicepgm5, studentinfo$rankplace)
e6 <- data.frame(studentinfo$schoolcode6, studentinfo$choicepgm6, studentinfo$rankplace)
names(e1) <- c("schoolcode","program name","rankplace")
names(e2) <- c("schoolcode","program name","rankplace")
names(e3) <- c("schoolcode","program name","rankplace")
names(e4) <- c("schoolcode","program name","rankplace")
names(e5) <- c("schoolcode","program name","rankplace")
names(e6) <- c("schoolcode","program name","rankplace")
choices_r <- rbind(e1,e2,e3,e4,e5,e6)
uchoices_r <- unique(choices_r)
uchoices_r[uchoices_r=="99"|uchoices_r =="NA"] <- NA
unchoices_r <- na.omit(uchoices_r)
#create new dataframe with schoolcode,program name,distance
dataspd
#create new dataframe with schoolcode,program name,cutoff,quality 
dataspcp
#merge distance in unchoices_r with schoolcode and program name
mdata5
#deal with data
mdata2u <-mdata2[!duplicated(mdata2[,c('schoolcode','program name')]),]
#merge cutoff & quality in unchoices_r with schoolcode and program name

 

#group the data with same rankplace, calculate the cutoff statistics
data_address_quality <- mdata2u %>%
  group_by(schoolcode, `program name`) %>%
  mutate(quality = mean(score))
#group the data with the same rankplace, calculate the quality statistics


mdata5 <- merge(mdata4,unchoices_r,by = "schoolcode",all.x = TRUE)
mdata4 <- merge(mdata3,districtxy,by = "jssdistrict",all.x = TRUE)
mdata4[mdata4==""|mdata4 =="NA"] <- NA
mdata5 <- na.omit(mdata4)  