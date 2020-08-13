L = c(74, 14, 20, 32, 42, 55, 91, 56, 84, 42, 13, 7,61,41,46,1)
#Task2
#Q1
print(paste("the mean of L is ", mean(L)," and standard deviation of L is ",sd(L)))
#Q2
median(L)
median(abs(L-median(L)))
#Q3
print("mode is 42")


#_____________________________
#Task3
#Q1
bin1 = sort(L)[1:8]
bin2 = sort(L)[9:16]
rep(median(bin1),8)
rep(median(bin2),8)
#Q2
width = (max(L)-min(L))/3
bin1 = sort(L)[1:5]
bin2 = sort(L)[6:13]
bin3 = sort(L)[14:16]
rep(mean(bin1),5)
rep(mean(bin2),8)
rep(mean(bin3),3)
#Q3
width = (max(L)-min(L))/4
bin1 = sort(L)[1:5]  # [1,23.5]
bin2 = sort(L)[6:10] # (23.5,46]
bin3 = sort(L)[11:13]# (46,68.5]
bin4 = sort(L)[14:16]# (68.5,91]
#Q4
bin1 = sort(L)[1:4]
bin2 = sort(L)[5:8]
bin3 = sort(L)[9:12]
bin4 = sort(L)[13:16]


#____________________
#Task 4
wrangling.data = read.csv("data_wrangling_medical_2020_u6141461.csv",header = T)
View(wrangling.data)
attch(wrangling.data)
#Q1
length(postcode[is.na(postcode)])
length(postcode[is.na(postcode)])/length(postcode)
length(phone[phone==""])
length(phone[phone==""])/length(phone)
length(email[email==""])
length(email[email==""])/length(email)
#Q2
cor(bmi,age_at_consultation)
cor(bmi,height)
chisq.test(state[marital_status!="n/a"],marital_status[marital_status!="n/a"])
#Q3
1-(sum(is.na(postcode))/length(postcode))
1-length(phone[phone==""])/length(phone)
1-length(middle_name[nchar(middle_name)<=1])/length(middle_name)
#b)
length(weight[weight>0])/length(weight)
length(grep("@",email))/length(email[email!=""])
#c)
consultation_timestamp
consultation_date = as.Date(substr(consultation_timestamp,1,10))
birth.d = as.Date(birth_date, format = "%d/%m/%Y")
length(age_at_consultation[floor((consultation_date-birth.d)/365)
                           ==age_at_consultation])/length(age_at_consultation)

#Q4
benford_pro =c(0.31,0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046)
benford_freq =benford_pro*20000
#(a)
cholesterol_level
count.first.char.a =as.data.frame(table(substr(sapply(cholesterol_level, as.character),1,1)))
count.first.char.a['benford_freq'] = benford_freq
count.first.char.a
par(mfrow = c(1,2))
barplot(height=count.first.char.a$Freq, names=count.first.char.a$Var1, col="#69b3a2",ylab = "frequency")
lines(count.first.char.a$Var1,count.first.char.a$Freq,col="blue")
pie(c(count.first.char.a$Freq[1:2],sum(count.first.char.a$Freq[3:9])),labels=c("1-digit","2-digit","3 to 9"),radius = 1)
title("barplot and piechart of first digits of cholesterol_level", line = -3, outer = TRUE)
#(b)
count.first.char.b =as.data.frame(table(substr(sapply(blood_pressure, as.character),1,1)))
count.first.char.b
barplot(height=count.first.char.b$Freq, names=count.first.char.b$Var1, col="#69b3a2",ylab = "frequency")
lines(count.first.char.b$Var1,count.first.char.b$Freq,col="blue")
pie(count.first.char.b$Freq,labels=paste0(count.first.char.b$Var1,"-digit")
,radius = 1)
title("barplot and piechart of first digits of blood_pressure", line = -3, outer = TRUE)
#(c)
count.first.char.c = as.data.frame(table(substr(medicare_number,2,2)))
count.first.char.c
barplot(height=count.first.char.c$Freq, names=count.first.char.c$Var1, col="#69b3a2",ylab = "frequency")
lines(count.first.char.c$Var1,count.first.char.c$Freq,col="blue")
pie(count.first.char.c$Freq,labels=paste0(count.first.char.c$Var1,"-digit")
    ,radius = 1)
title("barplot and piechart of first digits of medical_num", line = -3, outer = TRUE)


#Q5
Data <- data.frame(
  name=c("2011", "2012", "2013" ,"2014" ,"2015" ,"2016" ,"2017", "2018" ,"2019", "2020"),
  val=table(substr(consultation_timestamp,1,4))
)
data = Data[,-2]
par(mfrow =c(1,1))
# Uniform color
barplot(height=data$val.Freq, names=data$name, 
        col="#69b3a2",
        horiz=T, las=1,ylab = "year",xlab = "frequency",
        main="number of consulting people in every year"
)

chisq.test(gender,factor(smoking_status))

