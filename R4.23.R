Familydata[,2]
library(dplyr)
subset(Familydata,sex=="F")
subset(Familydata,sex=="F",select=c(ht,wt))

install.packages("missForest")
library(missForest)
data(iris)
set.seed(1234) #生成随机数种子
iris.miss<-prodNA(iris) #生成数据数目10%的缺失值
summary(iris.miss)
install.packages("VIM")
library(VIM)
aggr(iris.miss,prop=FALSE,numbers=TRUE,cex.axis=0.7)

Sample.rows<-sample(1:nrow(Familydata),size=3,replace=T)
Familydata[Sample.rows,]

Familydata[order(-Familydata$age),]
any(duplicated(Familydata$code))

Familydata1<-Familydata
Familydata[12,]<-Familydata[2,]
table(duplicated(Familydata$code))Unique.data<- Familydata[!duplicated(Familydata$code),]
which(duplicated(Familydata$code))

Unique.data<- Familydata[!duplicated(Familydata$code),]
identical(Unique.data, Familydata1)


compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

compute_s_n<-function(m){
  s<-0
  for(p in 1:m){
    s<-s+p^2
  }
  s
}
compute_s_n(3)z
mydata<-sasxport.get("C:/Users/冯立松/Downloads/AUXAR_D.XPT")

load("UCR.rdata")
library(epiDisplay)
plot(ucr~age,data=UCR,xlab="Age in years",
     ylab="Urine creatinine(mmol)")
mod<-lm(ucr~age,data=UCR)
summary(mod)

percent <- c(5.8, 27.0, 0.5, 20.8, 12.8, 33.1)
disease <- c("上感", "中风", "外伤", "昏厥", "食物中毒", "其他")
lbs <- paste0(disease, percent, "%")
pie(percent, labels = lbs, col = rainbow(6))

x <- c(0.84, 0.59, 0.67, 0.63, 0.69, 0.98, 0.75, 0.73, 1.20, 0.87)
y <- c(0.58, 0.51, 0.50, 0.32, 0.34, 0.52, 0.45, 0.51, 1.00, 0.51)
t.test(x, y, paired = TRUE)

library(Hmisc)
install.packages("gvlma")
library(gvlma)
gvlma(AUXTYM)

plot(ucr~age,data=UCR,xlab="Age in years",ylab="Urine creatinine(mmol)")

simple.fit = lm(Sales~Spend, data=dataset)
summary(simple.fit)

NHANES:
ls()
read.xport("C:/Users/冯立松/Downloads/P_AUXTYM.XPT")
AUXTYM<-read.xport("C:/Users/冯立松/Downloads/P_AUXTYM.XPT")
names(AUXTYM)
library(foreign)
library(epiDisplay)
summ(AUXTYM)
summary(AUXTYM)

UIO

install.packages("nhanesA")
install.packages("knitr")
install.packages("tidyverse")
install.packages("plyr")
install.packages("devtools")

library("nhanesA")
library("knitr")
library("tidyverse")
library("plyr")

nhanesTables(data_group = 'LAB',year=2015)
kable(nhanesTableVars(data_group = 'LAB',nh_table = 'UIO_I'),namesonly = FALSE)
browseNHANES(data_group = 'LAB',nh_table = 'UIO_I')

library(foreign)
UrineI<-read.xport("C:/Users/冯立松/Downloads/UIO_I.XPT")

library(dplyr)

#4.1：
library(foreign)
library(epiDisplay)
library("nhanesA")
library("knitr")
library("tidyverse")
library("plyr")
library(dplyr)
library(magrittr)
library(Hmisc)
library(devtools)
nhanesTables(data_group = 'Q',year=2017)
kable(nhanesTableVars(data_group = 'Q',nh_table = 'PAQ_J'),namesonly = FALSE)
browseNHANES(data_group = 'Q',nh_table = 'PAQ_J')

PAQ<-read.xport("C:/Users/冯立松/Downloads/PAQ_J.XPT")
PAQ
library(Hmisc)
PAQ<-sasxport.get("C:/Users/冯立松/Downloads/PAQ_J.XPT")
PAQ


PAQ<-nhanes('PAQ_J')#等效121-122
PAQ1<-PAQ %>% select(SEQN,PAQ605,PAQ620,PAQ635,PAQ650,PAQ665)
PAQ_vars<-names(PAQ1)
PAQ2<-nhanesTranslate('PAQ_J',PAQ_vars,data=PAQ1)

DXX<-nhanes('DXX_J')
DXX0<-read.xport("C:/Users/冯立松/Downloads/DXX_J.XPT")
browseNHANES(data_group = 'EXAM',nh_table = 'DXX_J')
DXX1<-DXX %>% select(SEQN,DXXHEBMD,DXXLABMD,DXXLLBMD,DXXRABMD,DXXRLBMD,DXXLRBMD
                     ,DXXRRBMD,DXXTSBMD,DXXLSBMD,DXXPEBMD)

OSQ<-nhanes('OSQ_J')
browseNHANES(data_group = 'Q',nh_table = 'OSQ_J')
OSQ1<-OSQ %>% select(SEQN,OSQ010A,OSQ010B,OSQ010C)

data<-join_all(list(PAQ1,DXX1,OSQ1),by='SEQN',type='full')

write.csv(data,file="nhanes_sample1.csv",row.names = F)


data$PAQ605<-recode(data$PAQ605,'1'="Yes",'2'="No")
head(data)
glm(data$PAQ605~OSQ010A)

plot(ucr~age,data=UCR,xlab="Age in years",
     ylab="Urine creatinine(mmol)")

data_d <- na.omit(data)
plot(PAQ605 ~ DXXHEBMD,data=data_d)#lao

summ(data$DXXHEBMD)
summary(data$DXXHEBMD)

data_d[,c(2:6)]<-lapply(data_d[,c(2:6)],factor)#因子化
str(data_d)
log<-glm(OSQ010A~PAQ605+PAQ620+PAQ635+PAQ650+PAQ665,data=data_d,family=binomial())
summary(log)




#4.8----
install.packages("tidyr")
library(foreign)
library(epiDisplay)
library("nhanesA")
library("knitr")
library("tidyverse")
library("plyr")
library(dplyr)
library(magrittr)
library(Hmisc)
library(devtools)
library(tidyr)
library(ggplot2)
library(tidyselect)
library(VIM)
library(mice)
library(vcd)

PAQ_dJ<-read.xport("C:/Users/冯立松/Downloads/PAQ_J.XPT")[,c("SEQN","PAQ605","PAQ620","PAQ635","PAQ650","PAQ665","PAD680")]
OSQ_dJ<-read.xport("C:/Users/冯立松/Downloads/OSQ_J.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dJ<-read.xport("C:/Users/冯立松/Downloads/DEMO_J.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dJ<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_J.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

PAQ_dE<-read.xport("C:/Users/冯立松/Downloads/PAQ_E.XPT")[,c("SEQN","PAQ605","PAQ620","PAQ635","PAQ650","PAQ665","PAD680")]
OSQ_dE<-read.xport("C:/Users/冯立松/Downloads/OSQ_E.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dE<-read.xport("C:/Users/冯立松/Downloads/DEMO_E.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dE<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_E.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

PAQ_dF<-read.xport("C:/Users/冯立松/Downloads/PAQ_F.XPT")[,c("SEQN","PAQ605","PAQ620","PAQ635","PAQ650","PAQ665","PAD680")]
OSQ_dF<-read.xport("C:/Users/冯立松/Downloads/OSQ_F.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dF<-read.xport("C:/Users/冯立松/Downloads/DEMO_F.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dF<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_F.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

PAQ_dH<-read.xport("C:/Users/冯立松/Downloads/PAQ_H.XPT")[,c("SEQN","PAQ605","PAQ620","PAQ635","PAQ650","PAQ665","PAD680")]
OSQ_dH<-read.xport("C:/Users/冯立松/Downloads/OSQ_H.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dH<-read.xport("C:/Users/冯立松/Downloads/DEMO_H.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dH<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_H.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

DEMO<-bind_rows(DEMO_dE,DEMO_dF,DEMO_dH,DEMO_dJ)
PAQ<-bind_rows(PAQ_dE,PAQ_dF,PAQ_dH,PAQ_dJ)
OSQ<-bind_rows(OSQ_dE,OSQ_dF,OSQ_dH,OSQ_dJ)
DXXFEM<-bind_rows(DXXFEM_dE,DXXFEM_dF,DXXFEM_dH,DXXFEM_dJ)

#考虑读入股骨骨密度的数据（已下载），但不方便确定不同年龄、种族人群的距骨密度峰值的标准差。先简单考虑BMC与BMD，后续会按年龄性别分组

#合并
d<-DEMO
d<-left_join(d,PAQ,by="SEQN")
d<-left_join(d,OSQ,by="SEQN")
d<-left_join(d,DXXFEM,by="SEQN")

#d<-d %>% drop_na()#待用

data<-xtabs(formula=~RIAGENDR+OSQ010A,data=d)
data
chisq.test(data)#p>0.05,hip组间不存在差异


data<-xtabs(formula=~RIAGENDR+OSQ010B,data=d)
data
chisq.test(data)#p<0.05,wrist存在组间差异


data<-xtabs(formula=~RIAGENDR+OSQ010C,data=d)
data
chisq.test(data)#p<<0.05,spine组差异显著

summary(d)
#table(is.na(d)),统计NA个数
mean(d$DXXOFBMD,na.rm=TRUE)

aggr(d,prop=FALSE,numbers=TRUE,cex.axis=0.7)

d.sub<-na.omit(d)#NA值消除
summary(d.sub)
nrow(d.sub)

#imputed.data<-mice(d,seed=1234)#智能插补NA值
#complete.data<-complete(imputed.data)
#summary(complete.data)
#aggr(complete.data,prop=FALSE,numbers=TRUE,cex.axis=0.7)#插补检验

#str(d.sub)
#str(complete.data)

#d.sub<-d.sub %>% mutate(RIAGENDR = factor(RIAGENDR,labels = c("male","female")),PAQ605 = factor(PAQ605,labels = c("yes","no")),PAQ620 = factor(PAQ620,labels = c("yes","no")),PAQ635 = factor(PAQ635,labels = c("yes","no")),PAQ650 = factor(PAQ650,labels = c("yes","no")),PAQ665 = factor(PAQ665,labels = c("yes","no")),OSQ010A = factor(OSQ010A,labels = c("yes","no")),OSQ010B = factor(OSQ010B,labels = c("yes","no")),OSQ010C = factor(OSQ010C,labels = c("yes","no")))




#富------
dd <- d.sub
dd$PAQ620 <- as.factor(dd$PAQ620)
class(dd$PAQ620)
levels(dd$PAQ620)
dd$PAQ620 <- recode(dd$PAQ620,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(PAQ620="Moderate work activity"))

dd$PAQ635 <- as.factor(dd$PAQ635)
dd$PAQ635 <- recode(dd$PAQ635,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(PAQ635="Walk or bicycle"))

dd$PAQ605<- as.factor(dd$PAQ605)
dd$PAQ605 <- recode(dd$PAQ605,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(PAQ605="Vigorous work activity"))

dd$PAQ650<- as.factor(dd$PAQ650)
dd$PAQ650 <- recode(dd$PAQ650,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(PAQ650="Vigorous recreational activities"))

dd$PAQ665<- as.factor(dd$PAQ665)
dd$PAQ665 <- recode(dd$PAQ665,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(PAQ665="Moderate recreational activities"))

dd$OSQ010A<- as.factor(dd$OSQ010A)
dd$OSQ010A <- recode(dd$OSQ010A,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(OSQ010A="Broken or fractured a hip"))

dd$OSQ010B<- as.factor(dd$OSQ010B)
dd$OSQ010B <- recode(dd$OSQ010B,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(OSQ010B="Broken or fractured a wrist"))

dd$OSQ010C<- as.factor(dd$OSQ010C)
dd$OSQ010C <- recode(dd$OSQ010C,'1'="yes",'2'="no",'9'="dont know")
dd <- plyr::rename(dd,c(OSQ010C="Broken or fractured spine"))

dd <- plyr::rename(dd,c(DXXOFBMD="Total femur BMD"))
dd <- plyr::rename(dd,c(DXXOFBMC="Total femur BMC"))
dd <- plyr::rename(dd,c(RIAGENDR="Gender"))

#相关性分析
mytable<-table(d.sub$PAQ605,d.sub$OSQ010A)
assocstats(mytable)

mytable<-table(d.sub$PAQ620,d.sub$OSQ010A)
assocstats(mytable)

mytable<-table(d.sub$PAQ635,d.sub$OSQ010A)
assocstats(mytable)

mytable<-table(d.sub$PAQ650,d.sub$OSQ010A)
assocstats(mytable)

mytable<-table(d.sub$PAQ665,d.sub$OSQ010A)
assocstats(mytable)

mytable<-table(d.sub$PAQ605,d.sub$OSQ010B)
assocstats(mytable)

mytable<-table(d.sub$PAQ605,d.sub$OSQ010C)
assocstats(mytable)

mytable<-table(d.sub$PAQ620,d.sub$OSQ010B)
assocstats(mytable)

mytable<-table(d.sub$PAQ635,d.sub$OSQ010B)
assocstats(mytable)

mytable<-table(d.sub$PAQ650,d.sub$OSQ010B)
assocstats(mytable)

mytable<-table(d.sub$PAQ665,d.sub$OSQ010B)
assocstats(mytable)

mytable<-table(d.sub$PAQ620,d.sub$OSQ010C)
assocstats(mytable)

mytable<-table(d.sub$OSQ010C,d.sub$PAQ635)
assocstats(mytable)

mytable<-table(d.sub$PAQ650,d.sub$OSQ010C)
assocstats(mytable)

mytable<-table(d.sub$PAQ665,d.sub$OSQ010C)
assocstats(mytable)
#均无关


cov(d.sub)
cor(d.sub,method = "pearson")
cor(d.sub,method = "spearman")
cor.test(d.sub$PAD680,d.sub$DXXOFBMC)


#创建定量运动数据
PAQ_dJ<-read.xport("C:/Users/冯立松/Downloads/PAQ_J.XPT")[,c("SEQN","PAD615","PAD630","PAD645","PAD660","PAD675","PAD680")]
OSQ_dJ<-read.xport("C:/Users/冯立松/Downloads/OSQ_J.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dJ<-read.xport("C:/Users/冯立松/Downloads/DEMO_J.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dJ<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_J.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

PAQ_dE<-read.xport("C:/Users/冯立松/Downloads/PAQ_E.XPT")[,c("SEQN","PAD615","PAD630","PAD645","PAD660","PAD675","PAD680")]
OSQ_dE<-read.xport("C:/Users/冯立松/Downloads/OSQ_E.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dE<-read.xport("C:/Users/冯立松/Downloads/DEMO_E.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dE<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_E.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

PAQ_dF<-read.xport("C:/Users/冯立松/Downloads/PAQ_F.XPT")[,c("SEQN","PAD615","PAD630","PAD645","PAD660","PAD675","PAD680")]
OSQ_dF<-read.xport("C:/Users/冯立松/Downloads/OSQ_F.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dF<-read.xport("C:/Users/冯立松/Downloads/DEMO_F.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dF<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_F.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

PAQ_dH<-read.xport("C:/Users/冯立松/Downloads/PAQ_H.XPT")[,c("SEQN","PAD615","PAD630","PAD645","PAD660","PAD675","PAD680")]
OSQ_dH<-read.xport("C:/Users/冯立松/Downloads/OSQ_H.XPT")[,c("SEQN","OSQ010A","OSQ010B","OSQ010C")]
DEMO_dH<-read.xport("C:/Users/冯立松/Downloads/DEMO_H.XPT")[,c("SEQN","RIAGENDR")]
DXXFEM_dH<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_H.XPT")[,c("SEQN","DXXOFBMD","DXXOFBMC")]

DEMO<-bind_rows(DEMO_dE,DEMO_dF,DEMO_dH,DEMO_dJ)
PAQ<-bind_rows(PAQ_dE,PAQ_dF,PAQ_dH,PAQ_dJ)
OSQ<-bind_rows(OSQ_dE,OSQ_dF,OSQ_dH,OSQ_dJ)
DXXFEM<-bind_rows(DXXFEM_dE,DXXFEM_dF,DXXFEM_dH,DXXFEM_dJ)

#考虑读入股骨骨密度的数据（已下载），但不方便确定不同年龄、种族人群的距骨密度峰值的标准差。先简单考虑BMC与BMD，后续会按年龄性别分组

#合并
d.s<-full_join(PAQ,DXXFEM,by="SEQN")

d.s.sub<-na.omit(d.s)#NA值消除
imputed.data<-mice(d.s,seed=1234)#智能插补NA值
complete.data.s<-complete(imputed.data)

cov(complete.data.s)
cor(complete.data.s,method = "pearson")
cor(complete.data.s,method = "spearman")

cov(d.s.sub)
cor(d.s.sub,method = "pearson")
cor(d.s.sub,method = "spearman")

#r语言学习---------
x1<-c(1,2,5,6,7)
x1
class(x1)
x1<-as.factor(x1)
x1
class(x1)

f1<-factor(c(1,2,1,3,2,4,2,4,2,1))
f1
f3<-factor(c(1,2,1,3,2,4,2,4,2,1),levels=c(2,1,4,3),
           labels = c("A","B","AB","O"))
f3
table(f1)  #因子列表展示
class(f1) #查看f1的类型
levels(f1) #查看f1 因子的分组水平
summary(f1) #f1因子的汇总


x1<-c(2,3,5,7,10)
x1[which.max(x1)]

names(x1)<-c("two","three","five","seven","ten")
x1[c("three","seven")]

subset(x1, x1>3 & x1<8)







#2005-2006，不采用
DEMO_D<-read.xport("C:/Users/冯立松/Downloads/DEMO_D.XPT")
demo_d<-DEMO_D[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]

DXXSPN_D<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_D.XPT")
dxxspn_d<-DXXSPN_D[,c("SEQN","DXXOSBMD")]

DXXFEM_D<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_D.XPT")
dxxfem_d<-DXXFEM_D[,c("SEQN","DXXOFBMD")]

ALQ_D<-read.xport("C:/Users/冯立松/Downloads/ALQ_D.XPT")
alq_d<-ALQ_D[,c("SEQN","ALQ101")]

SMQ_D<-read.xport("C:/Users/冯立松/Downloads/SMQ_D.XPT")
smq_d<-SMQ_D[,c("SEQN","SMQ040")]

PAQ_D<-read.xport("C:/Users/冯立松/Downloads/PAQ_D.XPT")

data<-join_all(list(demo_d,dxxspn_d,dxxfem_d,alq_d,smq_d,PAQ_D),by='SEQN',type='full')

write.csv(data,file="2005-2006 original.csv",row.names = F)



#2007-2008
DEMO_E<-read.xport("C:/Users/冯立松/Downloads/DEMO_E.XPT")

#demo_e<-DEMO_E[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]

DXXSPN_E<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_E.XPT")
#dxxspn_e<-DXXSPN_E[,c("SEQN","DXXOSBMD")]

DXXFEM_E<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_E.XPT")
dxxfem_e<-DXXFEM_E[,c("SEQN","DXXOFBMD")]

ALQ_E<-read.xport("C:/Users/冯立松/Downloads/ALQ_E.XPT")
alq_e<-ALQ_E[,c("SEQN","ALQ101")]

SMQ_E<-read.xport("C:/Users/冯立松/Downloads/SMQ_E.XPT")
smq_e<-SMQ_E[,c("SEQN","SMQ040")]

PAQ_E<-read.xport("C:/Users/冯立松/Downloads/PAQ_E.XPT")

data_e<-join_all(list(demo_e,dxxspn_e,dxxfem_e,alq_e,smq_e,PAQ_E),by='SEQN',type='full')

write.csv(data_e,file="2007-2008 original.csv",row.names = F)


#2009-2010
DEMO_F<-read.xport("C:/Users/冯立松/Downloads/DEMO_F.XPT")
demo_f<-DEMO_F[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]

DXXSPN_F<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_F.XPT")
dxxspn_f<-DXXSPN_F[,c("SEQN","DXXOSBMD")]

DXXFEM_F<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_F.XPT")
dxxfem_f<-DXXFEM_F[,c("SEQN","DXXOFBMD")]

ALQ_F<-read.xport("C:/Users/冯立松/Downloads/ALQ_F.XPT")
alq_f<-ALQ_F[,c("SEQN","ALQ101")]

SMQ_F<-read.xport("C:/Users/冯立松/Downloads/SMQ_F.XPT")
smq_f<-SMQ_F[,c("SEQN","SMQ040")]

PAQ_F<-read.xport("C:/Users/冯立松/Downloads/PAQ_F.XPT")

data_f<-join_all(list(demo_f,dxxspn_f,dxxfem_f,alq_f,smq_f,PAQ_F),by='SEQN',type='full')

write.csv(data_f,file="2009-2010 original.csv",row.names = F)


#2013-2014
DEMO_H<-read.xport("C:/Users/冯立松/Downloads/DEMO_H.XPT")
demo_h<-DEMO_H[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]

DXXSPN_H<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_H.XPT")
dxxspn_h<-DXXSPN_H[,c("SEQN","DXXOSBMD")]

DXXFEM_H<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_H.XPT")
dxxfem_h<-DXXFEM_H[,c("SEQN","DXXOFBMD")]

ALQ_H<-read.xport("C:/Users/冯立松/Downloads/ALQ_H.XPT")
alq_h<-ALQ_H[,c("SEQN","ALQ101")]

SMQ_H<-read.xport("C:/Users/冯立松/Downloads/SMQ_H.XPT")
smq_h<-SMQ_H[,c("SEQN","SMQ040")]

PAQ_H<-read.xport("C:/Users/冯立松/Downloads/PAQ_H.XPT")
paq_h<-PAQ_H[,c("SEQN","PAQ605","PAQ610","PAD615","PAQ620","PAQ625","PAD630","PAQ635","PAQ640","PAD645","PAQ650","PAQ655","PAD660","PAQ665","PAQ670","PAD675","PAD680","PAAQUEX")]

data_h<-join_all(list(demo_h,dxxspn_h,dxxfem_h,alq_h,smq_h,paq_h),by='SEQN',type='full')

write.csv(data_h,file="2013-2014 original.csv",row.names = F)




#导入数据集2007-2008--------
data_e<-read.csv(file="2007-2008 original.csv",header = TRUE)

data_e[is.na(data_e)]<-0
data_e[data_e==9999]<-0

data_e$VPAwork<-data_e$PAQ610*data_e$PAD615
data_e$VPAleisure<-data_e$PAQ655*data_e$PAD660
data_e$VPA<-data_e$VPAwork+data_e$VPAleisure

data_e$MPAwork<-data_e$PAQ625*data_e$PAD630
data_e$MPAleisure<-data_e$PAQ670*data_e$PAD675
data_e$MPAwalk<-data_e$PAQ640*data_e$PAD645
data_e$MPA<-data_e$MPAwork+data_e$MPAleisure+data_e$MPAwalk

data_e$PA<-data_e$VPA*2+data_e$MPA

for(i in 1:length(data_e$PAQ625)){ 
  data_e$freq[i]<-max(max(data_e$PAQ625[i],data_e$PAQ670[i],data_e$PAQ640[i]),
                   max(data_e$PAQ610[i],data_e$PAQ655[i]))
}

data_e[data_e == 0] <- NA

data_e$inactive<-ifelse(data_e$PA<150,ifelse(data_e$PA>0,1,0),0)
data_e$WWorRA<-ifelse(data_e$PA>=150,ifelse(data_e$freq<=2,1,2),0)


write.csv(data_e,file="2007-2008 version 1.csv",row.names = F)

#测试----------
s<-0
for(i in 1:length(data_f$PAQ625)){ 
  if((data_f$freq[i]==1||data_f$freq[i]==2)&&data_f$PA[i]>=150){
    s<-s+1
  }
}
s

s<-0
for(i in 1:length(data_f$PAQ625)){ 
  if(data_f$freq[i]>2&&data_f$PA[i]>=150){
    s<-s+1
  }
}
s

table(data_h$WWorRA,useNA="ifany")
table(data_e$WWorRA,useNA="ifany")
table(data_f$WWorRA,useNA="ifany")

#导入数据集2009-2010----------
data_f<-read.csv(file="2009-2010 original.csv",header = TRUE)

data_f[is.na(data_f)]<-0
data_f[data_f==9999]<-0

data_f$VPAwork<-data_f$PAQ610*data_f$PAD615
data_f$VPAleisure<-data_f$PAQ655*data_f$PAD660
data_f$VPA<-data_f$VPAwork+data_f$VPAleisure

data_f$MPAwork<-data_f$PAQ625*data_f$PAD630
data_f$MPAleisure<-data_f$PAQ670*data_f$PAD675
data_f$MPAwalk<-data_f$PAQ640*data_f$PAD645
data_f$MPA<-data_f$MPAwork+data_f$MPAleisure+data_f$MPAwalk

data_f$PA<-data_f$VPA*2+data_f$MPA

for(i in 1:length(data_f$PAQ625)){ 
  data_f$freq[i]<-max(max(data_f$PAQ625[i],data_f$PAQ670[i],data_f$PAQ640[i]),
                      max(data_f$PAQ610[i],data_f$PAQ655[i]))
}

data_f[data_f == 0] <- NA

data_f$inactive<-ifelse(data_f$PA<150,ifelse(data_f$PA>0,1,0),0)
data_f$WWorRA<-ifelse(data_f$PA>=150,ifelse(data_f$freq<=2,1,2),0)

write.csv(data_f,file="2009-2010 version 1.csv",row.names = F)


#导入数据集2013-2014---------
data_h<-read.csv(file="2013-2014 original.csv",header = TRUE)

data_h[is.na(data_h)]<-0
data_h[data_h==9999]<-0

data_h$VPAwork<-data_h$PAQ610*data_h$PAD615
data_h$VPAleisure<-data_h$PAQ655*data_h$PAD660
data_h$VPA<-data_h$VPAwork+data_h$VPAleisure

data_h$MPAwork<-data_h$PAQ625*data_h$PAD630
data_h$MPAleisure<-data_h$PAQ670*data_h$PAD675
data_h$MPAwalk<-data_h$PAQ640*data_h$PAD645
data_h$MPA<-data_h$MPAwork+data_h$MPAleisure+data_h$MPAwalk

data_h$PA<-data_h$VPA*2+data_h$MPA

for(i in 1:length(data_h$PAQ625)){ 
  data_h$freq[i]<-max(max(data_h$PAQ625[i],data_h$PAQ670[i],data_h$PAQ640[i]),
                      max(data_h$PAQ610[i],data_h$PAQ655[i]))
}

data_h$inactive<-ifelse(data_h$PA<150,ifelse(data_h$PA>0,1,0),0)
data_h$WWorRA<-ifelse(data_h$PA>=150,ifelse(data_h$freq<=2,1,2),0)

data_h[data_h == 0] <- NA

write.csv(data_h,file="2013-2014 version 1.csv",row.names = F)



#正确数据处理----------
library(foreign)
library(epiDisplay)
library("nhanesA")
library("knitr")
library("tidyverse")
library("plyr")
library(dplyr)
library(magrittr)
library(Hmisc)
library(devtools)
library(tidyr)
library(ggplot2)
library(tidyselect)
library(VIM)
library(mice)
library(vcd)
library("rms")
getwd()
source("NHS_rcs（更新）.R")
if (!require("do")) install.packages("do")
library("do")
library("ggplot2")
library("dplyr")
library(tableone)
library("pROC")
library("RISCA")

DEMO_E<-read.xport("C:/Users/冯立松/Downloads/DEMO_E.XPT")
DEMO_F<-read.xport("C:/Users/冯立松/Downloads/DEMO_F.XPT")
DEMO_H<-read.xport("C:/Users/冯立松/Downloads/DEMO_H.XPT")

#人口多周期数据分别合并
DEMO_E$Year <- "2007-2008"
DEMO_F$Year <- "2009-2010"
DEMO_H$Year <- "2013-2014"

demo_e<-DEMO_E[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]
demo_f<-DEMO_F[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]
demo_h<-DEMO_H[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","DMDMARTL","INDFMPIR")]
Demo<-rbind(demo_e,demo_f,demo_h)
Demo_dat <- rename(Demo,
                   c(RIAGENDR="Gender",
                     RIDAGEYR="Age",
                     RIDRETH1="Race",
                     INDFMPIR="PIR",
                     DMDEDUC2="Education",
                     DMDMARTL="Marital")
)
head(Demo_dat)

#暴露数据  运动  处理

PAQ_E<-read.xport("C:/Users/冯立松/Downloads/PAQ_E.XPT")
PAQ_F<-read.xport("C:/Users/冯立松/Downloads/PAQ_F.XPT")
PAQ_H<-read.xport("C:/Users/冯立松/Downloads/PAQ_H.XPT")

paq_e<-PAQ_E[,c("SEQN","PAQ605","PAQ610","PAD615","PAQ620","PAQ625","PAD630","PAQ635","PAQ640","PAD645","PAQ650","PAQ655","PAD660","PAQ665","PAQ670","PAD675","PAAQUEX")]
paq_f<-PAQ_F[,c("SEQN","PAQ605","PAQ610","PAD615","PAQ620","PAQ625","PAD630","PAQ635","PAQ640","PAD645","PAQ650","PAQ655","PAD660","PAQ665","PAQ670","PAD675","PAAQUEX")]
paq_h<-PAQ_H[,c("SEQN","PAQ605","PAQ610","PAD615","PAQ620","PAQ625","PAD630","PAQ635","PAQ640","PAD645","PAQ650","PAQ655","PAD660","PAQ665","PAQ670","PAD675","PAAQUEX")]
  
Paq<-rbind(paq_e,paq_f,paq_h)  
head(Paq)
Paq_dat <- rename(Paq,
                   c(PAQ605="v_work",
                     PAQ610="day_v_work",
                     PAD615="min_v_work",
                     PAQ620="m_work",
                     PAQ625="day_m_work",
                     PAD630="min_m_work",
                     PAQ635="trans",
                     PAQ640="day_trans",
                     PAD645="min_trans",
                     PAQ650="v_recreation",
                     PAQ655="day_v_recreation",
                     PAD660="min_v_recreation",
                     PAQ665="m_recreation",
                     PAQ670="day_m_recreation",
                     PAD675="min_m_recreation")
)
  
#结局 骨密度  
DXXSPN_E<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_E.XPT")
dxxspn_e<-DXXSPN_E[,c("SEQN","DXXOSBMD","DXXOSBCC","DXASPNST")]

#证明等价："DXXL1BCC","DXXL2BCC","DXXL3BCC","DXXL4BCC","DXXL1A","DXXL2A","DXXL3A","DXXL4A","DXXL1BMC","DXXL2BMC","DXXL3BMC","DXXL4BMC"
#table(dxxspn_e$DXXL1BCC,useNA="ifany")
#table(dxxspn_e$DXXL2BCC,useNA="ifany")
#table(dxxspn_e$DXXL3BCC,useNA="ifany")
#table(dxxspn_e$DXXL4BCC,useNA="ifany")
#table(dxxspn_e$DXXOSBCC,useNA="ifany")
#dxxspntest<-dxxspn_e
#dxxspntest$hh<-(ifelse(dxxspntest$DXXL1BCC!=0,1,ifelse(dxxspntest$DXXL2BCC!=0,1,ifelse(dxxspntest$DXXL3BCC!=0,1,ifelse(dxxspntest$DXXL4BCC!=0,1,0)))))
#table(dxxspntest$hh,useNA="ifany")

#dxxspntest$LBMD<-(dxxspntest$DXXL1BMC+dxxspntest$DXXL2BMC+dxxspntest$DXXL3BMC+dxxspntest$DXXL4BMC)/(dxxspntest$DXXL1A+dxxspntest$DXXL2A+dxxspntest$DXXL3A+dxxspntest$DXXL4A)
#summary(dxxspntest$LBMD)
#所以腰椎骨密度就等于“脊柱总骨密度”



DXXFEM_E<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_E.XPT")
dxxfem_e<-DXXFEM_E[,c("SEQN","DXXOFBMD","DXXFMBCC","DXAFMRST","DXXNKBMD")]  
  
DXXSPN_F<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_F.XPT")
dxxspn_f<-DXXSPN_F[,c("SEQN","DXXOSBMD","DXXOSBCC","DXASPNST")]
DXXFEM_F<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_F.XPT")
dxxfem_f<-DXXFEM_F[,c("SEQN","DXXOFBMD","DXXFMBCC","DXAFMRST","DXXNKBMD")]  

DXXSPN_H<-read.xport("C:/Users/冯立松/Downloads/DXXSPN_H.XPT")
dxxspn_h<-DXXSPN_H[,c("SEQN","DXXOSBMD","DXXOSBCC","DXASPNST")]
DXXFEM_H<-read.xport("C:/Users/冯立松/Downloads/DXXFEM_H.XPT")
dxxfem_h<-DXXFEM_H[,c("SEQN","DXXOFBMD","DXXFMBCC","DXAFMRST","DXXNKBMD")]
  
Dxxfem<-rbind(dxxfem_e,dxxfem_f,dxxfem_h)  
Dxxspn<-rbind(dxxspn_e,dxxspn_f,dxxspn_h)
Dxxspn_dat <- rename(Dxxspn,
                  c(DXXOSBMD="spine_BMD",
                    DXASPNST="spine_scan_status",
                    DXXOSBCC="spine_invalidity")
)
Dxxfem_dat <- rename(Dxxfem,
                     c(DXXOFBMD="femur_BMD",
                       DXAFMRST="femur_scan_status",
                       DXXFMBCC="femur_invalidity",
                       DXXNKBMD="femur_neck_BMD")
)

#协变量 工作状况(newly added)
OCQ_E<-read.xport("C:/Users/冯立松/Downloads/OCQ_E.XPT")
OCQ_F<-read.xport("C:/Users/冯立松/Downloads/OCQ_F.XPT")
OCQ_H<-read.xport("C:/Users/冯立松/Downloads/OCQ_H.XPT")
ocq_e<-OCQ_E[,c("SEQN","OCD150","OCQ380")]
ocq_f<-OCQ_F[,c("SEQN","OCD150","OCQ380")]
ocq_h<-OCQ_H[,c("SEQN","OCD150","OCQ380")]

Ocq<-rbind(ocq_e,ocq_f,ocq_h)
Ocq_dat <- rename(Ocq,
                     c(OCD150="work_last_week",
                       OCQ380="reason_not_work")
)

#协变量 抽烟，饮酒
ALQ_E<-read.xport("C:/Users/冯立松/Downloads/ALQ_E.XPT")
alq_e<-ALQ_E[,c("SEQN","ALQ101")]
SMQ_E<-read.xport("C:/Users/冯立松/Downloads/SMQ_E.XPT")
smq_e<-SMQ_E[,c("SEQN","SMQ040","SMQ020")]

ALQ_F<-read.xport("C:/Users/冯立松/Downloads/ALQ_F.XPT")
alq_f<-ALQ_F[,c("SEQN","ALQ101")]
SMQ_F<-read.xport("C:/Users/冯立松/Downloads/SMQ_F.XPT")
smq_f<-SMQ_F[,c("SEQN","SMQ040","SMQ020")]

ALQ_H<-read.xport("C:/Users/冯立松/Downloads/ALQ_H.XPT")
alq_h<-ALQ_H[,c("SEQN","ALQ101")]
SMQ_H<-read.xport("C:/Users/冯立松/Downloads/SMQ_H.XPT")
smq_h<-SMQ_H[,c("SEQN","SMQ040","SMQ020")]

Alq<-rbind(alq_e,alq_f,alq_h)
Smq<-rbind(smq_e,smq_f,smq_h)
Smq_dat <- rename(Smq,c(SMQ040="smoke_now"))
Alq_dat <- rename(Alq,c(ALQ101="drink_now"))

#多数据集横向合并
dat01<-merge(Demo_dat,Paq_dat,by="SEQN", all.x = T)
dat02<-merge(dat01,Dxxspn_dat,by="SEQN", all.x = T)
dat03<-merge(dat02,Dxxfem_dat,by="SEQN", all.x = T)
dat04<-merge(dat03,Ocq_dat,by="SEQN", all.x = T)
dat05<-merge(dat04,Smq_dat,by="SEQN", all.x = T)
dat06<-merge(dat05,Alq_dat,by="SEQN", all.x = T)

#调查对象纳排--------------
data00 <- dat06  #将data重新赋予数据
str(data00)

#纳入年龄
data01 <- subset(data00,data00$Age>=20 & data00$Age<=59)
#DXA invalid
table(data01$femur_scan_status,useNA = "ifany")
table(data01$v_work,useNA="ifany")

data02 <- subset(data01,!is.na(data01$spine_scan_status))
table(data02$spine_scan_status,useNA = "ifany")
table(data02$femur_scan_status,useNA = "ifany")
table(data02$spine_invalidity,useNA = "ifany")
table(data02$femur_invalidity,useNA = "ifany")

data03 <- subset(data02,data02$spine_scan_status<3)
table(data03$spine_scan_status,useNA = "ifany")
table(data03$femur_scan_status,useNA = "ifany")
table(data03$spine_invalidity,useNA = "ifany")
table(data03$femur_invalidity,useNA = "ifany")

data04 <- subset(data03,data03$femur_scan_status<3)# data3&4 exclude 因身体等条件不能做DXA的 
table(data04$spine_scan_status,useNA = "ifany")
table(data04$femur_scan_status,useNA = "ifany")
table(data04$spine_invalidity,useNA = "ifany")
table(data04$femur_invalidity,useNA = "ifany")

data05 <- subset(data04,data04$spine_invalidity==0)
table(data05$spine_invalidity,useNA = "ifany")
table(data05$femur_invalidity,useNA = "ifany")
table(data05$spine_scan_status,useNA = "ifany")
table(data05$femur_scan_status,useNA = "ifany")

data066 <- subset(data05,data05$femur_invalidity==0)
table(data066$spine_invalidity,useNA = "ifany")
table(data066$femur_invalidity,useNA = "ifany")
table(data066$spine_scan_status,useNA = "ifany")
table(data066$femur_scan_status,useNA = "ifany")#存疑

data06 <- subset(data066,data066$femur_scan_status==1)#发现唯一一个NA
summary(data06$femur_BMD)


#PAQ纳排
data07 <- subset(data06,data06$v_recreation!=9)
data08 <- subset(data07,data07$m_recreation!=9)

table(data08$v_recreation,useNA="ifany")# 9 don't know
table(data07$m_recreation,useNA="ifany")# 9 don't know
table(data08$day_v_work,useNA="ifany")#99 don't k
table(data06$day_m_work,useNA="ifany")#77、99
table(data06$day_v_recreation,useNA="ifany")#99
table(data06$day_m_recreation,useNA="ifany")#99
table(data06$day_trans,useNA="ifany")#99
table(data06$min_v_work,useNA="ifany")#9999
table(data06$min_m_work,useNA="ifany")#9999
table(data06$min_trans,useNA="ifany")#9999




data08a<-data08
data08a[is.na(data08a)]<--999
data08b <- subset(data08a,data08a$day_v_work!=99 & data08a$day_m_work!=99 & data08a$day_m_work!=77 
                  & data08a$day_v_recreation!=99 & data08a$day_m_recreation!=99 & data08a$day_trans!=99
                  & data08a$min_v_work!=9999 & data08a$min_m_work!=9999 & data08a$min_trans!=9999)
table(data08b$min_m_work,useNA="ifany")

data08b[data08b == -999] <- NA
data09<-data08b


#按PA分组--------------

data09$day_v_work<-ifelse(is.na(data09$day_v_work),0,data09$day_v_work)
data09$day_m_work<-ifelse(is.na(data09$day_m_work),0,data09$day_m_work)
data09$day_v_recreation<-ifelse(is.na(data09$day_v_recreation),0,data09$day_v_recreation)
data09$day_m_recreation<-ifelse(is.na(data09$day_m_recreation),0,data09$day_m_recreation)
data09$day_trans<-ifelse(is.na(data09$day_trans),0,data09$day_trans)
data09$min_v_work<-ifelse(is.na(data09$min_v_work),0,data09$min_v_work)
data09$min_m_work<-ifelse(is.na(data09$min_m_work),0,data09$min_m_work)
data09$min_v_recreation<-ifelse(is.na(data09$min_v_recreation),0,data09$min_v_recreation)
data09$min_m_recreation<-ifelse(is.na(data09$min_m_recreation),0,data09$min_m_recreation)
data09$min_trans<-ifelse(is.na(data09$min_trans),0,data09$min_trans)

str(data09)

#-----PA计算-------
data09$VPAwork<-data09$day_v_work*data09$min_v_work
data09$VPArecreation<-data09$day_v_recreation*data09$min_v_recreation

data09$MPAwork<-data09$day_m_work*data09$min_m_work
data09$MPArecreation<-data09$day_m_recreation*data09$min_m_recreation
data09$MPAtrans<-data09$day_trans*data09$min_trans

data09$PA<-data09$VPArecreation*2+data09$MPArecreation

for(i in 1:length(data09$day_v_recreation)){ 
  data09$freq[i]<-max(data09$day_m_recreation[i],data09$day_v_recreation[i])
}

data09$WWorRA<-ifelse(data09$PA>=150,ifelse(data09$freq<=2,1,2),3)#1=WW,2=RA,3=inactive
table(data09$WWorRA,useNA="ifany")

data09$work_related_PA<-data09$VPAwork*2+data09$MPAwork
summary(data09$work_related_PA)
table(data09$femur_scan_status,useNA="ifany")

#数据进一步整理Table1----------

#精简不需要的变量
data10 <- subset(data09, select=-c(spine_invalidity,femur_invalidity,spine_scan_status,femur_scan_status))
#性别factor化
data10$Gender <- factor(data10$Gender,levels = c(1,2),labels = c("Male","Female"))




####__________种族####
table(data10$Race,useNA = "ifany")  
# 1	Mexican American
# 2	Other Hispanic
# 3	Non-Hispanic White
# 4	Non-Hispanic Black
# 5	Other Race - Including Multi-Racial

data10$Race <- factor(data10$Race,levels = c(1,2,3,4,5),
                           labels = c("Mexican American",
                                      "Other Hispanic",
                                      "Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Other Race"))

str(data10)

table(data10$Marital,useNA = "ifany") 
# 1	Married		
# 2	Widowed	
# 3	Divorced
# 4	Separated	
# 5	Never married
# 6	Living with partner	
# 77	Refused	
# 99	Don't Know	
#文献的做法是已婚/未婚，根据样本量，Married	和Living with partner	进行的合并
data10$Marital <- ifelse(data10$Marital==77|data10$Marital==99,999,data10$Marital)
data10$Marital <- ifelse(data10$Marital==999,999,ifelse(data10$Marital %in% c(1,6),1,2))
data10$Marital <- factor(data10$Marital,
                              levels = c(1,2),
                              labels = c("Yes","No"))
table(data10$Marital,useNA = "ifany") 


####__________文化程度####
table(data10$Education,useNA = "ifany") 
# 1	Less than 9th grade	
# 2	9-11th grade (Includes 12th grade with no diploma)
# 3	High school graduate/GED or equivalent
# 4	Some college or AA degree	
# 5	College graduate or above	
# 7	Refused
# 9	Don't Know
# .	Missing
#文献的做法是将Less than 9th grade 和9-11th grade (Includes 12th grade with no diploma)合并
# 分类为：Below high school 和 High School or above

data10$Education <- ifelse(data10$Education>6,data10$Education,ifelse(data10$Education <=3 ,1,2))
data10$Education <- factor(data10$Education,c(1,2),
                                labels = c("High school or below",
                                           "Some college or above"))
table(data10$Education,useNA = "ifany") 




####__________工作####
table(data10$work_last_week,useNA = "ifany")
data10$work_last_week <- ifelse(is.na(data10$work_last_week),999,data10$work_last_week)
data10$employment <- ifelse(data10$work_last_week>6,data10$work_last_week,ifelse(data10$work_last_week==4 ,2,1))
table(data10$employment,useNA = "ifany")

data10$employment <- factor(data10$employment,c(1,2),
                           labels = c("Employed",
                                      "Unemployed"))


####__________PIR####
summary(data10$PIR) 
#data10$PIR_new <- ifelse(data10$PIR<1,1,ifelse(data10$PIR<1.33,2,ifelse(data10$PIR<1.50,3,ifelse(data10$PIR<1.85,4,5))))
data10$PIR_new <- ifelse(data10$PIR<1,1,2)
table(data10$PIR_new,useNA = "ifany") #很明显，文献将NA转在2组，也就是Not poor
data10$PIR_new <- ifelse(is.na(data10$PIR),2,data10$PIR_new)
data10$PIR_new <- factor(data10$PIR_new,levels = c(1,2),labels = c("Poor","Not poor"))
table(data10$PIR_new,useNA = "ifany") #再次分组


####__________吸烟####

table(data10$smoke_now,useNA = "ifany") 
table(data10$SMQ020,useNA = "ifany")
 
data10$SMQ020 <- ifelse(is.na(data10$SMQ020),999,data10$SMQ020)#没有
# Smoke_history
# 1	Yes
# 2	No
# 7	Refused	
# 9	Don't know
# .	Missing

data10$smoke_now <- ifelse(is.na(data10$smoke_now),999,data10$smoke_now)
# smoke_now
# 1	Every day
# 2	Some days
# 3	Not at all
# 7	Refused
# 9	Don't know
# .	Missing
data10$Smoke <- ifelse(data10$SMQ020==2,1, #Never
                     ifelse(data10$SMQ020==1 & data10$smoke_now==3,2, #former
                            ifelse(data10$SMQ020==1 & data10$smoke_now %in% c(1,2),3,999 )))

table(data10$Smoke,useNA = "ifany")  

data10$Smoke <- factor(data10$Smoke,levels = c(1,2,3),labels = c("Never","Former","Current"))


####__________饮酒####
table(data10$drink_now,useNA = "ifany")
# 1	Yes
# 2	No
# 7	Refused
# 9	Don't know
# .	Missing
data10$drink_now <- factor(data10$drink_now,
                        levels = c(1,2),
                        labels = c("Yes","No"))

options(digits=3)

#删除临时的无用变量
data11 <- subset(data10, select=-c(PIR,SMQ020,smoke_now,work_last_week,reason_not_work))


#数据从这里捡----------
#write.csv(data09,file="data09.csv",row.names = F)
#write.csv(data10,file="data10.csv",row.names = F)
#write.csv(data11,file="data.csv",row.names = F)

#data11<-read.csv(file="data11.csv",header = TRUE)



#data11 进一步处理-------
data11$T_Lumbar<-(data11$spine_BMD-1.065)/0.122
data11$T_Femur_neck<-(data11$femur_neck_BMD-0.888)/0.121
data11$Lumbar_loss<-ifelse(data11$T_Lumbar+1<0,1,0)
data11$Lumbar_loss <- factor(data11$Lumbar_loss,levels = c(0,1),labels = c("normal","low_bone_mass"))
data11$Femur_neck_loss<-ifelse(data11$T_Femur_neck+1<0,1,0)
data11$Femur_neck_loss <- factor(data11$Femur_neck_loss,levels = c(0,1),labels = c("normal","low_bone_mass"))
str(data11)

data11$Year<-as.factor(data11$Year)

data11$WT<-data11$WTMEC2YR/3#6year人口变3倍，权重除3

data11 <- rename(data11,
                 c(Lumbar_loss="Lloss",
                   Femur_neck_loss="Fnloss")
)



#注意
data11$WWorRA<-as.numeric(data11$WWorRA)#1=WW,2=RA,3=inactive
data11$WWorRA<-ifelse(data11$WWorRA==3,1,ifelse(data11$WWorRA==1,2,3))
data11$WWorRA<-factor(data11$WWorRA,levels=c(1,2,3),labels=c("inactive","WW","RA"))
table(data11$WWorRA)

data11$TPA<-data11$work_related_PA+data11$PA

data11$age50<-ifelse(data11$Age>=50,1,2)
data11$age50<-factor(data11$age50,levels=c(1,2),labels=c("50 and older","younger than 50"))

#为了便于logistics------
data11$L_status<-ifelse(data11$Lloss=="low_bone_mass",1,0)
data11$Fn_status<-ifelse(data11$Fnloss=="low_bone_mass",1,0)


#补充duration of session & intensity------
data11$PA_of_session<-ifelse(data11$freq==0,0,data11$PA/data11$freq)
data11$intensity<-ifelse(data11$PA==0,0,data11$VPArecreation*2/data11$PA)
for(i in 1:length(data11$min_v_recreation)){ 
  data11$duration[i]<-max(data11$min_m_recreation[i],data11$min_v_recreation[i])
}

table(data11$WWorRA)

data11not_inactive<-subset(data11,data11$WWorRA!="inactive")
data11_ww<- subset(data11,data11$WWorRA=="WW")
data11_ra<- subset(data11,data11$WWorRA=="RA")
data11_inactive<- subset(data11,data11$WWorRA=="inactive")


data11not_inactive$duration_value<-ifelse(data11not_inactive$WWorRA=="RA",1,ifelse(data11not_inactive$duration<=30,2,ifelse(data11not_inactive$duration>90,4,3)))
data11not_inactive$duration_value <- factor(data11not_inactive$duration_value,c(1,2,3,4),
                                            labels = c("RA",
                                                       "WW&<=30",
                                                       "WW&>30<=90",
                                                       "WW&>90"))
table(data11not_inactive$duration_value,useNA = "ifany") 


data11not_inactive$intensity_value<-ifelse(data11not_inactive$WWorRA=="RA",1,ifelse(data11not_inactive$intensity==0,2,ifelse(data11not_inactive$intensity<=0.50,3,ifelse(data11not_inactive$intensity<1.00,4,5))))
data11not_inactive$intensity_value<- factor(data11not_inactive$intensity_value,c(1,2,3,4,5),
                                            labels = c("RA",
                                                       " 0 ",
                                                       " >0 to ≤50 ",
                                                       " >50 to <100 ",
                                                       " 100 "))
table(data11not_inactive$intensity_value,useNA = "ifany") 

data11not_inactive$freq_value<- ifelse(data11not_inactive$WWorRA=="RA",1,ifelse(data11not_inactive$freq==1,2,3))
data11not_inactive$freq_value<- factor(data11not_inactive$freq_value,c(1,2,3),
                                 labels=c("RA","1","2"))
table(data11not_inactive$freq_value,useNA = "ifany")

data11$intensity_Percent <- data11$intensity / max(data11$intensity) * 100

#基线表，文章中的table1-------
#install.packages("tableone")
install.packages("glue")
library(glue)
#install.packages("compareGroups")
library(compareGroups)
library(tableone)
descrTable( ~ ., data = data11)
descrTable( ~ ., data = data11_inactive)
descrTable( ~ ., data = data11_ww)
descrTable( ~ ., data = data11_ra)





###加权###---------
library(survey)
study_design <- svydesign(
  data = data11, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

study_design1 <- svydesign(
  data = data11smoke_current, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

study_design2 <- svydesign(
  data = data11smoke_former, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

study_design3 <- svydesign(
  data = data11smoke_never, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

study_design4 <- svydesign(
  data = data11not_inactive, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

study_design5 <- svydesign(
  data = data11_ww, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

study_design6 <- svydesign(
  data = data12, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)


#svytable(~Gender +  Race + Year +
           #Education + Marital + employment + PIR_new + Smoke + drink_now + work_related_PA,study_design
  svytotal(~Gender,design=study_design,deff=TRUE)
  svyby(~WWorRA,~Year+Race,data11,svymean)
  #%>%
  #as.data.frame() %>%
  #mutate(prop = Freq / sum(Freq) * 100) %>%
  #arrange(desc(prop))


#生成描述性table
status_tab <- descrTable(WWorRA ~ Age + Gender + Race + Year +
                           Education + Marital + employment + PIR_new + Smoke + drink_now + work_related_PA + spine_BMD + femur_BMD, 
                         show.all=T,
                         data = data11) 
print(status_tab)
export2word(status_tab, file='table1.docx')

#均值和置信区间手工计算------
#t.test(data11$Age,conf.level = 0.95)$conf.int
#confint(object, level = 0.95)

#mean(data10$Age)


#####5.4加权分析####
library("survey")

#进行统计描述
#总体-频数+构成比-------
table(data11$Education)
#如遇NA
data13<-data11[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","WT","WWorRA","drink_now")]
data13<-subset(data13,!is.na(data13$drink_now))

temp <- data.frame(svytotal(~drink_now,study_design3))
sprintf("%0.1f",temp$total/sum(temp$total)*100)

#分组-频数+构成比-------
prop_temp <- svytable(~Gender+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~Race+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~Year+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~Education+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~Marital+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~employment+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~PIR_new+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~Smoke+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

prop_temp <- svytable(~drink_now+WWorRA, study_design)
prop <- cbind(sprintf("%0.1f",prop.table(prop_temp[,1])*100),
              sprintf("%0.1f",prop.table(prop_temp[,2])*100),
              sprintf("%0.1f",prop.table(prop_temp[,3])*100))
prop

#加权卡方检验------
svychisq(~Gender+depress,study_design,statistic = c("Chisq"))
svychisq(~PIR_new+WWorRA,study_design,statistic = c("Chisq"))
svychisq(~Year+WWorRA,study_design,statistic = c("Chisq"))
svychisq(~drink_now+WWorRA,study_design,statistic = c("Chisq"))
svychisq(~Marital+WWorRA,study_design,statistic = c("Chisq"))

#总体-均数和95ci
data12<-data11
data12$help<-1
svyby(~Age, ~help,study_design6,svymean,na.rm = T,vartype="ci")
svyby(~femur_neck_BMD, ~help,study_design6,svymean,na.rm = T,vartype="ci")

#分组--结局均数和95ci---------
svyby(~Age, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~work_related_PA, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~spine_BMD, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~femur_neck_BMD, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~T_Lumbar, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~T_Femur_neck, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )


#anova---------
survey::regTermTest(survey::svyglm(Age~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(work_related_PA~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(T_Lumbar~WWorRA,study_design),"WWorRA")#p=0.005
survey::regTermTest(survey::svyglm(T_Femur_neck~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(femur_neck_BMD~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(spine_BMD~WWorRA,study_design),"WWorRA")

#KW test秩和检验
svyranktest(Age~WWorRA,study_design,test = c("KruskalWallis"))

#总体-中位数和四分位数
#svyquantile(~Age,study_design,quantiles = c(0.25,0.5,0.75))
#分组--中位数和四分位数
#svyby(~Age, ~depress, study_design, svyquantile,quantiles=c(0.25,0.5,0.75))
#t test
#survey::svyttest(Age~depress,study_design)
#mannu test
#survey::svyranktest(Age~depress,study_design, test = c("wilcoxon"))

#非加权卡方
#mytable<-table(data11$PIR_new,data11$WWorRA)
#mytable
#chisq.test(mytable)


#因子化-------
table(data11not_inactive$WWorRA)
data11not_inactive$WWorRA<-as.numeric(data11not_inactive$WWorRA)
data11not_inactive$WWorRA<-factor(data11not_inactive$WWorRA,levels=c(3,2),labels=c("RA","WW"))




#调整顺序
#data11b$WWorRA<-as.numeric(data11b$WWorRA)
#data11b$WWorRA<-ifelse(data11b$WWorRA==3,1,ifelse(data11b$WWorRA==1,2,3))
#data11b$WWorRA<-factor(data11b$WWorRA,levels=c(1,2,3),labels=c("RA","inactive","WW"))

data11b$WWorRA<-as.numeric(data11b$WWorRA)
data11b$WWorRA<-ifelse(data11b$WWorRA==3,1,ifelse(data11b$WWorRA==1,2,3))
data11b$WWorRA<-factor(data11b$WWorRA,levels=c(1,2,3),labels=c("inactive","WW","RA"))




#线性回归------ 

#unadjusted
mod<-svyglm(spine_BMD~WWorRA,study_design,family="gaussian")
summary(mod)
mod<-svyglm(femur_neck_BMD~WWorRA,study_design,family="gaussian")
summary(mod)


#model 1
mod_mult<-svyglm(spine_BMD~WWorRA+Smoke+drink_now,study_design,family="gaussian")
summary(mod_mult)
mod_mult<-svyglm(femur_neck_BMD~WWorRA+Smoke+drink_now,study_design,family="gaussian")
summary(mod_mult)

#model 2
mod_mult<-svyglm(spine_BMD~WWorRA+Age+Gender,study_design,family="gaussian")
summary(mod_mult)
mod_mult<-svyglm(femur_neck_BMD~WWorRA+Age+Gender,study_design,family="gaussian")
summary(mod_mult)

#model 3
mod_mult<-svyglm(spine_BMD~WWorRA+Race+Marital+employment,study_design,family="gaussian")
summary(mod_mult)
mod_mult<-svyglm(femur_neck_BMD~WWorRA+Race+Marital+employment,study_design,family="gaussian")
summary(mod_mult)


#model 4
#interaction_mult<-svyglm(spine_BMD~WWorRA+Gender+WWorRA*Gender,study_design,family="gaussian")
#summary(interaction_mult)

mod_mult<-svyglm(spine_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,study_design,family="gaussian")
summary(mod_mult)
mod_mult<-svyglm(femur_neck_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,study_design,family="gaussian")
summary(mod_mult)

#table 3中以RA为参照------
multi_mod<-svyglm(spine_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)
multi_mod<-svyglm(femur_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)

multi_mod<-svyglm(femur_neck_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)

#β±1.96S.E

#duration
multi_mod<-svyglm(spine_BMD~duration_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)
multi_mod<-svyglm(femur_neck_BMD~duration_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)

#intensity
multi_mod<-svyglm(spine_BMD~intensity_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)
multi_mod<-svyglm(femur_neck_BMD~intensity_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)


#freq
multi_mod<-svyglm(spine_BMD~freq_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)
multi_mod<-svyglm(femur_neck_BMD~freq_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)





#分析权重
mod44<-lm(spine_BMD~WWorRA+drink_now+Smoke, data = data11,weight= WT)
summary(mod44)


#femoral neck BMD----------
mod_mult<-svyglm(femur_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,study_design,family="gaussian")
summary(mod_mult)
mod<-svyglm(femur_neck_BMD~WWorRA,study_design,family="gaussian")
summary(mod)

#趋势性检验
#开展加权logistic回归------

#通用多因素
fit1<-svyglm(L_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design1,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design1,family=quasibinomial)
summary(fit2)

#去掉性别
fit1<-svyglm(L_status~WWorRA+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design2,family=quasibinomial)
summary(fit2)

#去掉吸烟
fit1<-svyglm(L_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+employment+PIR_new+work_related_PA+PA,study_design3,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+employment+PIR_new+work_related_PA+PA,study_design3,family=quasibinomial)
summary(fit2)

#去掉饮酒
fit1<-svyglm(L_status~WWorRA+Year+Gender+Age+Race+Education+Marital+Smoke+employment+PIR_new+work_related_PA+PA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Gender+Age+Race+Education+Marital+Smoke+employment+PIR_new+work_related_PA+PA,study_design2,family=quasibinomial)
summary(fit2)

#去掉employment
fit1<-svyglm(L_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+PIR_new+work_related_PA+PA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+PIR_new+work_related_PA+PA,study_design2,family=quasibinomial)
summary(fit2)

#RA为参照，WW（Table 3）
fit1<-svyglm(L_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family=quasibinomial)
summary(fit2)

#freq_value
fit1<-svyglm(L_status~freq_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~freq_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family=quasibinomial)
summary(fit2)

#单因素
fit1<-svyglm(L_status~WWorRA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA,study_design2,family=quasibinomial)
summary(fit2)

#OR和置信区间--------
ShowRegTable(fit1)
ShowRegTable(fit2)


#male
fit1<-svyglm(L_status~WWorRA,study_design1,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA,study_design1,family=quasibinomial)
summary(fit2)

#female
fit1<-svyglm(L_status~WWorRA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA,study_design2,family=quasibinomial)
summary(fit2)

#age
fit1<-svyglm(Lloss~WWorRA,study_design1,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fnloss~WWorRA,study_design1,family=quasibinomial)
summary(fit2)

fit1<-svyglm(Lloss~WWorRA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fnloss~WWorRA,study_design2,family=quasibinomial)
summary(fit2)

#RCS---------
#logistics
#单因素
fitc <- svyglm(Fnloss ~ rcs(PA, 4),family ="quasibinomial",  design =study_design3)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#多因素
#性别
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design2)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#age
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Gender+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design2)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#employment
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design2)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#drink
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design2)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#smoke
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+drink_now+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design3)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))


#total
fitc <- svyglm(Lloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))


#linear
fitb <- svyglm(vd ~ rcs(TC, 4),family ="gaussian",  design = Svydesign)#加权RCS模型
plot_fitb <- NHS_rcs(fitb,reference = "median", pvalue_position=c(0.05,0.85))

ggplot()+geom_line(data=data11, aes(Fnloss,PA),linetype=1,linewidth=1,alpha = 0.9,colour="#EFA484")+
  geom_ribbon(data=data11, aes(PA,4,ymin = lower, ymax = upper),alpha = 0.3,fill="#FDCF9E")+
  geom_hline(yintercept=1, linetype=2,linewidth=1)+
  geom_vline(xintercept=51, linetype=2,linewidth=1)+
  theme_classic()+ 
  labs(x="VD", y="OR(95%CI)")+###绘图  
  theme(axis.title = element_text(
    family = "serif",##坐标轴标签字体
    size=20, ##字体大小
    lineheight = 1),##标签行间距的倍数
    axis.text = element_text(
      family = "serif",##字体
      face="bold", ##字体外形（粗斜体等）
      color="black",
      size=20))

test_PLOT<-get_rcs(fit_RCS)




#亚组分析-------
data11not_inactive<-subset(data11,data11$WWorRA!="inactive")
data11_ww<- subset(data11,data11$WWorRA=="WW")
data11_ra<- subset(data11,data11$WWorRA=="RA")
data11_inactive<- subset(data11,data11$WWorRA=="inactive")

data11_L_low<-subset(data11,data11$T_Lumbar+1<0)
table(data11_L_low$Gender)
table(data11_L_low$Race)
table(data11$Race)
data11_F_low<-subset(data11,data11$T_Femur_neck+1<0)
table(data11_F_low$Gender)

#session
data11freq1<-subset(data11_ww,data11_ww$freq==1)
data11freq2<-subset(data11_ww,data11_ww$freq==2)
data11duration1<-subset(data11_ww,data11_ww$duration<=30)

#性别分组
data11female<-subset(data11,data11$Gender=="Female")
data11male<-subset(data11,data11$Gender=="Male")

#age subgroup
data11age50<-subset(data11,data11$Age>=50)
data11age_young<-subset(data11,data11$Age<50)

#smoke
data11smoke_never<-subset(data11,data11$Smoke=="Never")
data11smoke_current<-subset(data11,data11$Smoke=="Current")
data11smoke_former<-subset(data11,data11$Smoke=="Former")

#drink
data11not_drink<-subset(data11,data11$drink_now=="No")
data11drink<-subset(data11,data11$drink_now=="Yes")

#employment
data11employed<-subset(data11,data11$employment=="Employed")
data11unemployed<-subset(data11,data11$employment=="Unemployed")
table(data11$employment,useNA = "ifany")

#ROC/AUC-------
library("pROC")
##roc1<-roc(Fnloss~TPA,data=data11)
roc1<-roc(Lloss~PA,data=data11)
##roc1<-roc(Fnloss~TPA,data=data11male)
roc2<-roc(Fnloss~PA,data=data11unemployed)
roc3<-roc(Fnloss~PA,data=data11employed)
roc4<-roc(Lloss~PA,data=data11smoke_never)
roc5<-roc(Lloss~PA,data=data11smoke_former)
roc5<-roc(Lloss~PA,data=data11smoke_current)



##roc1<-roc(Lloss~TPA,data=data11male)
##roc1<-roc(Fnloss~TPA,data=data11age_young)
##roc1<-roc(Fnloss~PA,data=data11age_young)


#ROC旧--------
attributes(roc1)
roc1$auc
roc.result<-data.frame(threshold=roc1$thresholds,
                       sensitivity=roc1$sensitivities,
                       specificity=roc1$specificities)
roc.result$youden<-roc.result$sensitivity+roc.result$specificity-1
head(roc.result)
which.max(roc.result$youden)
roc.result[190,]
plot.roc(roc1,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),auc.polygon.col = "lightblue",print.thres=T)


library("RISCA")
roc1 <- roc.binary(status="low_bone_mass", variable="PA", confounders=~1,
                   data=data11, precision=seq(0.1,0.9, by=0.1) )
plot(roc1, xlab="1-specificity", ylab="sensibility")




#roc2<-roc(Femur_neck_loss~PA,data=data11)
attributes(roc2)
roc2$auc

summary(data11$spine_BMD)
summary(data11$femur_BMD)
summary(data11$PA)
table(data11$PA)



#RCS 2-----
fitc <- svyglm(Fnloss ~ rcs(PA, 4),family ="quasibinomial",  design =study_design2)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

fitc <- svyglm(Lloss ~ rcs(PA, 4),family ="quasibinomial",  design =study_design2)#加权RCS模型
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))


#改进ROC----
#基础
g<-ggroc(roc1)
g
#设置对角线
g + theme_minimal() + ggtitle("输入标题") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", 
               linetype=6)###设置对角线，主要设置起点和终点的X,Y坐标
#坐标轴进行更改
gl <- ggroc(roc1, legacy.axes = TRUE)
gl
gl + xlab("1-specificity") + ylab("sensitivity") + 
geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")

#plot(效果较理想）-----
plot(roc3, 
     col = "red", #设置曲线颜色 
     legacy.axes = TRUE, #使x轴变为1-Specificity 
     main = "Femur neck BMD decrease", #添加标题 
     print.auc = TRUE, 
     auc.polygon = TRUE, #将auc曲线下面积转换为多边形 
     auc.polygon.col = "#fff7f7", #设置多边形的填充色 
     #grid = c(0.5, 0.2), #设置两轴网格线的间隔为0.5，0.2 
     #grid.col = c( "black", "black"), #设置两轴网格线的颜色 
     print.thres = TRUE) 
     legend( 0.4, 0.4, #设置位置的x轴和y轴坐标 
        lwd = 2, legend = "employed", col = "red") #颜色与ROC曲线的颜色一致
     
     

#ggroc组合-------


g2 <- ggroc(list("total"=roc1, "non-drinker"=roc2, "drinker"=roc3,"non-smoker"=roc4,"former-smoker"=roc5,"current-smoker"=roc6),legacy.axes = TRUE)
g2


g2  + ggtitle("Lumbar BMD decrease")+ xlab("1-specificity") + ylab("sensitivity") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")


#or
roc.list <- roc(Lloss ~ PA + TPA + work_related_PA, data = data11)
g.list <- ggroc(roc.list)
g.list


#修改线形
g5 <- ggroc(roc.list, aes=c("linetype", "color"))
g5

#对角线添加
g5+annotate(geom = "segment", x = 1, y = 0, xend =0, yend = 1)

#
g2+annotate(geom = "segment", x = 1, y = 0, xend =0, yend = 1)
  





library(ggplot2)
pa<- ggroc(smooth$PA, 
           legacy.axes = TRUE # 将X轴改为0-1，（默认是1-0）
)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
               color="darkgrey", linetype=4)+
  theme_bw() +# 设置背景
  ggtitle('a-ROC')
pb<- ggroc(smooth$b, legacy.axes = TRUE)+geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype=4)+theme_bw() +ggtitle('b-ROC')
pc<- ggroc(smooth$c, legacy.axes = TRUE)+geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype=4)+theme_bw() +ggtitle('c-ROC')
plot_grid(pa,labels = "AUTO",nrow = 1)

#combine curve
ggroc(smooth, legacy.axes = TRUE)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype=4)+
  theme_bw()+ggtitle('ROC')+ggsci::scale_color_lancet()+
  annotate("text",x=0.75,y=0.125,label=paste("a-AUC = ", round(res$a$auc,3)))+
  annotate("text",x=0.75,y=0.25,label=paste("b-AUC = ", round(res$b$auc,3)))+
  annotate("text",x=0.75,y=0.375,label=paste("c-AUC = ", round(res$c$auc,3)))


#可视化-----
library(ggplot2)



ggplot(data11) +
  #绘制每一个图层
  geom_bar(aes(x = WWorRA, y= PA_of_session, group = 1, color = "Total PA of session, min")) + 
  geom_line(aes(x = WWorRA, y= intensity, group = 1, color = "Intensity (VPA/total PA), %"), linewidth = 1) + 
  geom_bar(aes(x = WWorRA, y= duration, group = 1, color = "Duration of session, min")) +
 
  labs( y = "Minuyes") + 
  labs( y = "Percent") +
  #可以自己设置颜色
  scale_color_manual(values = c("Total PA of session, min" = "#96BDE6", 
                                "Intensity (VPA/total PA), %" = "#E94243", 
                               "Duration of session, min" = "#129AB3")) + 
  #也可以自己给配色方法
  scale_color_brewer(palette = "Set2",
                     limits = c("Total PA of session, min","Intensity (VPA/total PA), %","Duration of session, min","15-19 years")) + 

  theme_classic() +
  #修改各种字体和位置
  theme(legend.position=c(0.8,0.25),
        legend.title  = element_text(family = "serif",size=15),
        legend.text  = element_text(family = "serif",size=15))+
  theme(axis.title = element_text(family = "serif", size = 15),
        axis.text  = element_text(family = "serif", size = 15),
        axis.text.x = element_text(angle = 45,vjust = 0.8,hjust = 0.75))



#改进可视化-------


#Overall participants
dat <- data11 %>% 
  #dplyr::group_by(Gender,WWorRA) %>% 
  dplyr::group_by(WWorRA) %>% 
  dplyr::summarise(mean1=mean(PA_of_session,na.rm = T),sd1 = sd(PA_of_session,na.rm=T),se1=sd1/sqrt(length(PA_of_session)),
                   mean2=mean(duration,na.rm = T),sd2=sd(duration,na.rm=T),se2=sd2/sqrt(length(duration)),
                   mean3=mean(intensity_Percent,na.rm=T),sd3=sd(intensity_Percent,na.rm=T),se3=sd3/sqrt(length(intensity_Percent)),.groups = "drop") %>% 
  mutate(Adjust_Lower1=mean1 - se1,
         Adjust_Upper1=mean1 + se1,
         Adjust_Lower2=mean2 - se2,
         Adjust_Upper2=mean2 + se2,
         Adjust_Lower3=mean3 - se3,
         Adjust_Upper3=mean3 + se3) %>% 
  mutate(xx=ifelse(WWorRA=="inactive",1,
                   ifelse(WWorRA=="WW",2,3))) %>%  
  dplyr::rename(Group=WWorRA)

# 生成分组绘图
ggplot(dat) +
  #geom_bar(aes(x = Group,y = mean, fill = Gender),position = "dodge", stat = "identity")+ #分组条图
  geom_bar(aes(x=Group,y=mean1,group = 1, fill = "Duration of session, min"),position = "dodge", stat = "identity",width=0.5)+#分组条图,fill="#9DC3E6"
  geom_text(aes(x = Group,y = mean1,label=round(mean1,1)),vjust=-3,size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower1,ymax =Adjust_Upper1),width = 0.10)+ #误差图
  
  geom_bar(aes(x = Group,y = mean2 ,group = 1, fill = "Total PA of session, min"),position = dodge(width=-0.9), stat = "identity",width=0.5)+ #分组条图,fill="#DEEBF7"
  geom_text(aes(x = Group,y = mean2/2,label=round(mean2,1)),size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower2,ymax =Adjust_Upper2),width = 0.10)+ #误差图
  
  geom_line(aes(x = xx,y = mean3*3.3, group = 1, color = "Intensity (VPA/total PA), %"),linewidth=1)+ #折线图color="#E94243"
  geom_text(aes(x = xx,y = mean3*3.3,label=round(mean3,1)),hjust=1.8,size=3.5)+
  geom_errorbar(aes(x = Group, ymin = Adjust_Lower3*3.3, ymax = Adjust_Upper3*3.3), 
                width = 0.1, position = position_dodge(width = 0.6)) + #分组误差
  labs(x="PA patterns",
        title="Overall participants")+
  # 左侧y轴标签
  scale_y_continuous(
    name = "Minutes",
    sec.axis = sec_axis(~ ./3.3, name = "Percent(%)")
  ) +
  theme_minimal() +
  labs(color = "Group") + 
  #可以自己设置颜色
  scale_fill_manual(labels=c("Total PA of session, min","Duration of session, min"),values =c("#DEEBF7","#9DC3E6"))+ 
  scale_color_manual(labels="Intensity (VPA/total PA), %",values = "#E94243" )+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())
  #+theme(legend.background = element_rect(fill = "white",colour = "black"))



#Female
dat1 <- data11female %>% 
  #dplyr::group_by(Gender,WWorRA) %>% 
  dplyr::group_by(WWorRA) %>% 
  dplyr::summarise(mean1=mean(PA_of_session,na.rm = T),sd1 = sd(PA_of_session,na.rm=T),se1=sd1/sqrt(length(PA_of_session)),
                   mean2=mean(duration,na.rm = T),sd2=sd(duration,na.rm=T),se2=sd2/sqrt(length(duration)),
                   mean3=mean(intensity_Percent,na.rm=T),sd3=sd(intensity_Percent,na.rm=T),se3=sd3/sqrt(length(intensity_Percent)),.groups = "drop") %>% 
  mutate(Adjust_Lower1=mean1 - se1,
         Adjust_Upper1=mean1 + se1,
         Adjust_Lower2=mean2 - se2,
         Adjust_Upper2=mean2 + se2,
         Adjust_Lower3=mean3 - se3,
         Adjust_Upper3=mean3 + se3) %>% 
  mutate(xx=ifelse(WWorRA=="inactive",1,
                   ifelse(WWorRA=="WW",2,3))) %>%  
  dplyr::rename(Group=WWorRA)

# 生成分组绘图
ggplot(dat1) +
  #geom_bar(aes(x = Group,y = mean, fill = Gender),position = "dodge", stat = "identity")+ #分组条图
  geom_bar(aes(x=Group,y=mean1,group = 1, fill = "Duration of session, min"),position = "dodge", stat = "identity",width=0.5)+#分组条图,fill="#9DC3E6"
  geom_text(aes(x = Group,y = mean1,label=round(mean1,1)),vjust=-3,size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower1,ymax =Adjust_Upper1),width = 0.10)+ #误差图
  
  geom_bar(aes(x = Group,y = mean2 ,group = 1, fill = "Total PA of session, min"),position = position_dodge(width=-0.9), stat = "identity",width=0.5)+ #分组条图,fill="#DEEBF7"
  geom_text(aes(x = Group,y = mean2/2,label=round(mean2,1)),size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower2,ymax =Adjust_Upper2),width = 0.10)+ #误差图
  
  geom_line(aes(x = xx,y = mean3*3.3, group = 1, color = "Intensity (VPA/total PA), %"),linewidth=1)+ #折线图color="#E94243"
  geom_text(aes(x = xx,y = mean3*3.3,label=round(mean3,1)),hjust=1.8,size=3.5)+
  geom_errorbar(aes(x = Group, ymin = Adjust_Lower3*3.3, ymax = Adjust_Upper3*3.3), 
                width = 0.1, position = position_dodge(width = 0.6)) + #分组误差
  labs(x="PA patterns",
       title="Female")+
  # 左侧y轴标签
  scale_y_continuous(
    name = "Minutes",
    sec.axis = sec_axis(~ ./3.3, name = "Percent(%)")
  ) +
  theme_minimal() +
  labs(color = "Group") + 
  #可以自己设置颜色
  scale_fill_manual(labels=c("Total PA of session, min","Duration of session, min"),values =c("#DEEBF7","#9DC3E6"))+ 
  scale_color_manual(labels="Intensity (VPA/total PA), %",values = "#E94243" )+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())
#+theme(legend.background = element_rect(fill = "white",colour = "black"))



#Male
dat2 <- data11male %>% 
  #dplyr::group_by(Gender,WWorRA) %>% 
  dplyr::group_by(WWorRA) %>% 
  dplyr::summarise(mean1=mean(PA_of_session,na.rm = T),sd1 = sd(PA_of_session,na.rm=T),se1=sd1/sqrt(length(PA_of_session)),
                   mean2=mean(duration,na.rm = T),sd2=sd(duration,na.rm=T),se2=sd2/sqrt(length(duration)),
                   mean3=mean(intensity_Percent,na.rm=T),sd3=sd(intensity_Percent,na.rm=T),se3=sd3/sqrt(length(intensity_Percent)),.groups = "drop") %>% 
  mutate(Adjust_Lower1=mean1 - se1,
         Adjust_Upper1=mean1 + se1,
         Adjust_Lower2=mean2 - se2,
         Adjust_Upper2=mean2 + se2,
         Adjust_Lower3=mean3 - se3,
         Adjust_Upper3=mean3 + se3) %>% 
  mutate(xx=ifelse(WWorRA=="inactive",1,
                   ifelse(WWorRA=="WW",2,3))) %>%  
  dplyr::rename(Group=WWorRA)

# 生成分组绘图
ggplot(dat2) +
  #geom_bar(aes(x = Group,y = mean, fill = Gender),position = "dodge", stat = "identity")+ #分组条图
  geom_bar(aes(x=Group,y=mean1,group = 1, fill = "Duration of session, min"),position = "dodge", stat = "identity",width=0.5)+#分组条图,fill="#9DC3E6"
  geom_text(aes(x = Group,y = mean1,label=round(mean1,1)),vjust=-3,size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower1,ymax =Adjust_Upper1),width = 0.10)+ #误差图
  
  geom_bar(aes(x = Group,y = mean2 ,group = 1, fill = "Total PA of session, min"),position = position_dodge(width=-0.9), stat = "identity",width=0.5)+ #分组条图,fill="#DEEBF7"
  geom_text(aes(x = Group,y = mean2/2,label=round(mean2,1)),size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower2,ymax =Adjust_Upper2),width = 0.10)+ #误差图
  
  geom_line(aes(x = xx,y = mean3*3.3, group = 1, color = "Intensity (VPA/total PA), %"),linewidth=1)+ #折线图color="#E94243"
  geom_text(aes(x = xx,y = mean3*3.3,label=round(mean3,1)),hjust=1.8,size=3.5)+
  geom_errorbar(aes(x = Group, ymin = Adjust_Lower3*3.3, ymax = Adjust_Upper3*3.3), 
                width = 0.1, position = position_dodge(width = 0.6)) + #分组误差
  labs(x="PA patterns",
       title="Male")+
  # 左侧y轴标签
  scale_y_continuous(
    name = "Minutes",
    sec.axis = sec_axis(~ ./3.3, name = "Percent(%)")
  ) +
  theme_minimal() +
  labs(color = "Group") + 
  #可以自己设置颜色
  scale_fill_manual(labels=c("Total PA of session, min","Duration of session, min"),values =c("#DEEBF7","#9DC3E6"))+ 
  scale_color_manual(labels="Intensity (VPA/total PA), %",values = "#E94243" )+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())
#+theme(legend.background = element_rect(fill = "white",colour = "black"))



#Age < 50 years
dat3 <- data11age_young %>% 
  #dplyr::group_by(Gender,WWorRA) %>% 
  dplyr::group_by(WWorRA) %>% 
  dplyr::summarise(mean1=mean(PA_of_session,na.rm = T),sd1 = sd(PA_of_session,na.rm=T),se1=sd1/sqrt(length(PA_of_session)),
                   mean2=mean(duration,na.rm = T),sd2=sd(duration,na.rm=T),se2=sd2/sqrt(length(duration)),
                   mean3=mean(intensity_Percent,na.rm=T),sd3=sd(intensity_Percent,na.rm=T),se3=sd3/sqrt(length(intensity_Percent)),.groups = "drop") %>% 
  mutate(Adjust_Lower1=mean1 - se1,
         Adjust_Upper1=mean1 + se1,
         Adjust_Lower2=mean2 - se2,
         Adjust_Upper2=mean2 + se2,
         Adjust_Lower3=mean3 - se3,
         Adjust_Upper3=mean3 + se3) %>% 
  mutate(xx=ifelse(WWorRA=="inactive",1,
                   ifelse(WWorRA=="WW",2,3))) %>%  
  dplyr::rename(Group=WWorRA)

# 生成分组绘图
ggplot(dat3) +
  #geom_bar(aes(x = Group,y = mean, fill = Gender),position = "dodge", stat = "identity")+ #分组条图
  geom_bar(aes(x=Group,y=mean1,group = 1, fill = "Duration of session, min"),position = "dodge", stat = "identity",width=0.5)+#分组条图,fill="#9DC3E6"
  geom_text(aes(x = Group,y = mean1,label=round(mean1,1)),vjust=-3,size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower1,ymax =Adjust_Upper1),width = 0.10)+ #误差图
  
  geom_bar(aes(x = Group,y = mean2 ,group = 1, fill = "Total PA of session, min"),position = position_dodge(width=-0.9), stat = "identity",width=0.5)+ #分组条图,fill="#DEEBF7"
  geom_text(aes(x = Group,y = mean2/2,label=round(mean2,1)),size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower2,ymax =Adjust_Upper2),width = 0.10)+ #误差图
  
  geom_line(aes(x = xx,y = mean3*3.3, group = 1, color = "Intensity (VPA/total PA), %"),linewidth=1)+ #折线图color="#E94243"
  geom_text(aes(x = xx,y = mean3*3.3,label=round(mean3,1)),hjust=1.8,size=3.5)+
  geom_errorbar(aes(x = Group, ymin = Adjust_Lower3*3.3, ymax = Adjust_Upper3*3.3), 
                width = 0.1, position = position_dodge(width = 0.6)) + #分组误差
  labs(x="PA patterns",
       title="Age < 50 years")+
  # 左侧y轴标签
  scale_y_continuous(
    name = "Minutes",
    sec.axis = sec_axis(~ ./3.3, name = "Percent(%)")
  ) +
  theme_minimal() +
  labs(color = "Group") + 
  #可以自己设置颜色
  scale_fill_manual(labels=c("Total PA of session, min","Duration of session, min"),values =c("#DEEBF7","#9DC3E6"))+ 
  scale_color_manual(labels="Intensity (VPA/total PA), %",values = "#E94243" )+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())
#+theme(legend.background = element_rect(fill = "white",colour = "black"))



#Age ≥ 50 years
dat4 <- data11age50 %>% 
  #dplyr::group_by(Gender,WWorRA) %>% 
  dplyr::group_by(WWorRA) %>% 
  dplyr::summarise(mean1=mean(PA_of_session,na.rm = T),sd1 = sd(PA_of_session,na.rm=T),se1=sd1/sqrt(length(PA_of_session)),
                   mean2=mean(duration,na.rm = T),sd2=sd(duration,na.rm=T),se2=sd2/sqrt(length(duration)),
                   mean3=mean(intensity_Percent,na.rm=T),sd3=sd(intensity_Percent,na.rm=T),se3=sd3/sqrt(length(intensity_Percent)),.groups = "drop") %>% 
  mutate(Adjust_Lower1=mean1 - se1,
         Adjust_Upper1=mean1 + se1,
         Adjust_Lower2=mean2 - se2,
         Adjust_Upper2=mean2 + se2,
         Adjust_Lower3=mean3 - se3,
         Adjust_Upper3=mean3 + se3) %>% 
  mutate(xx=ifelse(WWorRA=="inactive",1,
                   ifelse(WWorRA=="WW",2,3))) %>%  
  dplyr::rename(Group=WWorRA)

# 生成分组绘图
ggplot(dat4) +
  #geom_bar(aes(x = Group,y = mean, fill = Gender),position = "dodge", stat = "identity")+ #分组条图
  geom_bar(aes(x=Group,y=mean1,group = 1, fill = "Duration of session, min"),position = "dodge", stat = "identity",width=0.5)+#分组条图,fill="#9DC3E6"
  geom_text(aes(x = Group,y = mean1,label=round(mean1,1)),vjust=-3,size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower1,ymax =Adjust_Upper1),width = 0.10)+ #误差图
  
  geom_bar(aes(x = Group,y = mean2 ,group = 1, fill = "Total PA of session, min"),position = position_dodge(width=-0.9), stat = "identity",width=0.5)+ #分组条图,fill="#DEEBF7"
  geom_text(aes(x = Group,y = mean2/2,label=round(mean2,1)),size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower2,ymax =Adjust_Upper2),width = 0.10)+ #误差图
  
  geom_line(aes(x = xx,y = mean3*3.3, group = 1, color = "Intensity (VPA/total PA), %"),linewidth=1)+ #折线图color="#E94243"
  geom_text(aes(x = xx,y = mean3*3.3,label=round(mean3,1)),hjust=1.8,size=3.5)+
  geom_errorbar(aes(x = Group, ymin = Adjust_Lower3*3.3, ymax = Adjust_Upper3*3.3), 
                width = 0.1, position = position_dodge(width = 0.6)) + #分组误差
  labs(x="PA patterns",
       title="Age ≥ 50 years")+
  # 左侧y轴标签
  scale_y_continuous(
    name = "Minutes",
    sec.axis = sec_axis(~ ./3.3, name = "Percent(%)")
  ) +
  theme_minimal() +
  labs(color = "Group") + 
  #可以自己设置颜色
  scale_fill_manual(labels=c("Total PA of session, min","Duration of session, min"),values =c("#DEEBF7","#9DC3E6"))+ 
  scale_color_manual(labels="Intensity (VPA/total PA), %",values = "#E94243" )+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())
#+theme(legend.background = element_rect(fill = "white",colour = "black"))




#亚组森林图-------
#install.packages("forestplot")
#install.packages("gtsummary")
#install.packages("stringr")
#install.packages("forestploter")

library(readr)
library(forestplot)
library(tableone)
library(survey)
library(dplyr)
library(stringr)
library(tidyr)
library(grid)
library(forestploter)
#colnames(data11)
#colSums(is.na(data11))


#jstable亚组森林图-------
#install.packages("jstable")
library(jstable)



# 使用 TableSubgroupMultiGLM 进行亚组分析
data13<-data11
data13<-rename(data13,
              c(WWorRA="Pattern"))

res <- TableSubgroupMultiGLM(
  formula = L_status ~ Pattern,  
  var_subgroups = c("age50","employment", "Gender", "Education", "Marital",
                    "Smoke", "drink_now"),
  data = data13,  # 指定数据集
  family = "binomial",  # 使用逻辑回归
  decimal.estimate = 2,  # 估计值保留两位小数
  decimal.pvalue = 3  # P 值保留X位小数
)

# 查看分析结果
print(res)

#结果后处理
plot_df <- res
plot_df[,c(2,3,4,8,9)][is.na(plot_df[,c(2,3,4,8,9)])] <- " "

plot_df$` ` <- paste(rep(" ", 10), collapse = " ")
plot_df[,5:7] <- apply(plot_df[,5:7],2,as.numeric)

plot_df$"OR (95% CI)"<-ifelse(is.na(plot_df$"OR"),"",
                                sprintf("%.2f (%.2f to %.2f)",
                                        plot_df$"OR",plot_df$Lower,plot_df$Upper))#计算HR(95%CI)，以便显示在图形中

plot_df <- rename(plot_df,
                 c(Levels="PA Groups"))

plot_df



# 保存结果为 CSV 文件
write.csv(res, 'subgroup_analysis_results.csv', row.names = FALSE)
write.csv(plot_df, 'subgroup_analysis_results2.csv', row.names = FALSE)

plot_df2<-read.csv(file="subgroup_analysis_results2.csv",header = TRUE)
plot_df2[,c(2,3,5,8,9,10)][is.na(plot_df2[,c(2,3,5,8,9,10)])] <- " "

# 定义森林图的主题
tm <- forest_theme(
  base_size = 10,       
  refline_col = "grey",
  refline_lwd = 2,
  ci_col = "#1E9C76",
  refline_lty = "solid",
  arrow_type = "closed",    
  ci_lty = 1,
  ci_Theight = 0.2,
  ci_lwd = 2.3
)

str(plot_df)
# 绘制森林图

p <- forest(
  data=plot_df[,c(1,2,3,4,10,11,8,9)],
  est = as.numeric(plot_df$OR),
  lower = plot_df$Lower,
  upper = plot_df$Upper,
  ci_column = 5,#点估计对应的列
  xlim = c(0.5, 1.5),
  ticks_at = c(0.5, 1.00, 1.50),
  ref_line = 1,
  sizes = 0.8,
  arrow_lab = c("Low Risk", "High Risk"),
  theme = tm
) 
plot(p)

# 保存森林图为 PNG 文件
png("subgroup_forest_plot.png", width = 3000, height = 5000, res = 300)

# 打印森林图到文件中
print(p)

# 关闭 PNG 设备，完成保存
dev.off()