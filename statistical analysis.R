#package preparation
library(foreign)
library(readr)
library(Hmisc)
library(nhanesA)
library(tidyverse)
library(rms)
library(survey)
library(epiDisplay)
library(pROC)
library(RISCA)
library(tableone)
library(jstable)
library(forestplot)
library(forestploter)
library(grid)
library(knitr)
library(do)
library(VIM)

#PA calculation-------
data09$VPAwork<-data09$day_v_work*data09$min_v_work
data09$VPArecreation<-data09$day_v_recreation*data09$min_v_recreation
data09$MPAwork<-data09$day_m_work*data09$min_m_work
data09$MPArecreation<-data09$day_m_recreation*data09$min_m_recreation
data09$PA<-data09$VPArecreation*2+data09$MPArecreation

for(i in 1:length(data09$day_v_recreation)){ 
  data09$freq[i]<-max(data09$day_m_recreation[i],data09$day_v_recreation[i])
}

data09$WWorRA<-ifelse(data09$PA>=150,ifelse(data09$freq<=2,1,2),3)#1=WW,2=RA,3=inactive
data09$work_related_PA<-data09$VPAwork*2+data09$MPAwork

#T-score calculation-------
data11$T_Lumbar<-(data11$spine_BMD-1.065)/0.122
data11$T_Femur_neck<-(data11$femur_neck_BMD-0.888)/0.121
data11$Lumbar_loss<-ifelse(data11$T_Lumbar+1<0,1,0)
data11$Lumbar_loss <- factor(data11$Lumbar_loss,levels = c(0,1),labels = c("normal","low_bone_mass"))
data11$Femur_neck_loss<-ifelse(data11$T_Femur_neck+1<0,1,0)
data11$Femur_neck_loss <- factor(data11$Femur_neck_loss,levels = c(0,1),labels = c("normal","low_bone_mass"))
data11 <- rename(data11,
                 c(Lumbar_loss="Lloss",
                   Femur_neck_loss="Fnloss")
)

#duration of session & intensity------
data11$PA_of_session<-ifelse(data11$freq==0,0,data11$PA/data11$freq)
data11$intensity<-ifelse(data11$PA==0,0,data11$VPArecreation*2/data11$PA)
for(i in 1:length(data11$min_v_recreation)){ 
  data11$duration[i]<-max(data11$min_m_recreation[i],data11$min_v_recreation[i])
}

#generate descriptive table-------
status_tab <- descrTable(WWorRA ~ Age + Gender + Race + Year +
                           Education + Marital + employment + PIR_new + Smoke + drink_now + work_related_PA + spine_BMD + femur_BMD, 
                         show.all=T,
                         data = data11) 
print(status_tab)
export2word(status_tab, file='table1.docx')

#survey weighted---------
library(survey)
data11$WT<-data11$WTMEC2YR/3
study_design <- svydesign(
  data = data11, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)

#statistical description--------
#weighted proportion of overall(categorical variables)
#if NA exists
data13<-data11[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","WT","WWorRA","drink_now")]
data13<-subset(data13,!is.na(data13$drink_now))
study_design1 <- svydesign(
  data = data13, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)
temp <- data.frame(svytotal(~drink_now,study_design1))
sprintf("%0.1f",temp$total/sum(temp$total)*100)

#weighted proportion of different PA groups
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

#Weighted Chi-Square Test
svychisq(~Gender+depress,study_design,statistic = c("Chisq"))
svychisq(~PIR_new+WWorRA,study_design,statistic = c("Chisq"))
svychisq(~Year+WWorRA,study_design,statistic = c("Chisq"))
svychisq(~drink_now+WWorRA,study_design,statistic = c("Chisq"))
svychisq(~Marital+WWorRA,study_design,statistic = c("Chisq"))

#weighted mean and 95% CI of overall(continuous variables)
data12<-data11
data12$help<-1
study_design2 <- svydesign(
  data = data12, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)
svyby(~Age, ~help,study_design2,svymean,na.rm = T,vartype="ci")

#weighted mean and 95% CI of PA groups
svyby(~Age, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~work_related_PA, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~spine_BMD, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~femur_neck_BMD, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~T_Lumbar, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )
svyby(~T_Femur_neck, ~WWorRA, study_design, svymean,na.rm = T,vartype="ci" )

#weighted anova
survey::regTermTest(survey::svyglm(Age~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(work_related_PA~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(T_Lumbar~WWorRA,study_design),"WWorRA")#p=0.005
survey::regTermTest(survey::svyglm(T_Femur_neck~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(femur_neck_BMD~WWorRA,study_design),"WWorRA")
survey::regTermTest(survey::svyglm(spine_BMD~WWorRA,study_design),"WWorRA")


#linear regression(weighted)------ 

#unadjusted model
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
mod_mult<-svyglm(spine_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,study_design,family="gaussian")
summary(mod_mult)
mod_mult<-svyglm(femur_neck_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,study_design,family="gaussian")
summary(mod_mult)

##RA as reference
data11not_inactive<-subset(data11,data11$WWorRA!="inactive")
data11not_inactive$duration_value<-ifelse(data11not_inactive$WWorRA=="RA",1,ifelse(data11not_inactive$duration<=30,2,ifelse(data11not_inactive$duration>90,4,3)))
data11not_inactive$duration_value <- factor(data11not_inactive$duration_value,c(1,2,3,4),
                                            labels = c("RA",
                                                       "WW&<=30",
                                                       "WW&>30<=90",
                                                       "WW&>90"))
data11not_inactive$intensity_value<-ifelse(data11not_inactive$WWorRA=="RA",1,ifelse(data11not_inactive$intensity==0,2,ifelse(data11not_inactive$intensity<=0.50,3,ifelse(data11not_inactive$intensity<1.00,4,5))))
data11not_inactive$intensity_value<- factor(data11not_inactive$intensity_value,c(1,2,3,4,5),
                                            labels = c("RA",
                                                       " 0 ",
                                                       " >0 to ≤50 ",
                                                       " >50 to <100 ",
                                                       " 100 "))
data11not_inactive$freq_value<- ifelse(data11not_inactive$WWorRA=="RA",1,ifelse(data11not_inactive$freq==1,2,3))
data11not_inactive$freq_value<- factor(data11not_inactive$freq_value,c(1,2,3),
                                       labels=c("RA","1","2"))

#WW
multi_mod<-svyglm(spine_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)
multi_mod<-svyglm(femur_neck_BMD~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)

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

#frequency
multi_mod<-svyglm(spine_BMD~freq_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)
multi_mod<-svyglm(femur_neck_BMD~freq_value+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design4,family="gaussian")
summary(multi_mod)

#logistic regression(weighted)------
#unadjusted
fit1<-svyglm(L_status~WWorRA,study_design2,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA,study_design2,family=quasibinomial)
summary(fit2)

#adjusted
fit1<-svyglm(L_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design,family=quasibinomial)
summary(fit1)
fit2<-svyglm(Fn_status~WWorRA+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA+PA,study_design,family=quasibinomial)
summary(fit2)

#show OR
ShowRegTable(fit1)
ShowRegTable(fit2)

#subgroup analysis------
#sex subgroup
data11female<-subset(data11,data11$Gender=="Female")
data11male<-subset(data11,data11$Gender=="Male")
study_design3 <- svydesign(
  data = data11female, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)
study_design4 <- svydesign(
  data = data11male, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WT,
  survey.lonely.psu="adjust"
)
#age subgroup
data11age50<-subset(data11,data11$Age>=50)
data11age_young<-subset(data11,data11$Age<50)

#smoke subgroup
data11smoke_never<-subset(data11,data11$Smoke=="Never")
data11smoke_current<-subset(data11,data11$Smoke=="Current")
data11smoke_former<-subset(data11,data11$Smoke=="Former")

#drink subgroup
data11not_drink<-subset(data11,data11$drink_now=="No")
data11drink<-subset(data11,data11$drink_now=="Yes")

#employment subgroup
data11employed<-subset(data11,data11$employment=="Employed")
data11unemployed<-subset(data11,data11$employment=="Unemployed")


#Baseline PA data visualization------
#use female subgroup as example
dat1 <- data11female %>% 
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

ggplot(dat1) +
  geom_bar(aes(x=Group,y=mean1,group = 1, fill = "Duration of session, min"),position = "dodge", stat = "identity",width=0.5)+#分组条图,fill="#9DC3E6"
  geom_text(aes(x = Group,y = mean1,label=round(mean1,1)),vjust=-3,size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower1,ymax =Adjust_Upper1),width = 0.10)+ #误差图
  
  geom_bar(aes(x = Group,y = mean2 ,group = 1, fill = "Total PA of session, min"),position = position_dodge(width=-0.9), stat = "identity",width=0.5)+
  geom_text(aes(x = Group,y = mean2/2,label=round(mean2,1)),size=3.5,fontface='bold')+
  geom_errorbar(aes(x = xx,ymin = Adjust_Lower2,ymax =Adjust_Upper2),width = 0.10)+
  
  geom_line(aes(x = xx,y = mean3*3.3, group = 1, color = "Intensity (VPA/total PA), %"),linewidth=1)+
  geom_text(aes(x = xx,y = mean3*3.3,label=round(mean3,1)),hjust=1.8,size=3.5)+
  geom_errorbar(aes(x = Group, ymin = Adjust_Lower3*3.3, ymax = Adjust_Upper3*3.3), 
                width = 0.1, position = position_dodge(width = 0.6)) + 
  labs(x="PA patterns",
       title="Female")+
  scale_y_continuous(
    name = "Minutes",
    sec.axis = sec_axis(~ ./3.3, name = "Percent(%)")
  ) +
  theme_minimal() +
  labs(color = "Group") + 
  scale_fill_manual(labels=c("Total PA of session, min","Duration of session, min"),values =c("#DEEBF7","#9DC3E6"))+ 
  scale_color_manual(labels="Intensity (VPA/total PA), %",values = "#E94243" )+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())


#RCS analysis(weighted and adjusted)------
#overall
fitc <- svyglm(Lloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design)
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Gender+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design)
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#sex subgroup(the same for other subgroups)
fitc <- svyglm(Lloss ~ rcs(PA, 4)+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design3)
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design3)
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

fitc <- svyglm(Lloss ~ rcs(PA, 4)+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design4)
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))
fitc <- svyglm(Fnloss ~ rcs(PA, 4)+Year+Age+Race+Education+Marital+drink_now+Smoke+employment+PIR_new+work_related_PA,family ="quasibinomial",  design =study_design4)
plot_fitc <- NHS_rcs(fitc,reference = "median", pvalue_position=c(0.05,0.85))

#ROC/AUC-------
library("pROC")
roc1<-roc(Lloss~PA,data=data11)
roc2<-roc(Fnloss~PA,data=data11)
roc3<-roc(Lloss~PA,data=data11age50)
roc4<-roc(Fnloss~PA,data=data11age50)
roc5<-roc(Lloss~PA,data=data11age_young)
roc6<-roc(Fnloss~PA,data=data11age_young)
roc7<-roc(Lloss~PA,data=data11female)
roc8<-roc(Fnloss~PA,data=data11female)
roc9<-roc(Lloss~PA,data=data11male)
roc10<-roc(Fnloss~PA,data=data11male)

#draw ROC plot(the same for subgroups）
plot(roc1, 
     col = "red",  
     legacy.axes = TRUE,  
     main = "Lumbar BMD decrease",  
     print.auc = TRUE, 
     auc.polygon = TRUE, 
     auc.polygon.col = "#fff7f7",  
     print.thres = TRUE) 
legend( 0.4, 0.4, 
        lwd = 2, legend = "employed", col = "red") 

#combined ROC plot(the same for other subgroups）
g2 <- ggroc(list("total"=roc1, "age>=50"=roc3, "age<50"=roc5,"female"=roc7,"male"=roc9),legacy.axes = TRUE)
g2  + ggtitle("Lumbar BMD decrease")+ xlab("1-specificity") + ylab("sensitivity") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")

#stratification analysis and forest plot-------
data13<-data11
data13<-rename(data13,
               c(WWorRA="Pattern"))

res <- TableSubgroupMultiGLM(
  formula = Fn_status ~ Pattern,  
  var_subgroups = c("age50","employment", "Gender", "Education", "Marital",
                    "Smoke", "drink_now"),
  data = data13, 
  family = "binomial", 
  decimal.estimate = 2, 
  decimal.pvalue = 3  
)

plot_df <- res
plot_df[,c(2,3,4,8,9)][is.na(plot_df[,c(2,3,4,8,9)])] <- " "
plot_df$` ` <- paste(rep(" ", 10), collapse = " ")
plot_df[,5:7] <- apply(plot_df[,5:7],2,as.numeric)
plot_df$"OR (95% CI)"<-ifelse(is.na(plot_df$"OR"),"",
                              sprintf("%.2f (%.2f to %.2f)",
                                      plot_df$"OR",plot_df$Lower,plot_df$Upper))
plot_df <- rename(plot_df,
                  c(Levels="PA Groups"))

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

p <- forest(
  data=plot_df[,c(1,2,3,4,10,11,8,9)],
  est = as.numeric(plot_df$OR),
  lower = plot_df$Lower,
  upper = plot_df$Upper,
  ci_column = 5,
  xlim = c(0.5, 1.5),
  ticks_at = c(0.5, 1.00, 1.50),
  ref_line = 1,
  sizes = 0.8,
  arrow_lab = c("Low Risk", "High Risk"),
  theme = tm
) 
plot(p)