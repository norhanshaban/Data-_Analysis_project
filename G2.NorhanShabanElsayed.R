
anth_data<-read.csv("G2_anthropometry.csv")
View(anth_data)

#Dataset Sense

nrow(anth_data)#n.row

ncol(anth_data)#n.column
# Column names.
colnames(anth_data)

# Data structure.
str(anth_data)

# Data summary.
summary(anth_data)

#Get first/last 6 rows.
head(anth_data)
tail(anth_data)

#get first/last N rows.
head(anth_data, 25)
tail(anth_data, 25)


#Data cleaning

#1) Re-code the gender feature to Male and Female

anth_data$Sex[anth_data$gender=="F" ]="Female"
anth_data$Sex[anth_data$gender=="cm"]="Male"

#2) show all the rows that have NA

anth_data[!complete.cases(anthro) , ]

#3)Replace each NA in foot_length according to the mean of
#foot_length to all males for males and foot_length to all females for females

m_mean<-mean(anth_data[anth_data$Sex=="Male" , 'foot_length' ], na.rm=T)
anth_data[is.na(anth_data$foot_length) & anth_data$Sex=="Male"  , 'foot_length']=m_mean

f_mean<-mean(anth_data[anth_data$Sex=="Female" , 'foot_length' ], na.rm=T)
anth_data[is.na(anth_data$foot_length) & anth_data$Sex=="Female"  , 'foot_length']=f_mean

#4)Re_code age variable 
anth_data$ageRange[anth_data$age <= 5]="0 _ 5"
anth_data$ageRange[anth_data$age > 5 & anth_data$age <= 10]="6 _ 10"
anth_data$ageRange[anth_data$age >10 ]="11 _ .."

#5)remove the text(cm)in height feature to convert it to numeric to use in the analysis

anth_data$height<-gsub(" cm" ,"" , anth_data$height)
anth_data$height<-as.numeric(anth_data$height)


#6)Re_code the height feature using if else

x<-mean(anth_data$height)

anth_data$height1<-as.factor(ifelse(anth_data$height< x &anth_data$age >10,"Abnormal kid" 
                                   ,"Normal kid"))
#7)Re_code of code

anth_data$height2[anth_data$height1=="Normal kid"]='0'
anth_data$height2[anth_data$height1=="Abnormal kid"]='1'
anth_data$height2<-as.factor(anth_data$height2)

#8)Display The ratio of normal and abnormal child 

a<-mean(anth_data$height2==0)
b<-mean(anth_data$height2==1)
a
b

#9)Subset only males who have foot_length greater than  200

sub1<-anth_data[anth_data$Sex=="Male" & anth_data$foot_length >200 , ]

#10)Subset abnormal childs

sub2<-anth_data[anth_data$height2== 1 , ]
#or
sub2<-anth_data[anth_data$height1=="Abnormal kid" ,]

#11)Subset childs who have foot_length greater than the median and have height 
#greater than or equal to 135  for specific features(age , sex)

sub3<-anth_data[anth_data$foot_length > median(anth_data$foot_length) &
                  anth_data$height >= 135 ,c(1,5) ]

#12)sort the data set ascending according to 2 variables

sorted<-anth_data[order(anth_data$age ,anth_data$height) , ]

#13)Get only the first 30 rows
h<-head(anth_data ,30)

#14)Get only the last 50 rows
t<-tail(anth_data ,50)
#______________________________________________ now the data is ready to visualization

library(ggplot2)

#15)display the effect of the height on foot_length (co_relation) using scatter plot,name the figure

fig1<-ggplot(anth_data , aes(x=foot_length  , y= height))
fig1 + geom_point() + ggtitle("The co_relation between the Height and Foot_length")

#16)display the effect of height on foot_length colored by the groups of age range using scatterplot
fig2<-ggplot(anth_data , aes(foot_length , height))
fig2 + geom_point(aes(color=ageRange)) +stat_smooth(se=FALSE)  


#17)Show the distribution of footlength using histogram,name the figure and rename the x,y
fig3<-ggplot(anth_data , aes(foot_length))
fig3 + geom_histogram(binwidth = 8)
fig3 + geom_histogram(fill = "orange")+ ggtitle("Child's foot length distribution")+labs(x="Foot Length" , y="Frequency")

#18)Show the distribution of Height using histogram ,name the figure
fig4<-ggplot(anth_data , aes(height))
fig4 + geom_histogram(binwidth = 8)
fig4 + geom_histogram(fill = "red")+ ggtitle("Child's Height distribution") 

#19)summarize the heightcat2 0,1 to Sex and ageRange groups using Bar chart

fig5<-ggplot(anth_data , aes(x=height2  ,fill= Sex))
fig5 +geom_bar()+labs(y=" Heightcat count" ,title="Height category rate")
fig5 +geom_bar() +theme_light()+facet_wrap(~ageRange)

















