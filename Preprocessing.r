#Table for Age,Gender and Educational Qualification
data=read.csv("Sleep Patterns and Academic Performance.csv")
dim(data)
#Table for Gender
count_male=sum(data$Gender=='Male')
count_female=sum(data$Gender=='Female')
count_others=sum(data$Gender=='Others')
count_pnts=sum(data$Gender=="Prefer not to say")
per_male=(count_male/152)*100
per_female=(count_female/152)*100
per_others=(count_others/152)*100
per_pnts=(count_pnts/152)*100
gender_summary = data.frame(
  Gender = c('Male','Female','Others','Prefer Not to Say'),
  Count = c(count_male,count_female,count_others,count_pnts),
  Percentage = c(per_male,per_female,per_others,per_pnts)
)
print(gender_summary)
#Table for Age
count_18_22 = sum(data$Age == '18-22')
count_23_26 = sum(data$Age == '23-26')
count_15_17 = sum(data$Age == '15-17')
count_26 = sum(data$Age =='Above 26')
per_18_22 = (count_18_22 / 152) * 100
per_23_26 = (count_23_26 / 152) * 100
per_15_17 = (count_15_17 / 152) * 100
per_26 = (count_26/152)*100
age_summary = data.frame(
  Age = c('15-17','18-22', '23-26','26 and above' ),
  Count = c(count_15_17,count_18_22, count_23_26,count_26 ),
  Percentage = c(per_15_17,per_18_22, per_23_26,per_26 )
)
print(age_summary)
#Table for Educationol Qualification
count_undergraduate = sum(data$Degree == 'Undergraduate')
count_postgraduate = sum(data$Degree == 'Postgraduate')
count_high_school <- sum(data$Degree == 'High school')
count_phd = sum(data$degree=='PhD')
per_undergraduate = (count_undergraduate / 152) * 100
per_postgraduate = (count_postgraduate / 152) * 100
per_high_school = (count_high_school / 152) * 100
per_phd=(count_phd/152)*100
degree_summary = data.frame(
  Degree = c('High school','Undergraduate', 'Postgraduate','PhD' ),
  Count = c( count_high_school,count_undergraduate, count_postgraduate,count_phd),
  Percentage = c(per_high_school,per_undergraduate, per_postgraduate,per_phd)
)

print(degree_summary)
      
