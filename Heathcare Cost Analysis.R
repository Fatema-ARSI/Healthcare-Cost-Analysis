library(readxl)

#importing data sets
df <- read_excel("1555054100_hospitalcosts(1).xlsx")
head(df)

summary(df)

attach(df)

#insight-1
hist(AGE)

#to see the value of category of infants
age<-as.factor(AGE)
summary(age)

#age category of 0 seems to be frequently using the hospital
tapply(TOTCHG,AGE,sum)
which.max(tapply(TOTCHG,AGE,sum))

# patients with the age of 0, 15 and 17 has the maximum expenditure of 678118,111747 and 174777 respectively.

#insight-2
diagg<-as.factor(APRDRG)
summary(diagg)
which.max(summary(diagg))
tapply(TOTCHG,diagg,sum)
which.max(tapply(TOTCHG,diagg,sum))
max(tapply(TOTCHG,diagg,sum))

#From the results we can see that the category 640 has the maximum entries of hospitalization
#and also has the highest total expenditure i.e 437978

#insight_3
#h0:The race of the patient is related to the hospitalization costs.
#ha:The race of the patient is not related to the hospitalization costs.

race=as.factor(RACE)
summary(race)

#drop NA values from data set
df_na<-na.omit(df)
modelannova<-aov(TOTCHG~RACE)
summary(modelannova)

#p-value comes out to be very high 68.6%, which means we can take the risk and reject the null hypothesis.
#this indicates that there is no relation between the race of patient and the hospital cost.

#insight_4
model_1<-lm(TOTCHG~AGE+FEMALE)
summary(model_1)

#pvalue for age is very less which means it is an important factor in the hospital costs as seen by the significance level and p-value.
#gender also has less p-value indicating it  has the impact on the hospital cost.

#insight_5
model_2<-lm(LOS~AGE+FEMALE+RACE)
summary(model_2)
#high p-value signifies that there is no linear relation between the given variables.
#Hence,it is not possible to predict the Length Of Stay(LOS) of the patient with just age,gender and race.

#insight_6
model_3<-lm(TOTCHG~ .,data=df)
summary(model_3)

#We can see that age and length of stay affects the total hospital cost.