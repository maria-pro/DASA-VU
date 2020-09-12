install.packages("tidyverse")
install.packages("ggplot2")
install.packages("rmarkdown")
install.packages("devtools")

library(devtools)
library(tidyverse)
library(ggplot2)
library(rmarkdown)


# read the script file from Github

source_url("https://github.com/amandajcole/DASA_IV20_DATAVIZ_WORKSHOP/blob/master/script%201.R")

dataset<-mpg

#display the top 10 most efficient car in city
task1<- dataset%>%
  select(manufacturer, model, displ, year, cty)%>%
  slice_max(cty,n= 10)

#display the top 10 most efficient car on the highway
task2<-dataset%>%
  select(manufacturer, model, displ, year, hwy)%>%
  slice_max(hwy, n= 10)

# what brand is most efficient in city? show top 10 in barchart
task3<-dataset%>%
  select(manufacturer,cty)%>%
  group_by(manufacturer)%>%
  summarise(mpg_of_brand=mean(cty),.groups="drop")%>%
  slice_max(mpg_of_brand, n=10)
ggplot(task3, aes(x=reorder(manufacturer,-mpg_of_brand),y=mpg_of_brand,fill=manufacturer))+
  geom_col()

#now let see which class of vehicle is most efficient in the city? I mean we can always get an average of city
#and hwy to get the average mpg or we can convert it into L/100KM to make more Aussie friendly.
#knowing that the smaller car always run better but we want to see how much better? do a barchart as well.

task4<-dataset%>%
    select(class,cty)%>%
    group_by(class)%>%
    summarise(mpg=mean(cty),.groups="drop")%>%
    slice_max(mpg, n=10)
ggplot(task4, aes(x=reorder(class,-mpg),y=mpg,fill=class))+
    geom_col()#can use geom_bar(stat="identity") can flip this as well

#Facet wrap: Now I want to see how each brand perform in different class of vehicle
# First we see how many model does each brand has for each class

task5<-dataset%>%
  select(manufacturer, class, cty)%>%
  group_by(manufacturer, class)%>%
  summarise(number_of_model=n(),mpg=mean(cty),.groups='drop')%>%
  ungroup()
task5%>%
  ggplot(aes(y=reorder(manufacturer,-mpg),x=mpg, fill= manufacturer))+
  geom_col()+
  facet_wrap(~class)

#drivetrain which is most efficient? barchart
task6<-dataset%>%
  group_by(drv)%>%
  summarise(mpg_of_drv=mean(cty),.groups='drop')%>%
  ungroup()
ggplot(task6,aes(x=reorder(drv,-mpg_of_drv),y=mpg_of_drv, fill=drv))+
  geom_col()

#same thing for manual vs auto
#we need to group them for manual and auto only drop the I3 AV BEHIND THEM
task7<-dataset%>%
  mutate(trans2=str_sub(trans,1,-5))%>%
  #we need to let them do it normally first then we'll see that we have 
  #that we have weird things behind each drivetrains so then we'll introduce mutate and show them how
  #how to alter it and then after that, it's like all other task before
  group_by(trans2)%>%
  summarise(mpg=mean(cty),.groups='drop')%>%
  ungroup()
ggplot(task7,aes(y=reorder(trans2,-mpg),x=mpg, fill=trans2))+
  geom_col()

#compare between years 1999 vs 2008
task8<-dataset%>%
  group_by(manufacturer, model, year)%>%
  summarise(mpg=mean(cty),.groups='drop')%>%
  ungroup()%>%
  pivot_wider(names_from = year, values_from=mpg)
dataset%>%
  group_by(manufacturer, model, year)%>%
  summarise(mpg=mean(cty),.groups='drop')%>%
  mutate(name=paste(manufacturer,model))%>%
  ungroup()%>%
  ggplot(aes(x=name,y=mpg, fill=year))+
  geom_col()
#relationship between displacement and fuel efficiency 
dataset%>%
  ggplot(aes(x=displ, y=cty))+
  geom_point()+
  geom_smooth()
  