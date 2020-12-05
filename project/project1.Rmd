library(tidyverse)
library (carData)
library(fivethirtyeight)
library(ggplot2)

#My first dataset I chose was the amount of Murder, Rape, and Assault Arrests made in each state along with the percent urban population each state's population makes up. 
#My second dataset provides data regarding fatal collisions in each state and the percent of those drivers that were alcohol-impaired, speeding, not distracted, not involved in previous accidents, and number of drivers involved in fatal collisions. It also provided data regarding car insurance premiums and losses those companies incurred due to accidents. All of this data was found across each state. 
#The data regarding fatal collisions was gathered from historic data and insurance company information, while the arrest data was gathered form the McNeil monograph and from the Statistical Abstracts from 1975. Since this data is from 1975, it might not be reflective of modern society statistics as many societal changes have incurred over time. 
#I liked these datasets because it is interesting to see the various statistics they have about Arrests in each state and how they relate to each other. Does one state have on average more rape arrests only or do they have more in all three categories?
#I also like the bad_drivers dataset because you only really hear about drunk driving on the news so that is how you think about most fatal collisions. It is interesting to explore the different reasons behind fatal collisions to see how they compare against each other. 


#since my dataset was already tidy, I made it untidy with pivot_wider by pulling the values from 'num_driver' to make those the new columns, and put their corresponding values from 'perc_speeding' in their columns
bad_drivers <- bad_drivers
BDW <- bad_drivers %>% pivot_wider(names_from="num_drivers", values_from="perc_speeding")
head(BDW)

#to re-tidy my data, I used pivot_longer to retake those columns 
#and put them into 1 column with its values representing the percent of drivers
#involved in fatal accidents who were speeding. 
#I then removed the NAs from that column to condense the dataset 
#so that there weren't rows of the same state with empty values
baddrive_fixed <- BDW %>% pivot_longer(7:51, names_to="amt_drivers", values_to="percent_speeding")
baddrive_fixed <- baddrive_fixed %>%filter(!is.na(percent_speeding))
head(baddrive_fixed)

#now I converted my amt_drivers column from a character column, into a numeric column 
baddrive_fixed$amt_drivers <- as.numeric(as.character(baddrive_fixed$amt_drivers))


#here, the original data set didn't have a state column, but rather had the row names 
#as states, so here I converted the state row names into a column entitled 'state'
USArrests <- USArrests
d  <- USArrests %>% rownames_to_column("state")


#now, I untidyed the dataset with pivot_wider and then retidyed the dataset using pivot_longer, and removed all the duplicate rows w NAs
US_wide <- d %>% pivot_wider(names_from="state", values_from="Rape")
dim(US_wide)
USA_fixed <- US_wide %>% pivot_longer(4:53, names_to="state", values_to="Rape") %>% filter(!is.na(Rape))
USA_fixed <- USA_fixed %>% select(state,everything())
#at the end of my tidying, I just moved the state column from the end to the first column in the dataset 

#I did an inner join, joining both of my datasets on the common ID variable 'state'
full <- inner_join(USA_fixed,baddrive_fixed)
#the USA_fixed dataset has 50 observations while the baddrive_fixed dataset has 51 observations, because it included District of Colombia. 
#1 observation was dropped since I did an inner join and the District of Colombia did not have corresponding data in the USA_fixed dataset
#I chose to do an inner join because the state column matched pretty well in both datasets, other than the fact that baddrive_fixed contained an observation for District of Colombia and USA_fixed did not 
#inner join then dropped that observation since it did not have corresponding values in the USA_fixed dataset, so I just dropped that observation as a whole so we could just look at the data for the 50 states
#since the only observation that was dropped was that of the District of Colombia, I don't foresee this skewing my data because we still have a relatively large dataset


full %>% select(state, Rape) %>% group_by(state) %>% filter(Rape>30) %>% arrange(-Rape)
#here I used the select function to only look at the state and the Rape columns so that we can focus on the number of arrests that were due to rape for each state
#I then grouped the dataset by state so that if there were multiple observations concerning the same state they would be grouped together
#then, I used the filter function to only look at the those states with number of rape arrests that were greater than 30 per 100,000 people 
#then I arranged the rape column in ascending order, from those states with the highest number of rape arrests to those with the lowest number of rape arrests

full_mutate <- full %>% mutate(net_ins_profit = insurance_premiums-losses)
#here, I added a new column indicating the net profit insurance companies made per state from their premiums minus the amount they lost due to car accidents 

full_mutate %>% summarize_at(c("Rape","insurance_premiums","percent_speeding"), mean, na.rm=T)
#here, I looked at the columns indicating the number of arrests that were rape arrests, the insurance premiums, and the percent of drivers involved in fatal accidents that were speeding,and I got the average values across all 50 states for these 3 categories


murderstat <- cut((full_mutate$Murder), breaks = c(0, 10, 20), 
              labels = c("low", "high"))
#here, I created a new column indicating whether each state's murder arrest value was high or low, defining high as any value above 10

full_mutate <- full_mutate %>% mutate(murderstat = murderstat)
full_mutate <- full_mutate %>% mutate (pop_size = ifelse(UrbanPop<50, "small", "large"))
#here, I added the high/low indication of murder arrest statistics to my dataset and added another categorical column indicating whether the percent of the population that was urban was a small percent or a large percent--larger percents indicating larger urban populations 

full_mutate %>% group_by(murderstat) %>% summarize(max(Rape), min(Rape))
#here I grouped my data to look at the groups with high murder stats and low murder stats, and looked at the max and minimum amount of Rape arrests made in each group

full_mutate %>% select(Murder:net_ins_profit) %>% summarize_all(sd)
#here I calculated the standard deviations for each of my numeric columns 

full_mutate %>% group_by(pop_size) %>% summarize(number_of_states = n(),not_distract_var = var(perc_not_distracted))
#here, I looked at the two groups of urban population size and calculated the number of states within each group and the variance within each group of the percentage of inidividuals involved in fatal collisions who were not distracted
#We find that amongst the 42 states with large urban populations, on average, the variance is 117 for those with large urban populations and 926 for those with small urban populations
#Since the small urban population subset only consists of 8 states, this is a rather small sample size allowing for the possibility of extreme values to skew the data which could be why the variance value is much larger than that of the large dataset

full_mutate %>% group_by(pop_size, murderstat) %>% summarize(mean(amt_drivers))
#here we see that of those states with a small percentage of urban populations, the average amount of drivers involved in fatal collisions per billion miles are approximately the same across their murder statistics, while of those states with larger urban population percents, there is a small difference, with more drivers involved in fatal collisions in those states with higher murder arrest statistics.
#Once again, we need to consider our sample size in our small urban population group which could account for the lack of strong distinction.

library(kableExtra)
options(knitr.table.format = "html")

kable_data <- full_mutate %>% group_by(pop_size) %>% summarize(number_of_states = n(), losses_var = var(losses), max(Murder), min(Murder))
kable_data_states <- full_mutate %>% group_by(murderstat) %>% summarize(max(Rape), max(Assault), max(UrbanPop))

kable_data %>% kbl() %>% kable_styling()
kable_data_states %>% kbl() %>% kable_styling()
#Here I have combined some of my summary statistics into a clean table to see
#The first table the summary stats for the subgroup of large vs. small urban population size
  #it shows the number of states in each, along with variance among the losses incurred by car insurance companies due to car accidents, and the maximum and minimum murder arrest for the subgroups
  #from this table we can that a majority of the states have a large percent urban population and that compared to the states with smaller urban population percents, the murder arrest values are fairly similar, but a large difference in variation across insurance company losses, probably due to the difference in subset group size.  
#The second table shows summary stats for the subgroup of high vs. low percentage of murder arrest stats
  #it shows that of those states with high murder arrests, the max amount of Rape arrests was about 44, the max amount of assault arrests was 294, and the max percent of the population that was urban was 94%.
  #This table shows that those states with high murder arrests also higher Rape and Assault arrests, but lower percent urban population.

full_mutate%>%select_if(is.numeric)%>%cor%>%as.data.frame%>%
  rownames_to_column%>%pivot_longer(-1)%>%
  ggplot(aes(rowname,name,fill=value))+geom_tile()+
  geom_text(size=2.5, aes(label=round(value,2)))+
  xlab("")+ylab("")+coord_fixed()+
  scale_fill_gradient2(low="white",mid="pink",high="red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
#correlation matrix

ggplot(full_mutate, aes(x=UrbanPop, y=Rape, fill= pop_size))+
  geom_point(aes(color=pop_size, size=pop_size))+
  scale_size_discrete("pop_size",range=c(4,2))+
  labs(x= "Percent Urban Population", y= "Number of Rape Arrests (per 100,000 people)",
       title="Rape Arrests vs. Percent Urban Population")+
  scale_x_continuous(labels = function(x) paste0(x * 1, '%'))+
  scale_x_continuous(breaks = seq(0, 100, 10), limits= c(0,100))
#In this plot we see the relationship between urban populations and the number of rape arrests per 100,000 people. As the percentage of a population's urban population goes up, we see an increase in the number of rape arrests.
#We see a steep rather than gradual increase in rape arrests as we increase in the percent of a population's urban sector. This could lead us to hypothesize a relationship between urban population and rape arrests. The points in this plot are colored based off of large or small urban population percentage to show the relative sample of size of each, and how each contributes to the correlation.

 ggplot(full_mutate, aes(state))+
  geom_bar(aes(y=net_ins_profit, fill=murderstat), colour="black",
           stat="summary", fun=mean)+
  theme(axis.text.y = element_text(angle=0, hjust=1, size=5))+
  coord_flip()+
  scale_fill_brewer(palette = "Purples")+
  labs(y= "Net Insurance Profit", x= "State", title= "State vs. Net Insurance Profit")+
  labs(fill= "Murder Arrests Stats")
#In this plot we are looking at the net profit insurance companies made per state after deducting their losses due to auto collisions from their car insurance premiums. The graph is also colored based on whether or not certain states had higher or lower murder arrests. The graph shows that there isn't really a correlatin on whether mostly high/low murder arrests had higher or lower net profits. We can tell that New Jersay is a state with low murder arrests and their car insurance companies have a higher net insurance profit. 
#Ohio is also a state with low murder arrests, but their insurance companies have lower net profits. Also, by looking at the coloring, we can see there is a descrepency in the amount of bars that are colored purple vs. white, indicating and unequal sample for each group. For further research, I would suggest splitting the data into half so that you are looking at groups of 25 and 25 to eliminate the confounding variable of unequal comparison groups. 


#clustering
clust_dat<-full_mutate%>% select(perc_alcohol,perc_no_previous,percent_speeding)

library(cluster)
sil_width<-vector() #empty vector to hold mean sil width
for(i in 2:10){  
  kms <- kmeans(clust_dat,centers=i) 
  sil <- silhouette(kms$cluster,dist(clust_dat)) 
  sil_width[i]<-mean(sil[,3]) 
}
ggplot()+geom_line(aes(x=1:10,y=sil_width))+scale_x_continuous(name="k",breaks=1:10)
#On the basis of just these three variables, the graph of average silhouette widths shows that 2 clusters is the best (the highest peak!).


kmeans1 <- clust_dat %>% kmeans(2) 
kmeans1

kmeansclust <- clust_dat %>% mutate(cluster=as.factor(kmeans1$cluster))
kmeansclust %>% ggplot(aes(x= perc_alcohol, y= percent_speeding,color=cluster)) + geom_point()
#This graph of our clusters shows that the clusters are relatively distinct, but close togeter so the datapoints within the clusters are relatively close together.
#But within each cluster, we see that the clusters are very spread apart. This shows there is a lot of spread within our clusters, and that there is probably greater distance between the average datapoint and other points within each cluster. 


library(GGally)
ggpairs(kmeansclust, columns=1:4, aes(color=cluster))
#This graph shows how each variable correlates with each other. We can see all the correlation values around 0.2 or 0 yielding a pretty weak correlation for any two variables.
#When looking at percent of drivers who got into fatal collisions who had not had previous accidents and those who were alcohol-impaired, we see a negative correlation, meaning that as the percent of drivers involved in fatal accidents that were alcohol-impaired increase, the percent of drivers involved in fatal collisions but not previously involved in any previous accidents, decreases. 
#Although we see this negative correlation, the correlation is relatively weak. 

library(cluster)
pam1 <- clust_dat %>% pam(k=2)
pam1  




pamclust<-clust_dat %>% mutate(cluster=as.factor(pam1$clustering)) 
pamclust %>% ggplot(aes(perc_alcohol,perc_no_previous,percent_speeding,color=cluster)) + geom_point()
#with PAM, we see that our 2 clusters overlap and that there isn't really two distinct clusters. 
#This yields me to believe that the two clusters contain relatively similar data and it would be difficult to establish them as two distinct groupings based on these variables.



pamclust %>% group_by(cluster) %>% summarize_if(is.numeric,mean,na.rm=T)
#Here we have the means for each cluster based on each variable. As we can see our cluster means for all three variables are very similar, which is probably what yielded our indistinctive cluster graph.


plot(pam1,which=2)
#Here we have our average silhouette width from PAM, which is 0.36. This indicates that our structure is weak and could be artificial. This means our clusters based on these variables are not reliable. 




