##main R script to compute project

#Import packages possibly needed for project
source("src/00_Packages.R")


#Import data
source("src/01_data_import.R")
#data is titled as df
s <- summary(df)
variables <- colnames(df)
variables

#subsetting main data for plots.
association_df <- df %>% select("Citations","Density","Age","Level.of.Evidence")
table1_df <- df %>% select("Journal.Full.Name","Citations","Level.of.Evidence","Quartile.in.Category","Journal.Publisher.Country")
table2_df <- df %>% select("First.Author","Last.Author", "Citations", "Level.of.Evidence","Title")
#Shapiro-Wilk test on columns as formed in df
normalitySW<- association_df %>% 
  group_by(Level.of.Evidence) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))


#Kruskall-Wallis test
lapply(association_df[,c("Citations", "Density", "Age")], function(x) kruskal.test(x ~ association_df$Level.of.Evidence))



#Correlation coeficient
gp = group_by(association_df, Level.of.Evidence)
correlCit = dplyr::summarise(gp, correlCit = cor(Citations, Age))
print(correlCit)
correlDen = dplyr::summarise(gp, correlDen = cor(Density, Age))
print(correlDen)


#Kendell Mann test and rank
MannKendall(association_df$Citations)
MK = group_by(association_df, Level.of.Evidence)
MKCit = dplyr::summarise(MK,MannKendall(Citations))
print(MKCit)

filter(table1_df, Citations)
arrange(First.Author,Citations,Density,Age)

?arrange

author <- table2_df %>% group_by(First.Author) %>% filter(Citations > 10000)

filter(author, Citations > 10000)
sumCitations <- table1_df %>% group_by(Journal.Full.Name, Level.of.Evidence) %>% summarise(Citations = sum(Citations))
medianCitations <- table1_df %>% group_by(Journal.Full.Name, Level.of.Evidence) %>% summarise(Citations = median(Citations))
df2 <-cbind(c(sumCitations,medianCitations[,3]))
write.csv(sumCitations, file = "data/JournalCitationsLevel.csv")
write.csv(medianCitations, file = "data/JournalCitationsLevelMedian.csv")
write.csv(author, file = "data/author.csv")



