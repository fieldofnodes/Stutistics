#Plots
association_df <- df %>% select("Citations","Density","Age","Level.of.Evidence")
#Create a scatter plot of Age and Density relationships

#First scatter plot: Number of citations over time and level of evidence
ppp <- association_df %>% ggplot(aes(x = Age, y = Citations))
ppp + geom_point(aes(colour = Level.of.Evidence), size = 2) +labs(tag = "Plot of Association", title = "Number of citations over time", subtitle = "Grouped by the levels of evidence presented", x= "Time since article was published", y= "Number of citations", caption ="For levels of evidence labelled V, we \nsee that age does not severley affect \nhow often it is cited. This is true too \nof an evidence level of IV. Note that \nthe lines of best fit have been extended \nto the range of the data in totality.") + guides(colour=guide_legend(title="Levels of evidence"))+ geom_smooth(method = "lm", aes(colour=Level.of.Evidence), se = FALSE)+geom_rug(aes(colour=Level.of.Evidence))

#Second scatter plot density over time and levels of evidence
pp <- association_df %>% ggplot(aes(x = Age, y = Density))+ylim(-500,2500)
pp + geom_point(aes(colour = Level.of.Evidence), size = 2) +labs(tag = "Plot of Association", title = "Density over time", subtitle = "Grouped by the levels of evidence presented", x= "Time since article was published", y= "Density", caption = "An important note is that since this data is related to \nactually occurences we can not obtain a negative \ndensity. We can consider the density to have \napproached zero. ") + guides(colour=guide_legend(title="Levels of evidence"))+ geom_smooth(method = "lm", aes(colour=Level.of.Evidence), se = FALSE, fullrange = TRUE)+geom_rug(aes(colour=Level.of.Evidence))

#Third scatter plot levels of evidence over age
p <- association_df %>% ggplot(aes(x = Level.of.Evidence, y = Citations, colour = Age))
p+geom_violin(colour = "orange")+ stat_summary(fun.y=median, geom="point", size=2, color="red")+ stat_summary(fun.data = median_hilow, mult=1, geom="pointrange", color="red")+geom_jitter(aes(colour = Age))+labs(tag = "Plot of Association", title = "Levels of Evidence with respect number of citations", subtitle = "Density dots show age of publication", x= "Levels of evidence", y= "Citations", caption = "The fatter sections of the violin plot shows clusters of citations. \nThe points are shaded for the age of the publication.") + guides(colour=guide_legend(title="Age"))

#Fourth scatter plot levels of evidence over age
p <- association_df %>% ggplot(aes(x = Level.of.Evidence, y = Age, colour = Citations))
p+geom_violin(colour = "orange")+ stat_summary(fun.y=median, geom="point", size=2, color="red")+ stat_summary(fun.data = median_hilow, mult=1, geom="pointrange", color="red")+geom_jitter(aes(colour = Citations), size = 2)+labs(tag = "Plot of Association", title = "Levels of Evidence with respect to age", subtitle = "Density dots show number of citations", x= "Levels of evidence", y= "Age", caption = "The fatter sections of the violin plot shows clusters of articles in certain ages. \nThe points are shaded for the number of citations.") + guides(colour=guide_legend(title="Citations"))
