#read the data into an object

Oji_1996<-read.csv("~/Repos/Statistics_Data/20160524_Oji_1996_Paleobiology.csv", header=T, sep=",")

#histograms of the variables without taking into account the phenotype of the organism

hist(Oji_1996$Depth)
hist(Oji_1996$Regenerated_arms)

#this histogram makes no sense because this variable indicates whether the crinoids showed regeneration or not.
hist(Oji_1996$Regeneration)

hist(Oji_1996$Preserved_arms)
hist(Oji_1996$Total_arms)
hist(Oji_1996$Stalk_diameter)

#Get a summary of all the columns without considering grouping by phenotype
summary(Oji_1996$Depth)
summary(Oji_1996$Regenerated_arms)
summary(Oji_1996$Total_arms)
summary(Oji_1996$Stalk_diameter)

#boxplots of the variables by phenotypic group
boxplot(Oji_1996$Depth~Oji_1996$Phenotype)
boxplot(Oji_1996$Regenerated_arms~Oji_1996$Phenotype)
boxplot(Oji_1996$Total_arms~Oji_1996$Phenotype)
boxplot(Oji_1996$Stalk_diameter~Oji_1996$Phenotype)

#counting the number of missing data points per variable by phenotype
table(is.na(Oji_1996$Depth), Oji_1996$Phenotype)
table(is.na(Oji_1996$Regenerated_arms), Oji_1996$Phenotype)
table(is.na(Oji_1996$Total_arms), Oji_1996$Phenotype)
table(is.na(Oji_1996$Stalk_diameter), Oji_1996$Phenotype)

#Drop the phenotype class INT
Oji_1996_NoINT<-subset(Oji_1996, Oji_1996$Phenotype!="INT")

#after subseting, one needs to re-level the factor Phenotype... this is why it didn't work
Oji_1996_NoINT$Phenotype<-droplevels(Oji_1996_NoINT$Phenotype)

#how many observations you have by phenotype by regeneration class
table(Oji_1996_NoINT$Regeneration, Oji_1996_NoINT$Phenotype)

#note that the levels are correct now. Let's graph again
boxplot(Oji_1996_NoINT$Depth~Oji_1996_NoINT$Phenotype)
boxplot(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Phenotype)
boxplot(Oji_1996_NoINT$Total_arms~Oji_1996_NoINT$Phenotype)
boxplot(Oji_1996_NoINT$Stalk_diameter~Oji_1996_NoINT$Phenotype)

boxplot(log10(Oji_1996_NoINT$Regenerated_arms+1)~Oji_1996_NoINT$Phenotype)


#now let's check for significance
t.test(Oji_1996_NoINT$Depth~Oji_1996_NoINT$Phenotype)
t.test(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Phenotype)
t.test(Oji_1996_NoINT$Total_arms~Oji_1996_NoINT$Phenotype)
t.test(Oji_1996_NoINT$Stalk_diameter~Oji_1996_NoINT$Phenotype)


#correlations plots:
plot(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Depth)
plot(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Total_arms)
plot(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Stalk_diameter)


#laying out graphs with par
par(mfrow=c(1,2))
plot(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Total_arms)
plot(Oji_1996_NoINT$Regenerated_arms~Oji_1996_NoINT$Stalk_diameter)

dev.off()

#Producing plots per phenotype with subset

par(mfrow=c(1,2))
plot(subset(Oji_1996_NoINT, Oji_1996_NoINT$Phenotype == "PA")$Regenerated_arms~subset(Oji_1996_NoINT, Oji_1996_NoINT$Phenotype == "PA")$Total_arms)
plot(subset(Oji_1996_NoINT, Oji_1996_NoINT$Phenotype == "PR")$Regenerated_arms~subset(Oji_1996_NoINT, Oji_1996_NoINT$Phenotype == "PR")$Total_arms)

dev.off()

#more advanced plotting with ggplot2
library(ggplot2)

#what do you want to graph?
p<-ggplot(Oji_1996_NoINT, aes(Total_arms, Regenerated_arms))

#graph it as points
p+geom_point()

#now lets add some colors
p+geom_point(aes(colour=Depth))

#change how the gradient looks like
p+geom_point(aes(colour=Depth)) + scale_colour_gradient(low="light blue", high="dark blue")

#change the size of the dots to reflect the diameter of the stalk
p+geom_point(aes(colour=Depth, size=Stalk_diameter)) + scale_colour_gradient(low="light blue", high="dark blue")

#change the size of the dots to reflect the diameter of the stalk but changing the scale max and min sizes
p+geom_point(aes(colour=Depth, size=Stalk_diameter)) + scale_size(range=c(1,12)) + scale_colour_gradient(low="light blue", high="dark blue")

#change the shape to reflect the different phenotypes
p+geom_point(aes(colour=Depth, size=Stalk_diameter, shape=Phenotype)) + scale_size(range=c(1,12)) + scale_colour_gradient(low="light blue", high="dark blue")

#white background 
p+geom_point(aes(colour=Depth, size=Stalk_diameter, shape=Phenotype)) + scale_size(range=c(1,12)) + scale_colour_gradient(low="light blue", high="dark blue") + theme_bw()

#faceting
p+geom_point(aes(colour=Depth, size=Stalk_diameter, shape=Phenotype)) + scale_size(range=c(1,12)) + scale_colour_gradient(low="light blue", high="dark blue") + theme_bw() + facet_wrap(~Phenotype)






