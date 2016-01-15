library(ggplot2)
library(dplyr)
library(scatterplot3d)

#reading data
data <- read.csv(file="data.csv")
champs <- read.csv(file="champions.csv")
champs <- na.omit(champs)
men <- filter(data,Class != "Women_Strawweight" & Class != "Women_Bantamweight")
women <- filter(data,Class == "Women_Strawweight" | Class == "Women_Bantamweight")
men.champ <- filter(champs,Class != "Women_Strawweight" & Class != "Women_Bantamweight")
women.champ <- filter(champs,Class == "Women_Strawweight" | Class == "Women_Bantamweight")


#3D
p3d <- scatterplot3d(x=data$Height.cm,y=data$Reach.cm,z=data$Weight.kg,angle=60)
p3d$plane3d(fit)

a <- ggplot(fighter.data,aes(x=Height.cm,y=Reach.cm,colour=Class))
a <- a + geom_point() 
a <- a + labs(title="Fighter Height vs Reach",xlab="Height (cm)",ylab="Reach (cm)")
a

b <- ggplot(women,aes(x=Height.cm,y=Reach.cm,colour=Class))
b <- b + geom_point(aes(size=2))
b

c <- ggplot() + geom_point(data=men,aes(x=Height.cm,y=Reach.cm,colour=Class))
c <- c + geom_point(data=men.champ,aes(x=Height.cm,y=Reach.cm,colour=Class,size=3))
c

d <- ggplot() + geom_point(data=women,aes(x=Height.cm,y=Reach.cm,colour=Class))
d <- d + geom_point(data=women.champ,aes(x=Height.cm,y=Reach.cm,color="red",size=3))
d

#comparing bantam weight men and women
bantam <- filter(data,Class == "Bantamweight" | Class == "Women_Bantamweight")
e <- ggplot() + geom_point(data=bantam,aes(x=Height.cm,y=Reach.cm,colour=Class))
e

square <- mutate(men,sqr = Height.cm*Reach.cm)
square$Class2 <- factor(square$Class)
f <- ggplot() + geom_boxplot(data=square,aes(x=Class2,y=sqr))
f

#fitting a linear model
fit <- lm(Weight.kg ~ Height.cm + Reach.cm,data=data)
heavy <- filter(data,Class=="Heavyweight")
heavy.fit <- lm(Weight.kg ~ Height.cm + Reach.cm,data=heavy)

data <- mutate(data,beta = (0.975*Height.cm)+(0.35*Reach.cm))
square.fit$Class2 <- factor(square.fit$Class)
g <- ggplot() + geom_boxplot(data=square.fit,aes(x=Class2,y=sqr))
g
h <- ggplot() + geom_point(data=square.fit,aes(x=Weight.kg,y=sqr))
h

avg <- aggregate(data[,6],list(data$Class), mean)
std <- aggregate(data[,6],list(data$Class), sd)
avg <- cbind(avg,std$x)
names(avg) <- c("Weight.Class","sqr.mean","sqr.sd")
avg <- arrange(avg,sqr.mean)


#quadratic model
q.data <- mutate(data,height2=Height.cm^2,reach2=Reach.cm^2)
q.fit <- lm(Weight.kg ~ height2 + reach2,q.data)
q.fit

plot(q.fit)