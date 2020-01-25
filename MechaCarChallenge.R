mecha <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors=F)
head(mecha)
model <- lm(mpg ~ `vehicle weight`,mecha)
summary(lm(mpg ~ `vehicle weight`,mecha))
yvals <- model$coefficients['`vehicle weight`']*mecha$`vehicle weight` + model$coefficients['(Intercept)']
plt <- ggplot(mecha, aes(x=`vehicle weight`, y= mpg))
plt + geom_point() + geom_line(aes(y=yvals), color="red")

model <- lm(mpg ~ `vehicle length`,mecha)
summary(lm(mpg ~ `vehicle length`,mecha))
yvals <- model$coefficients['`vehicle length`']*mecha$`vehicle length` + model$coefficients['(Intercept)']
plt <- ggplot(mecha, aes(x=`vehicle length`, y= mpg))
plt + geom_point() + geom_line(aes(y=yvals), color="red")

model <- lm(mpg ~ `spoiler angle`,mecha)
summary(lm(mpg ~ `spoiler angle`,mecha))
yvals <- model$coefficients['`spoiler angle`']*mecha$`spoiler angle` + model$coefficients['(Intercept)']
plt <- ggplot(mecha, aes(x=`spoiler angle`, y= mpg))
plt + geom_point() + geom_line(aes(y=yvals), color="red")

model <- lm(mpg ~ `ground clearance`,mecha)
summary(lm(mpg ~ `ground clearance`,mecha))
yvals <- model$coefficients['`ground clearance`']*mecha$`ground clearance` + model$coefficients['(Intercept)']
plt <- ggplot(mecha, aes(x=`ground clearance`, y= mpg))
plt + geom_point() + geom_line(aes(y=yvals), color="red")

model <- lm(mpg ~ `AWD`,mecha)
summary(lm(mpg ~ `AWD`,mecha))
yvals <- model$coefficients['`AWD`']*mecha$`AWD` + model$coefficients['(Intercept)']
plt <- ggplot(mecha, aes(x=`AWD`, y= mpg))
plt + geom_point() + geom_line(aes(y=yvals), color="red")

#suspension coil statistics
suspension <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors=F)
head(suspension)
summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),St.Dev=sd(PSI))
summary

#suspension coil t tst
head(suspension)
head(summarize)
t.test(summary$Mean,mu=mean(suspension$PSI))
       
       