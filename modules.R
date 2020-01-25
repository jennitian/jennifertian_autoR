demo_table <- read.csv(file='demo.csv', check.names=F, stringsAsFactors = F)
library(jsonlite)
demo_table2 <- fromJSON('demo.json')
population_table <- read.csv('used_car_data.csv', check.names = F, stringsAsFactors = F)
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot
sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven)))
