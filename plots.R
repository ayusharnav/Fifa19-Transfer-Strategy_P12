rm(list = ls(all = T))
source('./libraries.R')

FullData <- as.data.frame(read.csv("./data/data_new.csv",header=TRUE,encoding = "UTF-8"))
ggplot(FullData,aes(x=FullData[,4], y=FullData[,7], color=FullData[,2]))+geom_point()+ geom_smooth(method="auto",colour="red") +   labs(x = "Overall (Discrete)", y = "Value", color = "Age") +
ggsave('G10_plot01.pdf',width=8,height=8)

