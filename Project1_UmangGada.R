setwd("C:/Users/umang/OneDrive/Documents/College files/Sem 4/STAT_611_R/Projects")
#----ii----#
pdf("Duncanplot.pdf")

#----i----#
library("car")
data(Duncan)
Duncan[1:5,]
x = Duncan$education
y = Duncan$income

plot(x,y,main="Duncan",type="n",xlab="education", ylab="income", xlim=c(0,100),ylim=c(0,100))
points(x[Duncan$prestige<=80],y[Duncan$prestige<=80])
points(x[Duncan$prestige>80],y[Duncan$prestige>80],pch=19,col="blue")

#----ii(continued)----#
legend(1,95, legend=c("Prestige<=80", "Prestige>80"), col = c("black","blue"), pch=c(21,19), cex=0.8)
dev.off()

#----iii----#
Duncan$Typecode=factor(Duncan$type)
levels(Duncan$Typecode) = c("3","1","2")

#----iv----#
write.table(x=Duncan,file="newduncanoutput.txt",
            sep=" ",na="??",quote=FALSE,row.names=FALSE)

#----v----#
lessthan20 = subset(Duncan, (Duncan$income<20)&(Duncan$education<20)&(Duncan$prestige<20))

#----vi----#
write.csv(lessthan20, file="lessthan20.csv")
