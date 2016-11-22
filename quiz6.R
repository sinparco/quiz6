#Load Libraries
library(tidyverse)
library(apaTables)
library(MBESS)

#Load data
analytic.data <- read_csv("mmr_labquiz_data.csv")

##Do anxiety and preparation interact to predict exam scores?

#ONLY keep columsn we need
analytic.data <- analytic.data %>% select(exam,anxiety,preparation)
#keep complete cases only, list-wise deletion of cases
analytic.data <- na.omit(analytic.data)

apa.cor.table(analytic.data, table.number = 1, filename="Table1.doc")

#Center variables
analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(anxiety, center=T, scale=F)))
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(preparation, center=T, scale=F)))

#Compute regression including interaction
interaction.regression <- lm(exam ~ x.centered + z.centered + I(x.centered*z.centered), 
                             data=analytic.data, na.action=na.exclude)
#na.action=na.exclude = if any row has a missing value, drop the row (can't make the interaction product)

#I get the values for the text this way
summary(interaction.regression)

#regression table

apa.reg.table(interaction.regression, table.number = 2, filename="Table2.doc")
              #OR can use block approach
block1 <- lm(exam ~ x.centered + z.centered,data=analytic.data, na.action=na.exclude)
block2 <- lm(exam ~ x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)
apa.reg.table(block1, block2)
#then look at delta R2 - will give same value as reg table approach
#although delta R2 is significant, its CI overlaps with 0 - inconsistent messaging = small effect
#explore further


##make the graph - getting the lines on the surface (+1 SD)

#get sd of z, then mutate a bit
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
#This may seem counter intuitive, but we lower the scores to increase the zero point to +1

#get formula
simple.slope.plus.1SD <- lm(exam ~ x.centered + z.centered.at.plus.1SD 
                            + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude) 
#values for text this way
summary(simple.slope.plus.1SD)

#make regression table
apa.reg.table(simple.slope.plus.1SD)
#drop last 2 lines - look only at x.centered and intercept lines



##make the graph - getting the lines on the surface (-1 SD)

#get sd of z, then mutate a bit
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD = z.centered + sd.z)
#This may seem counter intuitive, but we increase the scores to decrease the zero point to -1

#get formula
simple.slope.minus.1SD <- lm(exam ~ x.centered + z.centered.at.minus.1SD  
                             + I(x.centered*z.centered.at.minus.1SD),
                             data=analytic.data, na.action=na.exclude) 

#make regression table
apa.reg.table(simple.slope.minus.1SD)
#drop last 2 lines - look only at x.centered and intercept lines


#values for text this way
summary(simple.slope.minus.1SD)



##  Figure 1: 3D Plot

# See: summary(interaction.regression) for numbers to input here
library(MBESS)
sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)
intr.plot(b.0=49.2104,b.x=15.2537,b.z=9.5471,b.xz=6.0481,
          x.min=-1*sd.x,x.max=1*sd.x,z.min=-1*sd.z,z.max=1*sd.z,
          xlab="Anxiety Centered",zlab="Preparation Centered",ylab="Exam Score",
          expand=1,hor.angle=60,gray.scale=TRUE, line.wd=4,zlim=c(0,100)) 

#save using menu's in RStudio. Manually include in MSWord Document



##  Figure 2: 2D Plot

# 2D Graph Range on X-Axis

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

#graph from -1SD on Anxiety to +1SD Anxiety for a given level of Prepartion (+/-1 SD)
x.axis.range <-seq(-1*sd.x,1*sd.x,by=.25*sd.x) #this indicates range on the x-axis -2SD to +2SD Anxiety


# 2D Graph Lines for +1 SD on Prep and -1SD on Prep

#level of z (+/- 1SD on Preparation)
sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi=  1*sd.z
z.line.lo= -1*sd.z


# 2D Graph: Create Data for Drawing Lines
#+1SD Line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)

#-1SD Line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo) 
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)

#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)

# 2D Graph: Make the graph using above data 

my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z)) 

#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5) 

#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z), 
                               color="black",linetype="solid",size=1.5) 

#set APA part of graph below
my.plot <- my.plot + theme_classic(18)

#labels
my.plot <- my.plot + labs(x="Anxiety (mean centered)", y="Exam Grade")
my.plot <- my.plot+annotate("text", x = -1, y = 55.9, label = "+1 SD Preparation")
my.plot <- my.plot+annotate("text", x = -1, y =42.5, label = "-1 SD Preparation")

#the SD of Anxiety (see Table 1 is 2.00 so -1SD to +1SD is -2 to 2)
my.plot <- my.plot + coord_cartesian(xlim=c(-2,2),ylim=c(0,100))

print(my.plot)
#save using menu's in RStudio. Manually include in MSWord Document
