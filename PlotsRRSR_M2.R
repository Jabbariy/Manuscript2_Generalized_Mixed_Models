library(dtplyr)
library(readxl)
library(ggplot2)
library(data.table)
setwd("/Users/yaz/Desktop/M2")
data<- read_excel("M2_data.xlsx")

newdata <- aggregate(x = cbind(data$success,data$RR), by = list(landmark = data$landmark, Subject = data$Subject,motion = data$motion), FUN = mean)
setnames(newdata, "V1", "success")
setnames(newdata, "V2", "RR")

data_summary <- function(data, variable, groups){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  summary<-ddply(data, groups, .fun=summary_func,variable)
  summary <- rename(summary, c("mean" = variable))
  return(summary)
}

#Plotting RR:

rrdata <- data_summary(newdata, variable="RR", groups=c("landmark", "motion"))
# calculating SE:
rrdata$size[1] = nrow(newdata[newdata$landmark == 'D' & newdata$motion=='V-',])
rrdata$size[2] = nrow(newdata[newdata$landmark == 'D' & newdata$motion=='V+',])
rrdata$size[3] = nrow(newdata[newdata$landmark == 'N' & newdata$motion=='V-',])
rrdata$size[4] = nrow(newdata[newdata$landmark == 'N' & newdata$motion=='V+',])
rrdata$size[5] = nrow(newdata[newdata$landmark == 'P' & newdata$motion=='V-',])
rrdata$size[6] = nrow(newdata[newdata$landmark == 'P' & newdata$motion=='V+',])

rrdata$se <- rrdata$sd/sqrt(rrdata$size)
# caalculating 95% CI based on normal distribtion:
rrdata$CI <- qt(0.975,df=rrdata$size-1)*rrdata$sd/sqrt(rrdata$size)

#changing the order
rrdata$landmark<-factor(rrdata$landmark,levels = c("P","D","N"),ordered=TRUE)

# plot RR based on SE
p <- ggplot(rrdata, aes(x=motion, y=RR, fill=landmark)) + 
  geom_bar(stat="identity", position="dodge2") +
  geom_errorbar(aes(ymin=RR-se, ymax=RR+se), width=.3,position=position_dodge(.9))

p + scale_fill_brewer(palette="Set3") + theme_minimal(base_size = 12)

# plot RR based on CI
p <- ggplot(rrdata, aes(x=motion, y=RR, fill=landmark)) + 
  geom_bar(stat="identity", position="dodge2") +
  geom_errorbar(aes(ymin=RR-CI, ymax=RR+CI), width=.1,color= "gray33", position=position_dodge(.9))

p + scale_fill_brewer(palette="Set3") + theme_minimal(base_size = 12)




###################Plotting Success##########################

scdata <- data_summary(newdata, variable="success", groups=c("landmark", "motion"))
# calculating SE:
scdata$size[1] = nrow(newdata[newdata$landmark == 'D' & newdata$motion=='V-',])
scdata$size[2] = nrow(newdata[newdata$landmark == 'D' & newdata$motion=='V+',])
scdata$size[3] = nrow(newdata[newdata$landmark == 'N' & newdata$motion=='V-',])
scdata$size[4] = nrow(newdata[newdata$landmark == 'N' & newdata$motion=='V+',])
scdata$size[5] = nrow(newdata[newdata$landmark == 'P' & newdata$motion=='V-',])
scdata$size[6] = nrow(newdata[newdata$landmark == 'P' & newdata$motion=='V+',])
scdata$se <- scdata$sd/sqrt(scdata$size)
# caalculating 95% CI based on normal distribtion:
scdata$CI <- qt(0.975,df=scdata$size-1)*scdata$sd/sqrt(scdata$size)

#changing the order
scdata$landmark<-factor(scdata$landmark,levels = c("P","D","N"),ordered=TRUE)

# plot success based on SE
p <- ggplot(scdata, aes(x=motion, y=success, fill=landmark)) + 
  geom_bar(stat="identity", position="dodge2") +
  geom_errorbar(aes(ymin=success-se, ymax=success+se), width=.3,position=position_dodge(.9))

p + scale_fill_brewer(palette="Set3") + theme_minimal(base_size = 12)

# plot success based on CI
p <- ggplot(scdata, aes(x=motion, y=success, fill=landmark)) + 
  geom_bar(stat="identity", position="dodge2") +
  geom_errorbar(aes(ymin=success-CI, ymax=success+CI), width=.1,color= "gray33",position=position_dodge(.9))

p + scale_fill_brewer(palette="YlGnBu") + theme_minimal(base_size = 12)

