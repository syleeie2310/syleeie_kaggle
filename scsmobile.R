#data.table using
library(data.table)
library(ggplot2)
library(plyr)

#data loading
scsmobile <- fread("C:/Users/lee/Dropbox/넷마블인턴/제안서/데이터/scsmobile_201502271618.txt")
head(scsmobile)
str(scsmobile)
summary(scsmobile)
dim(scsmobile)
scsmobile2 <- scsmobile[,which(unlist(lapply(scsmobile, function(x)!all(is.na(x))))),with=F]
dim(scsmobile2)
head(scsmobile2)
str(scsmobile2)
colSums(is.na(scsmobile2))
scsmobile2$regdatetime <- as.POSIXct(strptime(scsmobile2$regdatetime, format = "%Y-%m-%d %H:%M:%S"))
scsmobile2$countrycd <- as.factor(scsmobile2$countrycd)
scsmobile2$guestflag <- as.factor(scsmobile2$guestflag)
scsmobile2$playmode <- as.factor(scsmobile2$playmode)
scsmobile2$stagetype <- as.factor(scsmobile2$stagetype)
scsmobile2$luckybagtype <- as.factor(scsmobile2$luckybagtype)
scsmobile2$os_2 <- as.factor(scsmobile2$os_2)
scsmobile2$achievement <- as.factor(scsmobile2$achievement)
scsmobile2$market <- as.factor(scsmobile2$market)
scsmobile2$result <- as.factor(scsmobile2$result)
scsmobile2$now_2 <- as.POSIXct(strptime(scsmobile2$now_2, format = "%Y-%m-%d %H:%M:%S"))
scsmobile2$lotterytype <- as.factor(scsmobile2$lotterytype)
save(scsmobile2, file="C:/Users/lee/Dropbox/넷마블인턴/제안서/데이터/scsmobile.RData")

load(file="C:/Users/Intern15042/Dropbox/넷마블인턴/제안서/데이터/scsmobile.RData")
str(scsmobile2)
unique(scsmobile2$characterid)
scsmobile2[, .N ,by = playmode]
scsmobile2[, .N ,by = result]
scsmobile2[, .N ,by = "playmode,result"]
scs_episoderesult <- scsmobile2[, .N ,by = "episode,result"]
scs_episoderesult[order(scs_episoderesult$episode, decreasing=TRUE)]
scs_episoderesult_16 <-data.frame(episode=16,result="c",N=0)
scs_episoderesult_15 <-data.frame(episode=15,result="c",N=0)
scs_episoderesult <- rbind(scs_episoderesult,scs_episoderesult_16,scs_episoderesult_15)
scs_episoderesult[order(scs_episoderesult$episode, decreasing=TRUE)]
str(scs_episoderesult)

scs_episodewinlose <- data.frame(stage=c(0:16),winlose=rep(0,17))
keys <- c("s")
keyc <- c("c")
keyf <- c("f")
keyq <- c("q")
for(i in 0:16)
{
  assign(paste("scs_episoderesult",i,sep=""),scs_episoderesult[scs_episoderesult$episode == i,])
  scs_episodewinlose[i+1,2] <- eval(parse(text=paste("(scs_episoderesult",i,"[scs_episoderesult",i,"$result == ", "keys", ",N]+","scs_episoderesult",i,"[scs_episoderesult",i,"$result == ", "keyc", ",N]) /","scs_episoderesult",i,"[scs_episoderesult",i,"$result == ", "keyf", ",N]", sep="")))[1]
}

ggplot(scs_episodewinlose[-1,], aes(x=stage, y=winlose)) + geom_line() +xlab("episode") + ylab("winlose ratio") + 
  theme_bw() +  theme(axis.text=element_text(size=24),  axis.title=element_text(size=30,face="bold")) + theme(legend.title = element_text(colour="black", size=30, face="bold")) + theme(legend.text = element_text(colour="black", size = 30)) 

scs_stageresult <- scsmobile2[, .N ,by = "stage,result"]
scs_stageresult[order(scs_stageresult$stage, decreasing=TRUE)]
unique(scs_stageresult$stage)
scs_stageno <- sort(unique(scs_stageresult$stage))

scs_stagewinlose <- data.frame(stage=scs_stageno[2:200],winlose=rep(0,199))

scs_stagewinlose <- na.omit(scs_stagewinlose)

for(i in scs_stageno[2:200])
{
  assign(paste("scs_stageresult",i,sep=""),scs_stageresult[scs_stageresult$stage == i,])
  scs_stagewinlose[i,2] <- eval(parse(text=paste("(scs_stageresult",i,"[scs_stageresult",i,"$result == ", "keys", ",N]+","scs_stageresult",i,"[scs_stageresult",i,"$result == ", "keyc", ",N]) /","(scs_stageresult",i,"[scs_stageresult",i,"$result == ", "keyf", ",N]+","scs_stageresult",i,"[scs_stageresult",i,"$result == ", "keyq", ",N])", sep="")))[1]
}

ggplot(scs_stagewinlose[20:169,], aes(x=stage, y=winlose)) + geom_line() +xlab("stage") + ylab("winlose ratio") + 
  theme_bw() +  theme(axis.text=element_text(size=24),  axis.title=element_text(size=30,face="bold")) + theme(legend.title = element_text(colour="black", size=30, face="bold")) + theme(legend.text = element_text(colour="black", size = 30)) 

unique(scsmobile2$episode)
ggplot(scsmobile2, aes(x=regdatetime, y=payamount)) + geom_point() 
sort(unique(scsmobile2$now_2))[16000:16128]

#subset
setkey(scsmobile2,logid)
unique(scsmobile2$logid)
unique(scsmobile2[J(c(1))]$logid)
scsmobile_log1 <- scsmobile2[J(c(1))]
scsmobile_log2 <- scsmobile2[J(c(2))]
scsmobile_log3 <- scsmobile2[J(c(3))]
scsmobile_log4 <- scsmobile2[J(c(4))]
scsmobile_log5 <- scsmobile2[J(c(5))]
scsmobile_log101 <- scsmobile2[J(c(101))]
scsmobile_log102 <- scsmobile2[J(c(102))]

setkey(scsmobile_log1,logdetailid)
scsmobile_log1_1 <- scsmobile_log1[J(c(1))]
scsmobile_log1_2 <- scsmobile_log1[J(c(2))]
scsmobile_log1_3 <- scsmobile_log1[J(c(3))]

#loginlog mean
str(scsmobile_log1_2)
sort(table(scsmobile_log1_2$pcseq),decreasing=T)[1:100]
hist(table(scsmobile_log1_2$pcseq), freq=FALSE)
lines(density(table(scsmobile_log1_2$pcseq)))
rug(jitter(table(scsmobile_log1_2$pcseq)))

#loginlog diff
scslog12_diff <- tapply(as.numeric(substr(gsub("[: -]", "" , scsmobile_log1_2$regdatetime, perl=TRUE),5,14)), scsmobile_log1_2$pcseq, diff) #1
scslog12_diffmean <- aggregate(regdatetime ~ pcseq, scsmobile_log1_2, diff) #2
head(scslog12_diffmean)
scslog12_diffmean[scslog12_diffmean$pcseq == 1321954,2][[1]]
mean(scslog12_diffmean[scslog12_diffmean$pcseq == 1321954,2][[1]])
#scslog12_diff <- by(sort(scsmobile_log1_2$regdatetime), scsmobile_log1_2$pcseq, diff)
str(scslog12_diff)
scslog12_diffmean2 <- abs(sapply(scslog12_diff,mean))/135.679
#scslog12_diffmean3 <- as.POSIXct(scslog12_diffmean2, origin = "2015-02-25")

scsmobile_log1_2$diffmean <- scslog12_diffmean2

#loginlog diffmean
boxplot(scslog12_diffmean2)
quantile(scsmobile_log1_2$mycash)
scsmobile_log1_2$mycash2 <- cut2(scsmobile_log1_2$mycash,c(10,300))
scsmobile_log1_2$mysingleclover2 <- cut2(scsmobile_log1_2$mysingleclover,c(5))
scsmobile_log1_2$mypoint2 <- cut2(scsmobile_log1_2$mypoint,c(150,300))
scsmobile_log1_2$myluna2 <- cut2(scsmobile_log1_2$myluna,c(5000,20000))
unique(scsmobile_log1_2$mycash2)

quantile(scsmobile_log1_2$mysingleclover)
quantile(scsmobile_log1_2$mypoint)
quantile(scsmobile_log1_2$myluna)

boxplot(diffmean~mycash2, data=scsmobile_log1_2)
boxplot(diffmean~mysingleclover2, data=scsmobile_log1_2)
boxplot(diffmean~mypoint2, data=scsmobile_log1_2)
boxplot(diffmean~myluna2, data=scsmobile_log1_2)

quantile(scslog12_diffmean2)
scslog12_diff$'1321954'
scslog12_diffmean <- lapply(scslog12_diff,mean)
scslog12_diffmean2[attributes(scslog12_diffmean2)[[1]] == 1321954]
length(scslog12_diffmean)
#scslog12_diffmean <- as.data.frame(scslog12_diffmean)
#scslog12_diffmean[1] == 1321954,]
#scslog12_diff <- as.data.frame(scslog12_diff)
strsplit(scslog12_diff$scslog12_diff, split=",")
head(scslog12_diff)
tail(scslog12_diff)
length(scslog12_diff)
hist(scslog12_diff)
sort(scslog12_diff, decreasing=T)[1:200]
mean(scslog12_diff)
scs_regdatetimediff <- diff(sort(scsmobile_log1_2$regdatetime, decreasing=TRUE))                                                                      
boxplot(scs_regdatetimedif)
quantile(scs_regdatetimediff)
sort(subset(scsmobile_log1_2, pcseq == 1321954)$regdatetime)
diff(sort(subset(scsmobile_log1_2, pcseq == 1321954)$regdatetime))
mean(diff(sort(subset(scsmobile_log1_2, pcseq == 1321954)$regdatetime)))

#usecash
p <- ggplot(scsmobile2, aes(factor(logid), usecash))
p + geom_boxplot()
pie <- ggplot(scsmobile2, aes(x = usecash, fill = factor(logid))) +
  geom_bar(width = 1)
pie + coord_polar(theta = "y")
scs_cashtable <- table(scsmobile2$usecash,scsmobile2$logid)
scs_cashtable <- as.matrix(scs_cashtable)
scs_cashtable2 <- scs_cashtable[2,]*2+scs_cashtable[3,]*5+scs_cashtable[4,]*6+scs_cashtable[5,]*10+scs_cashtable[6,]*12+scs_cashtable[7,]*15+scs_cashtable[8,]*20+scs_cashtable[9,]*25+scs_cashtable[10,]*30+scs_cashtable[11,]*40+scs_cashtable[12,]*50+scs_cashtable[13,]*100+scs_cashtable[14,]*300
scs_cashtablesum <- addmargins(scs_cashtable, FUN = list(Total = sum), quiet = TRUE)
scs_cashtablesum

percentlabels<- round(100*scs_cashtable2[-1]/sum(scs_cashtable2[-1]), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(scs_cashtable2[-1], main = "크리스탈 소모 컨텐츠", col=rainbow(length(scs_cashtable[2:7])), labels=pielabels) 
legend("topright", c("재화", "게임실행", "아이템", "캐릭터", "에피소드오픈", "럭키백"), cex=0.8, fill=rainbow(length(scs_cashtable[2:7])))

#gamelog
setkey(scsmobile_log3,logdetailid)
scsmobile_log3_1 <- scsmobile_log3[J(c(1))]
scsmobile_log3_2 <- scsmobile_log3[J(c(2))]
ggplot(scsmobile_log3_2, aes(x=stage, y=leftturn)) + geom_point(position="jitter") + scale_x_continuous(limits=c(0,200))

unique(scsmobile_log3_2$stage)

#read.csv 
scsmobile <- read.csv("C:/Users/Intern15042/Dropbox/넷마블인턴/제안서/데이터/scsmobile_201502271618.txt", header=T, sep=";", stringsAsFactors=FALSE)
head(scsmobile)
dim(scsmobile)
scsmobile <- scsmobile[,-colSums(is.na(scsmobile)) == 1034708]
colSums(is.na(scsmobile))
