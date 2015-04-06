library(data.table)
setwd("F:/kaggle/Acquire Valued Shoppers Challenge")
transactions <- fread("transactions.csv",integer64="character")
offers <- fread("offers.csv",integer64="character")
train <- fread("trainHistory.csv",integer64="character")

setkey(offers,offer,category)
setkey(train,offer)
train <- merge(train,offers,all.x=TRUE)

setkey(transactions,id,category)
setkey(train,id,category)
train.transcat <- transactions[train]
dim(train.transcat); head(train.transcat)
