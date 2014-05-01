require(gdata)
bbk_man <- read.xls("rollingsales_manhattan.xls",pattern="BOROUGH")
#bbk_bln <- read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH")
bk <- bbk_man
head(bk)
summary(bk)
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))
names(bk) <- tolower(names(bk))
## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n)

dev.new()
hist(sale.price.n[sale.price.n>0])

dev.new()
hist(gross.sqft[sale.price.n==0])
detach(bk)

## keep only the actual sales
bk.sale <- bk[bk$sale.praice.n!=0,]

dev.new()
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)

dev.new()
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]

dev.new()
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))

bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),]$sale.price.n),]
## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]

dev.new()
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))

#comparing neighborhoods
bk.areas <- bk.sale[!is.na(bk.sale$neighborhood),] 
areas = unique(bk.areas$neighborhood)
area.avg.price <- list()
for(i in 1:length(areas)){
area.avg.price[i] = mean(subset(bk.sale, bk.sale$neighborhood == areas[i])$sale.price.n)
}
plot(areas,area.avg.price,type="o")
axis(1, at=1:length(areas), lab=areas)

#comparing time
bk.time <- bk.sale[!is.na(bk.sale$sale.date),]
months <- c(as.Date("2012-09-01"),as.Date("2012-10-01"),as.Date("2012-11-01"),as.Date("2012-12-01"),as.Date("2013-01-01"),as.Date("2013-02-01"),as.Date("2013-03-01"),as.Date("2013-04-01"),as.Date("2013-05-01"),as.Date("2013-06-01"),as.Date("2013-07-01"),as.Date("2013-08-01"),as.Date("2013-09-01"))
total.sales <- list()
for(i in 2:length(months)) {
total.sales[i] = sum(subset(bk.time, bk.time$sale.date>months[i-1] & bk.time$sale.date<months[i])$sale.price.n)
}
plot(months,total.sales,type="o")