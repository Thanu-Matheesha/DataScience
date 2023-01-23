setwd("~/DBL")
library(reshape2)
library(openxlsx)
library(stringr)
Item_Master <- read.csv("Item MasterC.csv")
Customer_Master <- read.csv("Customer Master FinalC.csv")
#cleaning the sales file
Sales <- read.xlsx("Sales_All.xlsx", sheet = 1)
Sales <- Sales[Sales$Item.Code != "Sage Evolution (Registered to DEEN BROTHERS IMPORTS (PVT) LTD.)",]
Sales <- Sales[Sales$Item.Code != "Item Code",]
Sales <- Sales[!is.na(Sales$Item.Code),]
Sales <- Sales[!is.na(Sales$Item.Description),]
colnames(Sales)[3] <- "Rep.Code"
colnames(Sales)[5] <- "Value"
colnames(Sales)[6] <- "Customer.Code"
Sales <- Sales[,-c(8:11)]
Sales$Quantity <- as.numeric(Sales$Quantity)
#write.csv(Sales,"Check.csv")
#merging the sales file with item master and customer master
Sales_Final <- merge(Sales,Customer_Master, by=c("Customer.Code"), all.x = TRUE)
#write.csv(Sales_Final,"Check2.csv")
customer_codes <- setNames(data.frame(unique(Sales_Final$Customer.Code)),c("Customer_Code"))
customer_codes$new.code <- ifelse(nchar(as.character(customer_codes$Customer_Code))!=6,
                                        str_pad(as.numeric(customer_codes$Customer_Code),6,pad = 0)
                                        ,as.character(customer_codes$Customer_Code))
Sales_Final <- merge(Sales_Final,customer_codes,by.x = "Customer.Code",by.y="Customer_Code",all.x = T)
Sales_Final <- merge(Sales_Final,Item_Master, by=c("Item.Code"))
#write.csv(Sales_Final,"checking.csv")
Sales_Final <- Sales_Final[Sales_Final$Group.Description != "Promotion Oregon Group",] # Deleting the enteries relating to oregon promotion
Sales_Final <- Sales_Final[,-c(16:17)]
#filter machinery
Sales_Final <- Sales_Final[Sales_Final$Category=="Machinery",]
Sales_Final$Sub.Category.1 <- substr(Sales_Final$Sub.Category.1,1,3)
##sub category 2
Sales_Final$Sub.Category.2 <- str_pad(as.numeric(Sales_Final$Sub.Category.2),4,pad = 0)
##Item Code
Sales_Final$Item.Code <- ifelse(nchar(Sales_Final$Item.Code)>7,paste(substr(Sales_Final$Item.Code,1,3),substr(Sales_Final$Item.Code,5,8),sep=""),Sales_Final$Item.Code)
Sales_Final$key <- paste(Sales_Final$Sub.Category.1,Sales_Final$Sub.Category.2,Sales_Final$Item.Code,Sales_Final$Area,Sales_Final$new.code,sep = "")
Sales_Final$Tr.Date <- convertToDate(Sales_Final$Tr.Date)
Sales_Final <- Sales_Final[Sales_Final$key!="NANANANANA",]
write.csv(Sales_Final,"Check3.csv")

#time series
library(zoo)
Sales_Final$Year_Mon <- as.yearmon(Sales_Final$Tr.Date)
Sales_TS <- tapply(Sales_Final$Quantity, INDEX = list(Sales_Final$Year_Mon,Sales_Final$key), FUN=sum)
Sales_TS[is.na(Sales_TS)] <- 0
Sales_TS <- Sales_TS[,colSums(Sales_TS >0)>2]
ts <- ts(Sales_TS, start = c(2013,10), frequency = 12)
library(hts)
y <- gts(ts,characters = list(c(3,4,7),c(6,6)))
ally <- aggts(y)
fgts <- forecast(y,h=6,method = "comb",fmethod = "arima")
all_f_hts <- aggts(fgts)
save.image('~/DBL/hts.RData')
load("~/DBL/hts.RData")
cat_map <- read.csv('Cat_Mapping.csv')
Sales_Final <- merge(Sales_Final,cat_map,by="Item.Code",all.x = T)
Sales_Final <- Sales_Final[,1:27]
###### inventory ABC analysis
inventory <- read.csv("inventory.csv")    
inventory <- inventory[inventory$Qty.On.Hand>0,]
active_item_sales <- Sales_Final[Sales_Final$Tr.Date>Sys.Date()-365,]
active_item_sales$Value <- as.numeric(active_item_sales$Value)
active_item_sales <- setNames(aggregate(active_item_sales$Quantity,by=list(active_item_sales$Item.Code),sum),c("Item.Code","Quantity"))
active_item_sales <- active_item_sales[order(-active_item_sales$Quantity),]
library(ABCanalysis)
abc <- ABCanalysis( active_item_sales$Quantity, PlotIt = F)
a <- length(abc$Aind)
b <- length(abc$Bind)
c <- length(abc$Cind)
active_item_sales$ABC <- NA
active_item_sales$ABC[1:a] <- 'A'
active_item_sales$ABC[a+1:b] <- 'B'
active_item_sales$ABC[a+b+1:c] <- 'C'
abc_table <- data.frame(ftable(active_item_sales$ABC))
active_item_value <- Sales_Final[Sales_Final$Tr.Date>Sys.Date()-365,]
active_item_value$Value <- as.numeric(active_item_value$Value)
active_item_value <- setNames(aggregate(active_item_value$Value ,by=list(active_item_value$Item.Code),sum),c("Item.Code","Value"))
inventory_volume <- setNames(aggregate(inventory$Qty.On.Hand,by=list(inventory$Item.Code),sum),c("Item.Code","Volume"))
inventory_value <- setNames(aggregate(inventory$Value,by=list(inventory$Item.Code),sum),c("Item.Code","Inventory"))
abc <- merge(active_item_sales,active_item_value,by="Item.Code",all = T)
abc <- merge(abc,inventory_volume,by="Item.Code",all = T)
abc <- merge(abc,inventory_value,by="Item.Code",all = T)
abc <- merge(abc,cat_map,by="Item.Code",all.x = T)
abc[is.na(abc$Quantity),]$Quantity <-0
abc[is.na(abc$ABC),]$ABC <-"X"
abc[is.na(abc$Value),]$Value <-0
abc[is.na(abc$Volume),]$Volume <-0
abc[is.na(abc$Inventory),]$Inventory <-0
abc <- abc[abc$Quantity>0,]
save.image("~/DBL/hts2.RData")

#### Forecastability check
library(ForeCA)
sapply(ally,Omega,spectrum.control = list(method = "wosa"))
library(pracma)
sapply(ally,approx_entropy)


### Data mining (check wether there is a sale or not)
sales_returns <- Sales_Final[Sales_Final$Quantity<0,] 
returns_by_products <- setNames(aggregate(sales_returns$Quantity,by=list(sales_returns$Item.Code),sum),c("Item","Quantity"))
returns_by_products_by_month <- setNames(aggregate(sales_returns$Quantity,by=list(sales_returns$Item.Code,sales_returns$Item.Description.x,sales_returns$Year_Mon),sum),c("Item.Code","Month","Quantity"))
item_master <- read.csv("Item Master.csv")
returns_by_products_by_month <- merge(returns_by_products_by_month,item_master,by="Item.Code",all.x = T)
