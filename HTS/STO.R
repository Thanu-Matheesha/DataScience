setwd("~/STO")
library(openxlsx)
library(reshape2)
library(readxl)
library(data.table)
library(plyr)
library(dplyr)
#library(splitstackshape)
library(zoo)
library(stringr)
library(readxl)

item_master <- read.xlsx("item_master_new.xlsx", sheet = 2)
#item_master <- item_master[,-c(2:28)]
Type <- item_master[8]
Type3 <- unique(Type)
Type <- as.data.frame(apply(Type, 2, function(x)gsub('\\s+', '',x)))
Type2 <- unique(Type)
x <- "T"
Type2 <- data.frame(Type2[complete.cases(Type2), ])
colnames(Type2)[1] <- "Type"
type_codes <- data.frame(sprintf("%s%0*d", x, 5 - nchar(x), 1:nrow(Type2)))
Type_Master <- cbind(Type2,type_codes)
colnames(Type_Master)[2] <- "Type_Code"
Type_Master_1 <- Type_Master[complete.cases(Type_Master), ]

Brand <- item_master[4]
Brand1 <- unique(Brand)
Brand <- as.data.frame(apply(Brand, 2, function(x)gsub('\\s+', '',x)))        
Brand <- unique(Brand)
B <- "B"
Brand <- data.frame(Brand[complete.cases(Brand), ])
colnames(Brand)[1] <- "Brand"
Brand_Master <- cbind(Brand, data.frame(sprintf("%s%0*d", B, 6 - nchar(B), 1:nrow(Brand))))
colnames(Brand_Master)[2] <- "Brand_Code"

Category <- item_master[6]
Category <- as.data.frame(apply(Category, 2, function(x)gsub('\\s+', '',x)))
Category <- unique(Category)
C <- "C"
Category <- data.frame(Category[complete.cases(Category), ])
colnames(Category)[1] <- "Category"
Category_Master <- cbind(Category, data.frame(sprintf("%s%0*d", C, 4 - nchar(C), 1:nrow(Category))))
colnames(Category_Master)[2] <- "Category_Code"

#Supplier <- item_master[7]
#Supplier <- as.data.frame(apply(Supplier, 2, function(x)gsub('\\s+', '',x)))
#Supplier <- unique(Supplier)
#S <- "S"
#Supplier <- data.frame(Supplier[complete.cases(Supplier), ])
#colnames(Supplier)[1] <- "Supplier"
#Supplier_Master <- cbind(Supplier, data.frame(sprintf("%s%0*d", S, 5 - nchar(S), 1:nrow(Supplier))))
#colnames(Supplier_Master)[2] <- "Supplier_Code"
Staff_Member <- item_master[3]
Staff_Member_Master <- unique(Staff_Member)
Staff_Member_Master <- as.data.frame(apply(Staff_Member_Master, 2, function(x)gsub('\\s+', '',x)))
Staff_Member_Master <- data.frame(Staff_Member_Master[complete.cases(Staff_Member_Master), ])
Staff_Member_Master$Staff_Code <- substr(Staff_Member_Master$Staff_Member_Master.complete.cases.Staff_Member_Master...., 1, 3)
colnames(Staff_Member_Master)[1] <- "Staff_Member"

Master3 <- as.data.frame(apply(item_master, 2, function(x)gsub('\\s+', '', x)))
colnames(Master3)[3] <- "Staff_Member"
Master_file <- merge(Master3, Staff_Member_Master, by = "Staff_Member", all.x = TRUE)
colnames(Master_file)[6] <- "Category"
Master_file <- merge(Master_file, Category_Master, by = "Category", all.x = TRUE)
Master_file <- merge(Master_file, Brand_Master, by = "Brand", all.x = TRUE)
Master_file <- merge(Master_file, Type_Master_1, by.x = "NEW.Type", by.y = "Type", all.x = TRUE)
#Master_file <- merge(Master_file, Supplier_Master, by = "Supplier", all.x = TRUE)
Master_file1 <- Master_file[,-c(6:8)]
Master_file1 <- unique(Master_file1)
write.csv(Master_file1, "Master_file1.csv", row.names = FALSE)

Master_file1 <- read.csv("Master_file1.csv")

######################################################
setwd("~/STO/POS/2015/Supermart")

f <- list.files(pattern = "*.xlsx") 
sales_list_2015 <- lapply(f, function(x) read.xlsx(x, sheet = 1 ))
sales_2015 <- rbindlist(sales_list_2015)
sales_2015$Created.on <- convertToDate(sales_2015$Created.on)
dddd <- sales_2015[is.na(sales_2015$Created.on), ]

setwd("~/STO/POS/2016/Supermart")
f1 <- list.files(pattern = "*.xlsx") 
sales_list_2016 <- lapply(f1, function(x) read.xlsx(x, sheet = 1 ))
sales_2016 <- rbindlist(sales_list_2016)



setwd("~/STO/POS/2017/Supermart")
f2 <- list.files(pattern = "*.xlsx") 
sales_list_2017 <- lapply(f2, function(x) read.xlsx(x, sheet = 1 ))
sales_2017 <- rbindlist(sales_list_2017)

col_name <- colnames(sales_2015)
colnames(sales_2017) <- col_name
sales <- rbind(sales_2015, sales_2016, sales_2017)
setwd("~/STO")
write.csv(sales, "Total_Sales.csv", row.names = FALSE)

Total_sales <- read.csv("Totalsupermart.csv", na.strings = c("","NA"))
Total_sales$Created.on <- as.Date(Total_sales$Created.on)



############################################################################
#Total_sales <- read.csv("Total_Sales.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)
#Total_sales_2 <- merge(Total_sales, Master_file1, by = "Article", all.x = TRUE)
Total_sales_2 <- Total_sales[,c(3,6,8)]
Total_sales_2 <- merge(Total_sales_2, Master_file1, by = "Article", all.x = TRUE)
Total_sales_2 <- Total_sales_2[!is.na(Total_sales_2$Category_Code), ]
Total_sales_2 <- Total_sales_2[,-c(4:7,10)]
Total_sales_2 <- Total_sales_2[,-c(4)]
#Total_sales_2$Created.on <-as.Date(as.numeric(Total_sales_2$Created.on))

#Total_sales_2$Created.on <- convertToDate(Total_sales_2$Created.on)
Total_sales_2$year_mon <- as.yearmon(Total_sales_2$Created.on)
Total_sales_2$key <- paste(Total_sales_2$Category_Code, Total_sales_2$Type_Code, Total_sales_2$Article,sep ="")
sales_ts <- tapply(Total_sales_2$Billed.Quantity, INDEX = list(Total_sales_2$year_mon, Total_sales_2$key), FUN = sum)
sales_ts[is.na(sales_ts)] <- 0
sales_ts <- sales_ts[, colSums(sales_ts >0)>2]
ts <- ts(sales_ts, start = c(2015, 01), frequency = 12)
library(hts)
y = hts(ts, characters = c(4,5,8))






library(ABCanalysis)

sale_abc <- setnames(aggregate(Total_sales_1$Billed.Quantity, by=list(Total_sales_1$Type_Code, Total_sales_1$Base.Unit.of.Measure),sum),c("Type", "UOM", "Qty"))
sales_abc_graph <- ABCanalysis(sale_abc$Qty, PlotIt = TRUE) 
sale_abc$percentage <- sale_abc$Qty/sum(sale_abc$Qty)
sale_abc <- sale_abc[order(-sale_abc$percentage), ]
sale_abc$cum <- cumsum(sale_abc$percentage)
sale_abc$ABC <- ifelse(sale_abc$cum<sales_abc_graph$smallestAData, "A", ifelse(sale_abc$cum>sales_abc_graph$smallestBData, "C", "B"))
write.csv(sale_abc, "sale_abc.csv", row.names = TRUE)

##############################################################
sales_value_abc <- setnames(aggregate(Total_sales_1$Net.value, by=list(Total_sales_1$Type_Code),sum),c("Type","value"))
sales_value_abc_graph <- ABCanalysis(sales_value_abc$value, PlotIt = TRUE)
sales_value_abc$percentage <- sales_value_abc$value/sum(sales_value_abc$value)
sales_value_abc <- sales_value_abc[order(-sales_value_abc$percentage), ]
sales_value_abc$cum <- cumsum(sales_value_abc$percentage)
sales_value_abc$ABC <- ifelse(sales_value_abc$cum<sales_value_abc_graph$smallestAData, "A", ifelse(sales_value_abc$cum>sales_value_abc_graph$smallestBData, "C", "B"))
write.csv(sales_value_abc, "sale_value_abc.csv", row.names = TRUE)

