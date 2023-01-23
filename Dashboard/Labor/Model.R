setwd("~/KPMG/Dashboard/Labor")
library(plotly)
library(dplyr)
library(DT)
library(readxl)
library(reshape2)
library(tidyr)


# xl_data <- "Labor.xlsx"
# 
# tab_names <- excel_sheets(path = xl_data)
# 
# ####################################################################################
# list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, sheet = x))
# # df <- data.frame(matrix(unlist(list_all), nrow=5, byrow=T),stringsAsFactors=FALSE)
# df1 <- do.call(rbind.data.frame, list_all)
# df1$people <- excel_sheets(xl_data)
# list_all <- lapply(list_all, function(x) x[-1,-1])
# pre <- function(student) list_all[[student]]
# pre(1)


mysheets_fromexcel <- list()

mysheetlist <- excel_sheets(path="Labor.xlsx")
i=1
for (i in 1:length(mysheetlist)){
  tempdf <- read_excel(path="Labor.xlsx", sheet = mysheetlist[i])
  tempdf$sheetname <- mysheetlist[i]
  mysheets_fromexcel[[i]] <- tempdf 
}
df1 <- do.call(rbind.data.frame, mysheets_fromexcel)
####################################################################################################
df1$Shift <- as.numeric(as.factor(df1$Shift))
df1$sheetname <- as.numeric(as.factor(df1$sheetname))
shifft <- length(unique(df1$Shift))
labor <- length(unique(df1$sheetname))
day1 <- ncol(df1)
####################################################################################################
# df1$shiftn <- revalue(df1$Shift,
#                              c("midnight-4am"="1", "4am-8am"="2", "8am-noon"="3", "noon-4pm"="4","4pm-8pm"="5","8pm-midnight"="6"))
# df1$shiftn <- as.numeric(df1$shiftn)
# df1$namen <- revalue(df1$sheetname,
#                      c("Dinuka"="1", "San"="2", "JJ"="3", "Senthu"="4","Sabee"="5"))
# df1$namen <- as.numeric(df1$namen)
# labor <- unique(df1$namen)
# shifft <- unique(df1$shiftn)
# df2 <- df1[,-c(1,9)]
# day <- colnames(df2[1:7])
# day1 <- as.numeric(factor(day))
####################################################################################
 # df1[df1$shiftn==1 & df1$namen==1,]["Mon"]

# score <- numeric(0)
# for (i in 1:6) {
#   for (j in 1:5) {
#    score <- df1[df1$shiftn==i & df1$namen==j,]$Mon
#   }
#
# }

score <- function(lab, shi, day) {
 out <- df1[df1$sheetname==lab & df1$Shift==shi,][day]
 print(as.integer(out))
}
####################################################################################
# weightt <- function(labor, shift) {
#   p <- which(as.numeric(shift) == pre(as.numeric(labor)))
#   as.integer(if (length(p) == 0) {
#     -100000
#   } else {
#     p
#   })
# }
# 
# # weightt(1,1)
#initialize readin listing
###################################################################################
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
model <- MIPModel() %>%
  
  # 1 iff labor i is assigned to shift j in k day
  add_variable(x[i, j, k], i = 1: labor, j = 1:shifft, k = 2:8, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(score(i, j, k) * x[i, j, k], i = 1:labor, j =1:shifft, k = 2:8)) %>%
  

  add_constraint(sum_expr(x[i, j, k], j =1:shifft) <= 2, i = 1:labor, k = 2:8)  %>%
  
  
  add_constraint(sum_expr(x[i,j,k], k = 2:8) <= 2, j = 1:shifft, i = 1:labor) %>%
  
  
  add_constraint(sum_expr(x[i,j,k], i = 1:labor) <= 3, k = 2:8, j = 1:shifft)


  # add_constraint(sum_expr(x[i], j =shifft) <= 2, i = labor)
  # 
  # each student needs to be assigned to one course
  # add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)
model
###########################################################################################33
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

matching <- result %>% 
  get_solution(x[i,j,k]) %>%
  filter(value > .9) %>%
  select(i, j, k) %>%
  rowwise() %>%
  mutate(score = score(as.numeric(i), as.numeric(j), as.numeric(k)))

df2 <- do.call(rbind.data.frame, mysheets_fromexcel)
df2$Shiftn <- as.numeric(as.factor(df2$Shift))
df2$sheetnamen <- as.numeric(as.factor(df2$sheetname))

df3 <- df2 %>%
  select(sheetname,sheetnamen) %>%
  rename(i=sheetnamen)

df4 <- df2 %>%
  select(Shift,Shiftn) %>%
  rename(j=Shiftn)

df5 <- merge(matching, unique(df3), by="i", all.x = T)
df5 <- merge(df5, unique(df4), by="j", all.x = T)
df5$day <- ifelse(df5$k==2, "Mon",
                  ifelse(df5$k==3, "Tue",
                         ifelse(df5$k==4, "Wed",
                                ifelse(df5$k==5, "Thu",
                                       ifelse(df5$k==6, "Fri",
                                              ifelse(df5$k==7, "Sat", "Sun"))))))


df6 <- df5 %>% 
    select(Shift, sheetname, day) %>% 
    mutate(row = row_number()) %>% 
    spread(day, sheetname)

df6 <- df6[,-2]

df7 <- aggregate(. ~Shift, df6, c)


test <- data.frame(matrix(nrow = 6, ncol = 8))
colnames(test) <- c("Shift", "Mon", "Tue", "wed", "Thu", "Fri", "Sat", "Sun")
test$Shift <- unique(df5$Shift)



# head(matching)
# 
# matching %>% 
#   group_by(weight) %>% 
#   summarise(count = n())