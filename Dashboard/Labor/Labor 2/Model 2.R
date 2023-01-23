setwd("~/KPMG/Dashboard/Labor/Labor 2")
library(plotly)
library(dplyr)
library(DT)
library(readxl)
library(styler)
library(flextable)

df <- read_xlsx("Labor 2.xlsx")


la <- max(ncol(df))
lb <- max(df$Shift)

score <- function(labor, shift) {
  out <- df[df$Shift==shift,][labor]
  print(as.integer(out))
}

library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
modell <- MIPModel() %>%
  
  # 1 iff labor i is assigned to shift j 
  
  add_variable(x[i, j], i = 2:la, j = 1:lb, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(score(i,j) * x[i,j], i = 2:la, j = 1:lb)) %>%
  
  # each labor needs to be assigned to at most one shift
  add_constraint(sum_expr(x[i,j], j = 1:lb) <= 2, i = 2:la) %>%
  
  #each shift needs not more than the allocated value of labors
  add_constraint(sum_expr(x[i,j], i = 2:la) <= 3, j = 1:lb) 

resultt <- solve_model(modell, with_ROI(solver = "glpk", verbose = TRUE))

matchingx <- resultt %>% 
  get_solution(x[i,j])%>%
  filter(value > .9) %>%  
  select(i, j) %>%
  rowwise() %>%
  mutate(score = score(as.numeric(i), as.numeric(j))) %>%  
  rename(labor_name=i,shift=j)
  
  
  

matchingx$labor_name <- ifelse(matchingx$labor_name==2,colnames(df)[2],
                               ifelse(matchingx$labor_name==3,colnames(df)[3],
                                      ifelse(matchingx$labor_name==4,colnames(df)[4],
                                             ifelse(matchingx$labor_name==5, colnames(df)[5],
                                                    ifelse(matchingx$labor_name==6, colnames(df)[6],colnames(df)[7]))))
                               )



# changeCellColor <- function(row, col){
#   c(
#     "function(row, data, num, index){",
#     sprintf("  if(index == %d){", row-1),
#     sprintf("    $('td:eq(' + %d + ')', row)", col),
#     "    .css({'background-color': 'orange'});",
#     "  }",
#     "}"
#   )
# }
# 
# 
# datatable(df,
#           options = list(
#             dom = "t",
#             rowCallback = JS(changeCellColor(2,2))
#           )
# )
# 
# ft <- flextable(df)
# 
# ft <- color(ft, 1,2 ,
#             color="red")
# ft
