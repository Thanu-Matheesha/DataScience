setwd("~/KPMG/Dashboard/Labor/Labor 3")
library(plotly)
library(dplyr)
library(DT)
library(readxl)

Df <- read_xlsx('Lab_2.xlsx')

Df$Day <- as.numeric(as.factor(Df$Day))
Df$Shift <- as.numeric(as.factor(Df$Shift))
Day <- length(unique(Df$Day))
shift <- length(unique(Df$Shift))
Product <- length(unique(Df$Product))
Machine <- length(unique(Df$Machine))
labor <- length(Df[5:9])

score <- function(day, shi, mac, pro, lab) {
  out <- Df[Df$Day==day & Df$Shift==shi & Df$Machine==mac & Df$Product==pro,][lab]
  out1 <- ifelse(is.na(as.numeric(out)), 0, as.numeric(out))
  print(out1)
}

library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
model <- MIPModel() %>%
  

  add_variable(x[i, j, k, l, m], i = 1: Day, j = 1:shift, k = 1:Machine, l=1:Product, m=5:9 , type = "binary") %>%
  

  set_objective(sum_expr(score(i, j, k, l, m) * x[i, j, k, l, m], i = 1: Day, j = 1:shift, k = 1:Machine, l=1:Product, m=5:9)) %>%
  
  
  add_constraint(sum_expr(x[i, j, k, l, m], j = 1:shift) <= 1, i = 1: Day, k = 1:Machine, l=1:Product, m=5:9) %>% 
  
  
  add_constraint(sum_expr(x[i,j,k, l, m], i = 1: Day) <= 1, j = 1:shift, k = 1:Machine, l=1:Product, m=5:9) %>%


  add_constraint(sum_expr(x[i,j,k, l, m], m=5:9) <= 1, i = 1: Day, j = 1:shift, k = 1:Machine, l=1:Product) %>%


  add_constraint(sum_expr(x[i,j,k, l, m], l=1:Product) <= 1, i = 1: Day, j = 1:shift, k = 1:Machine, m=5:9) %>%


  add_constraint(sum_expr(x[i,j,k, l, m], k = 1:Machine) <= 1, i = 1: Day, j = 1:shift, l=1:Product, m=5:9)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

matching <- result %>% 
  get_solution(x[i,j,k,l,m]) %>%
  filter(value > .9) %>%
  select(i, j, k, l, m) %>%
  rowwise() %>%
  mutate(score = score(as.numeric(i), as.numeric(j), as.numeric(k), as.numeric(l), as.numeric(m)))
