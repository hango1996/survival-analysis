rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(frechet)
library(fdapace)
library(magrittr)
load("fly_male.Rda")
load("fly_female.Rda")

#construct survival function 
#: Male #### 
df_male <- fly_male #%>% group_by(Cohort,Size,Cage) 
df_nested_male <- df_male %>% group_by(Cohort,Size,Cage) %>% nest

df_nested_male$data %<>%  imap(~summarise(.x,age = age,
         lx = df_nested_male$data[[.y]]$num,
         dx = c(-diff(df_nested_male$data[[.y]]$num),0),
         qx = ifelse(lx==0, 0, dx / lx),
         px = 1 - qx)
)
get_rid_of_negative_numbers <- function(df){
df[which(sign(df$dx) == -1),"dx"] <- 0  
}
#smooth the survival function and create density function 
df_nested_male$data %<>% imap(~mutate(.x,survival = Lwls1D(bw = 2.5, 
                                 kernel_type = 'epan',
                                 xin = df_nested_male$data[[.y]]$age,
                                 yin = df_nested_male$data[[.y]]$lx / df_nested_male$data[[.y]]$lx[1], 
                                 xout = df_nested_male$data[[.y]]$age),
               density  = CreateDensity(y = rep(df_nested_male$data[[.y]]$age, df_nested_male$data[[.y]]$dx), 
                                        optns=list(outputGrid = df_nested_male$data[[.y]]$age))$y)
               )
df_nested_male$data %<>% imap(~mutate(.x,hazard = Lwls1D(bw = 2.5,
                                   kernel_type = "epan",
                                   xin = df_nested_male$data[[.y]]$age,
                                   yin = df_nested_male$data[[.y]]$density/df_nested_male$data[[.y]]$survival,
                                   xout = df_nested_male$data[[.y]]$age))
                   )


#: Female ####

df_female <- fly_female #%>% group_by(Cohort,Size,Cage) 
df_nested_female <- df_female %>% group_by(Cohort,Size,Cage) %>% nest

df_nested_female$data %<>%  imap(~summarise(.x,age = age,
         lx = df_nested_female$data[[.y]]$num,
         dx = c(-diff(df_nested_female$data[[.y]]$num),0),
         qx = ifelse(lx==0, 0, dx / lx),
         px = 1 - qx)
)
get_rid_of_negative_numbers <- function(df){
df[which(sign(df$dx) == -1),"dx"] <- 0  
}
#smooth the survival function and create density function 
df_nested_female$data %<>% imap(~mutate(.x,survival = Lwls1D(bw = 2.5, 
                                 kernel_type = 'epan',
                                 xin = df_nested_female$data[[.y]]$age,
                                 yin = df_nested_female$data[[.y]]$lx / df_nested_female$data[[.y]]$lx[1], 
                                 xout = df_nested_female$data[[.y]]$age),
               density  = CreateDensity(y = rep(df_nested_female$data[[.y]]$age, df_nested_female$data[[.y]]$dx), 
                                        optns=list(outputGrid = df_nested_female$data[[.y]]$age))$y)
               )
df_nested_female$data %<>% imap(~mutate(.x,hazard = Lwls1D(bw = 2.5,
                                   kernel_type = "epan",
                                   xin = df_nested_female$data[[.y]]$age,
                                   yin = df_nested_female$data[[.y]]$density/df_nested_female$data[[.y]]$survival,
                                   xout = df_nested_female$data[[.y]]$age))
                   )

                


colormap = c("density" = 'red', 'survival' = 'black', 'hazard' = 'blue')
