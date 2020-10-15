rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dplyr)
library(plyr)

combine = function(fly.df, dat){
  for(i in 2:ncol(dat)){
    dat.sub = data.frame(age = dat[-c(1,2,3,4),1] %>% as.numeric(),
                         num = dat[-c(1,2,3,4), i] %>% as.numeric(), 
                         Cohort = dat[1,i], 
                         Size = dat[2,i], 
                         Cage = dat[3,i],
                         seg  = dat[4,i])
    fly.df = fly.df %>% rbind(dat.sub)
  }
  return(fly.df)
}



L = 173
fly.df = data.frame()
df.male = read.table("MALE.txt", nrows = 21*(L+4)) 
col_time = read.table("MALE.txt", header = FALSE, nrows = L + 4)[,1]
for(i in 1:21){
  dat = df.male[(1+(i-1)*(L+4)):(i*(L+4)), ]
  if(dat[1,1] != "Cohort") {
    dat = cbind(col_time, dat)
    }
  fly.df = fly.df %>% combine(dat)
  
}
fly_male = fly.df 
save(fly_male, file = "fly_male.Rda")




fly.df = data.frame()
df.female = read.table("FEMALE.txt", nrows = 21*(L+4)) 
col_time = read.table("FEMALE.txt", header = FALSE, nrows = L + 4)[,1]
for(i in 1:21){
  dat = df.female[(1+(i-1)*(L+4)):(i*(L+4)), ]
  if(dat[1,1] != "Cohort") {
    dat = cbind(col_time, dat)
  }
  fly.df = fly.df %>% combine(dat)
  
}
fly_female = fly.df
save(fly_female, file = "fly_female.Rda")


