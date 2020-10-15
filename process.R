rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(frechet)
library(fdapace)
load("fly_male.Rda")
load("fly_female.Rda")

#construct survival function 
df = fly_male %>% filter(Cohort == "1", Size == '4', Cage == '1')
df = df %>% summarise(age=age, 
                      lx = num, 
                      dx = c(-diff(df$num),0),
                      qx = ifelse(lx==0, 0, dx / lx), 
                      px = 1 - qx,
                      Cohort = Cohort, 
                      Size = Size, 
                      Cage = Cage,
                      Sex  = seg
)

#smooth the survival function and create density function 
df = df %>% mutate(survival = Lwls1D(bw = 2.5, 
                                    kernel_type = 'epan',
                                    xin = df$age,
                                    yin = lx / lx[1], 
                                    xout = df$age) ,
                   density  = CreateDensity(y = rep(df$age, df$dx), 
                                           optns=list(outputGrid = df$age))$y)
                      

colormap = c("density" = 'red', 'survival' = 'black', 'hazard' = 'blue')
gg = ggplot(df) + 
  geom_line(aes(x = age, y = density, color = 'density')) + 
  geom_line(aes(x = age, y = survival, color = 'survival')) + 
  scale_color_manual(values = colormap)
  
gg





