library(MCMCpack)
library(stats)
library(CARBayes) 
library(rgdal) 
library(RColorBrewer) 
library(ggplot2)
library(rgeos) 
library(maptools) 
library(spdep)  
library(ggmap)  
library(sf)
library(dplyr)
library(tidyverse)
library(CARBayesST)
library(TSDT)
library(lubridate)

base_model_final <- readRDS("base_model_final.RDS")
neighb.mat_2 <- readRDS("neighb.mat_2.RDS")

base_model_final_2 <- base_model_final %>%
  mutate(year = as_factor(year)) %>%
  filter(VARNAME_2 != "Kien Hai", 
         VARNAME_2 != "Phu Quoc") %>%
  distinct(year, month, VARNAME_2, NAME_1, ENGTYPE_2, .keep_all = T)

advance_missing_func <- function(N_advance){
  
  base_model_for_model_1 <- base_model_final_2 %>% 
    mutate(year = unfactor(year),
           #monthN = as.numeric(gsub('m','', month)),
           date = as.Date(paste(year, month, '01', sep='-')),
           pop=if_else(is.na(pop), 0.5 ,pop)) %>%
    filter( date <= (as.Date('2006-12-01' ) %m+% months(N_advance))) %>%
    arrange(VARNAME_2, date) %>%
    group_by(VARNAME_2) %>%
    mutate( m_DHF_cases=if_else(is.na(m_DHF_cases),0, m_DHF_cases), ##ALERT...assuming nov, dec missing=0
            index= row_number() , 
            m_DHF_cases_fit = if_else(index==max(index,na.rm=T), NA_real_,m_DHF_cases)) %>%
    dplyr::select(month, year, VARNAME_2, m_DHF_cases,m_DHF_cases_fit, pop) %>%
    arrange(month, year, VARNAME_2)
}

mod.func <- function(N_advance) {
  ds <- advance_missing_func(N_advance) 
  
  x_model = model.matrix(~ as.factor(as.numeric(month)), data = ds)
  
  offset_model = log(ds$pop) 
  
  
  results<-ST.CARar(m_DHF_cases_fit ~ -1 + x_model + offset(offset_model) , 
                    data = ds,
                    
                    family = "poisson",
                    
                    W = neighb.mat_2,
                    
                    burnin = 1000, 
                    
                    n.sample = 11000,
                    
                    thin = 10,
                    
                    prior.mean.beta = rep(0.00,
                                          
                                          times = ncol(x_model)),
                    
                    prior.var.beta = rep((100.00^2),
                                         
                                         times = ncol(x_model)),
                    
                    prior.tau2 = c(0.01, 0.01),
                    
                    AR = 1,
                    
                    MALA = FALSE,
                    
                    verbose = TRUE)
  
  # save fitted values
  post_fit <- results$fitted.values
  saveRDS(post_fit, paste0("post_fit_values_", N_advance,".rds"))
  return(0)
}