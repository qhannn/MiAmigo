# Decomp and Wiod packages for data scratching and wwz decomposition Source: https://github.com/bquast/

#_________Loading the Packages_______________
			# Function that asks whether the package is installed if not, starts installing
	options(error = recover) # setting the error option
	check.packages <- function(pkg){
				new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
				if (length(new.pkg)) 
				    install.packages(new.pkg, dependencies = TRUE)
				sapply(pkg, require, character.only = TRUE)
										}
				# Packages need to be installed
				packages<-c("ggrepel","tibble","gganimate","reshape2", "GGally", "network", "sna", "decompr","wiod","gvc","tidyr","ggplot2","readxl","dplyr","tidyverse")
		
				check.packages(packages)

# Data Scratching funciton by bquast
getWIOT <- function(period = 2010,
                      format = "wide",  # or "long" or "list"
                      as.DT = TRUE,     # as data.table?
                      version = "October16") { # default and only option
                      ## sanity checks
                      ##
                      if(period < 2000 | period > 2014) {
                          stop(" -> WIOTs are available for the years 2000 till 2014!")
                      }
                      
                      if(!(format %in% c("wide", "long", "list"))) {
                          stop(" -> The only possible format options are 'wide', 'long' or 'list'!")
                      }
                      
                      if(version != "October16") {
                          warning("No other version available. This option is without effect.")
                      }
                      
                      if(!is.logical(as.DT)) {
                          stop(" -> Please specify either TRUE or FALSE for the as.DT-option.")
                      }

                      if((as.DT == FALSE) & (format == "list")) {
                          warning("For format = 'list', as.DT does not have an effect.")
                      }

                      ## WIOT2000_October16_ROW_list.rds
                      base.url <- "http://wiiw.ac.at/files/staff-content/reiter/"

                      res <- readRDS(file = gzcon(url(paste0(base.url, "WIOT", period, "_",
                                                             version, "_ROW",
                                                             ## "_", format,
                                                             ifelse(format == "wide", "",
                                                                    paste0("_", format)),
                                                             ".rds"))))
                          if(format %in% c("wide", "long") & !as.DT) {
                              ## print(format)
                              ## print(as.DT)
                              res <- as.data.frame(res)
                          }

                          return(res)
                      }
 

# Data downloading and naming
 for (a in 2000:2014) 
     { 
      # paste function in R with sep argument
    
    assign(paste("wiot.", a, sep=""),getWIOT(period = a, format = "list"))
 
     }
   

 for (a in 2000:2014) 
     { 

# Year 2000-2014 Data iot to wwz decomposed dataframe
  industries <- 1:56

  b<-get(paste("wiot.", a, sep=""))

  countries <- unique(substring(names(b$x), 1, 3))
  
  assign(paste("decomp", a, sep=""),
                   decomp(x = b$Z,
                   y = b$F,
                   k = countries,
                   i = industries,o = b$x,
                   V = b$v,
                  method = "wwz",
                  verbose = TRUE))
    }
    
  # putting all the years data together
   wwz<-rbind(decomp2000,
              decomp2001,
              decomp2002,
              decomp2003,
              decomp2004,
              decomp2005,
              decomp2006,
              decomp2007,
              decomp2008,
              decomp2009,
              decomp2010,
              decomp2011,
              decomp2012,
              decomp2013,
              decomp2014)
