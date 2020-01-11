# Clean the memory, Dont forget to add the packages !!
			rm(list = ls(all.names = TRUE))
				x <- 1

				repeat  {print(gc())
							x = x+1
							if (x ==20){break}
						}
				rm(x)
# input Stata file

check.packages <- function(pkg){
			new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
			if (length(new.pkg)) 
			    install.packages(new.pkg, dependencies = TRUE)
			sapply(pkg, require, character.only = TRUE)
									}
			# Packages need to be installed
			packages<-c("foreign","tibble","gganimate","reshape2", "GGally", "network", "sna", "decompr","wiod","gvc","tidyr","ggplot2","readxl","dplyr","tidyverse")
			check.packages(packages)

# Load the Bilateral Data
# Source: https://cid.econ.ucdavis.edu/nberus.html
	wtf_bilat <- read.dta("F:/Users/qhannn/Downloads/wtf_bilat/wtf_bilat.dta")

# Shape the data 
	wtf_bilat_long<-gather(wtf_bilat, "year", "value", 5:43)

	wtf_bilat_long 		<-wtf_bilat_long %>%separate(year, into = c("century", "year"), sep = 5)
	wtf_bilat_long$century[wtf_bilat_long$century=="value"]<-"19"

	wtf_bilat_long<-unite(wtf_bilat_long,"year",c(century,year),remove = TRUE,sep="")	
	wtf_bilat_long$year[wtf_bilat_long$year=="1900"]<-"2000"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="Czech Rep"]<-"Czech Republic"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Czech Rep"]<-"Czech Republic"
	
	wtf_bilat_long$importer[wtf_bilat_long$importer=="USA"]<-"United States"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="USA"]<-"United States"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="UK"]<-"United Kingdom"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="UK"]<-"United Kingdom"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="France,Monac"]<-"France"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="France,Monac"]<-"France"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="Russian Fed"]<-"Russian Federation"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Russian Fed"]<-"Russian Federation"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="Fm German FR"]<-"Germany"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Fm German FR"]<-"Germany"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="Korea Rep."]<-"Korea, Rep."
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Korea Rep."]<-"Korea, Rep."

	wtf_bilat_long$importer[wtf_bilat_long$importer=="Slovakia"]<-"Slovak Republic"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Slovakia"]<-"Slovak Republic"

	wtf_bilat_long$importer[wtf_bilat_long$importer=="Belgium-Lux"]<-"Belgium"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Belgium-Lux"]<-"Belgium"
	
	wtf_bilat_long$importer[wtf_bilat_long$importer=="Switz.Liecht"]<-"Switzerland"
	wtf_bilat_long$exporter[wtf_bilat_long$exporter=="Switz.Liecht"]<-"Switzerland"

	wtf_bilat_long<-filter(wtf_bilat_long,!importer%in% c("World"))
	wtf_bilat_long<-filter(wtf_bilat_long,!exporter%in% c("World"))

# 3-digit country codes with country names 
GDP_World 	<- read_excel("F:/Users/qhannn/Desktop/Project Dependency/Setup Codes/DATA/Wiot/Other Data (Excel)/GDP_World.xls")

	names(GDP_World)[names(GDP_World) 	== 'Country Name'] 			<- 'Country_Name'
	names(GDP_World)[names(GDP_World) 	== 'Country Code'] 			<- 'Country_Code'

GDP_World<-GDP_World [,
							c(	'Country_Name',
								'Country_Code'
							)
						]
	

	bilateral 	<-merge(	x = wtf_bilat_long, 
											y = GDP_World, 
											by.x = c("importer"),  
	 										by.y = c("Country_Name"),  
											all = TRUE)
	bilateral<-bilateral [,
							c(	'importer',
								'exporter',
								'year',
								'Country_Code',
								'value'
							)
						]
	bilateral2 	<-merge(	x = bilateral, 
											y = GDP_World, 
											by.x = c("exporter"),  
	 										by.y = c("Country_Name"),  
											all = TRUE)

	bilateral2<-bilateral2 [,
							c(	'importer',
								'exporter',
								'year',
								'Country_Code.x',
								'Country_Code.y',
								'value'
							)
						]

			Korea<-filter(bilateral2,Country_Code.x%in% c("KOR"))
				
# Load the 44 countries for equalizing the data


# 44 Countries and the rest of the world

	names(bilateral2)[names(bilateral2) 	== 'Country_Code.x'] 			<- 'Importing_Country'
	names(bilateral2)[names(bilateral2) 	== 'Country_Code.y'] 			<- 'Exporting_Country'

	load("F:/Users/qhannn/Desktop/Regression/Countries.RData")

	Depen_Co$Importing_Country[Depen_Co$Importing_Country=="Luxemburg"]<-"Belgium"

	ExpoCo <-(unique(Depen_Co$Importing_Country))
	ac<-as.character(ExpoCo)
	



	bilateral2$Exporting_Country <- ifelse(bilateral2$Exporting_Country %in% ac, bilateral2$Exporting_Country, "ROW")
	bilateral2$Importing_Country <- ifelse(bilateral2$Importing_Country %in% ac, bilateral2$Importing_Country, "ROW")
	names(bilateral2)[names(bilateral2) 	== 'value'] 			<- 'VA'

	bilateral2<-bilateral2 [,
							c(	'year',
								'Exporting_Country',
								'Importing_Country',
								'VA'
							)
						]
	save.image("F:/Users/qhannn/Desktop/Regression/VA&DVA/Step1(Bilateral).RData")	

# Clean the memory and Load Step 3, Dont forget to add the packages !!
			rm(list = ls(all.names = TRUE))
				x <- 1

				repeat  {print(gc())
							x = x+1
							if (x ==20){break}
						}
				rm(x)
	
	load("F:/Users/qhannn/Desktop/Regression/VA&DVA/Step1(Bilateral).RData")
