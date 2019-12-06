## Graphs

# Load Packages
  check.packages <- function(pkg){
				new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
				if (length(new.pkg)) 
				    install.packages(new.pkg, dependencies = TRUE)
				sapply(pkg, require, character.only = TRUE)							}
				# Packages need to be installed
      packages<-c("data.table",
				            "huxtable",
				            "citr",
				            "ggrepel",
				            "tibble",
				            "gganimate",
				            "reshape2",
				            "GGally",
				            "network",
				            "sna",
				            "decompr",
				            "wiod",
				            "gvc",
				            "tidyr",
				            "ggplot2",
				            "readxl",
				            "dplyr",
				            "tidyverse")
				
				check.packages(packages)

# Load Files
	Country_Categories  	<- read_excel("../Country_Categories.xls")
		GDP_World 				<- read_excel("../GDP_World.xls")
		GDPChange_World 		<- read_excel("../GDPChange_World.xlsx")
		BEC 					<- read_excel("../BEC.xlsx")
		CO2 					<- read_excel("../CO2 emissions - Overall.xlsx")
		EREU 					<- read_excel("../EREU.xlsx")
		GDP_perCap			 	<- read_excel("../WorldBank_GDP.xlsx")



	load("F:/Users/qhannn/Desktop/Regression/Operation/VA.RData")
		C_Mean_Power_VA<-C_Mean_Power
		C_Wgted_Power_VA<-C_Wgted_Power

	load("F:/Users/qhannn/Desktop/Regression/Operation/DVA.RData")
		C_Mean_Power_DVA<-C_Mean_Power
		C_Wgted_Power_DVA<-C_Wgted_Power


#VA
  C_Mean_Power_VA_P<-C_Mean_Power_VA [,
  							c('Exporting_Country',
  								'year',
  								'Power_Dir',
  								'Power_Dir_Ind'
  							)
  						]
  C_Wgted_Power_VA_P<-C_Wgted_Power_VA [,
  							c('Exporting_Country',
  								'year',
  								'Power_Dir',
  								'Power_Dir_Ind'
  							)
  						]
  C_Mean_Power_VA_C<-C_Mean_Power_VA [,
  							c('Exporting_Country',
  								'year',
  								'Constraint_Dir',
  								'Constraint_Ind'
  							)
  						]
  C_Wgted_Power_VA_C<-C_Wgted_Power_VA [,
  							c('Exporting_Country',
  								'year',
  								'Constraint_Dir',
  								'Constraint_Ind'
  							)
  						]
  
  C_Wgted_Power_VA_P<-gather(C_Wgted_Power_VA_P,"Power_Type","Power_Value",3:4)
  C_Wgted_Power_VA_C<-gather(C_Wgted_Power_VA_C,"Constraint_Type","Constraint_Value",3:4)
  
  C_Wgted_Power_VA_P <- C_Wgted_Power_VA_P %>%
    mutate(United = ifelse(Power_Type == "Power_Dir", 1,0))%>% na.omit
  C_Wgted_Power_VA_C <- C_Wgted_Power_VA_C %>%
    mutate(United = ifelse(Constraint_Type == "Constraint_Dir", 1,0))%>% na.omit
  C_Wgted_Power_VA<-inner_join(C_Wgted_Power_VA_P, C_Wgted_Power_VA_C, by=c(	"Exporting_Country","year","United"))
  C_Mean_Power_VA_P<-gather(C_Mean_Power_VA_P,"Power_Type","Power_Value",3:4)
  C_Mean_Power_VA_C<-gather(C_Mean_Power_VA_C,"Constraint_Type","Constraint_Value",3:4)
  C_Mean_Power_VA<-inner_join(C_Mean_Power_VA_P, C_Mean_Power_VA_C, by=c(	"Exporting_Country","year"))
    C_Mean_Power_VA_P <- C_Mean_Power_VA_P %>%
    mutate(United = ifelse(Power_Type == "Power_Dir", 1,0))%>% na.omit
  C_Mean_Power_VA_C <- C_Mean_Power_VA_C %>%
    mutate(United = ifelse(Constraint_Type == "Constraint_Dir", 1,0))%>% na.omit
  C_Mean_Power_VA<-inner_join(C_Mean_Power_VA_P, C_Mean_Power_VA_C, by=c(	"Exporting_Country","year","United"))

  C_Wgted_Power_VA<-C_Wgted_Power_VA%>%mutate(Avg_Type="Weighted",Trade="VA")
  
  C_Mean_Power_VA<-C_Mean_Power_VA%>%mutate(Avg_Type="Mean",Trade="VA")
  
  C_Power_VA<-rbind(C_Wgted_Power_VA,C_Mean_Power_VA)

 #DVA
	 	C_Mean_Power_DVA_P<-C_Mean_Power_DVA [,
	  							c('Exporting_Country',
	  								'year',
	  								'Power_Dir',
	  								'Power_Dir_Ind'
	  							)
	  						]
	  C_Wgted_Power_DVA_P<-C_Wgted_Power_DVA [,
	  							c('Exporting_Country',
	  								'year',
	  								'Power_Dir',
	  								'Power_Dir_Ind'
	  							)
	  						]
	  C_Mean_Power_DVA_C<-C_Mean_Power_DVA [,
	  							c('Exporting_Country',
	  								'year',
	  								'Constraint_Dir',
	  								'Constraint_Ind'
	  							)
	  						]
	  C_Wgted_Power_DVA_C<-C_Wgted_Power_DVA [,
	  							c('Exporting_Country',
	  								'year',
	  								'Constraint_Dir',
	  								'Constraint_Ind'
	  							)
	  						]
	  
	  C_Wgted_Power_DVA_P<-gather(C_Wgted_Power_DVA_P,"Power_Type","Power_Value",3:4)
	  C_Wgted_Power_DVA_C<-gather(C_Wgted_Power_DVA_C,"Constraint_Type","Constraint_Value",3:4)
	    C_Wgted_Power_DVA_P <- C_Wgted_Power_DVA_P %>%
	    mutate(United = ifelse(Power_Type == "Power_Dir", 1,0))%>% na.omit
	  C_Wgted_Power_DVA_C <- C_Wgted_Power_DVA_C %>%
	    mutate(United = ifelse(Constraint_Type == "Constraint_Dir", 1,0))%>% na.omit
	  C_Wgted_Power_DVA<-inner_join(C_Wgted_Power_DVA_P, C_Wgted_Power_DVA_C, by=c(	"Exporting_Country","year","United"))
	 
	  C_Mean_Power_DVA_P<-gather(C_Mean_Power_DVA_P,"Power_Type","Power_Value",3:4)
	  C_Mean_Power_DVA_C<-gather(C_Mean_Power_DVA_C,"Constraint_Type","Constraint_Value",3:4)
	   C_Mean_Power_DVA_P <- C_Mean_Power_DVA_P %>%
	    mutate(United = ifelse(Power_Type == "Power_Dir", 1,0))%>% na.omit
	  C_Mean_Power_DVA_C <- C_Mean_Power_DVA_C %>%
	    mutate(United = ifelse(Constraint_Type == "Constraint_Dir", 1,0))%>% na.omit
	  C_Mean_Power_DVA<-inner_join(C_Mean_Power_DVA_P, C_Mean_Power_DVA_C, by=c(	"Exporting_Country","year","United"))
	  rm(C_Wgted_Power_DVA_P,C_Wgted_Power_DVA_C,C_Mean_Power_DVA_P,C_Mean_Power_DVA_C)
	  
	  
	  C_Wgted_Power_DVA<-C_Wgted_Power_DVA%>%mutate(Avg_Type="Weighted",Trade="DVA")
	  
	  C_Mean_Power_DVA<-C_Mean_Power_DVA%>%mutate(Avg_Type="Mean",Trade="DVA")
	  
	  C_Power_DVA<-rbind(C_Wgted_Power_DVA,C_Mean_Power_DVA)
  
#Combine VA and DVA
  
    C_Power<-rbind(C_Power_DVA,C_Power_VA)

  rm(C_Mean_Power_DVA,
     C_Mean_Power_VA,
     C_Mean_Power_DVA_C,
     C_Mean_Power_DVA_P,
     C_Mean_Power_VA_C,
     C_Mean_Power_VA_P,
     C_Wgted_Power_DVA,
     C_Wgted_Power_VA,
     C_Wgted_Power_DVA_C,
     C_Wgted_Power_DVA_P,
     C_Wgted_Power_VA_C,
     C_Wgted_Power_VA_P,
     C_Power_DVA,
     C_Power_VA,Depen_Co)


  	C_Power<-unite(C_Power,Type,3,7,8, remove = FALSE)
  	save.image("F:/../Graph_Data_Shaping.RData")
