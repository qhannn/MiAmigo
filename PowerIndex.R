
# This code is Power Index according traditional trade data 
# Clean the memory
	rm(list = ls(all.names = TRUE))
		
		x <- 1

		repeat  {print(gc())
					x = x+1
					if (x ==20){break}
				}
		rm(x)
    
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
        
   
# Loading Wiot 2016 Data
	
load("F:/../WIOT_2016 (2000-2014).RData")
# This data set includes the data between 2000-2014

# !!! for the DVA Calculation add this part
# --  DVA ---
  wwz_clean.filter<-wwz
	rm(wwz)
	wwz_clean.filter$VA<-wwz_clean.filter$DVA+wwz_clean.filter$RDV
	wwz_clean.filter<-wwz_clean.filter%>%select(1:3,30:35)
	wwz_clean.filter<-wwz_clean.filter %>% group_by(Exporting_Country,Importing_Country,
															year) %>% summarise(VA = sum(VA))
# --End of DVA Addition-----------
#_________Data Filtering______________________
	#-- Three Phase Filtering for year, import and export countries.
	# wwz_clean.filter=filter(wwz_clean,wwz_clean$year %in% chosen_year)
	# ---- Clean the Negative numbers  

	wwz_clean.filter<-wwz
	rm(wwz)
	wwz_clean.filter<-wwz_clean.filter%>%select(1:3,30:35)
	wwz_clean.filter<-wwz_clean.filter %>% group_by(Exporting_Country,Importing_Country,
															year) %>% summarise(VA = sum(VA))
# -----**** Data Shaping for Back_VA 
	b1<-unite(wwz_clean.filter,"code",c(Importing_Country,Exporting_Country),remove = FALSE)	
	b1$Back_VA<-b1$VA
		
		names(b1)[names(b1) == 'VA'] 				 <- 'Old_VA'
	
	b2<-unite(wwz_clean.filter,"code",c(Exporting_Country,Importing_Country))

	B2B 				<-merge(x= b2, 
								y= b1, 
								by= c("code","year"), 
									all.x 	= TRUE)	
	B2B 				=B2B[,!grepl("No.x",names(B2B))]
	
	wwz_clean.filter 	<-B2B

		rm(b1,b2,B2B)
    
 
# ------- Country Level Dependency -------------
	# --- D1-Forward  (Updated) ----  Country Level "Exporting_Country,year"
		# Exporting Country Total VA Calculation
			Co.group_ForEx<-wwz_clean.filter %>% group_by(Exporting_Country,
															year) %>% summarise(Total_Ex_CoVA = sum(VA))
	 		

	 		Co.group_ForEx<-Co.group_ForEx[complete.cases(Co.group_ForEx), ]
			
			Co.group_ForImp<-wwz_clean.filter %>% group_by(Importing_Country,
																year) %>% summarise(Total_Imp_CoVA = sum(VA))
			
			Co.group_ForImp<-Co.group_ForImp[complete.cases(Co.group_ForImp), ]
	
		# Two country Total VA 
			Co2.group_ForEx<-wwz_clean.filter %>% group_by(	Exporting_Country,
	 															Importing_Country,
																year) %>% summarise(Total_Ex_Co2VA = sum(VA))
	 		

	 		Co2.group_ForEx<-Co2.group_ForEx[complete.cases(	Co2.group_ForEx), ]
			


			Co2.group_ForImp<-wwz_clean.filter %>% group_by(	Importing_Country,
																Exporting_Country,
																year) %>% summarise(Total_Imp_Co2VA = sum(VA))
			
			Co2.group_ForImp<-Co2.group_ForImp[complete.cases(Co2.group_ForImp), ]
		
		# Combining Aggregate VA VAlues for country pairs and individual 
			
			depen_ForCoImp  <-inner_join(wwz_clean.filter,
										 Co.group_ForImp, 
										 by=c(	"Importing_Country", 
												"year"))
			depen_ForCoExp  <-inner_join(wwz_clean.filter,
										 Co.group_ForEx, 
										 by=c(	"Exporting_Country", 
												"year"))
			depen_ForCo<-cbind(depen_ForCoImp,depen_ForCoExp$Total_Ex_CoVA)
			depen_ForCo2 	<-merge(	x = Co2.group_ForImp, 
										y = Co2.group_ForEx, 
										by.x = c(	"Importing_Country",
													"Exporting_Country",
													"year"),  
										by.y = c(	"Exporting_Country",
													"Importing_Country",
													"year"),  
										all = TRUE)
			depen_ForCo  <-inner_join(		depen_ForCo,
											depen_ForCo2, 
											 by=c(	"Importing_Country", 
										 			"Exporting_Country",
													"year"))	
                          
     # --- D1-Backward  (Updated) ----  Country Level "Country,year"
		# Exporting Country Total Back_VA Calculation
			Co.group_BackEx<-wwz_clean.filter %>% group_by(	Exporting_Country,
																year) %>% summarise(Total_Ex_BackCoVA = sum(Back_VA))
	 		Co.group_BackEx<-Co.group_BackEx[complete.cases(	Co.group_BackEx), ]
			Co.group_BackImp<-wwz_clean.filter %>% group_by(	Importing_Country,
																year) %>% summarise(Total_Imp_BackCoVA = sum(Back_VA))
			Co.group_BackImp<-Co.group_BackImp[complete.cases(	Co.group_BackImp), ]
		# Two country Total Back_VA 
			Co2.group_BackEx<-wwz_clean.filter %>% group_by(	Exporting_Country,
	 															Importing_Country,
																year) %>% summarise(Total_Ex_BackCo2VA = sum(Back_VA))
	 		Co2.group_BackEx<-Co2.group_BackEx[complete.cases(	Co2.group_BackEx), ]
			Co2.group_BackImp<-wwz_clean.filter %>% group_by(	Importing_Country,
																Exporting_Country,
																year) %>% summarise(Total_Imp_BackCo2VA = sum(Back_VA))
			Co2.group_BackImp<-Co2.group_BackImp[complete.cases(Co2.group_BackImp), ]
		# Combining Aggregate Back_VA VAlues for country pairs and individual 
			depen_BackCoImp  		<-inner_join(wwz_clean.filter,
										 		Co.group_BackImp, 
										 by=c(	"Importing_Country", 
												"year"))
			depen_BackCoExp 	<-inner_join(wwz_clean.filter,
											Co.group_BackEx, 
											by = c(	"Exporting_Country",
													"year"))
			depen_BackCo<-cbind(depen_BackCoImp,depen_BackCoExp$Total_Ex_BackCoVA)
			depen_BackCo2 	<-merge(	x = Co2.group_BackImp, 
										y = Co2.group_BackEx, 
										by.x = c(	"Importing_Country",
													"Exporting_Country",
													"year"),  
										by.y = c(	"Exporting_Country",
													"Importing_Country",
													"year"),  
										all = TRUE)
			

			depen_BackCo 	<-inner_join(	depen_BackCo, 
											depen_BackCo2, 
											by = c(	"Importing_Country",
													"Exporting_Country",
													"year"))
			names(depen_ForCo)[names(depen_ForCo) 	== 'depen_ForCoExp$Total_Ex_CoVA'] 			<- 'Total_Ex_CoVA'
			names(depen_BackCo)[names(depen_BackCo) == 'depen_BackCoExp$Total_Ex_BackCoVA'] 		<- 'Total_Ex_BackCoVA'
			
			Depen_Co<-cbind(depen_ForCo,
							depen_BackCo$Total_Imp_BackCoVA,
							depen_BackCo$Total_Imp_BackCo2VA,
							depen_BackCo$Total_Ex_BackCoVA,
							depen_BackCo$Total_Ex_BackCo2VA
							)
			
			# ***Forward DepCoency Couct Level Final Formula***
			names(Depen_Co)[names(Depen_Co) == 'depen_BackCo$Total_Imp_BackCoVA'] 			<- 'Total_Imp_BackCoVA'
			names(Depen_Co)[names(Depen_Co) == 'depen_BackCo$Total_Imp_BackCo2VA'] 		<- 'Total_Imp_BackCo2VA'
			names(Depen_Co)[names(Depen_Co) == 'depen_BackCo$Total_Ex_BackCoVA'] 			<- 'Total_Ex_BackCoVA'
			names(Depen_Co)[names(Depen_Co) == 'depen_BackCo$Total_Ex_BackCo2VA'] 			<- 'Total_Ex_BackCo2VA'


			# Substracting the VA from the Totals
			Depen_Co$Total_Ex_CoVA<-Depen_Co$Total_Ex_CoVA-Depen_Co$Total_Ex_Co2VA
			Depen_Co$Total_Imp_CoVA<-Depen_Co$Total_Imp_CoVA-Depen_Co$Total_Ex_Co2VA	
			# Substracting the VA from the Totals
			Depen_Co$Total_Ex_BackCoVA<-Depen_Co$Total_Ex_BackCoVA-Depen_Co$Total_Imp_Co2VA
			Depen_Co$Total_Imp_BackCoVA<-Depen_Co$Total_Imp_BackCoVA-Depen_Co$Total_Imp_Co2VA

			
			Depen_Co$Co_Dep_For_Exp 	<-Depen_Co$Total_Imp_CoVA/Depen_Co$Total_Ex_CoVA
			Depen_Co$Co_Dep_For_Imp 	<-Depen_Co$Total_Ex_CoVA/Depen_Co$Total_Imp_CoVA
			Depen_Co$Co_Dep_Back_Exp 	<-Depen_Co$Total_Imp_BackCoVA/Depen_Co$Total_Ex_BackCoVA
			Depen_Co$Co_Dep_Back_Imp 	<-Depen_Co$Total_Ex_BackCoVA/Depen_Co$Total_Imp_BackCoVA
		

	# ---- Choosing the higher depCoency factor among forward & backward depCoencies ----	
		Depen_Co$Co_Dep_For_Exp[is.na(Depen_Co$Co_Dep_For_Exp)] 	<- 0
		Depen_Co$Co_Dep_Back_Exp[is.na(Depen_Co$Co_Dep_Back_Exp)] 	<- 0
		Depen_Co$Co_Dep_For_Imp[is.na(Depen_Co$Co_Dep_For_Imp)] 	<- 0
		Depen_Co$Co_Dep_Back_Imp[is.na(Depen_Co$Co_Dep_Back_Imp)] 	<- 0
    
   # Data Shaping for Direct Power Index
		Depen_Co 		<-Depen_Co %>%separate(year, into = c("century", "year"), sep = 1)
		Depen_Co 		=Depen_Co[,!grepl("century",names(Depen_Co))]
		Depen_Co$year 	<-as.numeric(as.character(Depen_Co$year))
		Depen_Co<-Depen_Co [,
							c(	'Exporting_Country',
								'Importing_Country',
								'year',
								'Co_Dep_For_Exp',
								'Co_Dep_For_Imp',
								'Co_Dep_Back_Exp',
								'Co_Dep_Back_Imp',
								'Total_Ex_CoVA',
								'Total_Imp_CoVA'
							)
						]
		names(Depen_Co)[names(Depen_Co) 	== 'Co_Dep_For_Imp'] 			<- 'Power_Export_VA'
		names(Depen_Co)[names(Depen_Co) == 'Co_Dep_Back_Imp'] 		<- 'Constraint_Import_VA'
		names(Depen_Co)[names(Depen_Co) 	== 'Co_Dep_Back_Exp'] 			<- 'Power_Import_VA'
		names(Depen_Co)[names(Depen_Co) == 'Co_Dep_For_Exp'] 		<- 'Constraint_Export_VA'


	# Section for indirect calculations
		Depen_Co<-Depen_Co%>% group_by(Exporting_Country,year)%>%mutate(Total_PowerExp_VA=sum(Power_Export_VA))
		Depen_Co<-Depen_Co%>% group_by(Importing_Country,year)%>%mutate(Total_PowerImp_VA=sum(Power_Import_VA))
		Depen_Co<-Depen_Co%>%group_by(Exporting_Country,Importing_Country,year)%>% mutate(Power_Export_VA=Power_Export_VA/Total_PowerExp_VA)
		Depen_Co<-Depen_Co%>%group_by(Exporting_Country,Importing_Country,year)%>% mutate(Power_Import_VA=Power_Import_VA/Total_PowerImp_VA)
		Depen_Co<-Depen_Co%>% group_by(year)%>%mutate(Total_ConstraintExp_VA=sum(Constraint_Export_VA))
		Depen_Co<-Depen_Co%>%group_by(Exporting_Country,Importing_Country,year)%>% mutate(Constraint_Export_VA=Constraint_Export_VA/Total_ConstraintExp_VA)
		Depen_Co<-Depen_Co%>% group_by(year)%>%mutate(Total_ConstraintImp_VA=sum(Constraint_Import_VA))
		Depen_Co<-Depen_Co%>%group_by(Exporting_Country,Importing_Country,year)%>% mutate(Constraint_Import_VA=Constraint_Import_VA/Total_ConstraintImp_VA)
		
	# Indirect Power : Power_Export_VA
	  b<-list()
	  for (a in 2000:2014) 
		 {   
			
			Power<-filter(Depen_Co,year %in% c(a),)
			Power<-Power [,
								c(	'Exporting_Country',
									'Importing_Country',
									'Power_Export_VA'
								)
							]
			# Dataframe to matrix https://stackoverflow.com/questions/46562173/creating-a-square-matrix-from-a-data-frame				]

			## set up storage matrix
			# get names for row and columns
			nameVals <- sort(unique(unlist(Power[1:2])))
			# construct 0 matrix of correct dimensions with row and column names
			Exp <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))

			# fill in the matrix with matrix indexing on row and column names
			Exp[as.matrix(Power[c("Exporting_Country", "Importing_Country")])] <- Power[["Power_Export_VA"]]	
			x<-diag(1,44,44) - Exp
		 	Indirect_Power<-t(Exp) * Exp 
		 	Indirect_Power<-setNames(melt(Indirect_Power), c('Exporting_Country', 'Importing_Country', 'Indirect_Power_Exp'))

		 	Indirect_Power$year<-a
			
			b<-rbind(b,Indirect_Power)
		 }

			Depen_Co 	<-inner_join(	b, 
											Depen_Co, 
											by = c(	"Importing_Country",
													"Exporting_Country",
													"year"))
	# Indirect Power : Power_Import_VA
		
		b<-list()

	  for (a in 2000:2014) 
		 {   
			Power<-filter(Depen_Co,year %in% c(a),)
			Power<-Power [,
								c(	'Exporting_Country',
									'Importing_Country',
									'Power_Import_VA'
								)
							]
			# Dataframe to matrix https://stackoverflow.com/questions/46562173/creating-a-square-matrix-from-a-data-frame				]

			## set up storage matrix
			# get names for row and columns
			nameVals <- sort(unique(unlist(Power[1:2])))
			# construct 0 matrix of correct dimensions with row and column names
			Imp <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))

			# fill in the matrix with matrix indexing on row and column names
			Imp[as.matrix(Power[c("Exporting_Country", "Importing_Country")])] <- Power[["Power_Import_VA"]]	
			x<-diag(1,44,44) - Imp
		 	Indirect_Power<-t(Imp) * Imp 
		 	Indirect_Power<-setNames(melt(Indirect_Power), c('Exporting_Country', 'Importing_Country', 'Indirect_Power_Imp'))

		 	Indirect_Power$year<-a
			
			b<-rbind(b,Indirect_Power)
		 }

			
		 Depen_Co 	<-inner_join(	b, 
											Depen_Co, 
											by = c(	"Importing_Country",
													"Exporting_Country",
													"year"))

			# Depen_Co1<-Depen_Co[-which(Depen_Co$Exporting_Country == Depen_Co$Importing_Country), ]
	# Indirect Power : Constraint_Export_VA
		
		b<-list()

	  for (a in 2000:2014) 
		 {   
			
			Power<-filter(Depen_Co,year %in% c(a),)
			Power<-Power [,
								c(	'Exporting_Country',
									'Importing_Country',
									'Constraint_Export_VA'
								)
							]
			# Dataframe to matrix https://stackoverflow.com/questions/46562173/creating-a-square-matrix-from-a-data-frame				]

			## set up storage matrix
			# get names for row and columns
			nameVals <- sort(unique(unlist(Power[1:2])))
			# construct 0 matrix of correct dimensions with row and column names
			Exp_c <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))

			# fill in the matrix with matrix indexing on row and column names
			Exp_c[as.matrix(Power[c("Exporting_Country", "Importing_Country")])] <- Power[["Constraint_Export_VA"]]	
			x<-diag(1,44,44) - Exp_c
		 	Indirect_Power<-t(Exp_c) * Exp_c 
		 	Indirect_Power<-setNames(melt(Indirect_Power), c('Exporting_Country', 'Importing_Country', 'Indirect_Cons_Exp'))

		 	Indirect_Power$year<-a
			
			b<-rbind(b,Indirect_Power)
		 }

			 Depen_Co 	<-inner_join(	b, 
											Depen_Co, 
											by = c(	"Importing_Country",
													"Exporting_Country",
													"year"))		
	# Indirect Power : Constraint_Import_VA
	 b<-list()
	 
	  for (a in 2000:2014) 
		 {   
			
			Power<-filter(Depen_Co,year %in% c(a),)
			Power<-Power [,
								c(	'Exporting_Country',
									'Importing_Country',
									'Constraint_Import_VA'
								)
							]
			# Dataframe to matrix https://stackoverflow.com/questions/46562173/creating-a-square-matrix-from-a-data-frame				]

			## set up storage matrix
			# get names for row and columns
			nameVals <- sort(unique(unlist(Power[1:2])))
			# construct 0 matrix of correct dimensions with row and column names
			Imp2 <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))

			# fill in the matrix with matrix indexing on row and column names
			Imp2[as.matrix(Power[c("Exporting_Country", "Importing_Country")])] <- Power[["Constraint_Import_VA"]]	
			x<-diag(1,44,44) - Imp2
		 	Indirect_Power<-t(Imp2) * Imp2 
		 	Indirect_Power<-setNames(melt(Indirect_Power), c('Exporting_Country', 'Importing_Country', 'Indirect_Cons_Imp'))

		 	Indirect_Power$year<-a
			
			b<-rbind(b,Indirect_Power)
		 }

			
		 	 Depen_Co 	<-inner_join(	b, 
											Depen_Co, 
											by = c(	"Importing_Country",
													"Exporting_Country",
													"year"))

			Depen_Co<-Depen_Co[-which(Depen_Co$Exporting_Country == Depen_Co$Importing_Country), ]
		
		Depen_Co<-Depen_Co [,
							c(	'Exporting_Country',
								'Importing_Country',
								'year',
								'Indirect_Cons_Imp',
								'Indirect_Cons_Exp',
								'Indirect_Power_Imp',
								'Indirect_Power_Exp',
								'Constraint_Import_VA',
								'Constraint_Export_VA',
								'Power_Export_VA',
								'Power_Import_VA',
								'Total_Ex_BackCo2VA',
								'Total_Imp_BackCo2VA'
							)
						]
		rm( x,b,
			depen_BackCo,
			depen_BackCoExp,
			depen_ForCoImp,
			depen_ForCoExp,
			depen_BackCoImp,
			Co.group_ForEx,
			Co.group_ForImp,
			Co.group_BackImp,
			Co.group_BackEx,
			Co2.group_BackImp,
			Co2.group_BackEx,
			Co2.group_ForImp,
			Co2.group_ForEx,
			depen_ForCo,
			depen_BackCo2,
			depen_ForCo2,
			Exp,Exp_c,Imp,Imp2,a,nameVals,Indirect_Power,Power,wwz_clean.filter)
      
      # Burt Power Formulation : Adaptation of Ppwer index into the networks	
			Depen_Co<-Depen_Co%>%mutate(Power_Dir_Exp=(Power_Export_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Power_Dir_Ind_Exp=(Indirect_Power_Exp+Power_Export_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Constraint_Dir_Exp=(Constraint_Export_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Constraint_Dir_Ind_Exp=(Indirect_Cons_Exp+Constraint_Export_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Power_Dir_Imp=(Power_Import_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Power_Dir_Ind_Imp=(Indirect_Power_Imp+Power_Import_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Constraint_Dir_Imp=(Constraint_Import_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Constraint_Dir_Ind_Imp=(Indirect_Cons_Imp+Constraint_Import_VA)^2)
			Depen_Co<-Depen_Co%>%mutate(Power_Dir=log(Power_Dir_Exp)-log(Power_Dir_Imp))
			Depen_Co<-Depen_Co%>%mutate(Power_Dir_Ind=log(Power_Dir_Ind_Exp)-log(Power_Dir_Ind_Imp))
			Depen_Co<-Depen_Co%>%mutate(Constraint_Dir=log(Constraint_Dir_Exp)-log(Constraint_Dir_Imp))
			Depen_Co<-Depen_Co%>%mutate(Constraint_Ind=log(Constraint_Dir_Ind_Exp)-log(Constraint_Dir_Ind_Imp))
			
			C2C_Power<-Depen_Co
      
     
# One Country Power Index
	# Weighted Average
		C_Wgted_Power<-Depen_Co %>% group_by(Exporting_Country,year) %>% summarise(	Power_Dir_Exp = weighted.mean(Power_Dir_Exp,Total_Imp_CoVA),
																						Power_Dir_Ind_Exp = weighted.mean(Power_Dir_Ind_Exp,Total_Imp_CoVA),
																						Constraint_Dir_Exp = weighted.mean(Constraint_Dir_Exp,Total_Imp_CoVA),
																						Constraint_Dir_Ind_Exp = weighted.mean(Constraint_Dir_Ind_Exp,Total_Imp_CoVA),
																						Power_Dir_Imp = weighted.mean(Power_Dir_Imp,Total_Ex_CoVA),
																						Power_Dir_Ind_Imp = weighted.mean(Power_Dir_Ind_Imp,Total_Ex_CoVA),
																						Constraint_Dir_Imp = weighted.mean(Constraint_Dir_Imp,Total_Ex_CoVA),
																						Constraint_Dir_Ind_Imp = weighted.mean(Constraint_Dir_Ind_Imp,Total_Ex_CoVA)
																					)
		
		C_Wgted_Power<-C_Wgted_Power%>%mutate(Power_Dir=log(Power_Dir_Exp)-log(Power_Dir_Imp))
		C_Wgted_Power<-C_Wgted_Power%>%mutate(Power_Dir_Ind=log(Power_Dir_Ind_Exp)-log(Power_Dir_Ind_Imp))
		C_Wgted_Power<-C_Wgted_Power%>%mutate(Constraint_Dir=log(Constraint_Dir_Exp)-log(Constraint_Dir_Imp))
		C_Wgted_Power<-C_Wgted_Power%>%mutate(Constraint_Ind=log(Constraint_Dir_Ind_Exp)-log(Constraint_Dir_Ind_Imp))
	
  # Mean
		C_Mean_Power<-Depen_Co %>% group_by(Exporting_Country,year) %>% summarise(	Power_Dir_Exp = mean(Power_Dir_Exp),
																						Power_Dir_Ind_Exp = mean(Power_Dir_Ind_Exp),
																						Constraint_Dir_Exp = mean(Constraint_Dir_Exp),
																						Constraint_Dir_Ind_Exp = mean(Constraint_Dir_Ind_Exp),
																						Power_Dir_Imp = mean(Power_Dir_Imp),
																						Power_Dir_Ind_Imp = mean(Power_Dir_Ind_Imp),
																						Constraint_Dir_Imp = mean(Constraint_Dir_Imp),
																						Constraint_Dir_Ind_Imp = mean(Constraint_Dir_Ind_Imp))

		C_Mean_Power<-C_Mean_Power%>%mutate(Power_Dir=log(Power_Dir_Exp)-log(Power_Dir_Imp))
		C_Mean_Power<-C_Mean_Power%>%mutate(Power_Dir_Ind=log(Power_Dir_Ind_Exp)-log(Power_Dir_Ind_Imp))
		C_Mean_Power<-C_Mean_Power%>%mutate(Constraint_Dir=log(Constraint_Dir_Exp)-log(Constraint_Dir_Imp))
		C_Mean_Power<-C_Mean_Power%>%mutate(Constraint_Ind=log(Constraint_Dir_Ind_Exp)-log(Constraint_Dir_Ind_Imp))
    
  # Final
	save.image("F:/Users/qhannn/Desktop/Regression/Operation/VA.RData")
