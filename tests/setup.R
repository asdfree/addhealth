if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)
my_email_address <- Sys.getenv( "my_email_address" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
lodown( "addhealth" , output_dir = file.path( getwd() ) , 
	your_email = my_email_address , 
	your_password = my_password )
library(lodown)
# examine all available ADDHEALTH microdata files
addhealth_cat <-
	get_catalog( "addhealth" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address , 
		your_password = my_password )

# wave i only
addhealth_cat <- subset( addhealth_cat , wave == "wave i" )
# download the microdata to your local computer




options( survey.lonely.psu = "adjust" )

library(survey)

addhealth_df <- 
	readRDS( 
		file.path( getwd() , 
		"wave i consolidated.rds" ) 
	)

addhealth_design <- 
	svydesign( 
		id = ~cluster2 , 
		data = addhealth_df , 
		weights = ~ gswgt1 , 
		nest = TRUE 
	)
addhealth_design <- 
	update( 
		addhealth_design , 
		
		one = 1 ,
		
		male = as.numeric( as.numeric( bio_sex ) == 1 ) ,
		
		hours_of_computer_games = ifelse( h1da10 > 99 , NA , h1da10 ) ,
		
		hours_of_television = ifelse( h1da8 > 99 , NA , h1da8 )
		
	)
sum( weights( addhealth_design , "sampling" ) != 0 )

svyby( ~ one , ~ h1gh25 , addhealth_design , unwtd.count )
svytotal( ~ one , addhealth_design )

svyby( ~ one , ~ h1gh25 , addhealth_design , svytotal )
svymean( ~ hours_of_computer_games , addhealth_design , na.rm = TRUE )

svyby( ~ hours_of_computer_games , ~ h1gh25 , addhealth_design , svymean , na.rm = TRUE )
svymean( ~ h1gh24 , addhealth_design , na.rm = TRUE )

svyby( ~ h1gh24 , ~ h1gh25 , addhealth_design , svymean , na.rm = TRUE )
svytotal( ~ hours_of_computer_games , addhealth_design , na.rm = TRUE )

svyby( ~ hours_of_computer_games , ~ h1gh25 , addhealth_design , svytotal , na.rm = TRUE )
svytotal( ~ h1gh24 , addhealth_design , na.rm = TRUE )

svyby( ~ h1gh24 , ~ h1gh25 , addhealth_design , svytotal , na.rm = TRUE )
svyquantile( ~ hours_of_computer_games , addhealth_design , 0.5 , na.rm = TRUE )

svyby( 
	~ hours_of_computer_games , 
	~ h1gh25 , 
	addhealth_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ hours_of_computer_games , 
	denominator = ~ hours_of_television , 
	addhealth_design ,
	na.rm = TRUE
)
sub_addhealth_design <- subset( addhealth_design , as.numeric( h1gh1 ) %in% c( 4 , 5 ) )
svymean( ~ hours_of_computer_games , sub_addhealth_design , na.rm = TRUE )
this_result <- svymean( ~ hours_of_computer_games , addhealth_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ hours_of_computer_games , 
		~ h1gh25 , 
		addhealth_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( addhealth_design )
svyvar( ~ hours_of_computer_games , addhealth_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ hours_of_computer_games , addhealth_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ hours_of_computer_games , addhealth_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ male , addhealth_design ,
	method = "likelihood" )
svyttest( hours_of_computer_games ~ male , addhealth_design )
svychisq( 
	~ male + h1gh24 , 
	addhealth_design 
)
glm_result <- 
	svyglm( 
		hours_of_computer_games ~ male + h1gh24 , 
		addhealth_design 
	)

summary( glm_result )
library(srvyr)
addhealth_srvyr_design <- as_survey( addhealth_design )
addhealth_srvyr_design %>%
	summarize( mean = survey_mean( hours_of_computer_games , na.rm = TRUE ) )

addhealth_srvyr_design %>%
	group_by( h1gh25 ) %>%
	summarize( mean = survey_mean( hours_of_computer_games , na.rm = TRUE ) )

result <-
	svymean( 
		~ hours_of_television ,
		addhealth_design ,
		na.rm = TRUE
	)
	
stopifnot( round( coef( result ) , 3 ) == 15.642 )
stopifnot( round( SE( result ) , 4 ) == 0.3902 )

