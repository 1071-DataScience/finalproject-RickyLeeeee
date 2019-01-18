parseCmdArgs <- function( pArgs ) {
  
  refinedCAGs <- list(
    inputFile = NaN, 
    outputFile = NaN
  )
  
  if (length(pArgs)==0) {
    cat('USAGE: Rscript rscriptName.R --input inputfileName.csv --output outputfileName.csv\n' )
    stop('Terminating The Session ...\n\n', call.=FALSE)
  }
  
  # parse parameters
  i <- 1
  while( i < length(pArgs) )
  {
	if( pArgs[[ i ]] == "--input" ){
		refinedCAGs$inputFile <- pArgs[[ i + 1 ]]
		i <- i + 1
	}else if( pArgs[[ i ]] == "--output" ){
		refinedCAGs$outputFile <- pArgs[[ i + 1 ]]
		i <- i + 1
	}else{
		stop( paste( "Unknown flag: \n", pArgs[[ i ]] ), call.=FALSE )
	}
	i <- i + 1

  }
  
  #Print some log to realize if the previous parsing was done well to what extent.
  cat('input file path: \n')
  print(refinedCAGs$inputFile)
  cat('\n')
  cat('output file path: \n')
  print(refinedCAGs$outputFile)
  cat('\n')

  return ( refinedCAGs )
  
}

calculateNO2AQI <- function(ConcentrationVal){
	result <- NaN
	BaseBound <- c( 0, 54, 101, 361, 650, 1250, 1650 )
	TopBound <- c( 53, 100, 360, 649, 1249, 1649, 2049 )
	AQIBound <- list( r1 = c(0,50), r2 = c(51,100), r3 = c(101,150), r4 = c(151,200), r5 = c(201,300), r6 = c(301,400), r7 = c(401,500) )
	for (i in c( 1:length(TopBound) ) ) {
	  if( ConcentrationVal >= BaseBound[[ i ]] && ConcentrationVal <= TopBound[[ i ]] ){
	    result <- ( (AQIBound[[ i ]][[ 2 ]]- AQIBound[[ i ]][[ 1 ]]) / (TopBound[[ i ]] - BaseBound[[ i ]]) ) * (ConcentrationVal - BaseBound[[ i ]]) + AQIBound[[ i ]][[ 1 ]]
	    break
	  }
	}

	return( result )
}

calculateSO2AQI <- function(ConcentrationVal){
	result <- NaN
	BaseBound <- c( 0, 36, 76, 186, 305, 605, 805 )
	TopBound <- c( 35, 75, 185, 304, 604, 804, 1004 )
	AQIBound <- list( r1 = c(0,50), r2 = c(51,100), r3 = c(101,150), r4 = c(151,200), r5 = c(201,300), r6 = c(301,400), r7 = c(401,500) )
	for (i in c( 1:length(TopBound) ) ) {
	  if( ConcentrationVal >= BaseBound[[ i ]] && ConcentrationVal <= TopBound[[ i ]] ){
	    result <- ( (AQIBound[[ i ]][[ 2 ]]- AQIBound[[ i ]][[ 1 ]]) / (TopBound[[ i ]] - BaseBound[[ i ]]) ) * (ConcentrationVal - BaseBound[[ i ]]) + AQIBound[[ i ]][[ 1 ]]
	    break
	  }
	}

	return( result )
}

calculateCOAQI <- function(ConcentrationVal){
	result <- NaN
	BaseBound <- c( 0, 4.5, 9.5, 12.5, 15.5, 30.5, 40.5 )
	TopBound <- c( 4.4, 9.4, 12.4, 15.4, 30.4, 40.4, 50.4 )
	AQIBound <- list( r1 = c(0,50), r2 = c(51,100), r3 = c(101,150), r4 = c(151,200), r5 = c(201,300), r6 = c(301,400), r7 = c(401,500) )
	for (i in c( 1:length(TopBound) ) ) {
	  if( ConcentrationVal >= BaseBound[[ i ]] && ConcentrationVal <= TopBound[[ i ]] ){
	    result <- ( (AQIBound[[ i ]][[ 2 ]]- AQIBound[[ i ]][[ 1 ]]) / (TopBound[[ i ]] - BaseBound[[ i ]]) ) * (ConcentrationVal - BaseBound[[ i ]]) + AQIBound[[ i ]][[ 1 ]]
	    break
	  }
	}

	return( result )
}

calculatePM25AQI <- function(ConcentrationVal){
	result <- NaN
	BaseBound <- c( 0, 15.5, 35.5, 54.5, 150.5, 250.5, 350.5)
	TopBound <- c( 15.4, 35.4, 54.4, 150.4, 250.4, 350.4, 500.4 )
	AQIBound <- list( r1 = c(0,50), r2 = c(51,100), r3 = c(101,150), r4 = c(151,200), r5 = c(201,300), r6 = c(301,400), r7 = c(401,500) )
	for (i in c( 1:length(TopBound) ) ) {
	  if( ConcentrationVal >= BaseBound[[ i ]] && ConcentrationVal <= TopBound[[ i ]] ){
	    result <- ( (AQIBound[[ i ]][[ 2 ]]- AQIBound[[ i ]][[ 1 ]]) / (TopBound[[ i ]] - BaseBound[[ i ]]) ) * (ConcentrationVal - BaseBound[[ i ]]) + AQIBound[[ i ]][[ 1 ]]
	    break
	  }
	}

	return( result )
}

calculatePM10AQI <- function(ConcentrationVal){
	result <- NaN
	BaseBound <- c( 0, 55, 126, 255, 355, 425, 505 )
	TopBound <- c( 54, 125, 254, 354, 424, 504, 604 )
	AQIBound <- list( r1 = c(0,50), r2 = c(51,100), r3 = c(101,150), r4 = c(151,200), r5 = c(201,300), r6 = c(301,400), r7 = c(401,500) )
	for (i in c( 1:length(TopBound) ) ) {
	  if( ConcentrationVal >= BaseBound[[ i ]] && ConcentrationVal <= TopBound[[ i ]] ){
	    result <- ( (AQIBound[[ i ]][[ 2 ]]- AQIBound[[ i ]][[ 1 ]]) / (TopBound[[ i ]] - BaseBound[[ i ]]) ) * (ConcentrationVal - BaseBound[[ i ]]) + AQIBound[[ i ]][[ 1 ]]
	    break
	  }
	}

	return( result )
}

calculateO3AQI <- function(ConcentrationVal){
	result <- NaN
	levelInd <- NaN
	lowINV <- list( r1 = c(0.000,0.054), r2 = c(0.055,0.070) )
	upINV <- list( r1 = c(0.405,0.504), r2 = c(0.505,0.604) )
	midINV <- list( r1 = list( a = c(0.071, 0.085), b= c(0.125, 0.164) ), 
					r2 = list( a = c(0.086, 0.105), b= c(0.165, 0.204) ), 
					r3 = list( a = c(0.106, 0.200), b= c(0.205, 0.404) ) )

	AQIBound <- list( r1 = c(0,50), r2 = c(51,100), r3 = c(101,150), r4 = c(151,200), r5 = c(201,300), r6 = c(301,400), r7 = c(401,500) )
	
	if( ConcentrationVal > lowINV[[2]][[2]] &&  ConcentrationVal< upINV[[1]][[1]] ){
		d1Ind <- NaN
		d2Ind <- NaN
		levelInd <- 3
		for ( i in c( 1:length(midINV) ) ) {
			d1Ind <- i
			
			if( ConcentrationVal >= midINV[[i]][[1]][[1]] && ConcentrationVal <= midINV[[i]][[1]][[2]] ) {
				d2Ind <- 1
				break
			} else if ( ConcentrationVal >= midINV[[i]][[2]][[1]] && ConcentrationVal <= midINV[[i]][[2]][[2]] ) {
				d2Ind <- 2
				break
			}
			levelInd <- levelInd + 1
		}

		result <- ( (AQIBound[[ levelInd ]][[ 2 ]]- AQIBound[[ levelInd ]][[ 1 ]]) / (midINV[[d1Ind]][[d2Ind]][[2]] - midINV[[d1Ind]][[d2Ind]][[1]]) ) * (ConcentrationVal - midINV[[d1Ind]][[d2Ind]][[1]]) + AQIBound[[ levelInd ]][[ 1 ]]
	
	}else if(ConcentrationVal <= lowINV[[2]][[2]]){
		levelInd <- 1
		for (i in c( 1:length(lowINV) ) ) {
			if( ConcentrationVal >= lowINV[[i]][[1]] && ConcentrationVal <= lowINV[[i]][[2]] ) {
				result <- ( (AQIBound[[ levelInd ]][[ 2 ]]- AQIBound[[ levelInd ]][[ 1 ]]) / (lowINV[[i]][[2]] - lowINV[[i]][[1]]) ) * (ConcentrationVal - lowINV[[i]][[1]]) + AQIBound[[ levelInd ]][[ 1 ]]
				break
			}

			levelInd <- levelInd + 1
		}

	}else if(ConcentrationVal >= upINV[[1]][[1]]){
		levelInd <- 6
		for (i in c( 1:length(upINV) ) ) {
			if( ConcentrationVal >= upINV[[i]][[1]] && ConcentrationVal <= upINV[[i]][[2]] ) {
				result <- ( (AQIBound[[ levelInd ]][[ 2 ]]- AQIBound[[ levelInd ]][[ 1 ]]) / (upINV[[i]][[2]] - upINV[[i]][[1]]) ) * (ConcentrationVal - upINV[[i]][[1]]) + AQIBound[[ levelInd ]][[ 1 ]]
				break
			}

			levelInd <- levelInd + 1
		}
	}
	
	return( result )
}

calculateALLAQI <- function(dataPath, initalColIndex, format) {
	if( file.exists(dataPath) == FALSE ){
		cat('Fatal Error: No input file available to convert!\n')
        stop( 'Terminating The Session ...\n\n', call.=FALSE )
	}

	if(format == 'CSV'){
		srcDataCSV <- read.csv( dataPath, header=TRUE, sep= "," )
		progressRate <- NaN
		progressBase <- length(srcDataCSV$City) * length( c( initalColIndex:length( colnames(srcDataCSV) ) ) )
		progressPoint <- 0
		conv.SO2 <- c()
		conv.CO <- c()
		conv.O3 <- c()
		conv.PM10  <- c()
		conv.PM25  <- c()
		conv.NO2  <- c()

		dataRange <- c( 1:length(srcDataCSV$City) )
		for (i in dataRange) {
			cat('Converting SO2 values now ...\n')
			conv.SO2[[i]] <- calculateSO2AQI(srcDataCSV$SO2[[i]])
			progressPoint <- progressPoint +1
			progressRate <- (progressPoint/progressBase) * 100
			cat('Completion Progress:\n')
			print(progressRate)
		}
		cat('SO2 conversion done!\n\n')

		for (i in dataRange) {
			cat('Converting CO values now ...\n')
			conv.CO[[i]]<- calculateCOAQI(srcDataCSV$CO[[i]])
			progressPoint <- progressPoint +1
			progressRate <- (progressPoint/progressBase) * 100
			cat('Completion Progress:\n')
			print(progressRate)
		}
		cat('CO conversion done!\n\n')

		for (i in dataRange) {
			cat('Converting O3 values now ...\n')
			conv.O3[[i]]<- calculateO3AQI(srcDataCSV$O3[[i]])
			progressPoint <- progressPoint +1
			progressRate <- (progressPoint/progressBase) * 100
			cat('Completion Progress:\n')
			print(progressRate)
		}
		cat('O3 conversion done!\n\n')

		for (i in dataRange) {
			cat('Converting PM10 values now ...\n')
			conv.PM10[[i]]<- calculatePM10AQI(srcDataCSV$PM10[[i]])
			progressPoint <- progressPoint +1
			progressRate <- (progressPoint/progressBase) * 100
			cat('Completion Progress:\n')
			print(progressRate)
		}
		cat('PM10 conversion done!\n\n')

		for (i in dataRange) {
			cat('Converting PM25 values now ...\n')
			conv.PM25[[i]]<- calculatePM25AQI(srcDataCSV$PM25[[i]])
			progressPoint <- progressPoint +1
			progressRate <- (progressPoint/progressBase) * 100
			cat('Completion Progress:\n')
			print(progressRate)
		}
		cat('PM25 conversion done!\n\n')

		for (i in dataRange) {
			cat('Converting NO2 values now ...\n')
			conv.NO2[[i]]<- calculateNO2AQI(srcDataCSV$NO2[[i]])
			progressPoint <- progressPoint +1
			progressRate <- (progressPoint/progressBase) * 100
			cat('Completion Progress in percentage:\n')
			print(progressRate)
		}
		cat('NO2 conversion done!\n\n')

		cat('All AQI factors conversion is complete!\n\n')
		cat('Fill in the converted result ... \n')
		convDataTable <- data.frame( #SO2, CO, O3, PM10, PM25, NO2 
			City = noquote( srcDataCSV$City ), 
			YMD = noquote( srcDataCSV$YMD ), 
			Station = noquote( srcDataCSV$Station ), 
			SO2 = noquote( conv.SO2 ), 
			CO = noquote( conv.CO  ), 
			O3 = noquote( conv.O3 ), 
			PM10 = noquote( conv.PM10  ), 
			PM25 = noquote( conv.PM25 ), 
			NO2 = noquote( conv.NO2  ), 
			stringsAsFactors = FALSE 
		)
		return(convDataTable)

	}else{
		cat('Fatal Error: No specific method available to verify!\n')
        stop( 'Terminating The Session ...\n\n', call.=FALSE )
	}

}

writeOutdataTable <-function (dataTable, destination){
  
  #Write the final results to the given csv path if the path does exist
  writtenResult <- try( write.csv(dataTable, file = destination, row.names=FALSE, quote = FALSE ), silent = TRUE )
  
  if( class(writtenResult) == 'try-error' ){
    cat ( 'Error - The output path is invalid or non-existent.\n')
    stop( call.=FALSE )
  }else{
    cat( 'Wrote the file out successfully! \n' )
    cat( 'You may check the file at: \n' )
    cat( paste( '\t', destination ) )
    cat('\n\n')
    cat( 'Hasta la vista! \n' )
  }
  
}



#=============================Main===========================
originalArgs <- commandArgs(trailingOnly=TRUE) # read parameters for command line

refinedArgs <- parseCmdArgs( originalArgs )
#extract necessary arguments out; they are ordered as: Target Type, Source Files, Output File Path

#Please make sure the columns of your input CSV file follow this format(Left to Right): City, YMD, Station, SO2, CO, O3, PM10, PM25, NO2 
outputCSV <- calculateALLAQI(refinedArgs$inputFile,  4, 'CSV')
writeOutdataTable( outputCSV,  refinedArgs$outputFile )