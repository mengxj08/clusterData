processData <- function(datafilename, end){
  	data <- read.csv(datafilename,header=TRUE);
  	data <- as.matrix(sapply(data,as.numeric));
  	newData <- data.frame(year=character(),tech=integer(),type=integer(),HS=double(),diff_ratio=double(),average_ratio=double(),stringsAsFactors=FALSE);
  	newDataLine = 1;
  	breakLine = 1;
  	nrow = nrow(data);
  	end = nrow;
  	for(i in 1:end){
  		if(i == 1) next;

  		if(data[i,'HS'] == data[i-1,'HS'] && i != end){
  			next;
  		}
  		else{
  			timeOne = c(0,0);
  			timeTwo = c(0,0);
  			timeThree = c(0,0);
  			timeOneDiff_ratio = 0;
  			timeTwoDiff_ratio = 0;
  			timeThreeDiff_ratio = 0;
  			timeOneAverage_ratio = 0;
  			timeTwoAverage_ratio = 0;
  			timeThreeAverage_ratio = 0;
  			for(j in breakLine:(i-1)){
  				if(data[j,'year'] <= 2000){#Time One
  					if(!is.na(data[j,'diff_ratio'])){
  						timeOne[1] = timeOne[1] + 1;
  						timeOneDiff_ratio = timeOneDiff_ratio + data[j,'diff_ratio'];
  					}
  					if(!is.na(data[j,'average_ratio'])){
  						timeOne[2] = timeOne[2] + 1;
  						timeOneAverage_ratio = timeOneAverage_ratio + data[j,'average_ratio'];
  					}
  				}
  				else if(data[j,'year'] >= 2008){#Time Three
  					if(!is.na(data[j,'diff_ratio'])){
  						timeThree[1] = timeThree[1] + 1;
  						timeThreeDiff_ratio = timeThreeDiff_ratio + data[j,'diff_ratio'];
  					}
  					if(!is.na(data[j,'average_ratio'])){
  						timeThree[2] = timeThree[2] + 1;
  						timeThreeAverage_ratio = timeThreeAverage_ratio + data[j,'average_ratio'];
  					}
  				}
  				else{#Time Two
  					if(!is.na(data[j,'diff_ratio'])){
  						timeTwo[1] = timeTwo[1] + 1;
  						timeTwoDiff_ratio = timeTwoDiff_ratio + data[j,'diff_ratio'];
  					}
  					if(!is.na(data[j,'average_ratio'])){
  						timeTwo[2] = timeTwo[2] + 1;
  						timeTwoAverage_ratio = timeTwoAverage_ratio + data[j,'average_ratio'];
  					}
  				}
  			}

  			if(timeOne[1] != 0 && timeOne[2] != 0){
  				newData[newDataLine, ] <- c('1994-2000', data[breakLine,'tech'],data[breakLine,'type'],data[breakLine,'HS'],timeOneDiff_ratio/timeOne[1], timeOneAverage_ratio/timeOne[2]);
  				newDataLine = newDataLine + 1;
  			}
  			if(timeTwo[1] != 0 && timeTwo[2] != 0){
  				newData[newDataLine, ] <- c('2001-2007', data[breakLine,'tech'],data[breakLine,'type'],data[breakLine,'HS'],timeTwoDiff_ratio/timeTwo[1], timeTwoAverage_ratio/timeTwo[2]);
  				newDataLine = newDataLine + 1;
  			}
  			if(timeThree[1] != 0 && timeThree[2] != 0){
  				newData[newDataLine, ] <- c('2008-2014', data[breakLine,'tech'],data[breakLine,'type'],data[breakLine,'HS'],timeThreeDiff_ratio/timeThree[1], timeThreeAverage_ratio/timeThree[2]);
  				newDataLine = newDataLine + 1;
  			}

  			breakLine = i;
  		}
  	}

  	write.csv(newData,file="newData.csv");
}