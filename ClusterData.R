library(fpc);

ProcessData <- function(datafilename, end){
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

ProcessDataForKmeansCluster <- function(){
  years <- c('1994-2000','2001-2007','2008-2014')
  for(y in years)
  {
    filename <- paste0("N01(",y,").csv")
    
    #K-means
    data <- read.csv(filename,header=TRUE);
    data <- as.matrix(sapply(data,as.numeric));
    data <- data[complete.cases(data[,17:18]),];
    write.csv(data,file=paste0("N01(",y,")-update.csv"));
  }
}

#聚类分析 —— K-means cluster
ExportDataKMeans <- function(datafilename, K=NA) {
    data <- read.csv(datafilename,header=TRUE);
    data <- as.matrix(sapply(data,as.numeric));
    nrow = nrow(data);
    print(paste("The old row number is:",nrow));
    exportData <- data.frame(diff_ratio=double(),average_ratio=double(),stringsAsFactors=FALSE);
    j = 1;
    for(i in 1:nrow){
      if(!is.na(data[i,'diff_ratio']) && !is.na(data[i,'average_ratio'])){
        #del the NA data
        exportData[j, ] <- c(data[i,'diff_ratio'],data[i,'average_ratio']);
        j = j + 1;
      }
    }
    print(paste("The old row number is:",nrow(exportData)));
    #print(exportData);
    if(!is.numeric(K))  #未知分类数，则逐一试探，画出曲线
    {
      # k取2到8，评估K
      K <- 2:8;
      round <- 20 # 每次迭代20次，避免局部最优
      rst <- sapply(K, function(i){
        print(paste("K = ",i))
        mean(sapply(1:round,function(r){
          print(paste("Round",r))
          result <- kmeans(exportData, i)
          stats <- cluster.stats(dist(exportData), result$cluster)
          stats$avg.silwidth
        }))
      })
      plot(K,rst,type='l',main='轮廓系数与K的关系', ylab='轮廓系数')
      
    }
    else {#指定了分类数，则聚类后画图
      #进行多次聚类，选取Sum of Squared Error（SSE）最小的一个
      print(paste('K = ', K))

      clu_results <- sapply(1:20, function(r){
        print(paste("round",r))
        result <- kmeans(exportData, K)
        stats <- cluster.stats(dist(exportData), result$cluster)
        list(result,stats$avg.silwidth)
      })
      best_clu <- clu_results[1,which.max(unlist(clu_results[2,]))][[1]]
      
      # 降维度观察
      plot(exportData[,2:1], col=best_clu$cluster, main=paste0('kmeans聚类 k=',K), pch = 19,xlab='中国进出口单位产品价值差异幅度',ylab='中国进出口差异幅度')
      
      #按分类排序
      res <- data.frame(names(best_clu$cluster),best_clu$cluster);
      res <- as.matrix(sapply(res,as.numeric));
      res <- res[order(res[,1]),];
      print(best_clu);
      print(paste('---------------------------------------------------'));
      print(' ');
      return(res);
    }
}

kmeans.findK <- function()
{
  years <- c('1994-2000','2001-2007','2008-2014')
  for(y in years)
  {
    filename <- paste0("N01(",y,").csv")
    
    #K-means
    k_value <- ExportDataKMeans(filename)
    #save png
    dev.copy(png,paste0("轮廓系数-N01(",y,").png"))
    dev.off()
  }
}

kmeans.setK <- function()
{
  years <- c('1994-2000','2001-2007','2008-2014')
  k_value <- c(8,6,5); #input the K from the kmeans.findK function
  k_index = 1;

  sink("output.txt", append=FALSE, split=FALSE);
  for(y in years)
  {
    filename <- paste0("N01(",y,")-update.csv")
    
    print(paste0("Open the file N01(",y,")-update.csv"));
    #K-means
    res <- ExportDataKMeans(filename, k_value[k_index])
    #save png
    dev.copy(png,paste0("kmeans聚类-N01(",y,").png"))
    dev.off()

    #保存结果
    write.csv(res,file=paste0("N01(",y,")-result.csv"));

    k_index = k_index + 1;
  }

  sink();
}