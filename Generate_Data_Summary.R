rm(list = ls(all = TRUE))
options(scipen = 999)
options(java.parameters = "-Xmx15g")
pkgLoad <- function(package) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, dep=TRUE, repos = "http://cran.csie.ntu.edu.tw/")
        if(!require(package, character.only = TRUE)) stop("Package not found")
    }
    suppressMessages(library(package, character.only=TRUE))
}
pkgLoad("data.table")
pkgLoad("xlsx")
pkgLoad("properties")
props<-read.properties("reco.property")
setwd(props$WORK_DIR)
dataFilePath <- "{RatingFile}"
data <- fread(dataFilePath)
getColumnsType <- function(data) {
	header <- names(data)
	numeric_list <- NULL
	non_numeric_list <- NULL
	others_list <- NULL
	for(col in header) {
		ifelse(is.ts(data[, col]), others_list <- c(others_list, col),
			ifelse(is.factor(data[, col]), non_numeric_list <- c(non_numeric_list, col),
				ifelse(is.numeric(data[, col]), numeric_list <- c(numeric_list, col), non_numeric_list <- c(non_numeric_list, col))
			)
		)
	}
	return(list(numeric_list, non_numeric_list, others_list))
}
dataSummary<- function(data) {
	data <- as.data.frame(data)
	if(is.data.frame(data)) {
		col_types <- getColumnsType(data)
		numeric_list <- unlist(col_types[1])
		non_numeric_list <- unlist(col_types[2])
		non_numeric_summary <- NULL
		mean_list <- NULL
		sd_list <- NULL
		min_list <- NULL
		Q1_list <- NULL
		median_list <- NULL
		Q3_list <- NULL
		max_list <- NULL
		IQR_list <- NULL
		N_list <- NULL
		for(col in numeric_list) {
			mean_list <- c(mean_list, mean(data[, col], na.rm = TRUE))
			sd_list <- c(sd_list, sd(data[, col], na.rm = TRUE))
			quantile <- quantile(data[, col], na.rm = TRUE)
			min_list <- c(min_list, quantile[1])
			Q1_list <- c(Q1_list, quantile[2])
			median_list <- c(median_list, quantile[3])
			Q3_list <- c(Q3_list, quantile[4])
			max_list <- c(max_list, quantile[5])
			IQR_list <- c(IQR_list, IQR(data[, col], na.rm = TRUE))
			N_list <- c(N_list, length(data[, col]) - sum(is.na(data[, col])))
		}
		numeric_summary <- data.frame(numeric_list, mean_list, sd_list, min_list, Q1_list, median_list, Q3_list, max_list, IQR_list, N_list)
		names(numeric_summary) <- c("Attribute", "Mean", "Sd", "Min", "25%", "Median", "75%", "Max", "IQR", "N")
		correlation_matrix <- cor(data[, numeric_list], use="complete")
		i = 1
		for(col in non_numeric_list) {
			temp <- data.frame(table(data[, col]))
			names(temp) <- c(col, "Freq")
			non_numeric_summary[[i]] <- temp
			i = i + 1
		}
		finalList <- list(numeric_summary, correlation_matrix, non_numeric_summary)
		create_summary_report(finalList)
		return(finalList)
	}else {
		return(NULL)
	}
}
create_summary_report <- function(summaryList){
	numeric_data <- as.data.frame(summaryList[1])
	correlation_matrix <- as.data.frame(summaryList[2])
	correlation_matrix <- cbind(Row.Names = rownames(correlation_matrix), correlation_matrix)
	wb<-createWorkbook(type="xlsx") 
	TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb,color="black", isBold=TRUE)
	TABLE_TARGET_STYLE<-CellStyle(wb) + Font(wb, heightInPoints=10, isBold=TRUE, color ="9", name="Arial") + Fill(foregroundColor="#0069AA")
	TABLE_CONTROL_STYLE<-CellStyle(wb) + Font(wb, heightInPoints=10, isBold=TRUE, color ="9", name="Arial") + Fill(foregroundColor="#000000")
	TABLE_COLNAMES_STYLE <- CellStyle(wb) + Fill(foregroundColor = "#0014AA")+ Font(wb, heightInPoints=10,isBold=TRUE,color ="9", name="Arial") + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") + Border(color="#0014AA", position=c("TOP","RIGHT","BOTTOM","LEFT"), pen=c("BORDER_THICK","BORDER_THICK","BORDER_THICK","BORDER_THICK")) 
	CS2<-CellStyle(wb) + Alignment(horizontal="ALIGN_CENTER") + Border(color="black", position=c("TOP","RIGHT","BOTTOM","LEFT"), pen=c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN")) 
	CS1<-CellStyle(wb) + Alignment(horizontal="ALIGN_RIGHT") + Border(color="black", position=c("TOP","RIGHT","BOTTOM","LEFT"), pen=c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN")) 
	CS3 <- CellStyle(wb) + Fill(foregroundColor = "#000000") + Font(wb, heightInPoints=10,isBold=TRUE,color ="9", name="Arial") + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") + Border(color="black", position=c("TOP","RIGHT","BOTTOM","LEFT"), pen=c("BORDER_THICK","BORDER_THICK","BORDER_THICK","BORDER_THICK")) 
	CS4 <- CellStyle(wb) + Fill(foregroundColor = "#0069AA") + Font(wb, heightInPoints=10,isBold=TRUE,color ="9", name="Arial") + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") + Border(color="#0069AA", position=c("TOP","RIGHT","BOTTOM","LEFT"), pen=c("BORDER_THICK","BORDER_THICK","BORDER_THICK","BORDER_THICK"))
	if(!is.null(summaryList[1]) & !is.null(summaryList[2])){
		sheet1<-createSheet(wb,sheetName="Numeric_Summary") 
		sheet2<-createSheet(wb,sheetName="Correlation_Matrix")
		addDataFrame(numeric_data, sheet1, startRow=3, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,showNA=FALSE,colStyle=list(`1`=CS2,`2`=CS2,`3`=CS1,`4`=CS1,`5`=CS1,`6`=CS1,`7`=CS1,`8`=CS1,`9`=CS1,`10`=CS1,`11`=CS1,`12`=CS1,`13`=CS1,`14`=CS1,`15`=CS1,`16`=CS1,`17`=CS1,`18`=CS1,`19`=CS1,`20`=CS1,`21`=CS1,`22`=CS1)) 
		addDataFrame(correlation_matrix, sheet2, startRow=3, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,showNA=FALSE,colStyle=list(`1`=CS2,`2`=CS2,`3`=CS1,`4`=CS1,`5`=CS1,`6`=CS1,`7`=CS1,`8`=CS1,`9`=CS1,`10`=CS1,`11`=CS1,`12`=CS1,`13`=CS1,`14`=CS1,`15`=CS1,`16`=CS1,`17`=CS1,`18`=CS1,`19`=CS1,`20`=CS1,`21`=CS1,`22`=CS1))
	}
	if(!is.null(summaryList[3])){
		for(i in 1:length(summaryList[[3]])){
			non_numeric <- as.data.frame(summaryList[[3]][i])
			temp <- createSheet(wb,sheetName = colnames(non_numeric)[1])
			addDataFrame(non_numeric, temp, startRow=3, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,showNA=FALSE,colStyle=list(`1`=CS2,`2`=CS2,`3`=CS1,`4`=CS1,`5`=CS1,`6`=CS1,`7`=CS1,`8`=CS1,`9`=CS1,`10`=CS1,`11`=CS1,`12`=CS1,`13`=CS1,`14`=CS1,`15`=CS1,`16`=CS1,`17`=CS1,`18`=CS1,`19`=CS1,`20`=CS1,`21`=CS1,`22`=CS1))
		}
	}
	saveWorkbook(wb,file="Data_Summary_Report.xlsx")
	summaryFile <- paste(getwd(),"Data_Summary_Report.xlsx",sep="/")
	print(paste("Data summary has been generated as",summaryFile,sep="::::"))
}
summaryList <- dataSummary(data)
detach("package:data.table",unload=TRUE)
detach("package:xlsx",unload=TRUE)
detach("package:properties",unload=TRUE)
rm(list = ls(all = TRUE))