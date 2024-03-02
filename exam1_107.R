#107exam1

#ex1
#直接讀入資料檔「薪情平臺匯出資料.xlsx」，產生兩個資料框 (data.frame)「每人每月總薪 資 (新臺幣元)」及「總工時 (小時)」。其中資料框中每㇐欄位為每㇐行業別，並新增㇐欄位 「性別」。 

#讀入xlsx檔#把不需要的列位刪掉
library(readxl)
dataset <- read_excel("D:\\ntpu\\ntpu_class\\1081HDDA\\exam\\107-1-HDDA-exam1\\SalaryGov.xlsx")
dataset_new <- dataset[-2, ]

#分成薪資/工時兩檔#命名性別列位
salary_per_month <- data.frame(dataset_new[(1:74), -1])
work_hour <- data.frame(dataset_new[c(1, 75:147), -1])
rownames(salary_per_month) <- c("gender", 1:73)
rownames(work_hour) <- c("gender", 1:73)

##薪資資料整理##
#取出欄位(變數)名稱#分別取出欄位中男女資料欄位號
varname <- colnames(salary_per_month[seq(2, ncol(salary_per_month), 2)])
male <- seq(2, ncol(salary_per_month), 2)
female <- seq(3, ncol(salary_per_month), 2)

#把欄位的性別欄去除,分別取出男女資料分開存#命名新存資料欄位名稱
salary_per_month_m <- salary_per_month[-1, male]
salary_per_month_f <- salary_per_month[-1, female]
colnames(salary_per_month_m) <- varname
colnames(salary_per_month_f) <- varname

#增加性別列
# gender <- rep("男", each=nrow(salary_per_month_m))
# salary_per_month_m <- data.frame(gender, salary_per_month_m)
# gender <- rep("女", each=nrow(salary_per_month_f))
# salary_per_month_f <- data.frame(gender, salary_per_month_f)

gender <- rep(c("男","女"), each=nrow(salary_per_month_m))
salary_per_month_m <- data.frame(gender[1:73], salary_per_month_m)
salary_per_month_f <- data.frame(gender[74:146], salary_per_month_f)

#增加時間欄
salary_per_month_m <- data.frame(salary_per_month[-1, 1], salary_per_month_m)
salary_per_month_f <- data.frame(salary_per_month[-1, 1], salary_per_month_f)
#重新命名新增欄位名稱(改欄位名稱要全部一起設定)
colnames(salary_per_month_m) <- c("月份", "性別", varname)
colnames(salary_per_month_f) <- c("月份", "性別", varname)

##工時資料整理##
#取出欄位(變數)名稱#分別取出欄位中男女資料欄位號
# varname <- colnames(work_hour[seq(2, ncol(work_hour), 2)])
# male <- seq(2, ncol(work_hour), 2)
# female <- seq(3, ncol(work_hour), 2)

#把欄位的性別欄去除,分別取出男女資料分開存#命名新存資料欄位名稱
work_hour_m <- work_hour[-1, male]
work_hour_f <- work_hour[-1, female]
colnames(work_hour_m) <- varname
colnames(work_hour_f) <- varname

#增加性別列
# gender <- rep("男", each=nrow(work_hour_m))
# work_hour_m <- data.frame(gender, work_hour_m)
# gender <- rep("女", each=nrow(work_hour_f))
# work_hour_f <- data.frame(gender, work_hour_f)

gender <- rep(c("男","女"), each=nrow(work_hour_m))
work_hour_m <- data.frame(gender[1:73], work_hour_m)
work_hour_f <- data.frame(gender[74:146], work_hour_f)

#增加時間欄
work_hour_m <- data.frame(work_hour[-1, 1], work_hour_m)
work_hour_f <- data.frame(work_hour[-1, 1], work_hour_f)
#重新命名新增欄位名稱(改欄位名稱要全部一起設定)
colnames(work_hour_m) <- c("月份", "性別", varname)
colnames(work_hour_f) <- c("月份", "性別", varname)

#ex2
# 印出資料摘要 (summary) 及結構 (str)。請確認每㇐變數 (欄位) 皆是正確的 R 類別 (例如: 數值變數、日期變數)。若不是請做必要的轉換。 

##薪資資料##
#合併男女資料#重新命名列位
salary_per_month_new <- rbind(salary_per_month_m, salary_per_month_f)
rownames(salary_per_month_new) <- c(1:146)

#時間欄位調整成日期變數
salary_per_month_new$月份 <- as.character(salary_per_month_new$月份)
salary_per_month_new$月份 <- gsub("年", "/", salary_per_month_new$月份)
salary_per_month_new$月份 <- gsub("月", "", salary_per_month_new$月份)
salary_per_month_new$月份 <- as.Date(paste(salary_per_month_new$月份, "/1",sep=""), format = "%Y/%m/%d")
#簡單舉例
# x <- gsub("年", "/", c("101年8月", "101年9月"))
# y <- gsub("月", "", x)
# as.Date(paste(y, "/1",sep=""), format = "%Y/%m/%d")

#把數據轉成數值資料
salary_per_month_new[3:19] <- sapply(salary_per_month_new[3:19], as.numeric)

summary(salary_per_month_new)
str(salary_per_month_new)

##工時資料##
#合併男女資料#重新命名列位
work_hour_new <- rbind(work_hour_m, work_hour_f)
rownames(work_hour_new) <- c(1:146)

#時間欄位調整成日期變數
work_hour_new$月份 <- as.character(work_hour_new$月份)
work_hour_new$月份 <- gsub("年", "/", work_hour_new$月份)
work_hour_new$月份 <- gsub("月", "", work_hour_new$月份)
work_hour_new$月份 <- as.Date(paste(work_hour_new$月份, "/1",sep=""), format = "%Y/%m/%d")

#把數據轉成數值資料
work_hour_new[3:19] <- sapply(work_hour_new[3:19], as.numeric)

summary(work_hour_new)
str(work_hour_new)

#ex3
#針對「每人每月總薪資 (新臺幣元)，男性」，畫出時間序列圖: 橫軸為時間，緃軸為薪資，圖上每㇐條趨勢線代表每㇐行業別之薪資。需標出圖例說明。需對結果作㇐些簡單解釋。

#把數據轉成數值資料
salary_per_month_m[3:19] <- sapply(salary_per_month_m[3:19], as.numeric)

salary_per_month_m_scale <- t(scale(salary_per_month_m[,3:19], center=T, scale=T))    

p <- ncol(salary_per_month_m_scale)#18個變數(時間點alpha)#p=18
 
phase <- varname

windows()
par(mai=c(0.5, 0.5, 0.5, 2))
matplot(t(salary_per_month_m_scale), lty=1, type = "l", ylab="gene expression", 
        col=rainbow(17), xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep="")) #t[1]~t[18]     
axis(1, 1:(p+1), time.label)#在x軸上的1~19位置標上t[1]~t[18](數字小標)
legend("right", legend=phase, col=rainbow(17), lty=1, horiz = F, lwd=2, xpd=T, cex=0.7)



#ex4
#畫出 side-by-side 盒形圖: 橫軸為每㇐行業別 (請依照每㇐行業別之中位數從大至小，由左至 右排序)，緃軸為近五年 (102∼106) 薪資之五數綜合。需對結果作㇐些簡單解釋。

#求欄位變數中位數#存入新變數將名稱改成原本欄位順序並做中位數大小排列
median <- apply(salary_per_month_new[3:19], 2, median)
names(median) <- 3:19
median <- rev(sort(median))
#依照中位數大小(依序畫出的欄位順序已排好)畫盒形圖
windows(title="side-by-side 盒形圖")
boxplot(salary_per_month_new[as.numeric(names(median))], las=2, xlab="行業", ylab="薪資")

#ex5
#畫出兩個資料框之熱圖 (heatmap)，其中需有「性別」之色條。(可搭配群集分析對產業別排 序) 需對結果作㇐些簡單解釋。

#載入套件#把資料轉為矩陣才能使用熱圖#要數值資料
library(pheatmap)
#salary_matrix <- as.matrix(salary_per_month_new[rowSums(salary_per_month_new[, 3:19])>950000, 3:19])
salary_matrix <- as.matrix(salary_per_month_new[, 3:19])
#預設畫出的圖
pheatmap(salary_matrix)


#把資料變dataframe
salary_frame <- data.frame(salary_matrix)

#設定性別變數#轉為dataframe#命名性別資料名稱和原資料一樣1,2,..(重要)
#my_gender <- data.frame(gender = ifelse(test = gender == "男", yes = "male", no = "female"))#不必要男女轉英文
my_gender <- data.frame(gender)
rownames(my_gender) <- c(1:146)

#生一資料內容(色階名稱設為行業名稱varname)#命名資料名稱為原資料的行業(重要)
#industry1,2..(色階名稱,可改)
#my_varname <- data.frame(industry = paste("industry", c(1:17), sep=""))
my_varname <- data.frame(industry = varname)
row.names(my_varname) <- colnames(salary_frame)
head(salary_frame)
#加性別色階
pheatmap(salary_frame, annotation_row = my_gender)
#加性別色階和行業色階
pheatmap(salary_frame, annotation_row = my_gender, annotation_col = my_varname)

# head(salary_frame)
# head(my_gender)
# head(my_varname)
#ex6
#針對此資料「每人每月總薪資 (新臺幣元)」及「總工時 (小時)」，自行問㇐個想了解的問題， 並進行探索性資料分析。需對結果作㇐些簡單解釋。
