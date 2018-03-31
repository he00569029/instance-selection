library(data.table)
library(hydroGOF)
library(dplyr)

PATH <- "E:/研究所/資料探勘/程式碼/dataset/"
data = fread(paste0(PATH,"autoMPG6.csv"), sep=",",na.string="")
##用fread讀取資料是因為相比其他的讀取方法時間縮短了一半以上
set.seed(12345)
newdata=data
n=nrow(newdata)        ##算列的總數
m=length(data)      ##算欄的總數
colna=names(data)   ##欄的名稱


random=sample(n)      ##做亂數
index=(random-1) %% 10  +1   
##從亂數做餘數  前面random-1 是因為要讓整除的數字做餘數後有數字9(因為要分1~10群)後面的-1是為了讓結果回來並且讓餘數為9的做+1就有10

##as.data.frame(table(index))   可以看到出現頻率					

newdata=cbind(newdata,index)  ##將餘數與newdata做合併  這樣才能夠有標記
individual_error=matrix(1:n)  ##建立個別誤差陣列
colnames(individual_error)=c("individual_error")
newdata=cbind(individual_error,newdata)  ##將個別誤差陣列加入到newdata
newdata$individual_error=as.numeric(newdata$individual_error)


##建立模型

f=paste(colna[m],sep="~",paste(colna[(m/m):(m-1)],collapse="+"))  #建立複回歸的formula
sum_model_error=0
sum_model_rmse=0
j=1
for(i in 1:10){
	model=lm(formula=f , data=newdata[index != i,])  ##建立複迴歸模型
	model_error=sum(abs(newdata[index == i,colna[m],with=FALSE] - predict(model,newdata[index == i,])))  ##模型的誤差
	individual_error=abs(newdata[index == i,colna[m],with=FALSE] - predict(model,newdata[index == i,]))  ##個別樣本的誤差
	model_rmse=sqrt( mean( (newdata[index == i,colna[m],with=FALSE] - predict(model,newdata[index == i,]))^2 ))  ##求模型平均的RMSE
	

	for(k in 1:n){
		if(newdata[k,]$index==i){
			newdata[k,]$individual_error = individual_error[j]    ##將個別樣本誤差存入newdata
			j=j+1
		}
	}
	j=1
	sum_model_error=sum_model_error+model_error   ##計算跑10次後模型的總誤差
	sum_model_rmse=sum_model_rmse+model_rmse   ##10次模型的RMSE總和

	cat("model_error=",model_error,"\n")   ##輸出模型的誤差
	cat("model_rmse=",model_rmse,"\n")  ##輸出模型的rmse
	
}

average_error=sum_model_error/10   ##全部誤差求平均
average_rmse=sum_model_rmse /10          ##模型平均的RMSE

# cat("總誤差",sum_model_error,"\n")
cat("平均誤差=",average_error,"\n")  
# cat("10次模型RMSE總和",sum_model_rmse,"\n")
cat("10次模型RMSE平均=",average_rmse,"\n")

-------------------------------------------------
挑選1個後  將剩下的分10群  每一群只需測一次就可以了  就可以得出誤差

sel_rmse_array=array(0,dim=c(n,4))
colnames(sel_rmse_array)=c("sel_rmse","difference_rmse","sel_error","difference_error")

for(i in 1:n){
	set.seed(12345)
	sel_data=data[-i,]  #一個一個將資料去掉
	sel_data_n = nrow(sel_data)
	random=sample(sel_data_n)
	index=(random-1) %% 10  +1
	sel_data=cbind(sel_data,index)

	# sel_individual_error=matrix(1:sel_data_n)  ##建立個別誤差陣列
	# sel_individual_error=as.numeric(matrix(0,nrow=sel_data_n,ncol=1))
	# colnames(sel_individual_error)=c("sel_individual_error")
	# sel_data=cbind(sel_individual_error,sel_data)  ##將個別誤差陣列加入到newdata
	# sel_data$sel_individual_error=as.numeric(sel_data$sel_individual_error)
	temp_rmse=0
	temp_error=0

	for(j in 1:10){
		sel_model=lm(formula=f , data=sel_data[index != j,])  ##建立模型
		temp1=mean(sum(abs(sel_data[index == j,colna[m],with=FALSE] - predict(sel_model,sel_data[index == j,]))))
		temp2=rmse(sel_data[index == j,colna[m],with=FALSE],predict(sel_model,sel_data[index == j,]))
		temp_rmse=temp_rmse + temp2
		temp_error=temp_error + temp1
	}
	sel_rmse=temp_rmse / 10  ##RMSE平均
	sel_error=temp_error /10  ##error平均
	cat("sel_rmse=",sel_rmse,"\n")
	sel_rmse_array[i,"sel_rmse"]=sel_rmse
	sel_rmse_array[i,"difference_rmse"]=average_rmse - sel_rmse
	sel_rmse_array[i,"sel_error"]=sel_error
	sel_rmse_array[i,"difference_error"]=average_error - sel_error
	sel_rmse=0
	sel_error=0
}
rm(sel_rmse,sel_error,temp1,temp2,temp_error,temp_rmse,sum_model_rmse,sum_model_error,PATH)














