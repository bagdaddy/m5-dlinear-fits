library(plyr)
setwd("/Users/mantasbagdonas/Studies/Magistras/MOFC/M5/Evaluation/submission_evaluation/DLinear submissions v1")
load("M5dataset.Rdata")
benchmarks <- 1
masters<-1
Dlinear_wd <- '/Users/mantasbagdonas/Studies/Magistras/MOFC/M5/Evaluation/submission_evaluation/DLINEAR_eval'
DLinear_np_wd <- '/Users/mantasbagdonas/Studies/Magistras/MOFC/M5/Evaluation/submission_evaluation/DLinear nonparametric v3'
Fits_wd <- '/Users/mantasbagdonas/Studies/Magistras/MOFC/M5/Evaluation/submission_evaluation/FITS_eval'
Masters_wd <- '/Users/mantasbagdonas/Studies/Magistras/MOFC/M5/Evaluation/submission_evaluation/MASTERS/cur'
setwd(DLinear_np_wd)
sub_names = list.files(pattern="*.csv")
sub_names <- c('dlinear-lb-7-ep-100-1.0.csv')
team <- sub_names
#if (masters==1){
#  setwd("./MASTERS/cur")
#  sub_names = list.files(pattern="*.csv")
#  team <- sub_names
#}else if (benchmarks==1){
#  setwd("./Benchmarks")
#  sub_names = list.files(pattern="*.csv")
#  team <- sub_names
#}else{
#  setwd("./Submissions")
#  sub_names = list.files(pattern="*.csv")
#  team <- substr(sub_names, 6,1000)
#}

team <- gsub(".csv", "", team)

###########################################################################

errors_total = evaluation_matrix_fh <- NULL
template <- read.csv("../Accuracy_template.csv", stringsAsFactors = F)
template <- tail(template, 30490)

for (sub_id in 1:length(team)){
  print(paste("Submission", sub_names[sub_id]))
  submission <- read.csv(sub_names[sub_id], stringsAsFactors = F)
  submission <- submission[submission$id %in% template$id,]
  submission_clear <- NULL
  for (rcid in 1:nrow(template)){
    submission_clear <- rbind(submission_clear, submission[submission$id==template$id[rcid],])
  }
  submission <- submission_clear
  rm(submission_clear)
  row.names(submission) <- NULL
  
  #Create search keys
  submission$item_id <- unlist(lapply(c(1:nrow(submission)), function(x) paste0(strsplit(submission$id[x], "_")[[1]][1],"_",
                                                                                strsplit(submission$id[x], "_")[[1]][2],"_",
                                                                                strsplit(submission$id[x], "_")[[1]][3])))
  
  submission$dept_id <- unlist(lapply(c(1:nrow(submission)), function(x) paste0(strsplit(submission$id[x], "_")[[1]][1],"_",
                                                                                strsplit(submission$id[x], "_")[[1]][2])))
  
  submission$cat_id <- unlist(lapply(c(1:nrow(submission)), function(x) 
    paste0(strsplit(submission$id[x], "_")[[1]][1])))
  
  submission$store_id <- unlist(lapply(c(1:nrow(submission)), function(x) 
    paste0(strsplit(submission$id[x], "_")[[1]][4],"_",
           strsplit(submission$id[x], "_")[[1]][5])))
  
  submission$state_id <- unlist(lapply(c(1:nrow(submission)), function(x) 
    paste0(strsplit(submission$id[x], "_")[[1]][4])))
  submission$id <- NULL
  
  evaluation_matrix <- NULL
  
  #Level 12
  sid <- as.numeric(row.names(info[info$level==12,]))
  for (tsid in sid){
    frc <- as.numeric(submission[tsid,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 12
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
  }
  #Level 11
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"item_id","state_id")], .(item_id,state_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==11,]))
  temp_agg$item_id = temp_agg$state_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 11
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 10
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"item_id")], .(item_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==10,]))
  temp_agg$item_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 10
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 9
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"dept_id","store_id")], .(dept_id,store_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==9,]))
  temp_agg$dept_id = temp_agg$store_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 9
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 8
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"cat_id","store_id")], .(cat_id,store_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==8,]))
  temp_agg$cat_id = temp_agg$store_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 8
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 7
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"dept_id","state_id")], .(dept_id,state_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==7,]))
  temp_agg$dept_id = temp_agg$state_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 7
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 6
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"cat_id","state_id")], .(cat_id,state_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==6,]))
  temp_agg$cat_id = temp_agg$state_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 6
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 5
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"dept_id")], .(dept_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==5,]))
  temp_agg$dept_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 5
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 4
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"cat_id")], .(cat_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==4,]))
  temp_agg$cat_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 4
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 3
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"store_id")], .(store_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==3,]))
  temp_agg$store_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 3
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 2
  temp_agg <- ddply(submission[,c(colnames(submission)[1:28],"state_id")], .(state_id), colwise(sum))
  sid <- as.numeric(row.names(info[info$level==2,]))
  temp_agg$state_id <- NULL ; cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg[cnt,1:28])
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 2
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  #Level 1
  temp_agg <- colSums(submission[,colnames(submission)[1:28]])
  sid <- as.numeric(row.names(info[info$level==1,]))
  cnt <- 1
  for (tsid in sid){
    frc <- as.numeric(temp_agg)
    in_s <- insample[[tsid]]
    out_s <- outsample[[tsid]]
    rmsse <- sqrt(mean((frc-out_s)^2)/mean(diff(in_s)^2))
    level <- 1
    rsse <- sqrt(((frc-out_s)^2)/mean(diff(in_s)^2))
    dollar_sales <- info$dollar_sales[tsid]
    evaluation_matrix <- rbind(evaluation_matrix, data.frame(level,dollar_sales,rmsse,t(rsse)))
    cnt <- cnt+1
  }
  
  evaluation_matrix$weight <- evaluation_matrix$dollar_sales/tail(evaluation_matrix$dollar_sales,1)
  evaluation_matrix$wrmsse <- evaluation_matrix$rmsse*evaluation_matrix$weight
  colnames(evaluation_matrix)[4:31] <- paste0("H",c(1:28))
  
  if (sub_id==1){
    temp_df <- evaluation_matrix[,c("level","dollar_sales","weight","wrmsse")]
    errors_total <- temp_df
  }else{
    errors_total <- cbind(errors_total,evaluation_matrix$wrmsse)
  }
  
  evaluation_matrix$team <- team[sub_id]
  evaluation_matrix$id <- c(1:42840)
  evaluation_matrix_fh <- rbind(evaluation_matrix_fh, evaluation_matrix)
  
}

#temp
team <- team
colnames(errors_total)[4:ncol(errors_total)] <- team
avg_np <- ddply(errors_total, .(level), colwise(sum))[,c(1,c(4:ncol(errors_total)))]
avg_np <- rbind(avg_np, colMeans(avg_np))
View(avg_np)

write.csv(avg_np,"Accuracy_benchmarks_overall.csv", row.names = F)
#write.csv(avg,"Accuracy_benchmarks_overall.csv", row.names = F)
write.csv(errors_total,"Accuracy_benchmarks_results.csv", row.names = F)
write.csv(evaluation_matrix_fh,"Accuracy_benchmarks_results_fh.csv", row.names = F)

setwd("../Errors")
if (benchmarks==1){
  write.csv(avg,"Accuracy_benchmarks_overall.csv", row.names = F)
  write.csv(errors_total,"Accuracy_benchmarks_results.csv", row.names = F)
  write.csv(evaluation_matrix_fh,"Accuracy_benchmarks_results_fh.csv", row.names = F)
}else{
  write.csv(avg,"FITS-Accuracy_top50_overall.csv", row.names = F)
  write.csv(errors_total,"FITS-Accuracy_top50_results.csv", row.names = F)
  write.csv(evaluation_matrix_fh,"FITS-Accuracy_top50_results_fh.csv", row.names = F)
}
