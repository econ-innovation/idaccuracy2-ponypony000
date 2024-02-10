#导入包
library(dplyr)
library(tidyr)
library(stringr)

#读取scientist_pub数据
scientist_pub <- read_csv("/Users/mawenting/Documents/Bigdata_econ/Assignment/idaccuracy2-ponypony000/scientist_pub.csv")

#获取Aminer文件夹下的csv数据
aminer_list <- list.files("/Users/mawenting/Documents/Bigdata_econ/Assignment/Assignment_data/assignment_idaccuracy/Aminer",pattern = "*.csv")

#定义一个函数来处理每个文件并计算准确率和召回率
f <- function(file_path){
  aminer_data=read.csv(file_path)
  # 提取uniqueid
  uniqueid <- str_extract(file_path, "0_[0-9]+")
  # 筛选scientist_pub中uniqueID相同的作者
  scientist_pub_filtered <- filter(scientist_pub, uniqueID == uniqueid)
  # 将两个数据框的DOI、标题、期刊转换为大写
  scientist_pub_filtered$doi <- toupper(scientist_pub_filtered$doi)
  scientist_pub_filtered$title <- toupper(scientist_pub_filtered$title)
  scientist_pub_filtered$journal <- toupper(scientist_pub_filtered$journal)
  
  aminer_data$doi <- toupper(aminer_data$doi)
  aminer_data$标题 <- toupper(aminer_data$标题)
  aminer_data$期刊 <- toupper(aminer_data$期刊)
  
  #匹配两张表 
  matched_papers <- inner_join(aminer_data, scientist_pub_filtered, 
                               by = c("doi" = "doi", "标题" = "title", "期刊" = "journal", "年份" = 'pub_year'))
  #计算精准度和查全率
  precision <- nrow(matched_papers) / nrow(aminer_data)
  recall <- nrow(matched_papers) / nrow(scientist_pub_filtered)
  
  return(data.frame(file_name = basename(file_path), uniqueid, precision, recall))
}
#basename()提取文件名
# 对从aminer获取的list执行上述函数
results <- lapply(aminer_list, f)

# 合并结果
final_results <- bind_rows(results)

# 保存结果到文件
write_csv(final_results, "/Users/mawenting/Documents/Bigdata_econ/Assignment/idaccuracy2-ponypony000/accuracy_recall_results.csv")

# 计算整体准确率和召回率
overall_precision <- mean(final_results$precision)
overall_recall <- mean(final_results$recall)

# 打印整体准确率和召回率
print(paste("Overall Precision: ", overall_precision))
print(paste("Overall Recall: ", overall_recall))