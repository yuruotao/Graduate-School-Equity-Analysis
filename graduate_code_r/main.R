##############################################################################################
library("ggplot2")
library("see")
library("ggraph")
library("correlation")
library("reshape2")
library("ggdendro")
library("tidyr")
library('vcd')
library("qqplotr")
library("report")
library("poorman")
library("ggraph")
library("correlation")
library("dplyr")
library("datawizard")
library("lindia")
library("ggrepel")
library("patchwork")
library("hrbrthemes")
library("svglite")
library("sjPlot")
library("viridis")
library("ggmosaic")
library("ggfortify")
library("lfda")

# Initialization with R

# get the current working directory named working_directory, if not, then set the current directory as the working directory
working_directory <- getwd()
if (!is.null(working_directory)) setwd(working_directory)

# Create the file structure
data_dir = "/data"
output_dir = "/output"
output_data_dir = "/output/data"
output_figure_dir = "/output/figure"
output_report_dir = "/output/report"


dir.create(file.path(working_directory, data_dir), showWarnings = FALSE)
dir.create(file.path(working_directory, output_dir), showWarnings = FALSE)
dir.create(file.path(working_directory, output_data_dir), showWarnings = FALSE)
dir.create(file.path(working_directory, output_figure_dir), showWarnings = FALSE)
dir.create(file.path(working_directory, output_report_dir), showWarnings = FALSE)

# Load readxl package for later import of excel files
library("readxl")
# The parameter "sheet_name" specifies the sheet to read
data_file = read_excel(paste(working_directory, "/data/电子科技大学.xlsx", sep=""), sheet="2021")

##############################################################################################
# Raw data analysis

# Designate the columns that are character type
char_cols <- list("考号")
num_cols <- list("初试分数","复试分数", "录取状态")


# Create data frames that contain different types of data and loop through the raw data file
data_file_char = data.frame()
data_file_num = data.frame()


row_number <- nrow(data_file)
for (i in 1:row_number) {
  data_file_char[i, ] <- data_file[i, ]
  data_file_num[i, ] <- data_file[i, ]
}

for (col_name in colnames(data_file)) {
  in_list <- FALSE
  append_df <- data_file[, col_name, drop = FALSE]
  if (col_name %in% char_cols){
    data_file_char <- cbind(data_file_char, append_df)
    in_list <- TRUE
  }
  if (col_name %in% num_cols){
    append_df <- sapply(append_df, as.numeric)
    data_file_num <- cbind(data_file_num, append_df)
    in_list <- TRUE
  }
}

# Raw data analysis
library("datawizard")
raw_data_summary = describe_distribution(data_file_num)
# Save to excel(raw data analysis)
library("writexl")
write_xlsx(raw_data_summary, paste(working_directory, "/output/data/raw_data_analysis.xlsx", sep=""))

# Aov analysis
library("report")
library("dplyr")
#aov_report = aov(mean ~ class, data = data_file) %>% report()
#writeLines(aov_report, paste(working_directory, "/output/report/aov.txt",sep = ""))


##############################################################################################
# Correlation analysis

library("correlation")
library("Hmisc")
library("writexl")
if (ncol(data_file_num) > 1){
  correlation_result_num_num = correlation(data = data_file_num, method ="pearson", redundant = TRUE)
  write_xlsx(correlation_result_num_num, paste(working_directory, "/output/data/correlation_num_num.xlsx", sep=""))
}

# Correlation visualization
correlation_dir = paste(working_directory, "/output/figure/correlation", sep="")
dir.create(file.path(correlation_dir), showWarnings = FALSE)

cor_num_num_data <- correlation_result_num_num[c(1,2,3)]
cor_num_num_fig = ggplot(data = cor_num_num_data,aes(x=Parameter1, y=Parameter2, fill=r)) + 
  geom_tile(aes(fill = r), colour = "white")+ 
  geom_text(aes(Parameter1, Parameter2, label = round(r, digits = 3)),
            color = "black", size = 10)+
  scale_fill_gradient2(low = "#5aaed7",high = "#ff7a53", mid = "#FFFFFF", midpoint = 0)+ 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
    #axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    #axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank(),  #remove y axis ticks
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
  )

plot(cor_num_num_fig)
ggsave(paste(correlation_dir,"/cor_num_num",".png",sep = ""), plot = cor_num_num_fig)
