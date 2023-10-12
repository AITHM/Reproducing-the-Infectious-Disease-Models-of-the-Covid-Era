library(dplyr)
library(xtable)
options(xtable.floating = FALSE)

get_collumn_table_1 = function(data, papers, median, q1, q3){
  published = nrow(data[data$peer_reviewed,])
  contains_code = data[!is.na(data$language),]
  contains_data = nrow(data[data$has_data,])
  contains_code_and_data = nrow(data[data$has_data & !is.na(data$language),])
  full_reproduction = nrow(contains_code[contains_code$Outputs_fully_match == 1,])
  some_but_not_all = nrow(contains_code[contains_code$Outputs.partial.match == 1,])
  code_mandates = nrow(data[data$mandated_code,])
  data_mandates = nrow(data[data$mandated_data,])
  data_and_code_mandates = nrow(data[data$mandated_data & data$mandated_code,])
  covid = nrow(data[data$iscovid == 1,])
  other = nrow(data[data$iscovid == 0,])
  cites_iqr = sprintf("%0.0f (%0.0f - %0.0f)", median, q1, q3)
  return(c(papers, published, cites_iqr, "", "", data_mandates, code_mandates, data_and_code_mandates,
           "", "", covid, other, "", "",contains_data, nrow(contains_code), contains_code_and_data))
}

table_alignment = "l|rr"

papers_total_top = 100
papers_total_random = 100

citations_top = c(653, 480, 1072) #Median, IQR
citations_random = c(21, 5, 47) #Median, IQR

included_top = read.csv("../data/include_top.csv")
included_random = read.csv("../data/include_random.csv")


row_names = c("\\textbf{Papers}", 
              "\\hspace{5mm}Peer reviewed$^\\dag$, $n$",
              "\\hspace{5mm}Citations, median (IQR$^*$)",
              "",
              "\\textbf{Journal Mandate}",
              "\\hspace{5mm}Data availability, $n$",
              "\\hspace{5mm}Code availability, $n$",
              "\\hspace{5mm}Data and Code availability, $n$",
              " ",
              "\\textbf{Disease}",
              "\\hspace{5mm}COVID, $n$",
              "\\hspace{5mm}Other, $n$",
              "  ",
              "\\textbf{Link provided}",
              "\\hspace{5mm}Data, $n$",
              "\\hspace{5mm}Code, $n$",
              "\\hspace{5mm}Code and Data, $n$")
#              "   ",


results_table = data.frame(row.names = row_names)
results_table$Random = get_collumn_table_1(included_random, papers_total_random, citations_random[1], citations_random[2], citations_random[3])
results_table$Topcited = get_collumn_table_1(included_top, papers_total_top, citations_top[1], citations_top[2], citations_top[3])
print(results_table)
latex_of_table = xtable(results_table, type = "latex", auto = TRUE)
align(latex_of_table) <- table_alignment
print(latex_of_table, tabular.environment = "tabular", file = "table1.tex", sanitize.text.function=identity)


