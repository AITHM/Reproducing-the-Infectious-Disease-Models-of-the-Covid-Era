library(dplyr)
library(xtable)
options(xtable.floating = FALSE)

outputs_match = function(data){
  return(list(data[data$Outputs_fully_match == 1,], data[data$Outputs.partial.match == 1,], data))
}

table_alignment = "l|rrrrrr"
get_collumn_table = function(data, index){
  contains_code = data[!is.na(data$language),]
  reproduces = outputs_match(contains_code)
  
  Notebook = contains_code[contains_code$Is.a.notebook == 1,]
  notebook_reproduces = outputs_match(Notebook)
  not_a_notebook = contains_code[contains_code$Is.a.notebook != 1,]
  not_a_notebook_reproduces = outputs_match(not_a_notebook)
  
  Instructions = contains_code[contains_code$Instructions.Provided != 0,]
  instructions_reproduces = outputs_match(Instructions)
  noinstructions = contains_code[contains_code$Instructions.Provided == 0,]
  noinstructions_reproduces = outputs_match(noinstructions)
  
  languages = c("R", "matlab", "python", "Other")
  language_results = c()
  for (language in languages){
    l = contains_code[contains_code$language == language,]
    langauge_outputs = outputs_match(l)
    language_results = append(language_results, c(nrow(langauge_outputs[[index]])))
  }
  return(append(c(nrow(reproduces[[index]]), "","", nrow(notebook_reproduces[[index]]), nrow(not_a_notebook_reproduces[[index]]),
                  "","", nrow(instructions_reproduces[[index]]), nrow(noinstructions_reproduces[[index]]), "", ""),language_results))
}

row_names = c("{Total}",
              "",
              "\\textbf{Format}",
              "{Notebook}",
              "{Other}",
              " ",
              "\\textbf{Instructions}",
              "{Provided}",
              "{None}",
              "  ",
              "\\textbf{Language}",
              "\\hspace{5mm}{R}",
              "\\hspace{5mm}{Matlab}",
              "\\hspace{5mm}{Python}",
              "\\hspace{5mm}{Other}"
)

included_top = read.csv("../data/include_top.csv")
included_random = read.csv("../data/include_random.csv")

results_table = data.frame(row.names = row_names)
results_table$Totalrandom = get_collumn_table(included_random, 3)
results_table$Fullrandom = get_collumn_table(included_random, 1)
results_table$Partialrandom = get_collumn_table(included_random, 2)
results_table$Totaltop = get_collumn_table(included_top, 3)
results_table$FullTopcited = get_collumn_table(included_top, 1)
results_table$PartialTopcited = get_collumn_table(included_top, 2)
print(results_table)
latex_of_table = xtable(results_table, type = "latex", auto = TRUE)
align(latex_of_table) <- table_alignment
print(latex_of_table, tabular.environment = "tabular", file = "table2.tex", sanitize.text.function=identity)
