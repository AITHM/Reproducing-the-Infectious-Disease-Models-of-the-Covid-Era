library(car)
library(questionr)
library(xtable)

make_table = function(data_table, collumn_name, column_value){
  test_table = data.frame(matrix(NA, nrow = nrow(data_table), ncol = 4))
  colnames(test_table) = c(collumn_name, "datamandate", "codemandate")#, "Citations")
  test_table[collumn_name] = column_value
  test_table$datamandate = data_table$mandated_data
  test_table$codemandate = data_table$mandated_code
  #test_table$Citations = log(data_table$Citations.x + 1)
  return(test_table)
}

data_set_analysis = function(data_table, ouutcome){
  release = make_table(data_table, "contains", ouutcome)
  inputs = c("datamandate", "codemandate")#, "Citations")
  included_inputs = c()
  univariate_results = c()
  for(i in 1:length(inputs)){
    f = paste("as.factor(contains)", "~", inputs[i])
    m = glm(formula = f, data = release, family = "binomial")
    p_values = Anova(m)
    odds_ratio = odds.ratio(m)[-1,-4]
    results = cbind(odds_ratio, p_values)
    if(p_values[3] < 0.10){
      included_inputs = append(included_inputs, inputs[i])
    }
    univariate_results = rbind(univariate_results, results)
  }
  if(length(included_inputs) > 0){
    print("Included in multivariate analysis")
    print(included_inputs)
    f = paste("as.factor(contains)", "~", paste(included_inputs, collapse=" + "))
    m = glm(formula = f, data = release, family = "binomial")
    p_values = Anova(m)
    odds_ratio = odds.ratio(m)[-1,]
    print(odds_ratio)
    print(p_values)
  }
  return(univariate_results)
}

analysis = function(data_table){
  print("Data available")
  data_results = data_set_analysis(data_table, data_table$has_data)
  print("Code available")
  code_results = data_set_analysis(data_table, !is.na(data_table$language))
  return(rbind(data_results, code_results))
}

included_top = read.csv("../data/include_top.csv")
included_random = read.csv("../data/include_random.csv")
random_results = analysis(included_random)
top_results = analysis(included_top)
results_table = signif(cbind(random_results, top_results), 3)
latex_of_table = xtable(results_table, type = "latex", auto = TRUE)
print(latex_of_table, tabular.environment = "tabular", file = "univariate.tex", sanitize.text.function=identity)
print(results_table)
