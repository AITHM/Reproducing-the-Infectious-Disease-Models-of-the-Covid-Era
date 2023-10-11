library(MASS)
library(car)
library(nnet)

included_top = read.csv("../data/include_top.csv")
included_random = read.csv("../data/include_random.csv")

has_code_top = included_top[!is.na(included_top$language),]
has_code_random = included_random[!is.na(included_random$language),]
has_code_random[has_code_random$language == "python",]$language = "Other"

make_table = function(data_table){
  test_table = data.frame(matrix(NA, nrow = nrow(data_table), ncol = 4))
  colnames(test_table) = c("ouputs", "instructions", "notebook", "language")
  test_table$outputs = 2 * data_table$Outputs.match 
  test_table$instructions = ceiling(data_table$Instructions.Provided)
  test_table$notebook = data_table$Is.a.notebook
  test_table$language = factor(data_table$language)
  #test_table$Citations = log(data_table$Citations.x + 1)
  return(test_table)
}
univariate_table = function(data_table, inputs){
  release = make_table(data_table)
  included_inputs = c()
  univariate_results = c()
  for(i in 1:length(inputs)){
    f = paste("as.factor(outputs)", "~", inputs[i])
    m = polr(formula = f, data = release, Hess = TRUE)
    proportional_test = poTest(m)
    p_value_test = pchisq(poTest(m)[6]$chisq, df=poTest(m)[7]$df, lower.tail=FALSE)
    print(poTest(m))
    print(p_value_test)
    if(p_value_test < 0.05){
      print("Failed potest")
      print(Anova(m))
      m = multinom(formula = formula(f), data = release, Hess = TRUE)
      print(Anova(m))
    }
    p_values = Anova(m)
    or_random = exp(coef(m))
    confint_random = exp(confint(m))
    print(or_random)
    print(confint_random)
    results =  p_values
    if(p_values[3] < 0.10){
      included_inputs = append(included_inputs, inputs[i])
    }
    univariate_results = rbind(univariate_results, results)
  }
  if(length(included_inputs) > 0){
    print("Included in multivariate analysis")
    print(included_inputs)
    f = paste("as.factor(outputs)", "~", paste(included_inputs, collapse=" + "))
    m = polr(formula = f, data = release, Hess = TRUE)
    print(poTest(m))
    p_values = Anova(m)
    print(p_values)
  }
  return(univariate_results)
}
# Reproducible 
inputs = c("instructions", "notebook", "language")#, "Citations")
random_table = univariate_table(has_code_random, inputs)
print(random_table)
top_table = univariate_table(has_code_top, inputs)
print(top_table)