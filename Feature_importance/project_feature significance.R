X=read.csv('independent_var.csv',header = TRUE)
Y=read.csv('dependent_var.csv',header=TRUE)
data=cbind(X,Y)
options(max.print = 2000) 
# Fit logistic regression model
model <- glm(Churn.Value ~ factor(Gender) + factor(Married) + `Number.of.Dependents` +
               `Number.of.Referrals` + `Tenure.in.Months` + factor(Offer) +
               `Avg.Monthly.Long.Distance.Charges` + factor(`Multiple.Lines`) + factor(Internet.Type) +
               `Avg.Monthly.GB.Download` + factor(`Online.Security`) + factor(`Online.Backup`) +
               factor(`Device.Protection.Plan`) + factor(`Premium.Tech.Support`) + factor(`Streaming.TV`) +
               factor(`Streaming.Movies`) + factor(`Streaming.Music`) + factor(`Contract`) +
               factor(`Paperless.Billing`) + factor(`Payment.Method`) + `Monthly.Charge` +
               `Total.Refunds` + `Total.Extra.Data.Charges` + `Total.Revenue`  +
               `CLTV` + `Population` + factor(`Age.group`,levels = c("<30","30-39","40-49","50-59","60+"),ordered = TRUE), data = data, family = binomial(link = "logit"))



model_summary=summary(model)
p_values <- model_summary$coefficients[, "Pr(>|z|)"]


# Threshold for significance
threshold <- 0.05

# Create a table with coefficients, p-values, and significance
table_data <- data.frame(
  Coefficients = rownames(model_summary$coefficients),
  P_Values = p_values,
  Significance = ifelse(p_values < threshold, "Significant", "Not Significant")
)

# Print the table
print(table_data)


