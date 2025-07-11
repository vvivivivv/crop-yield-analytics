setwd("~/Downloads/BC2407 CBA")

library(data.table)
library(ggplot2)
library(dplyr)
library(corrplot)

library(arules)
library(arulesViz)
library(car)
library(caret)
library(randomForest)
library(caTools)
library(earth)


data = fread("cropyield1.csv")

# 2693 rows
nrow(data)
sum()

# -----------------------------------------------------------------------
# part a: data cleaning

# no duplicate or missing values
sum(duplicated(data))
sum(is.na(data))

# no wrongly recorded data (empty spaces)
for (i in seq_along(data)){
  column = names(data)[i]
  empty = sum(data[[i]] == "")
  cat(column, ":", empty, "\n")
}

str(data)
# check if cat var are factors
print(sapply(data, is.factor)) 

# since they are not, convert to factor
data$Crop = as.factor(data$Crop)
data$State = as.factor(data$State)
data$Season = as.factor(data$Season)

str(data)

unique(data$Crop)
unique(data$Crop_Year)
unique(data$State)


# treat Kharif, Rabi, and Whole Year as cropping seasons

unique(data$Season)
table(data$Season)

# replace Autumn with Kharif, and Winter/Summer to Others
data = data %>%
  mutate(Season=case_when(
    Season=="Autumn" ~ "Kharif",
    Season=="Winter" ~ "Others",
    Season=="Summer" ~ "Others",
    TRUE ~ Season  
))

data$Season = as.factor(data$Season)

#write.csv(data, "updated_data.csv", row.names=FALSE)

head(data)
str(data)

ggplot(data, aes(x = Yield)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Yield", x = "Yield", y = "Count") +
  theme_minimal()


summary(data)

# detect outliers
detect_outliers = function(variable) {
  q1 = quantile(variable, 0.25, na.rm=TRUE)
  q3 = quantile(variable, 0.75, na.rm=TRUE)
  iqr = q3 - q1
  
  lower = q1 - 1.5 * iqr
  upper = q3 + 1.5 * iqr
  
  outliers = variable[variable < lower | variable > upper]
  return(outliers)
}

outliers_yield = detect_outliers(data$Yield)
print(length(outliers_yield))

outlier_yield_rows = data[data$Yield %in% outliers_yield, ]
summary(outlier_yield_rows)

par(mfrow=c(1,1))
ggplot(data, aes(y = Yield, x = seq(1, length(data$Yield)))) + geom_point()

#write.csv(outlier_yield_rows, "outlier_yield_rows.csv", row.names = FALSE)

# in particular: 3 entries that are unusually high (yields of 249.99, 311, 311.02)
# remove these outliers

data = data %>% filter(Yield <= 40)
summary(data$Yield)
ggplot(data, aes(y = Yield, x = seq(1, length(data$Yield)))) + geom_point()

data$`Yield_Output (Yield*Area)` = data$Yield * data$Area

str(data)
nrow(data)


# -----------------------------------------------------------------------
# part a: notable finding 1

# yield
# association rules for cat var
summary(data$Yield)

data_assoc_yield = data
data_assoc_yield$`Yield_Output (Yield*Area)` = NULL

data_assoc_yield$Yield_Category = cut(data_assoc_yield$Yield, 
                               breaks=c(-Inf, 1.665, 6.303, Inf), 
                               labels=c("Low", "Medium", "High"))
table(data_assoc_yield$Yield_Category)
unique(data_assoc_yield$Yield_Category)
str(data_assoc_yield)

# models and analysis
# association rules (cat var)
transactions = as(data_assoc_yield[, c("Crop", "Season", "State", "Yield_Category")], "transactions")

rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2))
rules_df = as(rules, "data.frame")

# filter based on high yield, lift (strong association), and confidence (reliable)
rules_high_yield = subset(rules, subset=rhs %in% "Yield_Category=High" & lift > 1.5 & confidence > 0.7)
rules_high_yield
inspect(rules_high_yield)

plot(rules_high_yield, method="scatterplot", measure=c("support", "confidence"), shading="lift")

rules_high_yield_sorted = sort(rules_high_yield, by="lift", decreasing=TRUE)
top_rules = head(rules_high_yield_sorted, 10)
inspect(top_rules)

plot(rules_high_yield, method = "graph", engine = "htmlwidget")


# create interaction terms - only to be used in lin reg
for (i in 1:length(top_rules)) {
  rule = top_rules[i]
  
  # extract lhs
  lhs = unlist(lapply(as(lhs(rule), "list"), function(x) x))
  
  term_name = paste(lhs, collapse = "_")
  
  if (term_name %in% colnames(data_assoc_yield)) {
    next
  }
  
  # initialise 
  condition = rep(TRUE, nrow(data_assoc_yield))
  
  for (a in lhs) {
    var_name = strsplit(a, "=")[[1]][1]
    var_value = strsplit(a, "=")[[1]][2]
    
    condition = condition & (as.character(data_assoc_yield[[var_name]])==var_value)
  }
  data_assoc_yield[[term_name]] = as.integer(condition)
}

head(data_assoc_yield)

data_assoc_yield$`Crop=Potato` = NULL
data_assoc_yield$`Crop=Potato_Season=Others` = NULL

data_assoc_yield$`Season=Whole Year_State=Haryana` = as.factor(data_assoc_yield$`Season=Whole Year_State=Haryana`)
data_assoc_yield$`Crop=Potato_State=Uttarakhand` = as.factor(data_assoc_yield$`Crop=Potato_State=Uttarakhand`)
data_assoc_yield$`Crop=Potato_State=Meghalaya` = as.factor(data_assoc_yield$`Crop=Potato_State=Meghalaya`)
data_assoc_yield$`Crop=Potato_State=West Bengal` = as.factor(data_assoc_yield$`Crop=Potato_State=West Bengal`)
data_assoc_yield$`Season=Whole Year_State=Assam` = as.factor(data_assoc_yield$`Season=Whole Year_State=Assam`)
data_assoc_yield$`Crop=Potato_Season=Whole Year` = as.factor(data_assoc_yield$`Crop=Potato_Season=Whole Year`)
data_assoc_yield$`Crop=Potato_State=Karnataka` = as.factor(data_assoc_yield$`Crop=Potato_State=Karnataka`)
data_assoc_yield$`Crop=Potato_Season=Rabi` = as.factor(data_assoc_yield$`Crop=Potato_Season=Rabi`)


head(data_assoc_yield)
str(data)


# for yield output
data_assoc_yieldoutput = data
summary(data$`Yield_Output (Yield*Area)`)
data_assoc_yieldoutput$Yield = NULL
data_assoc_yieldoutput$Yield_Category = cut(data_assoc_yieldoutput$`Yield_Output (Yield*Area)`, 
                                breaks=c(-Inf, 21851, 854105, Inf), 
                                labels=c("Low", "Medium", "High"))

table(data_assoc_yieldoutput$Yield_Category)
unique(data_assoc_yieldoutput$Yield_Category)
str(data_assoc_yieldoutput)

# models and analysis
# association rules (cat var)
transactions_yieldoutput = as(data_assoc_yieldoutput[, c("Crop", "Season", "State", "Yield_Category")], "transactions")

rules_yieldoutput = apriori(transactions_yieldoutput, parameter = list(supp=0.01, conf=0.5, minlen=2))
rules_df_yieldoutput = as(rules_yieldoutput, "data.frame")

# filter based on high yield, lift (strong association), and confidence (reliable)
rules_high_yield_yieldoutput = subset(rules_yieldoutput, subset=rhs %in% "Yield_Category=High" & lift > 1.5 & confidence > 0.7)
rules_high_yield_yieldoutput
inspect(rules_high_yield_yieldoutput)

rules_high_yield_yieldoutput_sorted = sort(rules_high_yield_yieldoutput, by="lift", decreasing=TRUE)
inspect(rules_high_yield_yieldoutput_sorted)

plot(rules_high_yield_yieldoutput, method="scatterplot", measure=c("support", "confidence"), shading="lift")

plot(rules_high_yield_yieldoutput, method = "graph", engine = "htmlwidget")


# create interaction terms - only to be used in lin reg
for (i in 1:length(rules_high_yield_yieldoutput)) {
  rule = rules_high_yield_yieldoutput[i]
  
  # extract lhs
  lhs = unlist(lapply(as(lhs(rule), "list"), function(x) x))
  
  term_name = paste(lhs, collapse = "_")
  
  if (term_name %in% colnames(data_assoc_yieldoutput)) {
    next
  }
  
  # initialise 
  condition = rep(TRUE, nrow(data_assoc_yieldoutput))
  
  for (a in lhs) {
    var_name = strsplit(a, "=")[[1]][1]
    var_value = strsplit(a, "=")[[1]][2]
    
    condition = condition & (as.character(data_assoc_yieldoutput[[var_name]])==var_value)
  }
  data_assoc_yieldoutput[[term_name]] = as.integer(condition)
}

head(data_assoc_yieldoutput)
data_assoc_yieldoutput$`Crop=Rice_Season=Others_State=Assam` = NULL
data_assoc_yieldoutput$`Season=Others_State=West Bengal` = NULL
data_assoc_yieldoutput$`State=Punjab` = NULL

data_assoc_yieldoutput$`Crop=Rice_State=Tamil Nadu` = as.factor(data_assoc_yieldoutput$`Crop=Rice_State=Tamil Nadu`)
data_assoc_yieldoutput$`Season=Rabi_State=Uttar Pradesh` = as.factor(data_assoc_yieldoutput$`Season=Rabi_State=Uttar Pradesh`)
data_assoc_yieldoutput$`Season=Rabi_State=Bihar` = as.factor(data_assoc_yieldoutput$`Season=Rabi_State=Bihar`)
data_assoc_yieldoutput$`Season=Others_State=Assam` = as.factor(data_assoc_yieldoutput$`Season=Others_State=Assam`)
data_assoc_yieldoutput$`Crop=Rice_Season=Others_State=West Bengal` = as.factor(data_assoc_yieldoutput$`Crop=Rice_Season=Others_State=West Bengal`)
data_assoc_yieldoutput$`Crop=Rice_State=Andhra Pradesh` = as.factor(data_assoc_yieldoutput$`Crop=Rice_State=Andhra Pradesh`)

head(data_assoc_yieldoutput)


# -----------------------------------------------------------------------
# part a: notable finding 2

# multicollinearity, correlation, and creation of new variables
lm_assoc_yield = lm(Yield ~., data=data_assoc_yield)
vif(lm_assoc_yield)
alias(lm_assoc)

# correlation for numeric var 
# fertilizer, pesticide, and area has high correlation with one another
num_vars = data %>% select(Crop_Year, Area, Annual_Rainfall, Fertilizer, Pesticide, Yield)
cor_matrix = cor(num_vars, use="complete.obs")
corrplot(cor_matrix, method="color", addCoef.col="black")


unique(data$State)

# group states into regions
data_assoc_yield$Region = ifelse(data_assoc_yield$State %in% c("Madhya Pradesh", "Maharashtra", "Punjab", "Uttar Pradesh", 
                                                    "Haryana", "Himachal Pradesh", "Chhattisgarh", "Uttarakhand", 
                                                    "Delhi", "Jammu and Kashmir"), "North",
                            
                     ifelse(data_assoc_yield$State %in% c("Karnataka", "Puducherry", "Kerala", "Andhra Pradesh", "Tamil Nadu", 
                                                    "Telangana"), "South",
                            
                     ifelse(data_assoc_yield$State %in% c("Bihar", "Assam", "Meghalaya", "West Bengal", "Odisha", "Mizoram", 
                                                    "Tripura", "Nagaland", "Jharkhand", "Manipur", "Arunachal Pradesh", "Sikkim"), "East", "West")))
                     
data_assoc_yield$Region = as.factor(data_assoc_yield$Region)
#write.csv(data_assoc, "data_assoc.csv", row.names=FALSE)                     

# create new column for agricultural inputs (fertilizer + pesticide)
lm_agri_yield = lm(Yield ~ Fertilizer+Pesticide, data=data_assoc_yield)
summary(lm_agri_yield)

coef_pesticide_yield = coef(lm_agri_yield)["Pesticide"]
coef_fertilizer_yield = coef(lm_agri_yield)["Fertilizer"]
total_coef_yield = abs(coef_pesticide_yield) + abs(coef_fertilizer_yield)

weight_pesticide_yield = abs(coef_pesticide_yield) / total_coef_yield
weight_fertilizer_yield = abs(coef_fertilizer_yield) / total_coef_yield

data_assoc_yield$Agricultural_Inputs = (weight_fertilizer_yield * data_assoc_yield$Fertilizer) + (weight_pesticide_yield * data_assoc_yield$Pesticide)

data_assoc_yield$Area = NULL
data_assoc_yield$State = NULL
data_assoc_yield$Pesticide = NULL
data_assoc_yield$Fertilizer = NULL
data_assoc_yield$Yield_Category = NULL

head(data_assoc_yield)
str(data_assoc_yield)

lm_assoc_yield = lm(Yield ~., data=data_assoc_yield)
vif(lm_assoc_yield)




# yield output
str(data_assoc_yieldoutput)
lm_assoc_yieldoutput = lm(`Yield_Output (Yield*Area)` ~., data=data_assoc_yieldoutput)
vif(lm_assoc_yieldoutput)
alias(lm_assoc_yieldoutput)


# group states into regions
data_assoc_yieldoutput$Region = ifelse(data_assoc_yieldoutput$State %in% c("Bihar", "Madhya Pradesh", "Maharashtra", "Punjab", "Uttar Pradesh", 
                                                               "Haryana", "Himachal Pradesh", "Chhattisgarh", "Uttarakhand", 
                                                               "Delhi", "Jammu and Kashmir"), "North",
                                 
                                 ifelse(data_assoc_yieldoutput$State %in% c("Karnataka", "Puducherry", "Kerala", "Andhra Pradesh", "Tamil Nadu", 
                                                                      "Telangana"), "South",
                                        
                                        ifelse(data_assoc_yieldoutput$State %in% c("Assam", "Meghalaya", "West Bengal", "Odisha", "Mizoram", 
                                                                             "Tripura", "Nagaland", "Jharkhand", "Manipur", "Arunachal Pradesh", "Sikkim"), "East", "West")))

data_assoc_yieldoutput$Region = as.factor(data_assoc_yieldoutput$Region)
#write.csv(data_assoc, "data_assoc.csv", row.names=FALSE)                     

# create new column for agricultural inputs (fertilizer + pesticide)
lm_agri_yieldoutput = lm(`Yield_Output (Yield*Area)` ~ Fertilizer+Pesticide, data=data_assoc_yieldoutput)
summary(lm_agri_yieldoutput)

coef_pesticide_yieldoutput = coef(lm_agri_yieldoutput)["Pesticide"]
coef_fertilizer_yieldoutput = coef(lm_agri_yieldoutput)["Fertilizer"]
total_coef_yieldoutput = abs(coef_pesticide_yieldoutput) + abs(coef_fertilizer_yieldoutput)

weight_pesticide_yieldoutput = abs(coef_pesticide_yieldoutput) / total_coef_yieldoutput
weight_fertilizer_yieldoutput = abs(coef_fertilizer_yieldoutput) / total_coef_yieldoutput

data_assoc_yieldoutput$Agricultural_Inputs = (weight_fertilizer_yieldoutput * data_assoc_yieldoutput$Fertilizer) + (weight_pesticide_yieldoutput * data_assoc_yieldoutput$Pesticide)

data_full_yieldoutput = data_assoc_yieldoutput
head(data_full_yieldoutput)
str(data_full_yieldoutput)

data_assoc_yieldoutput$Area = NULL
data_assoc_yieldoutput$State = NULL
data_assoc_yieldoutput$Pesticide = NULL
data_assoc_yieldoutput$Fertilizer = NULL
data_assoc_yieldoutput$Yield_Category = NULL

head(data_assoc_yieldoutput)
str(data_assoc_yieldoutput)

lm_assoc_yieldoutput = lm(`Yield_Output (Yield*Area)` ~., data=data_assoc_yieldoutput)
vif(lm_assoc_yieldoutput)

# -----------------------------------------------------------------------
# part a: notable finding 3

# graph analysis
str(data)
print(colnames(data_full))
# numeric: Crop_Year, Area, Annual_Rainfall, Fertilizer, Pesticide, (Agricultural_Inputs), Yield_Output (Yield*Area)
# categorical: Crop, Season, (Region), State

num_var = c("Crop_Year", "Area", "Annual_Rainfall", "Fertilizer", "Pesticide")
cat_var = c("Crop", "Season", "State")


# Open PDF device
pdf("CBA_EDA_Yield.pdf", width = 8, height = 6)

# box plot for cat var
for (var in cat_var) {
  plot = ggplot(data, aes_string(x=var, y="Yield", fill=var)) +
    geom_boxplot() +
    labs(title=paste("Yield Distribution by", var),
         x=var, y="Crop Yield") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  print(plot)
}

# region
plot = ggplot(data_assoc_yield, aes_string(x="Region", y="Yield", fill="Region")) +
  geom_boxplot() +
  labs(title=paste("Yield Distribution by Region"),
       x="Region", y="Crop Yield") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
print(plot)


# crop yield over time
line_plot = ggplot(data, aes(x=Crop_Year, y=Yield, group=Crop, color=Crop)) +
  geom_line() +
  geom_point() +
  labs(title="Crop Yield Over Time", x="Year", y="Crop Yield") +
  theme_minimal()
print(line_plot)


# bar plots for cat var
for (var in cat_var) {
  plot = ggplot(data, aes_string(x=var, fill=var)) +
    geom_bar() +
    labs(title=paste("Frequency of", var), x=var, y="Count") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  print(plot)
}

# region
plot = ggplot(data_assoc_yield, aes_string(x="Region", fill="Region")) +
  geom_bar() +
  labs(title=paste("Frequency of Region"), x="Region", y="Count") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))
print(plot)


# facet plots for num var by crop
for (var in num_var) {
  plot = ggplot(data, aes_string(x=var, y="Yield")) +
    geom_point(alpha=0.5, color="blue") +
    geom_smooth(method="lm", se=FALSE, color="red") +
    facet_wrap(~ Crop, scales="free") +
    labs(title=paste("Effect of", var, "on Yield Across Crops"),
         x=var, y="Crop Yield") +
    theme_minimal()
  print(plot)
}

# agricultural inputs
plot = ggplot(data_assoc_yield, aes_string(x="Agricultural_Inputs", y="Yield")) +
  geom_point(alpha=0.5, color="blue") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  facet_wrap(~ Crop, scales="free") +
  labs(title=paste("Effect of Agricultural Inputs on Yield Across Crops"),
       x="Agricultural_Inputs", y="Crop Yield") +
  theme_minimal()
print(plot)


# avg yield by cat var
for (var in cat_var) {
  var_yield = data %>%
    group_by(!!sym(var)) %>%  
    summarise(Average_Yield=mean(Yield, na.rm=TRUE)) %>%
    arrange(desc(Average_Yield))  
  
  var_plot = ggplot(var_yield, aes(x=reorder(!!sym(var), -Average_Yield), y=Average_Yield)) +
    geom_bar(stat="identity", fill="blue") +
    labs(title=paste("Average Crop Yield by", var), x=var, y="Average Yield (metric tons per unit area)") +
    theme(axis.text.x=element_text(angle=90, hjust=1))  
  
  print(var_plot)
}

# region
var_yield = data_assoc_yield %>%
  group_by(!!sym("Region")) %>%  
  summarise(Average_Yield=mean(Yield, na.rm=TRUE)) %>%
  arrange(desc(Average_Yield))  

var_plot = ggplot(var_yield, aes(x=reorder(!!sym("Region"), -Average_Yield), y=Average_Yield)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title=paste("Average Crop Yield by Region"), x="Region", y="Average Yield (metric tons per unit area)") +
  theme(axis.text.x=element_text(angle=90, hjust=1))  

print(var_plot)


dev.off()


# yield output
pdf("CBA_EDA_Yield Output.pdf", width = 8, height = 6)


# box plot for cat var
for (var in cat_var) {
  plot = ggplot(data, aes_string(x=var, y="`Yield_Output (Yield*Area)`", fill=var)) +
    geom_boxplot() +
    labs(title=paste("Yield Output Distribution by", var),
         x=var, y="Crop Yield Output (Yield*Area)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  print(plot)
}

# region
plot = ggplot(data_assoc_yieldoutput, aes_string(x="Region", y="`Yield_Output (Yield*Area)`", fill="Region")) +
  geom_boxplot() +
  labs(title=paste("Yield Output Distribution by Region"),
       x="Region", y="Crop Yield") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
print(plot)


# crop yield over time
line_plot = ggplot(data_assoc_yieldoutput, aes(x=Crop_Year, y=`Yield_Output (Yield*Area)`, group=Crop, color=Crop)) +
  geom_line() +
  geom_point() +
  labs(title="Crop Yield Output (Yield*Area) Over Time", x="Year", y="Crop Yield Output (Yield*Area)") +
  theme_minimal()
print(line_plot)


for (var in num_var) {
  plot = ggplot(data, aes_string(x=var, y="`Yield_Output (Yield*Area)`")) +
    geom_point(alpha=0.5, color="blue") +
    geom_smooth(method="lm", se=FALSE, color="red") +
    facet_wrap(~ Crop, scales="free") +
    labs(title=paste("Effect of", var, "on Yield Output Across Crops"),
         x=var, y="Crop Yield Output (Yield*Area)") +
    theme_minimal()
  print(plot)
}

plot = ggplot(data_assoc_yieldoutput, aes_string(x="Agricultural_Inputs", y="`Yield_Output (Yield*Area)`")) +
  geom_point(alpha=0.5, color="blue") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  facet_wrap(~ Crop, scales="free") +
  labs(title=paste("Effect of Agricultural Inputs on Yield Output Across Crops"),
       x="Agricultural_Inputs", y="Crop Yield Output (Yield*Area)") +
  theme_minimal()
print(plot)

# avg yield by cat var
for (var in cat_var) {
  var_yield = data %>%
    group_by(!!sym(var)) %>%  
    summarise(Average_Yield_Output=mean(`Yield_Output (Yield*Area)`, na.rm=TRUE)) %>%
    arrange(desc(Average_Yield_Output))  
  
  var_plot = ggplot(var_yield, aes(x=reorder(!!sym(var), -Average_Yield_Output), y=Average_Yield_Output)) +
    geom_bar(stat="identity", fill="blue") +
    labs(title=paste("Average Crop Yield Output by", var), x=var, y="Average Yield Output (metric tons)") +
    theme(axis.text.x=element_text(angle=90, hjust=1))  
  
  print(var_plot)
}

# region
var_yield = data_assoc_yieldoutput %>%
  group_by(!!sym("Region")) %>%  
  summarise(Average_Yield_Output=mean(`Yield_Output (Yield*Area)`, na.rm=TRUE)) %>%
  arrange(desc(Average_Yield_Output))  

var_plot = ggplot(var_yield, aes(x=reorder(!!sym("Region"), -Average_Yield_Output), y=Average_Yield_Output)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title=paste("Average Crop Yield Output by Region"), x="Region", y="Average Yield Output (metric tons)") +
  theme(axis.text.x=element_text(angle=90, hjust=1))  

print(var_plot)


dev.off()


# -----------------------------------------------------------------------
# part b: linear regression

# include previously added interaction variables + derived variables
str(data_assoc_yield)

# yield
# 70% train set, 30% test set
set.seed(999)
data_trainset_lr = subset(data_assoc_yield, Crop_Year < 2018)
data_testset_lr = subset(data_assoc_yield, Crop_Year >= 2018)

data_trainset_lr$Crop_Year = NULL
data_testset_lr$Crop_Year = NULL

data_trainset_lr = as.data.frame(data_trainset_lr) 
data_testset_lr = as.data.frame(data_testset_lr) 

# train linear reg model on train set
lr_train = lm(Yield ~., data=data_trainset_lr)

# predict on test set
lr_pred = predict(lr_train, newdata=data_testset_lr)

summary(lr_train)

rmse_lr = sqrt(mean((lr_pred - data_testset_lr$Yield)^2))
print(rmse_lr)

r2_lr = 1 - (sum((data_testset_lr$Yield - lr_pred)^2) / sum((data_testset_lr$Yield - mean(data_testset_lr$Yield))^2))
print(r2_lr)

# rmse: 3.601028, r2: 0.621422

# plotting of lm: model diagnosis
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lr_train)



# lr for each crop type
unique(data$Crop)

# rice
set.seed(999)
data_trainset_lr_rice = subset(data_assoc_yield, Crop_Year < 2018 & Crop == "Rice")
data_testset_lr_rice = subset(data_assoc_yield, Crop_Year >= 2018 & Crop == "Rice")

data_trainset_lr_rice$Crop_Year = NULL
data_testset_lr_rice$Crop_Year = NULL
data_trainset_lr_rice$Crop = NULL
data_testset_lr_rice$Crop = NULL
data_trainset_lr_rice$`Crop=Potato_State=Uttarakhand`=NULL
data_trainset_lr_rice$`Crop=Potato_State=Meghalaya`=NULL
data_trainset_lr_rice$`Crop=Potato_State=West Bengal`=NULL
data_trainset_lr_rice$`Crop=Potato_Season=Whole Year`=NULL
data_trainset_lr_rice$`Crop=Potato_State=Karnataka`=NULL
data_trainset_lr_rice$`Crop=Potato_Season=Rabi`=NULL

data_testset_lr_rice$`Crop=Potato_State=Uttarakhand`=NULL
data_testset_lr_rice$`Crop=Potato_State=Meghalaya`=NULL
data_testset_lr_rice$`Crop=Potato_State=West Bengal`=NULL
data_testset_lr_rice$`Crop=Potato_Season=Whole Year`=NULL
data_testset_lr_rice$`Crop=Potato_State=Karnataka`=NULL
data_testset_lr_rice$`Crop=Potato_Season=Rabi`=NULL

# interestingly, values for both columns = 0
data_trainset_lr_rice$`Season=Whole Year_State=Haryana` = NULL
data_trainset_lr_rice$`Season=Whole Year_State=Assam` = NULL

data_testset_lr_rice$`Season=Whole Year_State=Haryana` = NULL
data_testset_lr_rice$`Season=Whole Year_State=Assam` = NULL

head(data_trainset_lr_rice)

data_trainset_lr_rice = as.data.frame(data_trainset_lr_rice) 
data_testset_lr_rice = as.data.frame(data_testset_lr_rice) 

# train linear reg model on train set
lr_train_rice = lm(Yield ~., data=data_trainset_lr_rice)

# predict on test set
lr_pred_rice = predict(lr_train_rice, newdata=data_testset_lr_rice)

summary(lr_train_rice)

rmse_lr_rice = sqrt(mean((lr_pred_rice - data_testset_lr_rice$Yield)^2))
print(rmse_lr_rice)

r2_lr_rice = 1 - (sum((data_testset_lr_rice$Yield - lr_pred_rice)^2) / sum((data_testset_lr_rice$Yield - mean(data_testset_lr_rice$Yield))^2))
print(r2_lr_rice)

# rmse: 0.7125767, r2: 0.1146725



# ginger
set.seed(999)
data_trainset_lr_ginger = subset(data_assoc_yield, Crop_Year < 2018 & Crop == "Ginger")
data_testset_lr_ginger = subset(data_assoc_yield, Crop_Year >= 2018 & Crop == "Ginger")

data_trainset_lr_ginger$Crop_Year = NULL
data_testset_lr_ginger$Crop_Year = NULL
data_trainset_lr_ginger$Crop = NULL
data_testset_lr_ginger$Crop = NULL
data_trainset_lr_ginger$`Crop=Potato_State=Uttarakhand`=NULL
data_trainset_lr_ginger$`Crop=Potato_State=Meghalaya`=NULL
data_trainset_lr_ginger$`Crop=Potato_State=West Bengal`=NULL
data_trainset_lr_ginger$`Crop=Potato_Season=Whole Year`=NULL
data_trainset_lr_ginger$`Crop=Potato_State=Karnataka`=NULL
data_trainset_lr_ginger$`Crop=Potato_Season=Rabi`=NULL

data_testset_lr_ginger$`Crop=Potato_State=Uttarakhand`=NULL
data_testset_lr_ginger$`Crop=Potato_State=Meghalaya`=NULL
data_testset_lr_ginger$`Crop=Potato_State=West Bengal`=NULL
data_testset_lr_ginger$`Crop=Potato_Season=Whole Year`=NULL
data_testset_lr_ginger$`Crop=Potato_State=Karnataka`=NULL
data_testset_lr_ginger$`Crop=Potato_Season=Rabi`=NULL


data_trainset_lr_ginger = as.data.frame(data_trainset_lr_ginger) 
data_testset_lr_ginger = as.data.frame(data_testset_lr_ginger) 

# train linear reg model on train set
lr_train_ginger = lm(Yield ~., data=data_trainset_lr_ginger)

# predict on test set
lr_pred_ginger = predict(lr_train_ginger, newdata=data_testset_lr_ginger)

summary(lr_train_ginger)

rmse_lr_ginger = sqrt(mean((lr_pred_ginger - data_testset_lr_ginger$Yield)^2))
print(rmse_lr_ginger)

r2_lr_ginger = 1 - (sum((data_testset_lr_ginger$Yield - lr_pred_ginger)^2) / sum((data_testset_lr_ginger$Yield - mean(data_testset_lr_ginger$Yield))^2))
print(r2_lr_ginger)

# rmse: 3.905633, r2: 0.3756294



# wheat

set.seed(999)
data_trainset_lr_wheat = subset(data_assoc_yield, Crop_Year < 2018 & Crop == "Wheat")
data_testset_lr_wheat = subset(data_assoc_yield, Crop_Year >= 2018 & Crop == "Wheat")

data_trainset_lr_wheat$Crop_Year = NULL
data_testset_lr_wheat$Crop_Year = NULL
data_trainset_lr_wheat$Crop = NULL
data_testset_lr_wheat$Crop = NULL
data_trainset_lr_wheat$`Crop=Potato_State=Uttarakhand`=NULL
data_trainset_lr_wheat$`Crop=Potato_State=Meghalaya`=NULL
data_trainset_lr_wheat$`Crop=Potato_State=West Bengal`=NULL
data_trainset_lr_wheat$`Crop=Potato_Season=Whole Year`=NULL
data_trainset_lr_wheat$`Crop=Potato_State=Karnataka`=NULL
data_trainset_lr_wheat$`Crop=Potato_Season=Rabi`=NULL

data_testset_lr_wheat$`Crop=Potato_State=Uttarakhand`=NULL
data_testset_lr_wheat$`Crop=Potato_State=Meghalaya`=NULL
data_testset_lr_wheat$`Crop=Potato_State=West Bengal`=NULL
data_testset_lr_wheat$`Crop=Potato_Season=Whole Year`=NULL
data_testset_lr_wheat$`Crop=Potato_State=Karnataka`=NULL
data_testset_lr_wheat$`Crop=Potato_Season=Rabi`=NULL


# interestingly, values for both columns = 0
data_trainset_lr_wheat$`Season=Whole Year_State=Haryana` = NULL
data_trainset_lr_wheat$`Season=Whole Year_State=Assam` = NULL

data_testset_lr_wheat$`Season=Whole Year_State=Haryana` = NULL
data_testset_lr_wheat$`Season=Whole Year_State=Assam` = NULL


data_trainset_lr_wheat = as.data.frame(data_trainset_lr_wheat) 
data_testset_lr_wheat = as.data.frame(data_testset_lr_wheat) 

# train linear reg model on train set
lr_train_wheat = lm(Yield ~., data=data_trainset_lr_wheat)

# predict on test set
lr_pred_wheat = predict(lr_train_wheat, newdata=data_testset_lr_wheat)

summary(lr_train_wheat)

rmse_lr_wheat = sqrt(mean((lr_pred_wheat - data_testset_lr_wheat$Yield)^2))
print(rmse_lr_wheat)

r2_lr_wheat = 1 - (sum((data_testset_lr_wheat$Yield - lr_pred_wheat)^2) / sum((data_testset_lr_wheat$Yield - mean(data_testset_lr_wheat$Yield))^2))
print(r2_lr_wheat)

# rmse: 0.8567925, r2: 0.4703414


# potato

set.seed(999)
data_trainset_lr_potato = subset(data_assoc_yield, Crop_Year < 2018 & Crop == "Potato")
data_testset_lr_potato = subset(data_assoc_yield, Crop_Year >= 2018 & Crop == "Potato")

data_trainset_lr_potato$Crop_Year = NULL
data_testset_lr_potato$Crop_Year = NULL
data_trainset_lr_potato$Crop = NULL
data_testset_lr_potato$Crop = NULL

data_trainset_lr_potato = as.data.frame(data_trainset_lr_potato) 
data_testset_lr_potato = as.data.frame(data_testset_lr_potato) 

# train linear reg model on train set
lr_train_potato = lm(Yield ~., data=data_trainset_lr_potato)

# predict on test set
lr_pred_potato = predict(lr_train_potato, newdata=data_testset_lr_potato)

summary(lr_train_potato)

rmse_lr_potato = sqrt(mean((lr_pred_potato - data_testset_lr_potato$Yield)^2))
print(rmse_lr_potato)

r2_lr_potato = 1 - (sum((data_testset_lr_potato$Yield - lr_pred_potato)^2) / sum((data_testset_lr_potato$Yield - mean(data_testset_lr_potato$Yield))^2))
print(r2_lr_potato)

# rmse: 3.843506, r2: 0.6950562






# yield output
str(data_assoc_yieldoutput)

set.seed(535)
data_trainset_lr2 = subset(data_assoc_yieldoutput, Crop_Year < 2018)
data_testset_lr2 = subset(data_assoc_yieldoutput, Crop_Year >= 2018)

data_trainset_lr2$Crop_Year = NULL
data_testset_lr2$Crop_Year = NULL

data_trainset_lr2 = as.data.frame(data_trainset_lr2) 
data_testset_lr2 = as.data.frame(data_testset_lr2) 

# train linear reg model on train set
lr_train2 = lm(`Yield_Output (Yield*Area)` ~., data=data_trainset_lr2)

summary(lr_train2)
# predict on test set
lr_pred2 = predict(lr_train2, newdata=data_testset_lr2)

rmse_lr2 = sqrt(mean((lr_pred2 - data_testset_lr2$`Yield_Output (Yield*Area)`)^2))
print(rmse_lr2)

r2_lr2 = 1 - (sum((data_testset_lr2$`Yield_Output (Yield*Area)` - lr_pred2)^2) / sum((data_testset_lr2$`Yield_Output (Yield*Area)` - mean(data_testset_lr2$`Yield_Output (Yield*Area)`))^2))
print(r2_lr2)

# rmse: 1526994, r2: 0.9079832

# plotting of lm: model diagnosis
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lr_train2)


# lr for each crop type

# rice
set.seed(999)
data_trainset_lr2_rice = subset(data_assoc_yieldoutput, Crop_Year < 2018 & Crop == "Rice")
data_testset_lr2_rice = subset(data_assoc_yieldoutput, Crop_Year >= 2018 & Crop == "Rice")

data_trainset_lr2_rice$Crop_Year = NULL
data_testset_lr2_rice$Crop_Year = NULL
data_trainset_lr2_rice$Crop = NULL
data_testset_lr2_rice$Crop = NULL

data_trainset_lr2_rice$`Season=Rabi_State=Uttar Pradesh` = NULL
data_trainset_lr2_rice$`Season=Rabi_State=Bihar` = NULL

data_testset_lr2_rice$`Season=Rabi_State=Uttar Pradesh` = NULL
data_testset_lr2_rice$`Season=Rabi_State=Bihar` = NULL


head(data_trainset_lr2_rice)

data_trainset_lr2_rice = as.data.frame(data_trainset_lr2_rice) 
data_testset_lr2_rice = as.data.frame(data_testset_lr2_rice) 

# train linear reg model on train set
lr_train2_rice = lm(`Yield_Output (Yield*Area)` ~., data=data_trainset_lr2_rice)

# predict on test set
lr_pred2_rice = predict(lr_train2_rice, newdata=data_testset_lr2_rice)

summary(lr_train2_rice)

rmse_lr2_rice = sqrt(mean((lr_pred2_rice - data_testset_lr2_rice$`Yield_Output (Yield*Area)`)^2))
print(rmse_lr2_rice)

r2_lr2_rice = 1 - (sum((data_testset_lr2_rice$`Yield_Output (Yield*Area)` - lr_pred2_rice)^2) / sum((data_testset_lr2_rice$`Yield_Output (Yield*Area)` - mean(data_testset_lr2_rice$`Yield_Output (Yield*Area)`))^2))
print(r2_lr2_rice)

# rmse: 1026618, r2: 0.8995647



# ginger
set.seed(999)
data_trainset_lr2_ginger = subset(data_assoc_yieldoutput, Crop_Year < 2018 & Crop == "Ginger")
data_testset_lr2_ginger = subset(data_assoc_yieldoutput, Crop_Year >= 2018 & Crop == "Ginger")

data_trainset_lr2_ginger$Crop_Year = NULL
data_testset_lr2_ginger$Crop_Year = NULL
data_trainset_lr2_ginger$Crop = NULL
data_testset_lr2_ginger$Crop = NULL

data_trainset_lr2_ginger$`Crop=Rice_State=Tamil Nadu` = NULL
data_trainset_lr2_ginger$`Crop=Rice_State=Andhra Pradesh` = NULL
data_trainset_lr2_ginger$`Crop=Rice_Season=Others_State=West Bengal` = NULL

data_testset_lr2_ginger$`Crop=Rice_State=Tamil Nadu` = NULL
data_testset_lr2_ginger$`Crop=Rice_State=Andhra Pradesh` = NULL
data_testset_lr2_ginger$`Crop=Rice_Season=Others_State=West Bengal` = NULL

# interestingly, these columns have 0 values
data_trainset_lr2_ginger$`Season=Rabi_State=Uttar Pradesh` = NULL
data_trainset_lr2_ginger$`Season=Rabi_State=Bihar` = NULL
data_trainset_lr2_ginger$`Season=Others_State=Assam` = NULL

data_testset_lr2_ginger$`Season=Rabi_State=Uttar Pradesh` = NULL
data_testset_lr2_ginger$`Season=Rabi_State=Bihar` = NULL
data_testset_lr2_ginger$`Season=Others_State=Assam` = NULL


data_trainset_lr2_ginger = as.data.frame(data_trainset_lr2_ginger) 
data_testset_lr2_ginger = as.data.frame(data_testset_lr2_ginger) 


# train linear reg model on train set
lr_train2_ginger = lm(`Yield_Output (Yield*Area)` ~., data=data_trainset_lr2_ginger)

# predict on test set
lr_pred2_ginger = predict(lr_train2_ginger, newdata=data_testset_lr2_ginger)

summary(lr_train2_ginger)

rmse_lr2_ginger = sqrt(mean((lr_pred2_ginger - data_testset_lr2_ginger$`Yield_Output (Yield*Area)`)^2))
print(rmse_lr2_ginger)

r2_lr2_ginger = 1 - (sum((data_testset_lr2_ginger$`Yield_Output (Yield*Area)` - lr_pred2_ginger)^2) / sum((data_testset_lr2_ginger$`Yield_Output (Yield*Area)` - mean(data_testset_lr2_ginger$`Yield_Output (Yield*Area)`))^2))
print(r2_lr2_ginger)

# rmse: 51404.62, r2: 0.7880011



# wheat

set.seed(999)
data_trainset_lr2_wheat = subset(data_assoc_yieldoutput, Crop_Year < 2018 & Crop == "Wheat")
data_testset_lr2_wheat = subset(data_assoc_yieldoutput, Crop_Year >= 2018 & Crop == "Wheat")


data_trainset_lr2_wheat$Crop_Year = NULL
data_testset_lr2_wheat$Crop_Year = NULL
data_trainset_lr2_wheat$Crop = NULL
data_testset_lr2_wheat$Crop = NULL


data_trainset_lr2_wheat$`Crop=Rice_State=Tamil Nadu` = NULL
data_trainset_lr2_wheat$`Crop=Rice_State=Andhra Pradesh` = NULL
data_trainset_lr2_wheat$`Crop=Rice_Season=Others_State=West Bengal` = NULL

data_testset_lr2_wheat$`Crop=Rice_State=Tamil Nadu` = NULL
data_testset_lr2_wheat$`Crop=Rice_State=Andhra Pradesh` = NULL
data_testset_lr2_wheat$`Crop=Rice_Season=Others_State=West Bengal` = NULL

# interestingly, these columns have 0 values
data_trainset_lr2_wheat$`Season=Others_State=Assam` = NULL

data_testset_lr2_wheat$`Season=Others_State=Assam` = NULL

data_trainset_lr2_wheat = as.data.frame(data_trainset_lr2_wheat) 
data_testset_lr2_wheat = as.data.frame(data_testset_lr2_wheat) 

# train linear reg model on train set
lr_train2_wheat = lm(`Yield_Output (Yield*Area)` ~., data=data_trainset_lr2_wheat)

# predict on test set
lr_pred2_wheat = predict(lr_train2_wheat, newdata=data_testset_lr2_wheat)

summary(lr_train_wheat)

rmse_lr2_wheat = sqrt(mean((lr_pred2_wheat - data_testset_lr2_wheat$`Yield_Output (Yield*Area)`)^2))
print(rmse_lr2_wheat)

r2_lr2_wheat = 1 - (sum((data_testset_lr2_wheat$`Yield_Output (Yield*Area)` - lr_pred2_wheat)^2) / sum((data_testset_lr2_wheat$`Yield_Output (Yield*Area)` - mean(data_testset_lr2_wheat$`Yield_Output (Yield*Area)`))^2))
print(r2_lr2_wheat)

# rmse: 1354738, r2: 0.979419


# potato

set.seed(999)
data_trainset_lr2_potato = subset(data_assoc_yieldoutput, Crop_Year < 2018 & Crop == "Potato")
data_testset_lr2_potato = subset(data_assoc_yieldoutput, Crop_Year >= 2018 & Crop == "Potato")

data_trainset_lr2_potato$Crop_Year = NULL
data_testset_lr2_potato$Crop_Year = NULL
data_trainset_lr2_potato$Crop = NULL
data_testset_lr2_potato$Crop = NULL


data_trainset_lr2_potato$`Crop=Rice_State=Tamil Nadu` = NULL
data_trainset_lr2_potato$`Crop=Rice_State=Andhra Pradesh` = NULL
data_trainset_lr2_potato$`Crop=Rice_Season=Others_State=West Bengal` = NULL

data_testset_lr2_potato$`Crop=Rice_State=Tamil Nadu` = NULL
data_testset_lr2_potato$`Crop=Rice_State=Andhra Pradesh` = NULL
data_testset_lr2_potato$`Crop=Rice_Season=Others_State=West Bengal` = NULL

# interestingly, these columns have 0 values
data_trainset_lr2_potato$`Season=Others_State=Assam` = NULL

data_testset_lr2_potato$`Season=Others_State=Assam` = NULL

data_trainset_lr2_potato = as.data.frame(data_trainset_lr2_potato) 
data_testset_lr2_potato = as.data.frame(data_testset_lr2_potato) 

# train linear reg model on train set
lr_train2_potato = lm(`Yield_Output (Yield*Area)` ~., data=data_trainset_lr2_potato)

# predict on test set
lr_pred2_potato = predict(lr_train2_potato, newdata=data_testset_lr2_potato)

summary(lr_train2_potato)

rmse_lr2_potato = sqrt(mean((lr_pred2_potato - data_testset_lr2_potato$`Yield_Output (Yield*Area)`)^2))
print(rmse_lr2_potato)

r2_lr2_potato = 1 - (sum((data_testset_lr2_potato$`Yield_Output (Yield*Area)` - lr_pred2_potato)^2) / sum((data_testset_lr2_potato$`Yield_Output (Yield*Area)` - mean(data_testset_lr2_potato$`Yield_Output (Yield*Area)`))^2))
print(r2_lr2_potato)

# rmse: 443282.9, r2: 0.980153



# -----------------------------------------------------------------------
# part b: MARS

# do not have to include interaction variables

str(data)
data2 = data
data2$`Yield_Output (Yield*Area)` = NULL
str(data2)

# test: use ori variables
set.seed(666)

data_trainset_marsv1 = subset(data2, Crop_Year < 2018)
data_testset_marsv1 = subset(data2, Crop_Year >= 2018)

data_trainset_marsv1$Crop_Year = NULL
data_testset_marsv1$Crop_Year = NULL

data_trainset_marsv1 = as.data.frame(data_trainset_marsv1) 
data_testset_marsv1 = as.data.frame(data_testset_marsv1) 

# MARS degree = 1
mars1 = earth(Yield ~ ., degree=1, trace=3, data=data_trainset_marsv1)
mars1$coefficients
summary(mars1)

mars1_pred = predict(mars1, newdata=data_testset_marsv1)
mars1_rmse = round(sqrt(mean((data_testset_marsv1$Yield - mars1_pred)^2)))
print(mars1_rmse)

r2_mars1 = 1 - (sum((data_testset_marsv1$Yield - mars1_pred)^2) / sum((data_testset_marsv1$Yield - mean(data_testset_marsv1$Yield))^2))
print(r2_mars1)

var_imp_mars1 = evimp(mars1)
print(var_imp_mars1)

# rmse: 3, r2: 0.7130894

# MARS degree = 2
mars2 = earth(Yield ~ ., degree=2, trace=3, data=data_trainset_marsv1)
mars2$coefficients
summary(mars2)

mars2_pred = predict(mars2, newdata=data_testset_marsv1)
mars2_rmse = round(sqrt(mean((data_testset_marsv1$Yield - mars2_pred)^2)))
print(mars2_rmse)

r2_mars2 = 1 - (sum((data_testset_marsv1$Yield - mars2_pred)^2) / sum((data_testset_marsv1$Yield - mean(data_testset_marsv1$Yield))^2))
print(r2_mars2)

var_imp_mars2 = evimp(mars2)
print(var_imp_mars2)

# rmse: 3, r2: 0.7625902


# test: use derived var
str(data_assoc_yield)
data_full2 = data_assoc_yield
str(data_full2)

data_full2$State = NULL
data_full2$Area = NULL
data_full2$Pesticide = NULL
data_full2$Fertilizer = NULL
data_full2$`Season=Whole Year_State=Haryana` = NULL
data_full2$`Crop=Potato_State=Uttarakhand` = NULL
data_full2$`Crop=Potato_State=Meghalaya` = NULL
data_full2$`Crop=Potato_State=West Bengal` = NULL
data_full2$`Season=Whole Year_State=Assam` = NULL
data_full2$`Crop=Potato_Season=Whole Year` = NULL
data_full2$`Crop=Potato_State=Karnataka` = NULL
data_full2$`Crop=Potato_Season=Rabi` = NULL


str(data_full2)

set.seed(777)
data_trainset_marsv2 = subset(data_full2, Crop_Year < 2018)
data_testset_marsv2 = subset(data_full2, Crop_Year >= 2018)

data_trainset_marsv2$Crop_Year = NULL
data_testset_marsv2$Crop_Year = NULL

data_trainset_marsv2 = as.data.frame(data_trainset_marsv2) 
data_testset_marsv2 = as.data.frame(data_testset_marsv2) 


# MARS degree = 1
set.seed(123)
mars3 = earth(Yield ~ ., degree=1, trace=3, data=data_trainset_marsv2)
mars3$coefficients
summary(mars3)

mars3_pred = predict(mars3, newdata=data_testset_marsv2)
mars3_rmse = round(sqrt(mean((data_testset_marsv2$Yield - mars3_pred)^2)))
print(mars3_rmse)

r2_mars3 = 1 - (sum((data_testset_marsv2$Yield - mars3_pred)^2) / sum((data_testset_marsv2$Yield - mean(data_testset_marsv2$Yield))^2))
print(r2_mars3)

# rmse: 4, r2: 0.6272192

# MARS degree = 2
set.seed(902)
mars4 = earth(Yield ~ ., degree=2, trace=3, data=data_trainset_marsv2)
mars4$coefficients
summary(mars4)

mars4_pred = predict(mars4, newdata=data_testset_marsv2)
mars4_rmse = round(sqrt(mean((data_testset_marsv2$Yield - mars4_pred)^2)))
print(mars4_rmse)

r2_mars4 = 1 - (sum((data_testset_marsv2$Yield - mars4_pred)^2) / sum((data_testset_marsv2$Yield - mean(data_testset_marsv2$Yield))^2))
print(r2_mars4)

# rmse: 3, r2: 0.692126

# using original data set gives better results



# MARS for each crop type

# rice

set.seed(666)

data_trainset_mars_rice = subset(data2, Crop_Year < 2018 & Crop == "Rice")
data_testset_mars_rice = subset(data2, Crop_Year >= 2018 & Crop == "Rice")

data_trainset_mars_rice$Crop_Year = NULL
data_testset_mars_rice$Crop_Year = NULL
data_trainset_mars_rice$Crop = NULL
data_testset_mars_rice$Crop = NULL

data_trainset_mars_rice = as.data.frame(data_trainset_mars_rice) 
data_testset_mars_rice = as.data.frame(data_testset_mars_rice) 

# MARS degree = 1
mars_rice = earth(Yield ~ ., degree=1, trace=3, data=data_trainset_mars_rice)
mars_rice$coefficients
summary(mars_rice)

mars_rice_pred = predict(mars_rice, newdata=data_testset_mars_rice)
mars_rice_rmse = round(sqrt(mean((data_testset_mars_rice$Yield - mars_rice_pred)^2)))
print(mars_rice_rmse)

r2_mars_rice = 1 - (sum((data_testset_mars_rice$Yield - mars_rice_pred)^2) / sum((data_testset_mars_rice$Yield - mean(data_testset_mars_rice$Yield))^2))
print(r2_mars_rice)

# rmse: 1, r2: 0.5600974


# ginger
set.seed(666)

data_trainset_mars_ginger = subset(data2, Crop_Year < 2018 & Crop == "Ginger")
data_testset_mars_ginger = subset(data2, Crop_Year >= 2018 & Crop == "Ginger")

data_trainset_mars_ginger$Crop_Year = NULL
data_testset_mars_ginger$Crop_Year = NULL
data_trainset_mars_ginger$Crop = NULL
data_testset_mars_ginger$Crop = NULL

data_trainset_mars_ginger = as.data.frame(data_trainset_mars_ginger) 
data_testset_mars_ginger = as.data.frame(data_testset_mars_ginger) 

# MARS degree = 1
mars_ginger = earth(Yield ~ ., degree=1, trace=3, data=data_trainset_mars_ginger)
mars_ginger$coefficients
summary(mars_ginger)

mars_ginger_pred = predict(mars_ginger, newdata=data_testset_mars_ginger)
mars_ginger_rmse = round(sqrt(mean((data_testset_mars_ginger$Yield - mars_ginger_pred)^2)))
print(mars_ginger_rmse)

r2_mars_ginger = 1 - (sum((data_testset_mars_ginger$Yield - mars_ginger_pred)^2) / sum((data_testset_mars_ginger$Yield - mean(data_testset_mars_ginger$Yield))^2))
print(r2_mars_ginger)

# rmse: 4, r2: 0.4198484


# wheat
set.seed(666)

data_trainset_mars_wheat = subset(data2, Crop_Year < 2018 & Crop == "Wheat")
data_testset_mars_wheat = subset(data2, Crop_Year >= 2018 & Crop == "Wheat")

data_trainset_mars_wheat$Crop_Year = NULL
data_testset_mars_wheat$Crop_Year = NULL
data_trainset_mars_wheat$Crop = NULL
data_testset_mars_wheat$Crop = NULL

data_trainset_mars_wheat = as.data.frame(data_trainset_mars_wheat) 
data_testset_mars_wheat = as.data.frame(data_testset_mars_wheat) 

# MARS degree = 1
mars_wheat = earth(Yield ~ ., degree=1, trace=3, data=data_trainset_mars_wheat)
mars_wheat$coefficients
summary(mars_wheat)

mars_wheat_pred = predict(mars_wheat, newdata=data_testset_mars_wheat)
mars_wheat_rmse = round(sqrt(mean((data_testset_mars_wheat$Yield - mars_wheat_pred)^2)))
print(mars_wheat_rmse)

r2_mars_wheat = 1 - (sum((data_testset_mars_wheat$Yield - mars_wheat_pred)^2) / sum((data_testset_mars_wheat$Yield - mean(data_testset_mars_wheat$Yield))^2))
print(r2_mars_wheat)

# rmse: 1, r2: 0.7571146


# potato
set.seed(838)

data_trainset_mars_potato = subset(data2, Crop_Year < 2018 & Crop == "Potato")
data_testset_mars_potato = subset(data2, Crop_Year >= 2018 & Crop == "Potato")

data_trainset_mars_potato$Crop_Year = NULL
data_testset_mars_potato$Crop_Year = NULL
data_trainset_mars_potato$Crop = NULL
data_testset_mars_potato$Crop = NULL

data_trainset_mars_potato = as.data.frame(data_trainset_mars_potato) 
data_testset_mars_potato = as.data.frame(data_testset_mars_potato) 

# MARS degree = 1
mars_potato = earth(Yield ~ ., degree=1, trace=3, data=data_trainset_mars_potato)
mars_potato$coefficients
summary(mars_potato)

mars_potato_pred = predict(mars_potato, newdata=data_testset_mars_potato)
mars_potato_rmse = round(sqrt(mean((data_testset_mars_potato$Yield - mars_potato_pred)^2)))
print(mars_potato_rmse)

r2_mars_potato = 1 - (sum((data_testset_mars_potato$Yield - mars_potato_pred)^2) / sum((data_testset_mars_potato$Yield - mean(data_testset_mars_potato$Yield))^2))
print(r2_mars_potato)

# rmse: 3, r2: 0.8146264




# yield output using ori variables
data3 = data
str(data3)
data3$Yield = NULL
str(data3)

set.seed(934)
data_trainset_marsv3 = subset(data3, Crop_Year < 2018)
data_testset_marsv3 = subset(data3, Crop_Year >= 2018)

data_trainset_marsv3 = as.data.frame(data_trainset_marsv3) 
data_testset_marsv3 = as.data.frame(data_testset_marsv3) 

# MARS degree = 1
mars5 = earth(`Yield_Output (Yield*Area)` ~ ., degree=1, trace=3, data=data_trainset_marsv3)
mars5$coefficients
summary(mars5)

mars5_pred = predict(mars5, newdata=data_testset_marsv3)
mars5_rmse = round(sqrt(mean((data_testset_marsv3$`Yield_Output (Yield*Area)` - mars5_pred)^2)))
print(mars5_rmse)

r2_mars5 = 1 - (sum((data_testset_marsv3$`Yield_Output (Yield*Area)` - mars5_pred)^2) / sum((data_testset_marsv3$`Yield_Output (Yield*Area)` - mean(data_testset_marsv3$`Yield_Output (Yield*Area)`))^2))
print(r2_mars5)

var_imp_mars5 = evimp(mars5)
print(var_imp_mars5)

# rmse: 1487771, r2: 0.9126496

# MARS degree = 2
mars6 = earth(`Yield_Output (Yield*Area)` ~ ., degree=2, trace=3, data=data_trainset_marsv3)
mars6$coefficients
summary(mars6)

mars6_pred = predict(mars6, newdata=data_testset_marsv3)
mars6_rmse = round(sqrt(mean((data_testset_marsv3$`Yield_Output (Yield*Area)` - mars6_pred)^2)))
print(mars6_rmse)

r2_mars6 = 1 - (sum((data_testset_marsv3$`Yield_Output (Yield*Area)` - mars6_pred)^2) / sum((data_testset_marsv3$`Yield_Output (Yield*Area)` - mean(data_testset_marsv3$`Yield_Output (Yield*Area)`))^2))
print(r2_mars6)

var_imp_mars6 = evimp(mars6)
print(var_imp_mars6)
# rmse: 1820309, r2: 0.8692375
# possibility of overfitting


# for each crop type


set.seed(666)

data_trainset_mars_rice_yieldoutput = subset(data3, Crop_Year < 2018 & Crop == "Rice")
data_testset_mars_rice_yieldoutput = subset(data3, Crop_Year >= 2018 & Crop == "Rice")

data_trainset_mars_rice_yieldoutput$Crop_Year = NULL
data_testset_mars_rice_yieldoutput$Crop_Year = NULL
data_trainset_mars_rice_yieldoutput$Crop = NULL
data_testset_mars_rice_yieldoutput$Crop = NULL

data_trainset_mars_rice_yieldoutput = as.data.frame(data_trainset_mars_rice_yieldoutput) 
data_testset_mars_rice_yieldoutput = as.data.frame(data_testset_mars_rice_yieldoutput) 

# MARS degree = 1
mars_rice_yieldoutput = earth(`Yield_Output (Yield*Area)` ~ ., degree=1, trace=3, data=data_trainset_mars_rice_yieldoutput)
mars_rice_yieldoutput$coefficients
summary(mars_rice_yieldoutput)

mars_rice_pred_yieldoutput = predict(mars_rice_yieldoutput, newdata=data_testset_mars_rice_yieldoutput)
mars_rice_rmse_yieldoutput = round(sqrt(mean((data_testset_mars_rice_yieldoutput$`Yield_Output (Yield*Area)` - mars_rice_pred_yieldoutput)^2)))
print(mars_rice_rmse_yieldoutput)

r2_mars_rice_yieldoutput = 1 - (sum((data_testset_mars_rice_yieldoutput$`Yield_Output (Yield*Area)` - mars_rice_pred_yieldoutput)^2) / sum((data_testset_mars_rice_yieldoutput$`Yield_Output (Yield*Area)` - mean(data_testset_mars_rice_yieldoutput$`Yield_Output (Yield*Area)`))^2))
print(r2_mars_rice_yieldoutput)

# rmse: 645402, r2: 0.9603056


# ginger
set.seed(666)

data_trainset_mars_ginger_yieldoutput = subset(data3, Crop_Year < 2018 & Crop == "Ginger")
data_testset_mars_ginger_yieldoutput = subset(data3, Crop_Year >= 2018 & Crop == "Ginger")

data_trainset_mars_ginger_yieldoutput$Crop_Year = NULL
data_testset_mars_ginger_yieldoutput$Crop_Year = NULL
data_trainset_mars_ginger_yieldoutput$Crop = NULL
data_testset_mars_ginger_yieldoutput$Crop = NULL

data_trainset_mars_ginger_yieldoutput = as.data.frame(data_trainset_mars_ginger_yieldoutput) 
data_testset_mars_ginger_yieldoutput = as.data.frame(data_testset_mars_ginger_yieldoutput) 

# MARS degree = 1
mars_ginger_yieldoutput = earth(`Yield_Output (Yield*Area)` ~ ., degree=1, trace=3, data=data_trainset_mars_ginger_yieldoutput)
mars_ginger_yieldoutput$coefficients
summary(mars_ginger_yieldoutput)

mars_ginger_pred_yieldoutput = predict(mars_ginger_yieldoutput, newdata=data_testset_mars_ginger_yieldoutput)
mars_ginger_rmse_yieldoutput = round(sqrt(mean((data_testset_mars_ginger_yieldoutput$`Yield_Output (Yield*Area)` - mars_ginger_pred_yieldoutput)^2)))
print(mars_ginger_rmse_yieldoutput)

r2_mars_ginger_yieldoutput = 1 - (sum((data_testset_mars_ginger_yieldoutput$`Yield_Output (Yield*Area)` - mars_ginger_pred_yieldoutput)^2) / sum((data_testset_mars_ginger_yieldoutput$`Yield_Output (Yield*Area)` - mean(data_testset_mars_ginger_yieldoutput$`Yield_Output (Yield*Area)`))^2))
print(r2_mars_ginger_yieldoutput)

# rmse: 72823, r2: 0.5745277


# wheat
set.seed(666)

data_trainset_mars_wheat_yieldoutput = subset(data3, Crop_Year < 2018 & Crop == "Wheat")
data_testset_mars_wheat_yieldoutput = subset(data3, Crop_Year >= 2018 & Crop == "Wheat")

data_trainset_mars_wheat_yieldoutput$Crop_Year = NULL
data_testset_mars_wheat_yieldoutput$Crop_Year = NULL
data_trainset_mars_wheat_yieldoutput$Crop = NULL
data_testset_mars_wheat_yieldoutput$Crop = NULL

data_trainset_mars_wheat_yieldoutput = as.data.frame(data_trainset_mars_wheat_yieldoutput) 
data_testset_mars_wheat_yieldoutput = as.data.frame(data_testset_mars_wheat_yieldoutput) 

# MARS degree = 1
mars_wheat_yieldoutput = earth(`Yield_Output (Yield*Area)` ~ ., degree=1, trace=3, data=data_trainset_mars_wheat_yieldoutput)
mars_wheat_yieldoutput$coefficients
summary(mars_wheat_yieldoutput)

mars_wheat_pred_yieldoutput = predict(mars_wheat_yieldoutput, newdata=data_testset_mars_wheat_yieldoutput)
mars_wheat_rmse_yieldoutput = round(sqrt(mean((data_testset_mars_wheat_yieldoutput$`Yield_Output (Yield*Area)` - mars_wheat_pred_yieldoutput)^2)))
print(mars_wheat_rmse_yieldoutput)

r2_mars_wheat_yieldoutput = 1 - (sum((data_testset_mars_wheat_yieldoutput$`Yield_Output (Yield*Area)` - mars_wheat_pred_yieldoutput)^2) / sum((data_testset_mars_wheat_yieldoutput$`Yield_Output (Yield*Area)` - mean(data_testset_mars_wheat_yieldoutput$`Yield_Output (Yield*Area)`))^2))
print(r2_mars_wheat_yieldoutput)

# rmse: 134495, r2: 0.9855669

# potato
set.seed(838)

data_trainset_mars_potato_yieldoutput = subset(data3, Crop_Year < 2018 & Crop == "Potato")
data_testset_mars_potato_yieldoutput = subset(data3, Crop_Year >= 2018 & Crop == "Potato")

data_trainset_mars_potato_yieldoutput$Crop_Year = NULL
data_testset_mars_potato_yieldoutput$Crop_Year = NULL
data_trainset_mars_potato_yieldoutput$Crop = NULL
data_testset_mars_potato_yieldoutput$Crop = NULL

data_trainset_mars_potato_yieldoutput = as.data.frame(data_trainset_mars_potato_yieldoutput) 
data_testset_mars_potato_yieldoutput = as.data.frame(data_testset_mars_potato_yieldoutput) 

# MARS degree = 1
mars_potato_yieldoutput = earth(`Yield_Output (Yield*Area)` ~ ., degree=1, trace=3, data=data_trainset_mars_potato_yieldoutput)
mars_potato_yieldoutput$coefficients
summary(mars_potato_yieldoutput)

mars_potato_pred_yieldoutput = predict(mars_potato_yieldoutput, newdata=data_testset_mars_potato_yieldoutput)
mars_potato_rmse_yieldoutput = round(sqrt(mean((data_testset_mars_potato_yieldoutput$`Yield_Output (Yield*Area)` - mars_potato_pred_yieldoutput)^2)))
print(mars_potato_rmse_yieldoutput)

r2_mars_potato_yieldoutput = 1 - (sum((data_testset_mars_potato_yieldoutput$`Yield_Output (Yield*Area)` - mars_potato_pred_yieldoutput)^2) / sum((data_testset_mars_potato_yieldoutput$`Yield_Output (Yield*Area)` - mean(data_testset_mars_potato_yieldoutput$`Yield_Output (Yield*Area)`))^2))
print(r2_mars_potato_yieldoutput)

# rmse: 428116, r2: 0.9814878


# -----------------------------------------------------------------------
# part b: random forest

# do not have to include interaction variables

# yield
str(data2)
# test: use ori variables 
set.seed(456)
data_train_rf = subset(data2, Crop_Year < 2018)
data_test_rf = subset(data2, Crop_Year >= 2018)

data_train_rf$Crop_Year = NULL
data_test_rf$Crop_Year = NULL

# defaults: B = 500, RSF size = int(sqrt(m)) = 3
rf1 = randomForest(Yield ~ . , data=data_train_rf, importance = T)

rf1  

rf1_pred = predict(rf1, newdata=data_test_rf)

rf1_rmse = sqrt(mean((data_test_rf$Yield - rf1_pred)^2))
print(rf1_rmse)

rf1_r2 = 1 - (sum((data_test_rf$Yield - rf1_pred)^2) / sum((data_test_rf$Yield - mean(data_test_rf$Yield))^2))
print(rf1_r2)

# rmse: 1.870224, r2: 0.897885


rf1_varimp = importance(rf1)
par(mfrow=(c(1,1)))
varImpPlot(rf1, type = 1)


# test: use derived variables
str(data_full2)
set.seed(717)

data_train_rf2 = subset(data_full2, Crop_Year < 2018)
data_test_rf2 = subset(data_full2, Crop_Year >= 2018)

data_train_rf2$Crop_Year = NULL
data_test_rf2$Crop_Year = NULL

rf2 = randomForest(Yield ~ . , data=data_train_rf2, importance = T)

rf2  

rf2_pred = predict(rf2, newdata=data_test_rf2)

rf2_rmse = sqrt(mean((data_test_rf2$Yield - rf2_pred)^2))
print(rf2_rmse)

rf2_r2 = 1 - (sum((data_test_rf2$Yield - rf2_pred)^2) / sum((data_test_rf2$Yield - mean(data_test_rf2$Yield))^2))
print(rf2_r2)

rf2_varimp = importance(rf2)
varImpPlot(rf2, type = 1)

# rmse: 2.989931, r2: 0.7390095

# using original data set gives better results


# by crop type

# rice
set.seed(333)
data_train_rf_rice2 = subset(data2, Crop_Year < 2018 & Crop == "Rice")
data_test_rf_rice2 = subset(data2, Crop_Year >= 2018 & Crop == "Rice")

data_train_rf_rice2$Crop_Year = NULL
data_test_rf_rice2$Crop_Year = NULL

rf_rice2 = randomForest(Yield ~ . , data=data_train_rf_rice2, importance = T)

rf_rice2

rf_rice_pred2 = predict(rf_rice2, newdata=data_test_rf_rice2)

rf_rice_rmse2 = sqrt(mean((data_test_rf_rice2$Yield - rf_rice_pred2)^2))
print(rf_rice_rmse2)

rf_rice2_r2 = 1 - (sum((data_test_rf_rice2$Yield - rf_rice_pred2)^2) / sum((data_test_rf_rice2$Yield - mean(data_test_rf_rice2$Yield))^2))
print(rf_rice2_r2)

rf_rice_varimp2 = importance(rf_rice2)
varImpPlot(rf_rice2, type = 1)

# rmse: 0.3887691, r2: 0.7364736


# ginger
set.seed(444)
data_train_rf_ginger2 = subset(data2, Crop_Year < 2018 & Crop == "Ginger")
data_test_rf_ginger2 = subset(data2, Crop_Year >= 2018 & Crop == "Ginger")

data_train_rf_ginger2$Crop_Year = NULL
data_test_rf_ginger2$Crop_Year = NULL

rf_ginger2 = randomForest(Yield ~ . , data=data_train_rf_ginger2, importance = T)

rf_ginger2

rf_ginger_pred2 = predict(rf_ginger2, newdata=data_test_rf_ginger2)

rf_ginger_rmse2 = sqrt(mean((data_test_rf_ginger2$Yield - rf_ginger_pred2)^2))
print(rf_ginger_rmse2)

rf_ginger2_r2 = 1 - (sum((data_test_rf_ginger2$Yield - rf_ginger_pred2)^2) / sum((data_test_rf_ginger2$Yield - mean(data_test_rf_ginger2$Yield))^2))
print(rf_ginger2_r2)

rf_rice_varimp2 = importance(rf_ginger2)
varImpPlot(rf_ginger2, type = 1)

# rmse: 2.857909, r2: 0.6656846


# wheat
set.seed(555)
data_train_rf_wheat2 = subset(data2, Crop_Year < 2018 & Crop == "Wheat")
data_test_rf_wheat2 = subset(data2, Crop_Year >= 2018 & Crop == "Wheat")

data_train_rf_wheat2$Crop_Year = NULL
data_test_rf_wheat2$Crop_Year = NULL

rf_wheat2 = randomForest(Yield ~ . , data=data_train_rf_wheat2, importance = T)

rf_wheat2

rf_wheat_pred2 = predict(rf_wheat2, newdata=data_test_rf_wheat2)

rf_wheat_rmse2 = sqrt(mean((data_test_rf_wheat2$Yield - rf_wheat_pred2)^2))
print(rf_wheat_rmse2)

rf_wheat2_r2 = 1 - (sum((data_test_rf_wheat2$Yield - rf_wheat_pred2)^2) / sum((data_test_rf_wheat2$Yield - mean(data_test_rf_wheat2$Yield))^2))
print(rf_wheat2_r2)

rf_wheat_varimp2 = importance(rf_wheat2)
varImpPlot(rf_wheat2, type = 1)

# rmse: 0.3764457, r2: 0.8977532


# potato

set.seed(666)
data_train_rf_potato2 = subset(data2, Crop_Year < 2018 & Crop == "Potato")
data_test_rf_potato2 = subset(data2, Crop_Year >= 2018 & Crop == "Potato")

data_train_rf_potato2$Crop_Year = NULL
data_test_rf_potato2$Crop_Year = NULL

rf_potato2 = randomForest(Yield ~ . , data=data_train_rf_potato2, importance = T)

rf_potato2

rf_potato_pred2 = predict(rf_potato2, newdata=data_test_rf_potato2)

rf_potato_rmse2 = sqrt(mean((data_test_rf_potato2$Yield - rf_potato_pred2)^2))
print(rf_potato_rmse2)

rf_potato2_r2 = 1 - (sum((data_test_rf_potato2$Yield - rf_potato_pred2)^2) / sum((data_test_rf_potato2$Yield - mean(data_test_rf_potato2$Yield))^2))
print(rf_potato2_r2)

rf_potato_varimp2 = importance(rf_potato2)
varImpPlot(rf_potato2, type = 1)

# rmse: 2.91647, r2: 0.8244181



# yield output using ori data
str(data3)
set.seed(222)
data_train_rf3 = subset(data3, Crop_Year < 2018)
data_test_rf3 = subset(data3, Crop_Year >= 2018)

data_train_rf3$Crop_Year = NULL
data_test_rf3$Crop_Year = NULL

rf3 = randomForest(`Yield_Output (Yield*Area)` ~ . , data=data_train_rf3, importance = T)

rf3

rf3_pred = predict(rf3, newdata=data_test_rf3)

rf3_rmse = sqrt(mean((data_test_rf3$`Yield_Output (Yield*Area)` - rf3_pred)^2))
print(rf3_rmse)

rf3_r2 = 1 - (sum((data_test_rf3$`Yield_Output (Yield*Area)` - rf3_pred)^2) / sum((data_test_rf3$`Yield_Output (Yield*Area)` - mean(data_test_rf3$`Yield_Output (Yield*Area)`))^2))
print(rf3_r2)

rf3_varimp = importance(rf3)
varImpPlot(rf3, type = 1)

# rmse: 984682.6, r2: 0.9617365


# by crop type

# rice
set.seed(333)
data_train_rf_rice = subset(data3, Crop_Year < 2018 & Crop == "Rice")
data_test_rf_rice = subset(data3, Crop_Year >= 2018 & Crop == "Rice")

data_train_rf_rice$Crop_Year = NULL
data_test_rf_rice$Crop_Year = NULL

rf_rice = randomForest(`Yield_Output (Yield*Area)` ~ . , data=data_train_rf_rice, importance = T)

rf_rice

rf_rice_pred = predict(rf_rice, newdata=data_test_rf_rice)

rf_rice_rmse = sqrt(mean((data_test_rf_rice$`Yield_Output (Yield*Area)` - rf_rice_pred)^2))
print(rf_rice_rmse)

rf_rice_r2 = 1 - (sum((data_test_rf_rice$`Yield_Output (Yield*Area)` - rf_rice_pred)^2) / sum((data_test_rf_rice$`Yield_Output (Yield*Area)` - mean(data_test_rf_rice$`Yield_Output (Yield*Area)`))^2))
print(rf_rice_r2)

rf_rice_varimp = importance(rf_rice)
varImpPlot(rf_rice, type = 1)

# rmse: 592663.1, r2: 0.9665278


# ginger
set.seed(444)
data_train_rf_ginger = subset(data3, Crop_Year < 2018 & Crop == "Ginger")
data_test_rf_ginger = subset(data3, Crop_Year >= 2018 & Crop == "Ginger")

data_train_rf_ginger$Crop_Year = NULL
data_test_rf_ginger$Crop_Year = NULL

rf_ginger = randomForest(`Yield_Output (Yield*Area)` ~ . , data=data_train_rf_ginger, importance = T)

rf_ginger

rf_ginger_pred = predict(rf_ginger, newdata=data_test_rf_ginger)

rf_ginger_rmse = sqrt(mean((data_test_rf_ginger$`Yield_Output (Yield*Area)` - rf_ginger_pred)^2))
print(rf_ginger_rmse)

rf_ginger_r2 = 1 - (sum((data_test_rf_ginger$`Yield_Output (Yield*Area)` - rf_ginger_pred)^2) / sum((data_test_rf_ginger$`Yield_Output (Yield*Area)` - mean(data_test_rf_ginger$`Yield_Output (Yield*Area)`))^2))
print(rf_ginger_r2)

rf_rice_varimp = importance(rf_ginger)
varImpPlot(rf_ginger, type = 1)

# rmse: 80612.04, r2: 0.4786503


# wheat
set.seed(555)
data_train_rf_wheat = subset(data3, Crop_Year < 2018 & Crop == "Wheat")
data_test_rf_wheat = subset(data3, Crop_Year >= 2018 & Crop == "Wheat")

data_train_rf_wheat$Crop_Year = NULL
data_test_rf_wheat$Crop_Year = NULL

rf_wheat = randomForest(`Yield_Output (Yield*Area)` ~ . , data=data_train_rf_wheat, importance = T)

rf_wheat

rf_wheat_pred = predict(rf_wheat, newdata=data_test_rf_wheat)

rf_wheat_rmse = sqrt(mean((data_test_rf_wheat$`Yield_Output (Yield*Area)` - rf_wheat_pred)^2))
print(rf_wheat_rmse)

rf_wheat_r2 = 1 - (sum((data_test_rf_wheat$`Yield_Output (Yield*Area)` - rf_wheat_pred)^2) / sum((data_test_rf_wheat$`Yield_Output (Yield*Area)` - mean(data_test_rf_wheat$`Yield_Output (Yield*Area)`))^2))
print(rf_wheat_r2)

rf_wheat_varimp = importance(rf_wheat)
varImpPlot(rf_wheat, type = 1)

# rmse: 2094779, r2: 0.9507924


# potato

set.seed(666)
data_train_rf_potato = subset(data3, Crop_Year < 2018 & Crop == "Potato")
data_test_rf_potato = subset(data3, Crop_Year >= 2018 & Crop == "Potato")

data_train_rf_potato$Crop_Year = NULL
data_test_rf_potato$Crop_Year = NULL

rf_potato = randomForest(`Yield_Output (Yield*Area)` ~ . , data=data_train_rf_potato, importance = T)

rf_potato

rf_potato_pred = predict(rf_potato, newdata=data_test_rf_potato)

rf_potato_rmse = sqrt(mean((data_test_rf_potato$`Yield_Output (Yield*Area)` - rf_potato_pred)^2))
print(rf_potato_rmse)

rf_potato_r2 = 1 - (sum((data_test_rf_potato$`Yield_Output (Yield*Area)` - rf_potato_pred)^2) / sum((data_test_rf_potato$`Yield_Output (Yield*Area)` - mean(data_test_rf_potato$`Yield_Output (Yield*Area)`))^2))
print(rf_potato_r2)

rf_potato_varimp = importance(rf_potato)
varImpPlot(rf_potato, type = 1)

# rmse: 428130.8, r2: 0.9814866



