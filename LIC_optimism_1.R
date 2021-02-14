##################################################################
######### Life in Conservation: Optimism in Conservation #########
##################################################################
# The following describes the primary steps used to generate the results presented in the article 'Personal traits predict conservationists' optimism about outcomes for nature'

###### Steps ###### 
# 1) Set up the environment 
# 2) Multiple imputation 
# 3) Post imputation manipulation 
# 4) Nationally-focused situational optimism structure development 
# 5) Dispositional optimism structure development 
# 6) Structural equation modelling 
# 7) Prepare the results
# 8) Plot the results

###### 1) Set up the environment ######
### Load packages ### 
library(mice)
library(fastDummies)
library(plyr)
library(psych)
library(kableExtra)
library(ltm)
library(lavaan)
library(semTools)
library(semPlot)
library(ggplot2)
library(ggpubr)

### Set seed ###
set.seed(123)

### Load data ###
# DF.opt.1.Rdata should be in the same folder as the code  
load("DF.opt.1.Rdata") 

### Load functions - save this ### 
# DF.opt.1.Rdata should be in the same folder as the code  
source("LIC_optimism_functions.R")

###


###### 2) Multiple imputation ######
### Function that ensures the data is in the correct format, drops "other" as a category from gender (because there are too few observations for stable estimation), determines the imputation method from the data type, and performs the imputation using MICE.
MI_function_1 <-function(DF1) {
  
  ### Correct data type ###
  # As ordinal
  DF1[,c("LOTR_1","LOTR_3","LOTR_6","LOTR_2","LOTR_4" , "LOTR_5","SO_1", "SO_2", "SO_3", "SO_4","SO_5", "SO_6", "SO_8", "SO_7", "SO_9","SO_10","SO_11")] <- lapply(DF1[,c("LOTR_1","LOTR_3","LOTR_6","LOTR_2","LOTR_4" , "LOTR_5","SO_1", "SO_2", "SO_3", "SO_4","SO_5", "SO_6", "SO_8", "SO_7", "SO_9","SO_10","SO_11")], as.ordered)
  
  # As numeric
  DF1[,c("age_year_scaled","years_cons_scaled", "rli", "Prim.cover.20", "spend.area", "I_VI_mean" , "Corruption" ,"Gov.Effect" , "Pol.Stab","Reg.Qual","Rule.Law", "Acco.")] <- lapply(DF1[,c("age_year_scaled","years_cons_scaled", "rli", "Prim.cover.20", "spend.area", "I_VI_mean" , "Corruption" ,"Gov.Effect" , "Pol.Stab","Reg.Qual","Rule.Law", "Acco.")], as.numeric)
  
  # As factor 
  DF1[,c("education_simple", "gender",  "position_simple", "SO_Region" , "Environment")] <- lapply(DF1[,c("education_simple", "gender","position_simple", "SO_Region",  "Environment")], as.factor)
  
  ### Drop "other" as a category in gender (there were too few observations to model)
  DF1<-DF1[!(DF1$gender=="Other"),]
  DF1<- droplevels(DF1)
  
  ### Number of imputed DF ###
  # 10 imputed DF
  N.Imp = 10
  
  ### determine the imputation method from the data type ##
  # The data type for each variable 
  str_out <- data.frame(capture.output(str(DF1)))
  
  # Delete the first and last row
  str_out <- data.frame(str_output = matrix(str_out[2:(nrow(str_out)-1),]))
  
  # Create a column that contain the appropriate model for each variable - this only works if the variable is of the correct data type in the first place 
  str_out$type <- ifelse(grepl("Ord.factor", str_out$str_output, fixed = TRUE)==T, "polr", 
                         ifelse(grepl("num", str_out$str_output, fixed = TRUE)==T, "pmm", 
                                ifelse(grepl("Factor", str_out$str_output, fixed = TRUE)==T, "polyreg",
                                       ifelse(grepl("int", str_out$str_output, fixed = TRUE)==T, "logreg", "ERROR"))))
  
  # Conduct the MI - with the number of datasets specified by N.Imp, and the estimation type specified by str_out$type (derived from the above)
  DF1_imp <- mice(DF1, m = N.Imp, method = str_out$type )
  
  # Print the first 50 logged events, if they occur 
  print(head(DF1_imp$loggedEvents, 50))
  
  # Return the imputed data
  return(DF1_imp)
}

### Conduct the imputation ### 
# Conduct the imputation using DF.opt.1 (excluding country code)
mice.imp.SO_int <- MI_function_1(DF = subset(DF.opt.1, select = -c(SO_CountryCode, ID)))

# Save the imputed data
save(mice.imp.SO_int, file =  paste0(path, "mice.imp.SO_int.Rdata")) 

###


###### 3) Post imputation manipulation ######
### Function to add SO_CountryCode & respondent ID, change the reference level for region, sum the governance indicators to create a single governance variable, create a series of binary dummy columns, and scale and centre SO-11 
MI_function_2 <-function(DF1, DF_imp) {
  
  ### Number of imputed DF ###
  # 10 imputed DF
  N.Imp = 10
  
  ### Drop "other" as a category in gender (there were too few observations to model)
  DF1<-DF1[!(DF1$gender=="Other"),]
  DF1<- droplevels(DF1)
  
  ### Extract each imputed dataset and perform additional manipulation ###
  # Create a list to store the imputed datasets 
  mice.imp <- list()
  
  # For i in each dataset
  for(i in 1:N.Imp) {
    
    ### Extract the imputed data
    mice.imp[[i]] <- mice::complete(DF_imp, action= i, inc=FALSE)
    
    ### Add data from DF1 
    # Add country code 
    mice.imp[[i]]$SO_CountryCode <- DF1$SO_CountryCode
    
    # Add respondent ID
    mice.imp[[i]]$ID <- DF1$ID
    
    # Change the reference level for region
    mice.imp[[i]]$SO_Region <- as.factor(mice.imp[[i]]$SO_Region)
    mice.imp[[i]]$SO_Region <- relevel(mice.imp[[i]]$SO_Region, ref="Europe and Northern America")
    
    ### Create the governance variable 
    mice.imp[[i]]$Gov.Total <- rowSums(mice.imp[[i]][,c("Corruption","Gov.Effect" , "Pol.Stab","Reg.Qual" ,"Rule.Law","Acco.")], na.rm = T)
    mice.imp[[i]]$Gov.Total <- scale(mice.imp[[i]]$Gov.Total, center = T, scale = T)
    
    ### Turn factor into series of binary variables and remove special charecters 
    # Create the dummy columns 
    mice.imp[[i]] <- dummy_cols(mice.imp[[i]], select_columns = c("gender", "education_simple", "position_simple", "SO_Region", "Environment"))
    
    # Remove spaces and other characters on column names
    colnames( mice.imp[[i]]) <- gsub(" ", "", colnames( mice.imp[[i]]), fixed = TRUE)
    colnames( mice.imp[[i]]) <- gsub("/", "", colnames( mice.imp[[i]]), fixed = TRUE)
    colnames( mice.imp[[i]]) <- gsub("-", "_", colnames( mice.imp[[i]]), fixed = TRUE)
    colnames( mice.imp[[i]]) <- gsub("&", "_", colnames( mice.imp[[i]]), fixed = TRUE)
    
    ### Scale and center SO-11
    mice.imp[[i]]$SO_11 <- scale(as.numeric(mice.imp[[i]]$SO_11), scale = T, center = T)
  }
  
  # Return the manipulated DF 
  return(mice.imp)
}

# Implement the function 
mice.imp.SO <- MI_function_2(DF1 = DF.opt.1, DF_imp = mice.imp.SO_int)

# Save the data 
save(mice.imp.SO, file = "mice.imp.SO.Rdata")


###


###### 4) Nationally-focused situational optimism structure development ###### 
# Load the data
load("mice.imp.SO.Rdata")

# Convert from ordered factor to numeric, for the exploratory factor analysis 
SO_row_name <- c("SO_1","SO_2","SO_3","SO_4","SO_5","SO_6","SO_7","SO_8","SO_9","SO_10","SO_11")
mice.imp.SO_num <- mice.imp.SO[[1]]
mice.imp.SO_num[SO_row_name] <- apply(mice.imp.SO_num[SO_row_name],2, as.numeric)

### Creating training and test datasets ### 
# The size of the training dataset (70%)
smp_size <- floor(.70*nrow(mice.imp.SO_num))

## Identify training observations
train_ind.SO <- sample(seq_len(nrow(mice.imp.SO_num)), size = smp_size)

# Subset to test and training datasets
train_DF2.SO <- mice.imp.SO_num[train_ind.SO, ]
test_DF2.SO <- mice.imp.SO_num[-train_ind.SO, ]

### Polychoric correlation between variables ### 
poly_cor = polychoric(train_DF2.SO[, SO_row_name[1:10]])
cor.plot(poly_cor$rho, numbers=F, upper=FALSE, main = "Polychoric correlation", show.legend = T)

### Ordinal Alpha ###
round(psych::alpha(poly_cor$rho)$total,2)

### Parallel analysis ###
fa.parallel(train_DF2.SO[, SO_row_name[1:10]], cor = "poly", fm="wls", fa="fa",   main = "Parallel analysis")

### Root mean square error of approximation across exploratory factor analysis ###
fa_mod1 <-fa(train_DF2.SO[, SO_row_name[1:10]], nfactors = 1,  cor = "poly",  fm = "wls", rotate =  "oblimin") # run the model with two factors
fa_mod2 <-fa(train_DF2.SO[, SO_row_name[1:10]], nfactors = 2, cor = "poly", fm = "wls", rotate =  "oblimin") # run the model with two factors
fa_mod3 <-fa(train_DF2.SO[, SO_row_name[1:10]], nfactors = 3, cor = "poly", fm = "wls", rotate =  "oblimin") # run the model with two factors
fa_mod4 <-fa(train_DF2.SO[, SO_row_name[1:10]], nfactors = 4, cor = "poly", fm = "wls",  rotate =  "oblimin") # run the model with two factors
fa_mod5 <-fa(train_DF2.SO[, SO_row_name[1:10]], nfactors = 5, cor = "poly", fm = "wls",  rotate =  "oblimin") # run the model with two factors
RMSEA_va <- cbind(data.frame("Factors"=seq(1,5,1)), rbind( fa_mod1$RMSEA,fa_mod2$RMSEA ,fa_mod3$RMSEA,fa_mod4$RMSEA, fa_mod5$RMSEA))

# Inspect the root mean square error of approximation
RMSEA_va <- round(RMSEA_va,3)
RMSEA_va[,1:4] %>%
  kable(format = "html", table.attr = "style='width:50%;'") %>%
  kable_styling()

### Five factor analysis ### 
fa_out <-fa(train_DF2.SO[, SO_row_name[1:10]], nfactors = 5, cor = "poly",  fm = "wls",  rotate =  "oblimin") # run the model with two factors

# Loadings as DF
fact_ma <- as.data.frame((fa_out$loadings)[1:10,1:5])

# Values under .3 shown as NA
fact_ma[,1] <- ifelse(fact_ma[,1] <=.3, NA, fact_ma[,1])
fact_ma[,2] <- ifelse(fact_ma[,2] <=.3, NA, fact_ma[,2])
fact_ma[,3] <- ifelse(fact_ma[,3] <=.3, NA, fact_ma[,3])
fact_ma[,4] <- ifelse(fact_ma[,4] <=.3, NA, fact_ma[,4])
fact_ma[,5] <- ifelse(fact_ma[,5] <=.3, NA, fact_ma[,5])

# Inspect the factor loadings
fact_ma %>%
  kable(format = "html", table.attr = "style='width:100%;'",
        col.names = c("Factor 1", "Factor 2",  "Factor 3",  "Factor 4",  "Factor 5")) %>%
  kable_styling()%>%
  row_spec(1:2,  background = "#b3e2cd")%>%
  row_spec(3:4,  background = "#fdcdac")%>%
  row_spec(5:6,  background = "#cbd5e8")%>%
  row_spec(7:8,  background = "#f4cae4")%>%
  row_spec(9:10,  background = "#e6f5c9")

### Graded response model ### 
# Using the items consistently loaded onto three factors 
S_Optimism_graded <- ltm::grm(mice.imp.SO[[1]][,c("SO_1" , "SO_2", "SO_3","SO_4" , "SO_5", "SO_6", "SO_8")])

# Plot the item response categories characteristic curves
plot(S_Optimism_graded,  cex.main = .9)

# Plot the item information curve
plot(S_Optimism_graded, type="IIC",  cex.main = .9)

### Nationally-focused situational optimism confirmatory factor analysis 
# Use the test data 
test_DF2_ord <-  mice.imp.SO[[1]][-train_ind.SO, ]

# The simple model
model_SO_simple <- '
# Estimate latent nationally-focused situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6+ SO_8  

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 
'

# Fitting the CFA
fit_SO_simple <- lavaan::cfa(model = model_SO_simple,  estimator = "WLSMVS", data=test_DF2_ord[,SO_row_name], ordered = c("SO_1" , "SO_2" , "SO_3" , "SO_4", "SO_5", "SO_6",   "SO_8"))

### Fit measures ###
# Comparative fit index 
format(round(fitmeasures(fit_SO_simple,fit.measures = "cfi"),3), nsmall=3)

# root mean square error of approximation 
format(round(fitmeasures(fit_SO_simple,fit.measures = "rmsea"),3), nsmall=3)

# Tucker-Lewis index w
format(round(fitmeasures(fit_SO_simple,fit.measures = "tli"),3), nsmall=3)

# Standardized root mean square residual 
format(round(fitmeasures(fit_SO_simple,fit.measures = "srmr"),3), nsmall=3)

###


###### 5) Dispositional optimism structure development ###### 

# Convert from ordered factor to numeric, for the exploratory factor analysis 
LOTR_row_name <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4","LOTR_5","LOTR_6")
train_DF2.SO[LOTR_row_name] <- apply(train_DF2.SO[LOTR_row_name],2, as.numeric)

### Polychoric correlation between variables ### 
poly_cor = polychoric(train_DF2.SO[, LOTR_row_name])
cor.plot(poly_cor$rho, numbers=F, upper=FALSE, main = "Polychoric Correlation", show.legend = T)

### Ordinal Alpha ###
round(psych::alpha(poly_cor$rho)$total,2)

### Graded response model ### 
# Using the items consistently loaded onto three factors 
S_dispo_graded <- ltm::grm(mice.imp.SO[[1]][,c("LOTR_1" , "LOTR_2" , "LOTR_3" ,  "LOTR_4" , "LOTR_5" , "LOTR_6")])

# Plot the item response categories characteristic curves
plot(S_dispo_graded,  cex.main = .9)

# Plot the item information curve
plot(S_dispo_graded, type="IIC",  cex.main = .9)

### Dispositional optimism confirmatory factor analysis 

# The dispositional optimism model
model_OP_method <- '
###### Dispositional optimism 
# Dispositional optimism 
DO =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# DO and the method effect are orthogonal  
DO ~~0*method
'

# Fitting the CFA
fit_OP_meth <- lavaan::cfa(model=model_OP_method,  estimator = "WLSMVS",  data=mice.imp.SO[[1]] , ordered = c("LOTR_1" , "LOTR_2" , "LOTR_3" , "LOTR_4", "LOTR_5", "LOTR_6") )

### Fit measures ###
# Comparative fit index 
format(round(fitmeasures(fit_OP_meth,fit.measures = "cfi"),3), nsmall=3)

# root mean square error of approximation 
format(round(fitmeasures(fit_OP_meth,fit.measures = "rmsea"),3), nsmall=3)

# Tucker-Lewis index w
format(round(fitmeasures(fit_OP_meth,fit.measures = "tli"),3), nsmall=3)

# Standardized root mean square residual 
format(round(fitmeasures(fit_OP_meth,fit.measures = "srmr"),3), nsmall=3)

######  6) Structural equation modelling ######
### Model 1 ### 

# The simple model
model_SO_1 <- '
### Latent part 
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Dispositional optimism 
DO =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# DO and the method effect are orthogonal 
method ~~ 0*DO

### Regression part
SO ~ DO  + gender_Male + gender_Unknown + 

# Years conservation
years_cons_scaled +

# Academic/practice - RL = academic
position_simple_Practice + position_simple_Unknown.other +

# Education - RL = non-university
education_simple_University  + education_simple_Unknown +

#Environment - Environment_Terrestrial
Environment_CC + Environment_Marine + Environment_Unknown + 

# Country context
SO_Region_CentralandSouthernAsia + SO_Region_EasternandSouth_EasternAsia + SO_Region_Sub_SaharanAfrica + SO_Region_LatinAmericaandtheCaribbean + SO_Region_NorthernAfricaandWesternAsia + SO_Region_Oceania +

# State
rli + Prim.cover.20 +

# Response
I_VI_mean  + spend.area + Gov.Total
'

# Run the SEM with model 1, using the imputed dataset 
model_SO_1_MI <- runMI(model_SO_1, data = mice.imp.SO, fun="sem", estimator = "WLSMVS", ordered = c("SO_1" , "SO_2" , "SO_3" , "SO_4", "SO_5", "SO_6", "SO_8","LOTR_1" , "LOTR_2" , "LOTR_3" , "LOTR_4", "LOTR_5", "LOTR_6"), FUN = fitMeasures)

### Model 2 ### 
model_SO_2 <- '
# Dispositional optimism 
DO =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# DO and the method effect are orthogonal 
method ~~ 0*DO

### Regression part
SO_11 ~ DO  + gender_Male + gender_Unknown + 

# Years conservation
years_cons_scaled +

# Academic/practice - RL = academic
position_simple_Practice + position_simple_Unknown.other +

# Education - RL = non-university
education_simple_University  + education_simple_Unknown +

#Environment - Environment_Terrestrial
Environment_CC + Environment_Marine + Environment_Unknown + 

# Country context
SO_Region_CentralandSouthernAsia + SO_Region_EasternandSouth_EasternAsia + SO_Region_Sub_SaharanAfrica + SO_Region_LatinAmericaandtheCaribbean + SO_Region_NorthernAfricaandWesternAsia + SO_Region_Oceania +

# State
rli + Prim.cover.20 +

# Response
I_VI_mean  + spend.area + Gov.Total
'

# Run the SEM with model 2, using the imputed dataset 
model_SO_2_MI <- runMI(model_SO_2, data = mice.imp.SO, fun="sem", estimator = "WLSMVS", ordered = c("SO_11","LOTR_1" , "LOTR_2" , "LOTR_3" , "LOTR_4", "LOTR_5", "LOTR_6"), FUN = fitMeasures)

###


###### 7) Prepare the results ###### 

### Model 1 results ### 
# Results table 
results_table_1 <- data.frame(summary(model_SO_1_MI, standardized = T, ci=T)) 

# Return the coefficients derived from regression
results_table_1_regre <- results_table_1[ which(results_table_1$op == '~'), ]

# Adding alternative labels 
results_table_1_regre$lhs<- label_recoder(results_table_1_regre$lhs)
results_table_1_regre$rhs<- label_recoder(results_table_1_regre$rhs)

# Calculate CI from P value (https://www.bmj.com/content/343/bmj.d2090)
results_table_1_regre$z <- -0.862 + sqrt(0.743-(2.404*log(results_table_1_regre$pvalue)))
results_table_1_regre$SE <- results_table_1_regre$std.lv/results_table_1_regre$z
results_table_1_regre$CI.lower.lv <- results_table_1_regre$std.lv - 1.96*results_table_1_regre$SE
results_table_1_regre$CI.upper.lv <- results_table_1_regre$std.lv + 1.96*results_table_1_regre$SE 

### Model 2 results ### 
# results table 
results_table_2 <- data.frame(summary(model_SO_2_MI, standardized = T, ci=T)) 

# Return the coefficients derived from regression
results_table_2_regre <- results_table_2[ which(results_table_2$op == '~'), ]

# Adding alternative labels 
results_table_2_regre$lhs<- label_recoder(results_table_2_regre$lhs)
results_table_2_regre$rhs<- label_recoder(results_table_2_regre$rhs)

# calculate CI from P value (https://www.bmj.com/content/343/bmj.d2090)
results_table_2_regre$z <- -0.862 + sqrt(0.743-(2.404*log(results_table_2_regre$pvalue)))
results_table_2_regre$SE <- results_table_2_regre$std.lv/results_table_2_regre$z
results_table_2_regre$CI.lower.lv <- results_table_2_regre$std.lv - 1.96*results_table_2_regre$SE
results_table_2_regre$CI.upper.lv <- results_table_2_regre$std.lv + 1.96*results_table_2_regre$SE 

###


###### 8) Plot the results ######
# My pallet 
mypal <- c('#1b9e77', "#d95f02", "#7570b3")

# Create response identifier 
results_table_1_regre$Response <- "Nationally-focused situational optimism"
results_table_2_regre$Response <- "Locally-focused situational optimism"

### Plot 1 ###
# Subset to individual characteristics 
results_table_1_regre_ind <- results_table_1_regre[which(results_table_1_regre$rhs %in% c("Dispositional optimism", "Male", "Years in conservation" ,"Practice/policy", "University" )),] 
results_table_2_regre_ind <- results_table_2_regre[ which(results_table_2_regre$rhs %in% c("Dispositional optimism", "Male", "Years in conservation" ,"Practice/policy", "University" )),]

# Combine the results tables 
P_table_regre_ind <- rbind(results_table_1_regre_ind, results_table_2_regre_ind)

# Create reference level DF
Ref_level <- data.frame(lhs = c(rep("", 3)), op = c(rep("~", 3)), rhs = c("Female", "Academia", "Non-university" ), exo = c(rep("NULL", 3)), est = c(rep(0,3)), se = c(rep("NULL", 3)), t = c(rep("NULL", 3)), df = c(rep("NULL", 3)), pvalue = c(rep("NULL", 3)),  ci.lower = c(rep(0,3)),   ci.upper = c(rep(0,3)), std.lv  = c(rep(0,3)),     std.all = c(rep(0,3)), std.nox= c(rep("NULL", 3)), label = c(rep(0,3)), z = c(rep("NULL", 3)), SE = c(rep("NULL", 3)), CI.lower.lv = c(rep(0,3)) ,  CI.upper.lv = c(rep(0,3)) ,  Response = c(rep("Reference-level", 3)))

# Combine 
P_table_regre_ind <- rbind(P_table_regre_ind,Ref_level)

# shorten string 
P_table_regre_ind$rhs <- as.factor(sapply( strwrap(P_table_regre_ind$rhs, 15, simplify=FALSE), paste, collapse="\n" ))

# Re-level 
P_table_regre_ind$rhs <- factor(P_table_regre_ind$rhs, levels = rev(c("Dispositional\noptimism",  "Years in\nconservation" ,"Academia",  "Practice/policy", "Non-university",  "University", "Female", "Male"))) 

# plot
P1<- ggplot(P_table_regre_ind, aes(x= rhs, y=std.lv)) + ylab("Situational\noptimism (SD)")  + 
  geom_pointrange(aes(color = Response, shape = Response, ymax = CI.upper.lv, ymin=CI.lower.lv,  fatten =2), position=position_dodge(width=c(0.6,0.6))) +
  geom_hline(yintercept = 0, linetype="dashed", colour="grey73")  + theme(legend.title = element_blank(), legend.position = "right") +  coord_flip()   + theme(axis.title.y=element_blank(), legend.position = "bottom", legend.direction = "vertical", axis.text.y = element_text(size=11))  +  scale_color_manual(values = c(mypal[1], mypal[2], "black" ) ) +  scale_shape_manual(values = c(16, 16, 1) )+ scale_x_discrete(breaks = c(levels(P_table_regre_ind$rhs)), limits = c(levels(P_table_regre_ind$rhs)[1:2], "skip", levels(P_table_regre_ind$rhs)[3:4], "skip",  levels(P_table_regre_ind$rhs)[5:6], "skip",  levels(P_table_regre_ind$rhs)[7:8])) 


### Plot 2 ###
# Subset to state & response 
results_table_1_regre_state<- results_table_1_regre[ which(results_table_1_regre$rhs %in% c("Red List Index", "Primary cover", "Protected area cover" , "Spending", "Governance","Cross-cutting", "Marine"  )),] 
results_table_2_regre_state<- results_table_2_regre[ which(results_table_2_regre$rhs %in% c("Red List Index", "Primary cover", "Protected area cover" , "Spending", "Governance","Cross-cutting", "Marine"   )),]# "Gov.Total"

# Combine the results tables 
P_table_regre_state<- rbind(results_table_1_regre_state, results_table_2_regre_state)

# Create reference level DF
Ref_level <- data.frame(lhs = c(rep("", 1)), op = c(rep("~", 1)), rhs = c("Terrestrial" ), exo = c(rep("NULL", 1)), est = c(rep(0,1)), se = c(rep("NULL", 1)), t = c(rep("NULL", 1)), df = c(rep("NULL", 1)), pvalue = c(rep("NULL", 1)),  ci.lower = c(rep(0,1)),   ci.upper = c(rep(0,1)), std.lv  = c(rep(0,1)),     std.all = c(rep(0,1)), std.nox= c(rep("NULL", 1)), label = c(rep(0,1)), z = c(rep("NULL", 1)), SE = c(rep("NULL", 1)), CI.lower.lv = c(rep(0,1)) ,  CI.upper.lv = c(rep(0,1)) ,  Response = c(rep("Reference-level", 1)))

# Add reference levels 
P_table_regre_state <- rbind(P_table_regre_state, Ref_level)

# Shorten string 
P_table_regre_state$rhs <- as.factor(sapply( strwrap(P_table_regre_state$rhs, 15, simplify=FALSE), paste, collapse="\n" ))

# Re-level 
P_table_regre_state$rhs <- factor(P_table_regre_state$rhs, levels = rev(c("Red List Index", "Primary cover", "Protected area\ncover", "Spending", "Governance", "Terrestrial", "Cross-cutting", "Marine" ))) 

# plot
P2<- ggplot(P_table_regre_state, aes(x= rhs, y=std.lv)) + ylab("Situational\noptimism (SD)")  + 
  geom_pointrange(aes(color = Response, shape = Response, ymax = CI.upper.lv, ymin=CI.lower.lv,  fatten =2), position=position_dodge(width=c(0.6,0.6))) +
  geom_hline(yintercept = 0, linetype="dashed", colour="grey73")  + theme(legend.title = element_blank(), legend.position = "right") +  coord_flip()   + theme(axis.title.y=element_blank(), legend.position = "none", axis.text.y = element_text(size=11)) +  scale_color_manual(values = c(mypal[1], mypal[2], "black" ) ) +  scale_shape_manual(values = c(16, 16, 1)) + scale_x_discrete(breaks = c(levels(P_table_regre_state$rhs)), limits = c(levels(P_table_regre_state$rhs)[1:3], "skip", levels(P_table_regre_state$rhs)[4:8])) 

### Plot 3 ###
# Subset to regions (and exlcude the coefficient for unknown region)
results_table_1_regre_reg <- results_table_1_regre[ which(results_table_1_regre$rhs %in% c("C. & S. Asia", "E. & S.E. Asia", "Sub-Saharan Africa", "Lat. America & Carib.", "N. Africa & W. Asia", "Oceania" )),]
results_table_2_regre_reg <- results_table_2_regre[ which(results_table_2_regre$rhs %in% c("C. & S. Asia", "E. & S.E. Asia", "Sub-Saharan Africa", "Lat. America & Carib.", "N. Africa & W. Asia", "Oceania")),]

# Combine DF
P_table_regre_reg <- rbind(results_table_1_regre_reg, results_table_2_regre_reg)

# Create reference level DF
Ref_level <- data.frame(lhs = c(rep("", 1)), op = c(rep("~", 1)), rhs = c("N. America & Europe" ), exo = c(rep("NULL", 1)), est = c(rep(0,1)), se = c(rep("NULL", 1)), t = c(rep("NULL", 1)), df = c(rep("NULL", 1)), pvalue = c(rep("NULL", 1)),  ci.lower = c(rep(0,1)),   ci.upper = c(rep(0,1)), std.lv  = c(rep(0,1)),     std.all = c(rep(0,1)), std.nox= c(rep("NULL", 1)), label = c(rep(0,1)), z = c(rep("NULL", 1)), SE = c(rep("NULL", 1)), CI.lower.lv = c(rep(0,1)) ,  CI.upper.lv = c(rep(0,1)) ,  Response = c(rep("Reference-level", 1)))

# Add reference levels 
P_table_regre_reg <- rbind(P_table_regre_reg, Ref_level)

# Shorten string 
P_table_regre_reg$rhs <- as.factor(sapply( strwrap(P_table_regre_reg$rhs, 15, simplify=FALSE), paste, collapse="\n" ))

# Re-level 
P_table_regre_reg$rhs <- factor(P_table_regre_reg$rhs, levels = rev(c("N. America &\nEurope", "C. & S. Asia", "E. & S.E. Asia", "Sub-Saharan\nAfrica", "Lat. America &\nCarib.", "N. Africa & W.\nAsia", "Oceania"))) 

# plot
P3<- ggplot(P_table_regre_reg, aes(x= rhs, y=std.lv)) + ylab("Situational\noptimism (SD)")  + 
  geom_pointrange(aes(color = Response, shape = Response, ymax = CI.upper.lv, ymin=CI.lower.lv,  fatten =2), position=position_dodge(width=c(0.6,0.6))) +
  geom_hline(yintercept = 0, linetype="dashed", colour="grey73")  + theme(legend.title = element_blank(), legend.position = "right") +  coord_flip()  + theme(axis.title.y=element_blank() ,legend.position = "none", axis.text.y = element_text(size=11)) +  scale_color_manual(values = c(mypal[1], mypal[2], "black" ) ) +  scale_shape_manual(values = c(16, 16, 1) )
#############

# Plot 
egg::ggarrange(P1, P2, P3, ncol = 3, labels = c("a.", "b.", "c.") )
