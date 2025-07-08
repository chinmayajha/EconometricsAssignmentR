# Filtering out relevant column, dropping the rest
# clean_data <- da36151.0001[,c("INCOME","WT", "RO3","RO5","RO7","NPERSONS","HHEDUC","WKFARM","URBAN2011","COTOTAL","ASSETS","WK

# According to our research question, we want to judge whether (absolute) participation in MGNREGA significantly affects household income 
# i.e. a 0-1 dummy variable will suffice. The intensity of participation (categorical variable) is not needed. 
clean_data$WKNREGA2 <- ifelse(as.character(clean_data$WKNREGA) == "(0) none 0", 0, 1)

clean_data <- na.omit(clean_data)
# Get GENDER from RO3 Categorical variable in dataset
clean_data$GENDER <- sapply(clean_data$RO3, function(x) {
  switch(x,
         "(1) Male 1" = 0,
         "(2) Female 2" = 1,
         0)  # Default if no match
})
clean_data$GENDER <- unlist(clean_data$GENDER) # YOS is of LIST type by default

# Get highest education in household 
clean_data$HHEDUC <- sapply(clean_data$HHEDUC, function(x) {
  switch(x,
         "(00) none 0" = 0,
         "(01) 1st class" = 1,
         "(02) 2nd class" = 2,
         "(03) 3rd class" = 3,
         "(04) 4th class" = 4,
         "(05) 5th class" = 5,
         "(06) 6th class" = 6,
         "(07) 7th class" = 7,
         "(08) 8th class" = 8,
         "(09) 9th class" = 9,
         "(10) Secondary" = 10,
         "(11) 11th Class" = 11,
         "(12) High Secondary" = 12,
         "(13) 1 year post-secondary" = 13,
         "(14) 2 years post-secondary" = 14,
         "(15) Bachelors" = 15,
         "(16) Above Bachelors" = 16,
         0)  # Default if no match
})
clean_data$HHEDUC <- unlist(clean_data$HHEDUC) # YOS is of LIST type by default

reg_model <- lm(I(INCOME^0.5) ~ GENDER + RO5 + RO7 + URBAN2011 + I(HHEDUC^0.5) + I(log(NPERSONS)) + exp(ASSETS) + I(COTOTAL^0.5)+ WKNREGA2, data = clean_data, weights = (1/WT) )
summary(reg_model)

# Multicollinearity test (VIF < 10 ideally)
# install.packages('car')
library(car)
vif(reg_model)

# Heteroskedasticity test (White test for Robust SE)
# install.packages('sandwich')
# install.packages('lmtest') 
library(sandwich)
library(lmtest)
# BP test
bptest(reg_model)

# Normal 
# install.packages('moments')
library(moments)
residuals <- residuals(reg_model)
skewness(residuals)
kurtosis(residuals)

# Ramsey's RESET test for model misspecification
library(lmtest)
resettest(reg_model, power = 2)