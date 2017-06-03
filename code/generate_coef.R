df = read.csv("Documents/Mine New/Harvard 2016/Stat 139/projectdata_1/data1.csv", header = T) 
df = df[,2:11]
library(plyr)
df=rename(df, c("X"="name", "X.1"="Y","Sociopolieconomic.Factors"
                = "gender_index","X.2"="primary","X.3"="govt",
                "X.4"="gdp","X.5"="develop","Biological.Factors"=
                  "teen","X.6"="obesity","X.7"="abortion"))

#create primary enrollment indicator
df$primary_ind <- cut(df$primary, c(0,0.95, max(df$primary)), labels=c(0:1))

#transform variables
gender_index <- df$gender_index
primary_ind <- df$primary_ind
govt <- sqrt(df$govt)
gdp <- log(df$gdp)
teen <- log(df$teen)
obesity <- sqrt(df$obesity)
abortion <- df$abortion
y <- (df$Y)^2

#new dataframe with transformed variables
trans_df <- cbind(y, gender_index, primary_ind, govt, gdp, teen,
             obesity, abortion)

#baseline main effect model
baseline <- lm(y ~ gender_index+primary_ind+govt+gdp+teen+obesity+abortion)
summary(baseline)

#stepwise selection
null = lm(y~1, data=as.data.frame(trans_df))
full = lm(y~.^2, data=as.data.frame(trans_df))
model1 = step(baseline, scope = list(lower = null, upper=full), direction="both")

#fit best model with data
fit1 = lm(y~primary_ind + govt + gdp + teen + obesity + abortion + primary_ind:abortion + 
            gdp:abortion + govt:obesity + govt:gdp + govt:teen)

#save coefficients
c20 = coef(summary(fit1))
