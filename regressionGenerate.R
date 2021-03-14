# Imports 
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("dotwhisker")
library(dotwhisker)
#rm(list=ls()) 

#Import data 
ltc_raw <- read.csv("./Data/CombinedLTC.csv", stringsAsFactors = FALSE)
#Add councils
ltc_raw$FamCouncil[ltc_raw$FamCouncil %in% "Yes"] <- 1
ltc_raw$FamCouncil[ltc_raw$FamCouncil %in% "No"] <- 0
ltc_raw$ResCouncil[ltc_raw$ResCouncil %in% "Yes"] <- 1
ltc_raw$ResCouncil[ltc_raw$ResCouncil %in% "No"] <- 0
ltc_raw$Accreditation[ltc_raw$Accreditation %in% "Yes"] <- 1
ltc_raw$Accreditation[ltc_raw$Accreditation %in% "No"] <- 0
ltc_raw$homeType[ltc_raw$homeType %in% "For-Profit"] <- 2
ltc_raw$homeType[ltc_raw$homeType %in% "Non-Profit"] <- 1
ltc_raw$homeType[ltc_raw$homeType %in% "Municipal"] <- 0

#Look 
colnames(ltc_raw)
table(ltc_raw$FamCouncil)
table(ltc_raw$ResCouncil)
table(ltc_raw$homeType)
#mutate(ltc_raw, councilNum = ltc_raw$ResCouncil + ltc_raw$FamCouncil)
ltc_raw <- mutate(ltc_raw, councilNum=as.numeric(ResCouncil)+as.numeric(FamCouncil))
selected <-  ltc_raw %>% select(Total_LTC_Resident_Deaths, Beds, Annual_Non.compliance, Annual_Orders, Targeted_Inspect_Num, Targeted_Orders, Targeted_Non.compliance, councilNum, Accreditation, homeType)
names <- c("Number of Residents", "Non-compliance issues found in targeted inspections", "Number of Councils for Resident Advocacy", "Non-Profit Status", "For-Profit Status")
names(bestModel$coefficients) <- names

#Make models 
model <- lm(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance + Annual_Orders + Targeted_Inspect_Num + Targeted_Orders + Targeted_Non.compliance + councilNum + Accreditation + homeType, data=ltc_raw)
bestModel <- step(model)
summary(bestModel)


# Remove all model estimates that start with cyl*
m_factor <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars %>% mutate(cyl = factor(cyl)))
m_factor_df <- tidy(bestModel) 
dwplot(bestModel)

modelPlot <-  ltc_raw %>% select(Beds, Targeted_Non.compliance, councilNum, Total_LTC_Resident_Deaths)
#modelPlot <- lm(Total_LTC_Resident_Deaths ~ Beds + Targeted_Non.compliance + councilNum, data=ltc_raw)
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
colors <- c("#999999", "#E69F00", "#56B4E9")
scatterplot3d(modelPlot, color=colors)

plot(ltc_raw$homeType, ltc_raw$Total_LTC_Resident_Deaths, xlab="Eruption duration", ylab="Time waited")   

# Visualize: https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html 
modelG <- glm(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance + Annual_Orders + Targeted_Inspect_Num + Targeted_Orders + Targeted_Non.compliance + councilNum + Accreditation + homeType, data=ltc_raw)
bestModelG <- step(model)
summary(bestModelG)
# Visualize: https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html 

#Plot regression 
Modelvars <-  ltc_raw %>% select(Total_LTC_Resident_Deaths, Beds, Targeted_Non.compliance, councilNum, homeType)
plot_ly(data = Modelvars, z = ~Beds, x = ~Targeted_Non.compliance, y = ~height, color = ~smoke, colors = c('#0C4B8E' ,'#BF382A'),opacity = 0.5) %>%
  add_markers( marker = list(size = 2)) 

plot_summs(bestModel, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

X1 <- rnorm(100)
X2 <- rnorm(100,5,3)
Y <- 4 + 5*X1 + 3*X2 + rnorm(100)
mod <- lm(Y ~ X1 + X2)
tmp <- data.frame(t(coef(mod) * t(cbind(1, X1, X2))))
names(tmp) <- c("Intercept", "X1", "X2")
qplot(x=as.factor(1:100), fill=variable, weight=value, geom="bar", data=melt(tmp)) + geom_point(aes(x=1:100, y=predict(mod))

# References:
# https://www.r-bloggers.com/2018/11/interpreting-linear-prediction-models/
# https://biologyforfun.wordpress.com/2014/04/16/checking-glm-model-assumptions-in-r/

