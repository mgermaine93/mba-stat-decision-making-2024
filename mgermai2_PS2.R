#=====================================
#   PS2 -- Matthew Germaine
#=====================================
#=========================================
# Load Packages and Set working directory
#=========================================
# Clear the working space
# This isn't necessary but is a good habit
rm(list = ls())

# Load the packages.
library(tprstats)
library(readxl)
tprstats::setup()
# Go to your working directory by clicking Session/Set working Directory/Choose Directory
setwd("~/Desktop/CODE/stat-decision-making-2024")

CCWS_CG <-
  read_excel("~/Desktop/CODE/stat-decision-making-2024/Data_Sets_2024/CCWS_CG.xlsx")
View(CCWS_CG)

###############################################################################
#                                 Problem #1                                  #
###############################################################################
#
# For this exercise, you will use data in file CCWS_CG.xlsx. This file contains 
# 71,643 observations from the Cobb County Water System (CCWS) in Atlanta, 
# Georgia. These are the 71,643 households in the control group in our water 
# conservation case. For ease of reference, I have assigned an account number 
# for each customer. Because these households are in the control group, they 
# were not impacted by the experiment. Hence, we can use these data to analyze 
# questions unrelated to the experiment.
#
#  a) Bring up a histogram of SUMMER_07. Try each of the following to get an 
#     idea of the use of the “breaks” option:
#
hist(CCWS_CG$SUMMER_07) 
hist(CCWS_CG$SUMMER_07,breaks = 200)
#
#  b) What is the most striking feature of your histogram in part (a)?
#
answer_1_b = "The most striking feature of the histogram is that is heavily 
skewed to the right."
#
#  c) The manager of CCWS is puzzled by the very high usage levels of some 
#     customers. She wants to investigate whether these may be coding errors in 
#     the data or whether the numbers reflect actual usage. She asks you for 
#     the account number of the largest user in SUMMER_07. Find and report the 
#     account number of that user. This can be done in R in the following way. 
#     Suppose you have a dataframe named MyData that has several variables 
#     including a variable named y. Suppose you want to select the row from 
#     MyData with the largest value of variable y. You can issue the following 
#     command in R.
#
#     Bigy=subset(MyData, y==max(y))
#
#     The dataframe Bigy will then contain the row of MyData that has the 
#     largest value of y.
#
#     Let’s use the above approach to select the row of CCWS_CG that has the 
#     largest water usage in the summer of 2007 by running the following command. 
#     Be sure you understand this command.
#
BiggestUser=subset(CCWS_CG, SUMMER_07==max(SUMMER_07)) # account 23788, 1632 usage.
#
#  d) Click the spreadsheet icon to the right of BiggestUser in URW. This will 
#     display the information for BiggestUser in the ULW. Report the water usage 
#     of this customer in 2006, and in the summers of 2007, 2008, and 2009. 
#     Comment in a sentence or two on any puzzles you see in the pattern of 
#     water use for this user.
#
answer_1_d = "The customer's account is 23788.  This customer's water usage in
2006 was 2441 (million gallons).  In summer 2007, they used 1632 units; in summer 2008,
43 units; in summer 2009, 34 units.  Based on this customer's data, his/her 
usage in summer 2007 FAR exceeds his/her usage in the subsequent summers.  They
also appear to have had high usage in spring 2007 (526 units), which also 
happens to be the year of their extremely high overall usage."
#
#  e) The manager of CCWS is interested in exploring whether a simple linear 
#     regression with readily available data can be used to predict customer 
#     water usage in the summer months. Estimate a regression with water use in 
#     summer of 2009 as the dependent variable. In your regression, include 
#     water use in 2006, water use in April- May of 2007, water use in summers 
#     of 2007 and 2008, and a constant. Name your regression WatReg09.
#
WatReg09 = lm(
  SUMMER_09 ~ WATER_2006 + APR_MAY_07 + SUMMER_07 + SUMMER_08,
  data = CCWS_CG
)
summaryH(WatReg09)
#
#  f) Are all the coefficients in your regression statistically significant at 
#     the 5% level?
#
answer_1_f = "Yes.  Since the p-value is 0.00000000000000022 (much less than 
0.05), all of the coefficients in the regression are statistically significant 
at the 5% level."
#
#  g) Provide a one-sentence interpretation of the coefficients of WATER_2006 
#     and SUMMER_08.
#
answer_1_g_1 = "An increase in a customer's water consumption of 1,000 gallons 
in 2006 implies a 77-gallon increase in the summer of 2009, holding constant 
water usage in the two previous summers and spring 2007."
#
answer_1_g_2 = "An increase in a customer's water consumption of 1,000 gallons 
in summer 2008 implies a 348-gallon increase in the summer of 2009, holding constant 
water usage in summer 2006, all of 2006, and spring 2007."
#
#  h) Test the null hypothesis that the coefficient of SUMMER_08=.4 using a 5% 
#     significance level. Report your p-value and your conclusion.
#
coefTestH(WatReg09,"SUMMER_08=0.4")
answer_h = "Since the p-value is 0.02311, we reject the null hypothesis."
#
#  i) What proportion of the variation in SUMMER_09 water use is explained by 
#     your regression?
#
answer_1_i = "R-squared reveals the proportion of the variation in SUMMER_09 
water use that is explained by the regression.  The R-squared value from 
WatReg09 above is 0.4263."
#
#  Questions of type (j) and (k) will not be on the quiz.
#
#  j) Report the elasticity at the sample means for the coefficients of 
#     WATER_2006 and SUMMER_08. Hint: tprstats provides the command 
#     elasticities().
#
elasticities(WatReg09)
answer_1_j = "The elasticity at the sample means for the coefficient of 
WATER_2006 is 0.16348259.  The elasticity at the sample means for the coefficient of 
SUMMER_2008 is 0.32376013."
#
#  k) Provide a one-sentence interpretation of the elasticity in part (j) in 
#     terms that would be understandable by an intelligent layman.
#
answer_1_k = "Since the elasticity at the sample means of WATER_2006 is 0.16,
this implies that a 1% increase in water consumption in 2006 corresponds to 
a 16% increase in water consumption in summer 2009.  Similarly, since the
elasticity at the sample means of SUMMER_2008 is 0.32, this implies that a 1%
increase in water consumption in summer 2008 corresponds to a 32% increase
in water consumption in summer 2009."
#
###############################################################################
#                                 Problem #2                                  #
###############################################################################
#
#  The data set we will use has information for 62 homes in Ward 14 in 
#  Pittsburgh. The data set is named Ward14_Homes.xlsx. It contains the sale 
#  price of each home as well as information about the total number of rooms, 
#  not counting bathrooms (Totalrm), number of full bathrooms (Fullbath), the 
#  size of the living space in square feet, and the size of the lot in square 
#  feet.
#
#  If you would like to see a map of the wards in Pittsburgh, you can find it 
#  here:  https://pittsburghpa.maps.arcgis.com/apps/OnePane/basicviewer/index.html?appid=2a57c 4fbe92248e38a57220d20f23ae2
#
Ward_14 <-
  read_excel("~/Desktop/CODE/stat-decision-making-2024/Data_Sets_2024/Ward14_Homes.xlsx")
View(Ward_14)
#
#  a) Begin by estimating the regression of Price against Fullbath, Lotarea, 
#     Liveable_sqft and Totalrm. Name your regression Ward14Reg1. (Do not use 
#     Halfbath in the regression.)
#
Ward14Reg1 = lm(
  Price ~ Fullbath + Lotarea + Liveable_sqft + Totalrm,
  data = Ward_14
)
summaryH(Ward14Reg1)
#
#  b) Interpret the coefficient of Fullbath in terms understandable by an 
#     intelligent layman.
#
answer_2_b = "According to the coefficient of Fullbath, the addition of one
full bath to a home in Ward 14 increases the price of a house by $51,736.82
whene everything else is held constant."
#
#  c) Interpret the coefficient of Lotarea in terms understandable by an 
#     intelligent layman.
#
answer_2_c = "According to the coefficient of Lotarea, for each additional
square foot of lot area belonging to a home in Ward 14, the price of the home
increases $7.34 when everything else is held constant."
#
#  d) Do you agree that this regression implies that an additional full 
#     bathroom is worth about 5 times as much as a regular room holding Lotarea 
#     and Liveable_sqft constant?
#
answer_2_d = "Since the regression estimates do indeed state that one additional
full bathroom adds at least five times as much to the sale price as a regular 
room, I do agree with this assessment.  However, the coefficient on 'Totalrm'
is not precisely estimated."
#
#  e) Are all the coefficients in the regression significant at the 5% level?
#
answer_2_e = "No.  From the Pr(>|t|) column in the linear regression estimates,
we can see that only the 'Fullbath' coefficient (at 0.00000147) is significant 
at the 5% level.  All of the other coefficients are not significant at the 5%
level."
#
#  f) When using regression analysis, some people use a mechanical rule such as 
#     dropping variables that are statistically insignificant. This is often a 
#     bad idea. We will see why in the analysis that follows. Suppose we drop 
#     livable square feet and estimate a regression: Price against Fullbath, 
#     Lotarea, and Totalrm. Name this regression Ward14Reg2.
#
Ward14Reg2 = lm(
  Price ~ Fullbath + Lotarea + Totalrm,
  data = Ward_14
)
summaryH(Ward14Reg1)
#
#  g) Do a plot with Liveable_sqft on the horizontal axis and Totalrm on the 
#     vertical axis. Give a one-sentence interpretation of the relationship in 
#     this plot.
#
plot(
  Ward_14$Liveable_sqft,
  Ward_14$Totalrm
)
answer_2_g = "There appears to be a positive correlation between a house's 
amount of liveable square feet and total rooms"
#
#  h) Calculate the correlation between livable square feet and total rooms. 
#     Hint: Slides Multiple_regression_bias_from_missing_variable show how to 
#     calculate a correlation using R.
#
cor(
  Ward_14$Liveable_sqft,
  Ward_14$Totalrm
)
answer_2_h = "The correlation between liveable square feet and number of rooms
is 0.7742792."
#
#  i) Using your regression in part (a) predict the price of a home Fullbath=2, 
#     Lotarea =5000, Liveable_sqft=2000, and Totalrm =7 and provide a 95% 
#     prediction interval.
#
predict(
  Ward14Reg1,
  data.frame(
    Fullbath=2,
    Lotarea=5000,
    Liveable_sqft=2000,
    Totalrm=7
    ),
  interval="predict"
)
answer_2_i = "Based on the regression, a home of this size and style is 
predicted to cost $241,346.10.  For a 95% prediction interval, the lower bound
is $138,943.20, and the upper bound is $343,749.10."
#
#  j) Which home has the largest positive residual in your regression in part 
#    (a). Hint: Calculate the residuals and name the result resid. Then issue 
#    the R command: which(resid==max(resid))
#
residual = resid(Ward14Reg1)
which(residual == max(residual))
answer_2_j = "Observation #28 has the largest positive residual in my 
regression from part (a)."
#
#  Questions of type (k) through (n) will not be on the quiz.
#
#  k) Provide an intuitive explanation for why the coefficient of Totalrm 
#     increased when you drop the livable square feet variable from the 
#     regression. Hint: Parts (g) and (h) are to help with this intuition.
#
answer_1_k = "We expect the correlation between Totalrm and liveable_sqft to be 
high.  This is suggested in part (g).  Naturally, when the variable representing
liveable square feet is dropped from the regression, the coefficient of Totalrm
adopts part of the effect of liveable square feet."
#
#  l) Provide an intuitive explanation for why the coefficient of Totalrm is 
#     statistically significant in the regression in part (f) but not 
#     statistically significant in the regression in part (a).
#
answer_2_l = "When two variables are highly correlated, it is difficult to 
separate out their individual effects. Totalrm and liveable_sqft tend to move 
together when we look across houses of different sizes. Hence, it is difficult 
to separate out the individual effects of the two variables on price. This is 
reflected in the higher standard errors on the coefficients of those variables. 
In this case, when we removed a highly correlated variable, the standard error 
on the other decreased, making it significant. Of course, this is problematic, 
because we are asserting that liveable_sqft does not matter when we leave it 
out of the regression. We are on safer ground with the regression in part (a)."
#
#  m) Suppose the owner of a home in Ward 14 plans to add a room to the home 
#     without increasing Liveable_sqft (e.g., by dividing one large room into 
#     two smaller rooms). Which regression do you think provides the better 
#     estimate of the effect of this change on the value of the home?
#
answer_2_m = "Regression (a) should provide the better estimate on the effect
of this change on the value of the home.  This is because 'Price' in regression
(a) has price depend in part both on liveable square feet and total rooms, 
whereas regression (g) has price depend in part only on total rooms (and not 
liveable square feet)."
#
#  n) Using your regression in part (a), do a joint test of the null hypothesis 
#     that the population coefficients of both Liveable_sqft and Totalrm are zero.
#
coefTestH(Ward14Reg1,c("Liveable_sqft=0","Totalrm=0"))
answer_2_n = "Since the p-value of this test is 0.01817 (which is less than 
0.05), we can reject the null hypothesis that the population coefficients of 
both liveable square feet and total room are zero."
#