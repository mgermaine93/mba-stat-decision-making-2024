#=====================================
#   PS1 -- Matthew Germaine
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

PS1_Data <- read_excel("~/Desktop/CODE/stat-decision-making-2024/MAG_Data_PS1.xlsx")
View(PS1_Data)

###############################################################################
#                                 Problem #1                                  #
###############################################################################
#
# 
# Suppose that an electrical component is designed for a mean performance
# standard of 𝜇 = 120 (N/mm2). A standard deviation of 𝜎 = 20 is anticipated in the
# production process.
#
# A sequence of 70 samples of 25 components has been taken and the averages of
# each of those 70 samples have been computed. Each of you has a separate data
# series with the 70 averages.
#
# When μ and σ are known, as in this exercise, a quality control chart has as its
# bounds: [𝜇 − 𝑍1−𝛼/2𝜎𝑋̅ , 𝜇 + 𝑍1−𝛼/2𝜎𝑋̅ ]. This is called the acceptance interval.
# In this application we have: 𝜇 = 120, σ𝑋̅ = σ/√𝑛 = 20/√25
#
#  a) Prepare your control chart.  Use a =.02.
controlChart(PS1_Data$`mgermai2@andrew.cmu.edu`,120,20,25,0.02)
#
#  b) Determine if the process was in control over the period for which your
#    70 samples were taken. What should you look for on your plot of the control
#    chart? The following are rules of thumb often applied in assessing whether the
#    process may be out of control:
#
#      i) More than 2a (in percentage) of values outside the control limits. (Recall
#         that, on average, a (in percentage) of the points will fall outside the limits
#         even when the process is working properly, so you should not expect
#         every point to be inside the control limits.)
#
answer_1_i = "The percentage we're looking for is 2 * 0.02 = 0.04, or 4%.
Looking at the controlChart, there are 7/70 points outside of the control limits.
Since 10% is greater than 4%, it is likely that the process may be out of control."
#
#     ii) A trend in the sample, i.e., six or more points in a row all increasing or all
#         decreasing.
#
answer_1_ii = "The control chart does not contain six or more points a row that are either
all increasing or all decreasing."
View(answer_1_ii)
#
#    iii) Too many points on one side of the center line, say nine consecutively or more.
#
answer_1_iii = "Yes, towards the right side of the control chart, there are 18 consecutive points
on one side of the center line."
View(answer_1_iii)
#
#  c) Find the probability that a draw from the standard normal distribution
#     will be less than Z=2.15. Hints: Read the Mini Tutorial below. Then see the
#     pnorm() command in the command summary.
#
probability = pnorm(2.15) # 0.9842224
View(probability)
#
#  d) What value of a is a firm using? A firm uses 𝑍1−α/2 = 2.24 in plotting its
#     control chart. Find the implied α for this firm. Hint: Build on the logic in the
#     previous question.
#
#     The command pnorm(-2.575) calculates α/2 = 0.005 and hence we conclude α = 0.01.
#     So we should do pnorm(-2.15), which equals 0.01577761
#
pnorm(-2.24)
alpha=pnorm(-2.24)*2
View(alpha) # alpha = 0.0251
#
#  e) Find the upper bound of a control chart: Suppose you have a
#     manufacturing process designed to produce a component with μ = 110 and σ = 12. 
#     The sample size per sample is 25 and α = .03. Calculate and enter the value
#     for the upper bound of your control chart. 
#        Hints:
#           i) The upper bound of a control chart is: μ + 𝑍1−α/2σ𝑋̅
#          ii) Recall that qnorm() is used to find the quantile of a normal distribution.
#         iii) The question gives you μ, σ, and 𝑛. Recall that σ𝑋̅ = σ√𝑛.
#
mean = 110
std_dev = 12
N = 25
alpha = 0.03
upper_bound = mean+qnorm(1-alpha/2)*std_dev/sqrt(N) # 115.2082
View(upper_bound)
#
#  f) Test the null hypothesis that the mean of the distribution generating your data
#     really is 120. Report how you tested the hypothesis and the result. As practice,
#     repeat it using data for some other columns in the dataset.
#
hypothesis_test = t.test(PS1_Data$`mgermai2@andrew.cmu.edu`,mu=120)
View(hypothesis_test) # p-value is 0.0019, so we reject the null hypothesis (since alpha is 0.03).
#
###############################################################################
#                                 Problem #2                                  #
###############################################################################
#
# A package delivery company, Never Late Delivery (NLD) is evaluating a brand of tires
# for equipping its fleet. Let the tire brand be denoted A. NLD plans to randomly select 𝑛1
# delivery vans to receive a type A tire. NLD will then measure the number of miles of
# wear provided by each tire before the tire needs to be replaced. The desired number of
# miles is 85.
#
# NLD conducts the experiment using 72 tires from the brand. Results of the experiment in
# part are as follows. Brand A tires obtained average mileage of 81.22 with variance 59.76.
# Consider testing the null hypothesis that the population miles of Brand A equals to 85
# with 5% significant level:
# 
x_bar = 85
mean = 81.22
variance = 59.76
N = 72
std_error = sqrt (variance) / sqrt(N)
#
#  a) What is the critical t value for the test?
#
crit_t = qt(.975,71) # 1.993943
#
#  b) What is the confidence interval for the test?
#
lower_bound=mean-crit_t*std_error # 79.4034
upper_bound=mean+crit_t*std_error # 83.0366
#
#  c) What is the t-statistic for the test?
#
t_stat=(mean - x_bar)/std_error # -4.1491
#
#  d) What is the p-value for the test?
#
p_value = 2*pt(t_stat,N-1) # 0.00009135
#
###############################################################################
#                                 Problem #3                                  #
###############################################################################
#
# A package delivery company, Never Late Delivery (NLD) is evaluating two alternative
# brands of tires for equipping its fleet. Let the tire brands be denoted A and B. NLD plans
# to randomly select 𝑛1 delivery vans to receive a type A tire and 𝑛2 delivery vans to
# receive a type B tire. NLD will then measure the number of miles of wear provided by
# each tire before the tire needs to be replaced. NLD plans to use the same number of tires
# for each brand, 𝑛1 = 𝑛2.
#
# Based on experience with tires on its vans, NLD estimates the standard deviation of miles
# to replacement to be 8. (All mileage numbers in this assignment are in thousands, so 8 is
# 8,000 miles.) NLD wishes to detect a difference in mean performance between the two
# brands of 4 thousand miles.
#
std_dev = 8
#
#  a) What is the definition of power?
#
answer_3_a = "The definition of power is the likelihood of correctly rejecting a false null hypothesis."
View(answer_3_a)
#
#  b) If NLD wishes a power of 0.75 and a significant level of 0.05, assuming the standard
# deviation is 8 for both brands A and B, how many tires of each brand should NLD test?
# Explain briefly the meaning of “power” in part (a).
#
answer_3_b = pwr.t.test(power = .75,d = .5,sig.level = .05)
View(answer_3_b) # 56.5 units of each tire should be tested
#
#  c) Suppose NLD wants a power of 0.9. How many tires should it test?
#
answer_3_c = pwr.t.test(power = .90,d = .5,sig.level = .05)
View(answer_3_c) # 85 units of each tire should be tested
#
#  d) Suppose the supplier of Brand B has a temporary shortage of tires and asks if it is
#     possible to conduct the experiment with fewer Brand B tires that you found in part
#     (c). Suppose NLD conducts its test using 3 Brand A tires for each 2 Brand B tires.
#     How many tires of each brand are required? Hint: Percent_B
#
answer_3_d = AB_t2n(
  percent_B = 0.4,mean_diff = 4,sd_A = 8,sd_B = 8,sig_level = 0.05,alternative = "two_sided",power = .9
)
View(answer_3_d) # 177.39 total tires.
# so 0.6 * 177 = 106 Brand A tires
# and 0.4 * 177 = 71 Brand B tires
#
#  e) NLD conducts the experiment using 72 tires from each brand. What power does
#     this test have if the standard deviations are 8 for each tire?
#
answer_3_e = AB_t2n(
  percent_B = 0.5,mean_diff = 4,sd_A = 8,sd_B = 8,sig_level = 0.05,alternative = "two_sided",N=144
)
View(answer_3_e) # power is 0.846
#
# Results of the experiment in part (e) are as follows. Brand A tires obtained average
# mileage of 81.22 with variance 59.76. Brand B tires obtained average mileage of 86.04
# and variance 56.98.
#
#  f) What was the difference in average mileage between Brand A and Brand B?
#
answer_3_f = abs(81.22 - 86.04)
View(answer_3_f) # 4.82
#
#  g) What is the t-statistic for testing the null hypothesis that there is no difference
#     in population miles between Brand A and Brand B?
#
answer_3_g=(81.22-86.04)/sqrt(59.76/72+56.98/72)
View(answer_3_g) # -3.785
#
#
#  h) (*) What is the p-value for the test in part (g)? Hint: 2*pt(-|t|,df)
#
answer_3_h = 2*pt(-abs(-3.785), (144-2))
View(answer_3_h) # 0.000226
#
#  i) Results of the experiment in part (e), are in file NLD_Mileage_Experiment.xlsx.
#     Tires were randomly assigned to vans in NLD’s fleet. The mileage obtained for
#     each tire was recorded in the variable in the file named “Miles”. Tire brand is
#     denoted by the indicator, Brand_A. This indicator variable equals 1 if the tire was
#     a Brand A tire and 0 if a Brand B tire. Using regression, test the null hypothesis
#     that the mean performance is the same for the two brands. Use a 5% significance
#     level.
#     Hint: During Access Weekend, we did a test of differences in means using the data
#     for the Atlanta water conservation experiment. To compare Treatment 1 to the control
#     group, we estimated the regression:
# 
#        WatReg34=lm(SUMMER_07~TREAT3,data=WatDat34)
#        summaryH(WatReg34)
#
#     You can use the same approach for testing the performance of tire brands using
#     Dataset NLD_Mileage_Experiment, with Miles as the dependent variable instead of
#     SUMMER_07, and Brand_A as the indicator variable instead of TREAT3.
#
# ******************** not really sure how I got this
NLD_Mileage_Experiment <- read_excel("~/Desktop/CODE/stat-decision-making-2024/NLD_Mileage_Experiment.xlsx")
answer_3_i = lm(Miles~Brand_A,data = NLD_Mileage_Experiment)
View(answer_3_i) # the p-value for the coefficient on Brand A is 0.0002.
# As a result, we reject the null hypothesis that the brands are the same.
# ******************** not really sure how I got this
#
#  j) A critic of the experiment argues that the results are invalid because some
#     delivery vans are older than other vans, and older vans tend to cause faster tire
#     wear. How would you respond to this criticism?
#
answer_3_j = "Since the tires were randomly assigned to the vans, the age of the vans does not matter."
View(answer_3_j)
#
#  k) Hearing your answer to the previous part, the critic says “OK, I understand the
#     validity of your argument, but can you provide any evidence that the actual
#     assignment to treatment and control is consistent with what you say you did?”
#     How would you answer. Hint: Variable “Age” in the data file is the age of each
#     van in months.
#
answer_3_k = lm(Age~Brand_A,data = NLD_Mileage_Experiment)
View(answer_3_k)
# The coefficient on brand_a is not significantly different than zero
# (p=0.1819), so we don’t have evidence that brand A is on older vans
#
###############################################################################
#                                 Problem #4                                  #
###############################################################################
#
# Interest rates charged on credit card debt of consumers are controversial. MasterCard and
# Visa are the two major cards, but they play no role in determining the rates charged by
# individual issuing banks. Individual banks own their cardholders' accounts and determine
# interest rates and other credit terms for their accounts. Thousands of firms issue bank
# credit cards. On the surface, this would appear to be an industry where competition would
# drive profits down to a razor-thin edge. Hence, one would expect that the cost of funds
# would be the major variable determining interest rates charged on this debt. The cost of
# funds is relatively easily established. There is a secondary market for securitization of
# credit card debt. Past investigation of this market has found that credit-card-backed
# securities offered yields of about 0.75 percent above U.S. Treasury Securities of
# comparable maturities. (With recent developments in the market for securitized debt, this
# may no longer be true.)
#
# We can use the following approach to test the hypothesis that credit-card rates are driven
# by the cost of funds. Let Δ𝑦𝑡 be the change in the interest rate on credit-card debt from
# period t-1 to t and let Δ𝑥𝑡 be the change in the interest rate on U.S. Treasury securities of
# comparable maturity between period 𝑡 − 1 and 𝑡.
#
#  a) Consider the regression:
#     Δ𝑦𝑡 = β0 + β1Δ𝑥𝑡 + ε𝑡
#     Do you agree with the following statement: “if the Treasury rate has some effect on
#     the credit card rate, then β1 > 0”. A one-sentence explanation is enough.
#
answer_4_a = "Yes, I agree with this, since β1 > 0 would indeed affect the change in the interest rate on credit card debt."
#
#  b) Do you agree with the following statement: “if the rate on credit card debt is
#     driven one-to-one by the cost of funds and the cost of funds varies with Treasury
#     securities of comparable maturity, then this regression should have a population
#     intercept β0 = 0 and a population slope coefficient of β1 = 1”. Explain briefly.
#
answer_4_b = "Yes, I agree.  If 𝛃0=0 and 𝛽1=1, then the equation relating credit card and treasury
rates becomes dy=dx. This states that the change in credit card rates is equal to
the change in treasury rates."
#
# File Compap_Credcrd_Treas_Rates.xlsx is in the datasets provided for the course. It
# contains quarterly data for 1997 through 2019. Assume α = 0.05 for all hypothesis tests.
# In this data set you have the Treasury Rate (Treas) along with the average Credit Card
# rate (Credcrd) and Commercial Paper Rate (Compap). We will investigate the extent to
# which changes in Treas predict changes in Credcrd and Compap.
# Launch R and import Dataset Compap_Credcrd_Treas_Rates.xlsx and execute the
# following commands, which are in the script.
#    For convenience, create a copy of the data set with a shorter name.
#       CCData=Compap_Credcrd_Treas_Rates
#       dCredcrd=with(CCData,Credcrd-lag(Credcrd,1))
#       dCompap=with(CCData,Compap-lag(Compap,1))
#       dTreas=with(CCData,Treas-lag(Treas,1))
#    The above commands calculate change from one quarter to the next in each of the three
#    interest rate variables. For example, dCredcard contains change from one quarter to the
#    next for Credit card rates.
#
CCData<- read_excel("~/Desktop/CODE/stat-decision-making-2024/Compap_Credcrd_Treas_Rates.xlsx")
dCredcrd=with(CCData,Credcrd-lag(Credcrd,1))
dCompap=with(CCData,Compap-lag(Compap,1))
dTreas=with(CCData,Treas-lag(Treas,1))
#
#  c) Estimate the model in part (a) for Credcrd and report your estimated intercept
#     and slope. Hence, estimate the following command in the script.
#        CredReg=lm(dCredcrd~dTreas)
#        summaryHAC(CredReg)
#     Note that I’ve asked you to use summaryHAC() in the above command. We do this for
#     time series data.
#
CredReg=lm(dCredcrd~dTreas)
summaryHAC(CredReg)
# Intercept estimate is 0.003618 / p-value is 0.913
# dTreas estimate is 0.346150 / p-value is 0.0000219
#
#  d) Test the null hypothesis that β1 = 0 and report your result. Remember that the
#     regression output always includes a test of the null hypothesis that each
#     population coefficient equals zero. Hence, you can simply look at the regression
#     output to find you answer.
#
answer_4_d = "Since the p-value for dTreas is 0.0000219 (and less than 0.05), we reject the null hypothesis."
#
#  e) From your result in (d), do you conclude that the treasury rate has some effect on
#     the credit card rate?
#
answer_4_e = "By rejecting the null hypothesis, we don't establish a causaul relationship.
But we also suggest that changes in the treasury rate similarly affects credit card rates."
#
#  f) Test the null hypothesis that β1 = 1 and report your result using the following
#     command:
#        coefTestHAC(CredReg,"dTreas=1")
#
coefTestHAC(CredReg,"dTreas=1") # p-value is less than 0.05 (0.0000000000004742), so we reject the null hypothesis.
answer_4_f = "Since the p-value is less than 0.05 (it's 0.0000000000004742), we reject the null hypothesis."
#
#  g) Repeat (c)-(f) for Compap instead of Credcrd.
#
Compap=lm(dCompap~dTreas)
summaryHAC(Compap)
# Intercept estimate is -0.002732 / p-value is 0.87
# dTreas estimate is 0.940797 / p-value is 0.000000000000000022
#
#  h) Test the null hypothesis that β0 = 0 for the Compap regression.
#
answer_4_h = "Since the p-value is 0.87, we do not reject the null hypothesis."
#
#  i) Would you agree with the following: “changes in Commercial Paper Rates follow
#     changes in Treasury Rates very closely. Changes in Credit Card Rates do not
#     follow changes in Treasury Rates very closely.”
#
answer_4_i = "Yes, I agree with this."
#
#  j) It is always a good idea to “look at your data”. Execute the following commands
#     in the script to plot changes in Commercial Paper and Treasury rates on the same
#     graph.
#
#        Create a variable named Observation to use for plotting
#           Time=seq(1,92)
#        Plot dCompap and dTreas on same graph
#           plot(dCompap~Time,col="white",ylab="Interest Rate Changes",
#           main = "Variation in Commercial Paper and Treasury Rates Over Time")
#           lines(Time,dCompap,lw=2)
#           lines(Time,dTreas,col="red",lw=2)
#        Familiarize yourself with using the “Export” feature in LRW so that you know how to
#        copy this graph to another document.
#
Time=seq(1,92)
plot(
  dCompap~Time,
  col="white",
  ylab="Interest Rate Changes",
  main = "Variation in Commercial Paper and Treasury Rates (red) Over Time"
)
lines(Time,dCompap,lw=2)
lines(Time,dTreas,col="red",lw=2)
#
#  k) Now do a plot with changes in Credit Card Rates and Treasury rates on the same
#     graph using the approach in part (j).
#
Time=seq(1,92)
plot(
  dCredcrd~Time,
  col="white",
  ylab="Interest Rate Changes",
  main = "Variation in Credit Card Rates and Treasury Rates (red) Over Time"
)
lines(Time,dCredcrd,lw=2)
lines(Time,dTreas,col="red",lw=2)
#
#  l) Does a comparison of your graphs in (j) and (k) seem consistent with your
#     conclusion in part (i)? Explain in a sentence or two.
#
answer_4_i = "Yes, the comparison of my graphs in (j) and(k) are consistent with 
my earlier conclucsion.  In graph (j), it's clear that the Commercial Paper and 
Treasury rates move closely together, while in graph (k) it's clear that Credit 
Card and Treasury rates do not move closely together."
#
# Remove items we no longer need 
rm(list=ls())
