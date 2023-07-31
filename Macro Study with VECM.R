# create function that ensures user has necessary packages installed
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs,require,character.only=TRUE))
  need <- libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

# pass packages into defined function
using("fredr", "tidyquant", "tidyverse", "stargazer", "dynlm", "lmtest",
      "astsa", "urca", "vars", "tsDyn")

# load packages
library(tidyverse)
library(fredr) # used to read in data from FRED
library(tidyquant) # used to read in data from yahoo finance
library(stargazer) # used to provide summary stats
library(dynlm)  # used to model time series data
library(lmtest) # used for statistical testing
library(astsa)  # used to determine level of serial correlation
library(urca)  # used to test for and estimate models with a unit root
library(vars)  # used to determine lag order and statistical tests for VAR model
library(tsDyn)  # used for VECM estimation


# assign API key for Federal Reserve Economic Data (FRED) to an object
FRED_API_KEY <- 'f800e29f337474e1c198a58fb3c3fc24'

# authenticate key for current R session
fredr_set_key(FRED_API_KEY)

# assign url links from public github to objects
medicare_enrollment_url <- "https://raw.githubusercontent.com/tendo-tommy/masters-thesis-vars/main/medicare_monthly_enrollment.csv"
ss_trust_fund_url <- "https://raw.githubusercontent.com/tendo-tommy/masters-thesis-vars/main/social_security_monthly_fund.csv"

# create a start and end date for every series
start_date <- as.Date('2013-01-01')
end_date <- as.Date('2019-12-01')


################################################################################
# The following section pulls in data from FRED, Medicare, the Social Security
# Administration, and Yahoo Finance. Each time series will belong to its own
# data frame in this section.
################################################################################

############################    FRED Data     ##################################
# extract monthly Total U.S. Population (in thousands)
ttl_pop <- fredr::fredr(
  series_id = 'POPTHM',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# keep date and corresponding values then rename the columns
ttl_pop <- as.data.frame(ttl_pop[,c('date','value')])
names(ttl_pop) <- c('Date', 'ttl_pop_thous')

# convert population to nominal values and remove values in thousands
ttl_pop <- dplyr::mutate(ttl_pop, ttl_pop = 1000*ttl_pop_thous)
ttl_pop <- ttl_pop[,c('Date', "ttl_pop")]

# extract monthly personal saving rate (as percent)
save_rate <- fredr::fredr(
  series_id = 'PSAVERT',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# keep date and corresponding values then rename the columns
save_rate <- as.data.frame(save_rate[,c('date','value')])
names(save_rate) <- c('Date', 'save')

# extract monthly unemployment rate (as percent)
unemp_rate <- fredr::fredr(
  series_id = 'UNRATE',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# keep date and corresponding values then rename the columns
unemp_rate <- as.data.frame(unemp_rate[,c('date','value')])
names(unemp_rate) <- c('Date', 'unemp')

# extract monthly labor for participation rate for 55+ (as percent)
elderly_labor <- fredr::fredr(
  series_id = 'LNS11324230',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# keep date and corresponding values then rename the columns
elderly_labor <- as.data.frame(elderly_labor[,c('date','value')])
names(elderly_labor) <- c('Date', 'elderly_labor')

# extract disposable personal income (in billions)
disp_inc <- fredr::fredr(
  series_id = 'DSPI',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# keep date and corresponding values then rename the columns
disp_inc <- as.data.frame(disp_inc[,c('date','value')])
names(disp_inc) <- c('Date', 'disp_inc_billi')

# convert disposable income to nominal values and remove values in billions
disp_inc <- dplyr::mutate(disp_inc, disp_inc = 1000000000*disp_inc_billi)
disp_inc <- disp_inc[,c('Date', "disp_inc")]

# extract personal consumption expenditure chain-type price index
cpi <- fredr::fredr(
  series_id = 'PCEPI',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# keep date and corresponding values then rename the columns
cpi <- as.data.frame(cpi[,c('date','value')])
names(cpi) <- c('Date', 'cpi')


#########################     Medicare Data     ################################
# extract Medicare monthly enrollment (in thousands) from github url
medicare_enroll <- readr::read_csv(url(medicare_enrollment_url))

# subset rows to intended date range and rename columns
medicare_enroll <- medicare_enroll[
  medicare_enroll$Date >= start_date & medicare_enroll$Date <= end_date, ]

names(medicare_enroll) <- c('Date', 'aged_thous')

# convert enrollment to nominal values and remove values in thousands
medicare_enroll <- dplyr::mutate(medicare_enroll, aged = 1000*aged_thous)
medicare_enroll <- medicare_enroll[,c('Date', "aged")]


######################     Social Security Data     ############################
# extract monthly social security trust fund data (in thousands) from github url
ss_fund <- readr::read_csv(url(ss_trust_fund_url))

# subset rows to intended date range and rename columns
ss_fund <- dplyr::mutate(ss_fund, Date = as.Date(Date, format = "%m/%d/%Y"))
ss_fund <- ss_fund[ss_fund$Date >= start_date & ss_fund$Date <= end_date, ]

names(ss_fund) <- c('Date', 'ss_thous')

# convert fund to nominal values and remove values in thousands
ss_fund <- dplyr::mutate(ss_fund, ss = 1000*ss_thous)
ss_fund <- ss_fund[,c('Date', "ss")]


##########################     S&P 500 Data     ################################
# assign stock symbol to a character vector
symbol1 = c('SPY')

# create data structure that contains stock quote objects
spy_data <- new.env()

# pull in monthly stock data
quantmod::getSymbols(symbol1, from=start_date, to=end_date %m+% months(1), 
                     periodicity="monthly", src="yahoo", env=spy_data)

# extract adjusted price column from object
spy_price <- as.data.frame(eapply(spy_data, Ad))

# create a date column from row names and rename columns
spy_price$Date <- as.Date(row.names(spy_price))
names(spy_price) <- c('spy', 'Date')

# reorder columns in a data frame
spy_price <- as.data.frame(spy_price[,c('Date', 'spy')])


################################################################################
# The following section combines each time series into one data frame
################################################################################

# condense data frames into a list
df_list <- list(ttl_pop, save_rate, unemp_rate, disp_inc, cpi, 
                medicare_enroll, ss_fund, elderly_labor, spy_price)

# merge each series on date
df_list %>% purrr::reduce(dplyr::full_join, by='Date')

# convert combined series to a data frame
combined_df <- data.frame(df_list)


################################################################################
# The following section makes per capita and inflation adjustments, creates the 
# age structure variable, creates a lag of each series, and then creates
# interaction terms.
################################################################################

# make inflation and per capita adjustments to disposable income and 
# social security, and only inflation adjustments to SPY
combined_df <- dplyr::mutate(combined_df, yd = (disp_inc/(cpi/100)/ttl_pop))
combined_df <- dplyr::mutate(combined_df, ss = (ss/(cpi/100)/ttl_pop))
combined_df <- dplyr::mutate(combined_df, spy = (spy/(cpi/100)))

# create age structure variables
combined_df <- dplyr::mutate(combined_df, age = 100*(aged/ttl_pop))

# keep only the relevant columns
combined_df <- combined_df[,c('Date', 'save', 'ss', 'unemp', 
                              'elderly_labor','yd', 'age', 'spy')]

# create lagged variables and combine with non-lagged variables in DF
l_combined_df <- lag(combined_df, 1)
DF <- data.frame(cbind(combined_df, l_combined_df))

# rename variables
names(DF) <- c(colnames(combined_df), paste0("l.",colnames(combined_df)))

# create interaction variables
DF <- dplyr::mutate(DF, ageXss = (age*ss))
DF <- dplyr::mutate(DF, ageXunemp = (age*unemp))
DF <- dplyr::mutate(DF, ageXyd = (age*yd))

# create an index column
DF$index <- 1:nrow(DF)


################################################################################
# Pre-processing for dataframe used in main analysis is complete. The
# following section creates a separate dataframe that will be used for a 
# visualization over a longer time horizon at an annual frequency.
################################################################################

# create a start and end date for the series
start_date <- as.Date('1960-01-01')
end_date <- as.Date('2021-01-01')

# extract age structure of U.S. Population (in percent)
p65.raw <- fredr::fredr(
  series_id = 'SPPOP65UPTOZSUSA',
  observation_start = start_date,
  observation_end = end_date,
  frequency = "a"
)

# create a data frame with a date and value column
p65 <- as.data.frame(p65.raw[,c('date','value')])
names(p65) <- c('Year', 'Pop_over_65')
p65$Year <- as.Date(p65$Year)

# assign important years to a list
sig.dates <- as.Date(c('1963-01-01', '1976-01-01', '1982-01-01', 
                       '1996-01-01', '1999-01-01'))

# add column to dataframe that only includes significant dates and NA otherwise
p65$sig.val <- dplyr::if_else(p65$Year %in% sig.dates, p65$Pop_over_65, NA)

# create a column for the sample period used in the final analysis
start.date = as.Date('2013-01-01')
end.date = as.Date('2020-01-01')

p65[,'sample'] <- NA

for (i in 1:nrow(p65)) {
  if(p65[i,'Year'] >= start.date & p65[i,'Year'] <= end.date){
    p65[i,'sample'] <- p65[i,'Pop_over_65']
  }else
  {
    p65[i,'sample'] <- NA
  }
}


################################################################################
# Data pre-processing is done. The following section creates a visualization
# that introduces the variable of interest (age structure) and provides 
# context to the extended life cycle model.
################################################################################

# create a visualization of age structure over time - important years are 
# highlighted red and the sample period is blue
ggplot2::ggplot(p65, ggplot2::aes(Year)) +
  ggplot2::geom_line(ggplot2::aes(y=Pop_over_65), linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y=sample), linewidth = 2, colour = 'blue')+ 
  ggplot2::geom_point(ggplot2::aes(y=sig.val), colour='red', size=3) + 
  ggplot2::ggtitle('Annual U.S. Age Structure \n (1960 - 2021)') + 
  ggplot2::ylab('Population Over 65 (%)') + 
  ggplot2::theme_classic()


################################################################################
# The following section provides summary statistics and applies standard OLS 
# estimation methods and testing for serial correlation and heteroskedasticity.
################################################################################

# initialize time series
df.ts <- ts(DF, start=c(2013,1), end=c(2019,12), frequency=12)

# summary statistics and plots
plot.ts(df.ts[,c(3,2,6)], type = "l", plot.type = "multiple", lty = 1:3, 
        main = "Per Capita Real Social Security Funds
        and Disposable Income", ylab = "Per Capita ($2012)")

stargazer::stargazer(df.ts, summary = TRUE, type= 'text')

# provide OLS estimates of the model
base.model <- dynlm::dynlm(save ~ yd + ss + age,
                           data=window(df.ts, start=c(2013,2)))
summary(base.model)

# testing for serial correlation
lmtest::dwtest(base.model, alternative='two.sided')
lmtest::bgtest(base.model, order=1)
astsa::acf2(base.model$residuals)

# testing for heteroskedasticity
bptest(base.model)


################################################################################
# The following section tests for a unit root in level and first difference form
# for each series using Augmented Dickey Fuller tests, DF-GLS tests, and KPSS 
# tests.
################################################################################

# testing for I(2) vs I(1) series - Augmented Dickey Fuller
summary(urca::ur.df(diff(df.ts[1:84,"save"]), type = c("trend"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(diff(df.ts[1:84,"yd"]), type = c("drift"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(diff(df.ts[1:84,"ss"]), type = c("trend"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(diff(df.ts[1:84,"age"]), type = c("drift"), lags = 12,
                    selectlags = c("BIC")))
summary(urca::ur.df(diff(df.ts[1:84,"ageXss"]), type = c("drift"), lags = 11,
                    selectlags = c("BIC")))
summary(urca::ur.df(diff(df.ts[1:84,"spy"]), type = c("drift"), lags = 12,
                    selectlags = c("BIC")))

# testing for I(1) vs I(0) series - Augmented Dickey Fuller
summary(urca::ur.df(df.ts[2:84,"save"], type = c("drift"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(df.ts[2:84,"yd"], type = c("drift"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(df.ts[2:84,"ss"], type = c("drift"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(df.ts[2:84,"age"], type = c("trend"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(df.ts[2:84,"ageXss"], type = c("drift"), lags = 1,
                    selectlags = c("BIC")))
summary(urca::ur.df(df.ts[2:84,"spy"], type = c("drift"), lags = 12,
                    selectlags = c("BIC")))

# testing for I(2) vs I(1) series - DF-GLS
summary(urca::ur.ers(diff(df.ts[1:84,"save"]), 
                     model = c("trend"), lag.max = 1))
summary(urca::ur.ers(diff(df.ts[1:84,"yd"]), 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(diff(df.ts[1:84,"ss"]), 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(diff(df.ts[1:84,"age"]), 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(diff(df.ts[1:84,"ageXss"]), 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(diff(df.ts[1:84,"spy"]), 
                     model = c("constant"), lag.max = 1))

# testing for I(1) vs I(0) series - DF-GLS
summary(urca::ur.ers(df.ts[2:84,"save"], 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(df.ts[2:84,"yd"], 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(df.ts[2:84,"ss"], 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(df.ts[2:84,"age"], 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(df.ts[2:84,"ageXss"], 
                     model = c("constant"), lag.max = 1))
summary(urca::ur.ers(df.ts[2:84,"spy"], 
                     model = c("constant"), lag.max = 1))

# testing for I(2) vs I(1) series - KPSS
summary(urca::ur.kpss(diff(df.ts[1:84,"save"]), 
                      type=c("tau"), lags=c("short")))
summary(urca::ur.kpss(diff(df.ts[1:84,"yd"]), 
                      type=c("tau"), lags=c("short")))
summary(urca::ur.kpss(diff(df.ts[1:84,"ss"]), 
                      type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(diff(df.ts[1:84,"age"]), 
                      type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(diff(df.ts[1:84,"ageXss"]), 
                      type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(diff(df.ts[1:84,"spy"]), 
                      type=c("mu"), lags=c("long")))

# testing for I(1) vs I(0) series - KPSS
summary(urca::ur.kpss(df.ts[2:84,"save"], type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(df.ts[2:84,"yd"], type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(df.ts[2:84,"ss"], type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(df.ts[2:84,"age"], type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(df.ts[2:84,"ageXss"], type=c("mu"), lags=c("short")))
summary(urca::ur.kpss(df.ts[2:84,"spy"], type=c("mu"), lags=c("long")))


################################################################################
# The following section tests for cointegration using the Johansen test. A model
# will be estimated using the VECM function and ca.jo function. Both will be
# tested for autocorrelation - the ca.jo() method can be tested in its VAR form.
################################################################################

# Johansen-Juselius test for cointegration
df2 <- as.data.frame(cbind(df.ts[,"save"], 
                           df.ts[,"yd"], df.ts[,"ss"], df.ts[,"age"]))
names(df2) <- c('save', 'yd', 'ss', 'age')

summary(urca::ca.jo(df2, type="trace", ecdet="const", spec="longrun", K=2))

# estimate vector error correction (VEC) model using VECM() function
summary(tsDyn::VECM(df2, r=1, lag=1, estim='2OLS', LRinclude='const'))

# test VEC model for autocorrelation
vars::serial.test(tsDyn:::vec2var.tsDyn(
  tsDyn::VECM(df2, r=1, lag=1, estim='2OLS', LRinclude='const')),
  lags.bg=4, type="BG")

# estimate vector error correction (VEC) model using ca.jo() function
jo.test <- urca::ca.jo(df2, type="trace", ecdet="trend", spec="longrun", K=2)
summary(urca::cajools(jo.test))

# estimate VECM model and transform VECM to VAR to test for serial correlation
jo.test <- vars::vec2var(urca::ca.jo(df2, type="trace", ecdet="const", 
                                     spec="longrun", K=2), r=1)

vars::serial.test(jo.test, lags.bg=4, type="BG")
vars::serial.test(jo.test, lags.pt=5, type="PT.adjusted")


################################################################################
# This section estimates specifications of the final model, obtains a goodness 
# of fit statistic, and checks for heteroskedasticity. Sub sections will contain 
# alternative specifications of the proposed model.
################################################################################
# define a function that calculates adjusted R-squared
adj_r_sq <- function(rss, tss, n , k){
  r_sq <- (1-(rss/tss))
  adj_r <- 1-((1-r_sq)*(n-1)/(n-k-1))
  return(adj_r)
}


######################### Initial version of model #############################
# assign the model variables from df.ts to an object then reassign var names
model.df <- as.data.frame(cbind(df.ts[,"save"], 
                                df.ts[,"yd"], df.ts[,"ss"], df.ts[,"age"]))
names(model.df) <- c('save', 'yd', 'ss', 'age')

# estimate the final model as a VECM
model <- tsDyn::lineVar(model.df, r=1, lag=1, estim='2OLS', 
                        LRinclude = 'const', model='VECM')

summary(model)

# get the first-difference series of the independent variable
diff.save.df <- as.data.frame(diff(DF$save))
diff.save.df <- diff.save.df[2:83,]  # reduce observations to match sample size

# get predictions, residuals, and explained variation from the mean
pred <- as.data.frame(fitted(model))
resid <- as.data.frame(resid(model))
explained_err <- as.data.frame(fitted(model) - mean(diff.save.df))

# obtain adjusted R-squared
n <- 82
k <- 3
ess <- sum((explained_err$save)^2)
rss <- sum((resid$save)^2)
tss <- ess + rss
adj_r_sq(rss, tss, n, k)

# testing for time-varying conditional volatility, or autoregressive conditional 
# heteroskedastic (ARCH) effects
vars::arch.test(tsDyn:::vec2var.tsDyn(model), lags.multi = 1)

######################### Interaction term included ############################
# assign the model variables from df.ts to an object then reassign var names
interaction.df <- as.data.frame(cbind(df.ts[,"save"], 
                                      df.ts[,"yd"], df.ts[,"ss"], df.ts[,"age"], 
                                      df.ts[,"ageXss"]))
names(interaction.df) <- c('save', 'yd', 'ss', 'age', 'ageXss')

# estimate the final model as a VECM
interaction.model <- tsDyn::lineVar(interaction.df, r=1, lag=1, estim='2OLS', 
                                    LRinclude = 'const', model='VECM')

summary(interaction.model)

# get the first-difference series of the independent variable
diff.save.df <- as.data.frame(diff(DF$save))
diff.save.df <- diff.save.df[2:83,]  # reduce observations to match sample size

# get predictions, residuals, and explained variation from the mean
pred <- as.data.frame(fitted(interaction.model))
resid <- as.data.frame(resid(interaction.model))
explained_err <- as.data.frame(fitted(interaction.model) - mean(diff.save.df))

# obtain adjusted R-squared
n <- 82
k <- 4  # change this when changing model
ess <- sum((explained_err$save)^2)
rss <- sum((resid$save)^2)
tss <- ess + rss
adj_r_sq(rss, tss, n, k)

########################### S&P 500 term included ##############################
# assign the model variables from df.ts to an object then reassign var names
wealth.df <- as.data.frame(cbind(df.ts[,"save"], 
                                 df.ts[,"yd"], df.ts[,"ss"], df.ts[,"age"], 
                                 df.ts[,"spy"]))
names(wealth.df) <- c('save', 'yd', 'ss', 'age', 'spy')

# estimate the final model as a VECM
wealth.model <- tsDyn::lineVar(wealth.df, r=1, lag=1, estim='2OLS', 
                               LRinclude = 'const', model='VECM')

summary(wealth.model)

# get the first-difference series of the independent variable
diff.save.df <- as.data.frame(diff(DF$save))
diff.save.df <- diff.save.df[2:83,]  # reduce observations to match sample size

# get predictions, residuals, and explained variation from the mean
pred <- as.data.frame(fitted(wealth.model))
resid <- as.data.frame(resid(wealth.model))
explained_err <- as.data.frame(fitted(wealth.model) - mean(diff.save.df))

# obtain adjusted R-squared
n <- 82
k <- 5  # change this when changing model
ess <- sum((explained_err$save)^2)
rss <- sum((resid$save)^2)
tss <- ess + rss
adj_r_sq(rss, tss, n, k)

######################### Lag of social security term ##########################
# assign the model variables from df.ts to an object then reassign var names
lagss.df <- as.data.frame(cbind(df.ts[,"save"], 
                                df.ts[,"yd"], df.ts[,"l.ss"], df.ts[,"age"]))
names(lagss.df) <- c('save', 'yd', 'l.ss', 'age')

# estimate the final model as a VECM
lagss.model <- tsDyn::lineVar(lagss.df[2:84,], r=1, lag=1, estim='2OLS', 
                              LRinclude = 'const', model='VECM')

summary(lagss.model)

# get the first-difference series of the independent variable
diff.save.df <- as.data.frame(diff(DF$save))
diff.save.df <- diff.save.df[3:83,]  # reduce observations to match sample size

# get predictions, residuals, and explained variation from the mean
pred <- as.data.frame(fitted(lagss.model))
resid <- as.data.frame(resid(lagss.model))
explained_err <- as.data.frame(fitted(lagss.model) - mean(diff.save.df))

# obtain adjusted R-squared
n <- 81  # change this when sample size changes
k <- 4  # change this when changing model
ess <- sum((explained_err$save)^2)
rss <- sum((resid$save)^2)
tss <- ess + rss
adj_r_sq(rss, tss, n, k)

################################################################################
## CLOSING REMARKS
## This study supports the extended life cycle model and finds that consumers
## are replacing future social security income with personal savings. It should
## be noted that the purpose of this project was to address statistical issues
## in the data and develop a model that provides accurate, non-spurious
## estimates and statistical tests. It is NOT intended for forecasting.
###############################################################################