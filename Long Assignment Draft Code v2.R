
# Write it so it is a function that returns fair value and next draft do it in markdown

# Input initial assumptions for WACC

shares <- 1000000 # shares outstanding
currentPrice <- 1 # current share price
mDebt <- 500000 # mv of debt
term <- 10 # 10 year projection

Kd <- 0.05 # Cost of debt
taxRate <- 0.35 # tax rate
beta <- 1.25 # beta
rf <- 0.025 # risk-free rate
rm <- 0.10 # expected market return

beta.future <- function(beta, cap, term) {
  if (term < cap) { # if it is decided that the trend to 1 will happen beyond the projection, then keep it the same
    beta.project <- c(rep(beta, term))
    return(beta.project)
  } else {
    beta.project <- c(rep(beta, cap), seq(beta, 1, length.out =  (term - cap + 1))[2:(term - cap + 1)]) # it will reach 1 by the end of the projection period
    return(beta.project)
  }
}

beta.project <- beta.future(beta, 5, term) # we assume five years for cap

wacc.calc <- function(mDebt, shares, currentPrice, Kd, taxRate, beta, rf, rm){ # we will assume weights and cost of debt remain constant
  mEquity <- shares * currentPrice 
  wDebt <- mDebt / (mEquity + mDebt)
  wEquity <- mEquity / (mEquity + mDebt)
  wacc <- wDebt * Kd * (1 - taxRate) + wEquity * (rf + beta * (rm - rf))
  return(wacc)
}

wacc.project <- wacc.calc(mDebt, shares, currentPrice, Kd, taxRate, beta.project, rf, rm)

# Operational assumptions

revenues <- 1000000 # current year revenues
ebitMargin <- 0.25 # EBIT margins
depreciation <- 50000 # depreciation 
capEx <- 65000 # capEx 
nwc <- 0.01 # net change in working capital % of revenue
g <- 0.02 # terminal growth

# projected figures/assumptions

revGrowth <- c(rep(0.06, 7), rep(0.03, 3)) # revenue growth 
ebitMargin.future <- c(rep(ebitMargin, 10)) # EBIT margin stays the same
capExGrowth <- c(rep(0.06, 5), rep(0.03, 5)) # capEx growth 
deprGrowth <- c(rep(0.06, 6), rep(0.03, 4)) # deprecation growth 
nwcTime <- c(rep(nwc, 10)) # nwc % of revenue over time 

projection <- function(start, growth.path) {
  path <- start * cumprod(1 + growth.path) # gives the vector of the projected financial line item
  return(path)
}

revenue.projection <- projection(revenues, revGrowth) # revenue projection vector
capex.projection <- projection(capEx, capExGrowth) # capex projection vector
depr.projection <- projection(depreciation, deprGrowth) # depreciation projection vector

fairvalue <- function(revenue, ebitmargin, t, capex, depr, nwc, wacc, g, term, mdebt, shares) {
  fcf <- revenue * ebitmargin * (1 - t) -  capex + depr - nwc * revenue # get unlevered fcf
  pvfcf <- fcf / cumprod(1 + wacc) # present value unlevered fcf vector
  terminal <- fcf[term] * (1 + g) / (wacc[term] - g) # terminal value using the Gordon Growth method
  enterprise.value <- sum(pvfcf) + terminal / cumprod(1 + wacc)[term] # add the present values together
  equity.value <- enterprise.value - mdebt # take out the mv of debt from the ev
  fair.value <- equity.value / shares # get the per share value
  return(fair.value)
}

fair.value <- fairvalue(revenue.projection, ebitMargin.future, taxRate, capex.projection, depr.projection, 
                        nwcTime, wacc.project, g, term, mDebt, shares)

print(fair.value)
