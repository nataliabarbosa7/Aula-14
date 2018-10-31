> BITCOIN <- read_excel("C:/Econometria/Bitcoin.xls")
> Bitcoin <- ts <- ts(BITCOIN$Close, start = 2017, frequency = 365)
> plot(Bitcoin)
> View(Bitcoin)
> chow <- Fstats(Bitcoin~1)
> sctest(chow)

supF test

data:  chow
sup.F = 456.08, p-value < 2.2e-16

> plot(Bitcoin)
> lines(breakpoints(chow))
> plot(chow)
> bp_ts <- breakpoints(Bitcoin ~ 1)
> bp_ts

Optimal 5-segment partition: 
  
  Call:
  breakpoints.formula(formula = Bitcoin ~ 1)

Breakpoints at observation number:
  218 350 448 546 

Corresponding to breakdates:
  2017(218) 2017(350) 2018(83) 2018(181) 
> summary(bp_ts)

Optimal (m+1)-segment partition: 
  
  Call:
  breakpoints.formula(formula = Bitcoin ~ 1)

Breakpoints at observation number:
  
m = 1           320        
m = 2           350 448    
m = 3       218 350 448    
m = 4       218 350 448 546
m = 5   120 218 350 448 546

Corresponding to breakdates:
  
m = 1                       2017(320)                   
m = 2                       2017(350) 2018(83)          
m = 3             2017(218) 2017(350) 2018(83)          
m = 4             2017(218) 2017(350) 2018(83) 2018(181)
m = 5   2017(120) 2017(218) 2017(350) 2018(83) 2018(181)

Fit:
  
  m   0      1      2      3      4      5     
RSS 934466 550191 254893 183606 125284 124296
BIC   6629   6295   5804   5602   5365   5373
> plot(Bitcoin)
> lines(bp_ts)
> ci_ts <- confint(bp_ts)
> ci_ts

Confidence intervals for breakpoints
of optimal 5-segment partition: 
  
  Call:
  confint.breakpointsfull(object = bp_ts)

Breakpoints at observation number:
  2.5 % breakpoints 97.5 %
  1   217         218    219
2   348         350    351
3   447         448    453
4   545         546    549

Corresponding to breakdates:
  2.5 %     breakpoints 97.5 %   
  1 2017(217) 2017(218)   2017(219)
2 2017(348) 2017(350)   2017(351)
3 2018(82)  2018(83)    2018(88) 
4 2018(180) 2018(181)   2018(184)
> fm0 <- lm(Bitcoin ~ 1)
> fm1 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 1))
> fm2 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 2))
> plot(Bitcoin)
> lines(ts(fitted(fm0), start = 2017, frequency=365), col = 3)
> lines(ts(fitted(fm1), start = 2017, frequency=365), col = 4)
> lines(ts(fitted(fm2), start = 2017, frequency=365), col = 1)
> lines(bp_ts)
> fm0 <- lm(Bitcoin ~ 1)
> fm1 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 1))
> fm2 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 2))
> fm3 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 3))
> fm4 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 4))
> plot(Bitcoin)
> lines(ts(fitted(fm0), start = 2017, frequency=365), col = 3)
> lines(ts(fitted(fm1), start = 2017, frequency=365), col = 4)
> lines(ts(fitted(fm2), start = 2017, frequency=365), col = 1)
> lines(ts(fitted(fm3), start = 2017, frequency=365), col = 2)
> lines(ts(fitted(fm4), start = 2017, frequency=365), col = 5)
> lines(bp_ts)
> MIO1 <- diff(Bitcoin)
> plot(MIO1)