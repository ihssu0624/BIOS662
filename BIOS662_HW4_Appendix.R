##1.

HW4_PKU <- read_table2("~/Documents/GitHub/BIOS662/HW4_PKU.txt")
qqnorm(HW4_PKU$case)
qqnorm(HW4_PKU$sibling)

HW4_PKU$diff <- HW4_PKU$case-HW4_PKU$sibling
mean(HW4_PKU$diff)
sd(HW4_PKU$diff)

mean(HW4_PKU$diff)/(sd(HW4_PKU$diff)/sqrt(21))

t.test(HW4_PKU$diff)

qt(0.025,20)
qt(0.975,20)

mean(HW4_PKU$diff)+qt(0.025,20)*(sd(HW4_PKU$diff)/sqrt(21))
mean(HW4_PKU$diff)+qt(0.975,20)*(sd(HW4_PKU$diff)/sqrt(21))

cumprob <- c(
  pbinom(0,21,0.5),
  pbinom(1,21,0.5),
  pbinom(2,21,0.5),
  pbinom(3,21,0.5),
  pbinom(4,21,0.5),
  pbinom(5,21,0.5),
  pbinom(6,21,0.5),
  pbinom(7,21,0.5),
  pbinom(8,21,0.5),
  pbinom(9,21,0.5),
  pbinom(10,21,0.5),
  pbinom(11,21,0.5),
  pbinom(12,21,0.5),
  pbinom(13,21,0.5),
  pbinom(14,21,0.5),
  pbinom(15,21,0.5),
  pbinom(16,21,0.5),
  pbinom(17,21,0.5),
  pbinom(18,21,0.5),
  pbinom(19,21,0.5),
  pbinom(20,21,0.5),
  pbinom(21,21,0.5)
)
cum_prob_df <- data.frame(
  r=seq(0:21),
  cumprob=cumprob
)

View(cum_prob_df)
2*pbinom(6,21,0.5)

HW4_PKU %>% arrange(diff)

2*pbinom(7,21,0.5)

##2.: two sample t-test p.29 row 4
##pretty normal
##sample small
#Welch-Satterthwaite Approximation
hypertensive <- c(1100,1320,1350,1450,1600,1850,1900,1990,2050,2120,2200,2210,2500,2610,2700)
qqnorm(hypertensive)
histogram(hypertensive)
mean(hypertensive)

normal <- c(1000,1220,1300,1400,1555,1600,1780,1780,1900,2020,2350,2375)
qqnorm(normal)
histogram(normal)
mean(normal)

W <-1+3+4+7+9+10.5+12.5+12.5+15.5+18+23+24
e_W <-6*(12+16)
var_W <-((12*15*(12+15+1))/12) - (3*(2*1*3))* (12*15/(12*(12+15)*(12+15-1)))
Z <-(W-e_W)/(sqrt(var_W))
Z
pvalue2sided=2*pnorm(-abs(Z))
pvalue2sided

