# hw 6
# codes
# lanqiu yao

# read in data
setwd('/Users/yaolanqiu/Desktop/NYU/intro to survival/HW/HW6')
kidney = read.table('KidneyTransplant00.txt', skip =11)
colnames(kidney) = c('obs','time','status','sex','race','age')
library(survival)

### question 1

# 1a
fit1a = survreg(Surv(time, status) ~ race + sex + age, kidney, dist="exponential")
beta1a = fit1a$coefficients['race']

summary(fit1a)$table[2,'p']

# 1b
paste('(',round(exp(-confint(fit1a, level=.95)['race',2]),3), ", "
      ,round(exp(-confint(fit1a, level=.95)['race',1]),3),')',sep='')

# Q2

# 1a
fit2a = survreg(Surv(time, status) ~ race + sex + age, kidney, dist="weibull")
summary(fit2a)
summary(fit2a)$table[2,'p']

# 1b
beta2a = fit2a$coefficients
beta2a

exp(beta2a[2] * -1 * 1/fit2a$scale)



# 4

fit_exp1 = survreg(Surv(time, status) ~ race + sex + age, kidney, dist="exponential")
fit_wei1 = survreg(Surv(time, status) ~ race + sex + age, kidney, dist="weibull")

fit_exp0 = survreg(Surv(time, status) ~ sex + age, kidney, dist="exponential")
fit_wei0 = survreg(Surv(time, status) ~ sex + age , kidney, dist="weibull")

fit_exp = survreg(Surv(time, status) ~ race, kidney, dist="exponential")
fit_wei = survreg(Surv(time, status) ~ race, kidney, dist="weibull")

anova(fit_exp0,fit_exp1)
anova(fit_wei0,fit_wei1)