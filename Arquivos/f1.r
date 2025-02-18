library(foreign)

base <- read.spss("BirthWeight.sav", to.data.frame=TRUE)

table(base$LOW, base$RACE)
chisq.test(table(base$LOW, base$RACE))
table(base$LOW, base$SMOKE)
chisq.test(table(base$LOW, base$SMOKE))
table(base$LOW, base$PTL2)
chisq.test(table(base$LOW, base$PTL2))
table(base$LOW, base$FTV2)
chisq.test(table(base$LOW, base$FTV2))
fisher.test(table(base$LOW, base$FTV2))
table(base$LOW, base$HT)
chisq.test(table(base$LOW, base$HT))
fisher.test(table(base$LOW, base$HT))
table(base$LOW, base$UI)
chisq.test(table(base$LOW, base$UI))

mod1 <- glm(LOW ~ AGE + I(as.factor(RACE)) + I(as.factor(SMOKE)) 
                  + I(as.factor(PTL2)) + I(as.factor(HT)) 
                  + I(as.factor(UI)) + I(as.factor(FTV2)),
            family=binomial,
		data=base)
summary(mod1)

