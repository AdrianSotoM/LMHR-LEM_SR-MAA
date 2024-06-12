##### REPLY TO J GONZALEZ #####
### Use this code after having executed the main one as this one assumes some 
### objects and functions are already running.

# Creating a group variable in each IPD dataset.
ipdlc <- ipdlc %>% mutate(group="LC")
ipdhc <- ipdhc %>% mutate(group="HC")

# Merging LCD and LFD datasets.
ipd <- rbind(ipdlc,ipdhc)

# Interaction test.
m1 <- lm(ldlchange~blbmi*group,data=ipd)
summary(m1)
confint(m1)

# Assessing if LDL change is normaly distributed in our IPD.
shapiro.test(ipdlc$ldlchange)

# Filtering our RCTs with imputed SEs.
data2 <- data %>% filter(`repldlch?`=="yes")

# Meta-regression without RCTs with imputed SEs.
MAmod2<-rma(yi=ldldiffch, vi=ldldiffchse, 
            mods=~bmi+satfatg+chogr+wghtlosskg,
            data=data2,
            method="REML",
            weighted=TRUE, 
            level=95, 
            digits=1)
summary.rma(MAmod2)

# Meta-regression with baseline LDL as covariate.
MAmod3<-rma(yi=ldldiffch, vi=ldldiffchse, 
            mods=~bmi+blldlmgdl+satfatg+chogr,
            data=data,
            method="REML",
            weighted=TRUE, 
            level=95, 
            digits=1)
summary.rma(MAmod3)
