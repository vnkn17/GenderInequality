#Abortion/Ratio Plot
ratio = Abortion.vs.ratio$Female.Male.Ratio.2005.
abortion = Abortion.vs.ratio$Abortion
model <- lm(ratio~abortion,data = Abortion.vs.ratio)
plot(abortion,ratio)

DevCountry = Stat.139.Project_Updated$Developing.Country.
Ratiofull = Stat.139.Project_Updated$Ratio.of.Births

# T Test
DevelopedRatio = Ratiofull[DevCountry==0]
DevelopingRatio = Ratiofull[DevCountry==1]
t.test(DevelopedRatio,DevelopingRatio)

#boxplot visual
boxplot(Ratiofull~DevCountry,data = Stat.139.Project,xlab = "Developed Country?(0 = Yes, 1 = No)",ylab = "Gender Ratio")