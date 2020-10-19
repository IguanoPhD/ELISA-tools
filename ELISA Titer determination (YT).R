# Copy the OD values of test sample (serially diluted)
# Modify the dilution as per method used in ELISA.

cutoff=0.8686231                   #Enter the cutoff value here
OD=read.table("clipboard")
OD
Dilutions=c(100,200,400,800,1600,3200,6400,12800,25600,51200,102400,204800)
Regression=lm(Dilutions~OD$V1)
log10_Regression=lm((log10(Dilutions))~OD$V1)
Reciprocal_Regression=lm((1/Dilutions)~OD$V1)
summary(Regression)
Titre_1=coef(Regression)[1] + (cutoff)*coef(Regression)[2]
Titre_1
#------------------------------------------------------------------------
#------------------------------------------------------------------------
summary(log10_Regression)
A=coef(log10_Regression)[1] + (cutoff)*coef(log10_Regression)[2]
Titre_2=10^A
Titre_2
#------------------------------------------------------------------------
#------------------------------------------------------------------------
summary(Reciprocal_Regression)
B=coef(Reciprocal_Regression)[1] + (cutoff)*coef(Reciprocal_Regression)[2]
Titre_3=1/B
Titre_3
