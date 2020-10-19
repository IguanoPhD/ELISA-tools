#Reference: Frey, A., Canzio, JD., Zurakowski. 1998. A statistically defined endpoint titer 
#          determination method for immunoassays. Journal of Immunological Methods. 221:35-41. 
#Copy the column containing the ODs of negative control in excel sheet (do not copy the heading).

ctrl_OD=read.table("clipboard")             
ctrl_OD
m=mean(ctrl_OD$V1)
s=sd(ctrl_OD$V1)
n=length(ctrl_OD$V1)
df=n-1
tcrit=qt(0.05, df=df, lower.tail = FALSE)  #95% confidence level
f=tcrit*(sqrt(1+(1/n)))
cutoff_value=m+(f*s)
cutoff_value

