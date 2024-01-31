
attach(data)
colnames(data)
library(cutpointr)
library(pROC)

#------------------------------------------------------------------------------------
library(dplyr)

data%>%filter(Locationofcardiacarrest=="IHCA")->df
df$Status=ifelse(df$Survivaltohospitaldischarge=="Survivors",0,1)
attach(df)

# Assuming your dataset is named 'your_data'
fit <- survfit(Surv(ROSC_minutes, Status) ~ 1, data =df)

# Specify the 'data' argument in ggsurvplot
ggsurvplot(
  fit,
  data =df,  # Add this line to specify the dataset
  conf.int = TRUE,
  conf.int.style = "step",  # Use step function for confidence intervals
  conf.int.draw = TRUE,     # Draw the confidence interval lines
  color="red",
  xlab = "Time (in minutes)",
  ylab = "Cumulative survival probability",
  #title = "Overall Survival",
  risk.table = TRUE,
  #pval = TRUE,
  break.x.by = 5,
  surv.median.line = "hv",
  legend.title = "",
  legend.labs ="",
  risk.table.title="Number at risk"
)



###############################################

#-----------------------------------------------------------------------------
# ROSC time score

cp<-cutpointr(df,ROSC_minutes,Survivaltohospitaldischarge, method = maximize_metric, metric=youden, 
              pos_class="Survivors",na.rm=TRUE)
summary(cp)

abc<- roc(Survivaltohospitaldischarge~ROSC_minutes,data=df)
auc(abc)
ci.auc(abc)


# Plotting ROC curve with reversed x-axis and modified labels
plot(1 - abc$specificities, abc$sensitivities,
     type = "l", lwd = 2, col = "blue",
     xlab = "1 - Specificity", ylab = "Sensitivity",
     xlim = c(0, 1), ylim = c(0, 1),
     main = "ROC Curve using ROSC time in minutes for IHCA")

# Adding a diagonal reference line
abline(a = 0, b = 1, col = "black", lty = 2,lwd=2)

# Adding legend
legend("bottomright", legend = paste("AUC =", round(auc(abc), 3)),
       col = "black", lty = 1, cex = 1)

# Adding a grid for clarity
grid()





