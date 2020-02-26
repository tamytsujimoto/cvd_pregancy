plot_km <- function(fit, legend=FALSE, color=NULL, main){
  
  if(!is.null(color)){
    plot(fit,
         las = 1, xlab='Time', bty='n', lwd=1.5, 
         pars=list(col=color, main=main))
    if(legend != FALSE)
      legend('bottomleft', legend=legend, 
             box.lty=0, lwd = 1.5, cex=1, col=color, bg='transparent')
  }
  else plot(fit, las = 1, xlab='Time', bty='n', lwd=1.5,
            main=main)
  
  
}


pdf(file = 'Output/km_cause_spec.pdf', width = 8, height = 8)
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)

# cvd_outcome -- all #
plot_km(svykm(Surv(time_exm,cvd_outcome)~1, design=subset(nhanes, flag_subpop == 1)),
        main="CVD outcome")

plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(pce_risk_cat), design=subset(nhanes, flag_subpop == 1)),
        legend=c("< 5%", "5%-7.5%", "7.5%-20%", "> 20%"),
        color=1:4,
        main="CVD outcome - PCE risk")

plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA")      

plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF")  

# cvd_outcome -- pce_risk_cat == 3 and 4#
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE 7.5%-20%")      

plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE 7.5%-20%")  

plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE > 20%")      

plot_km(svykm(Surv(time_exm,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE > 20%")  

# cvd_outcome2 #
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_km(svykm(Surv(time_exm,cvd_outcome2)~1, design=subset(nhanes, flag_subpop == 1)),
        main="CVD outcome + HTN/DM")

plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(pce_risk_cat), design=subset(nhanes, flag_subpop == 1)),
        legend=c("< 5%", "5%-7.5%", "7.5%-20%", "> 20%"),
        color=1:4,
        main="CVD outcome + HTN/DM - PCE risk")

plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome + HTN/DM - SGA")      

plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome + HTN/DM - BF")  

# cvd_outcome2 -- pce_risk_cat == 3 and 4#
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE 7.5%-20%")      

plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE 7.5%-20%")  

plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE > 20%")      

plot_km(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE > 20%")  

par(mfrow = c(1,1), mar = c(2, 4, 3, 1) + 0.1)
dev.off()

pdf(file = 'Output/km_comp_rsk.pdf', width = 8, height = 8)
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)

# cvd_outcome -- all #
plot_km(svykm(Surv(time_exm2,cvd_outcome)~1, design=subset(nhanes, flag_subpop == 1)),
        main="CVD outcome")

plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(pce_risk_cat), design=subset(nhanes, flag_subpop == 1)),
        legend=c("< 5%", "5%-7.5%", "7.5%-20%", "> 20%"),
        color=1:4,
        main="CVD outcome - PCE risk")

plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA")      

plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF")  

# cvd_outcome -- pce_risk_cat == 3 and 4#
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE 7.5%-20%")      

plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE 7.5%-20%")  

plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE > 20%")      

plot_km(svykm(Surv(time_exm2,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE > 20%")  

# cvd_outcome2 #
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_km(svykm(Surv(time_exm2,cvd_outcome2)~1, design=subset(nhanes, flag_subpop == 1)),
        main="CVD outcome + HTN/DM")

plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(pce_risk_cat), design=subset(nhanes, flag_subpop == 1)),
        legend=c("< 5%", "5%-7.5%", "7.5%-20%", "> 20%"),
        color=1:4,
        main="CVD outcome + HTN/DM - PCE risk")

plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome + HTN/DM - SGA")      

plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome + HTN/DM - BF")  

# cvd_outcome2 -- pce_risk_cat == 3 and 4#
par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE 7.5%-20%")      

plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 3)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE 7.5%-20%")  

plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No SGA", "SGA"),
        color=1:2,
        main="CVD outcome - SGA - PCE > 20%")      

plot_km(svykm(Surv(time_exm2,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1 & pce_risk_cat == 4)),
        legend=c("No BF", "BF"),
        color=1:2,
        main="CVD outcome - BF - PCE > 20%")  

par(mfrow = c(1,1), mar = c(2, 4, 3, 1) + 0.1)
dev.off()