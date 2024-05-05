library(RColorBrewer)
library(latex2exp)

general_filename = 'test_mse_33_pimp60_px1365_px219883_px315629_sigma25_factorstr2_factorx1_2_factorx2_2_factorx3_2_ustd1_factor2_SNR0_mode_comb.rds'

test_filename = paste0("/Users/bhavnasharma/Downloads/", general_filename)
test_mse_df = readRDS(test_filename)

diff_filename = paste0("/Users/bhavnasharma/Downloads/diff_", general_filename)
lasso_diff_df = readRDS(diff_filename)

support_filename = paste0("/Users/bhavnasharma/Downloads/support", substring(general_filename,9))
support_df = readRDS(support_filename)

alpha_filename =  paste0("/Users/bhavnasharma/Downloads/alpha", substring(general_filename,9))
alpha_df = readRDS(alpha_filename)

simN = 10
alphalist = c(0,0.2,0.4,0.6,0.8,1,3,5,9)
alpha_axis = sapply(as.character(round(alphalist,1)),
         function(x) paste0("(alpha=",x,")"))
color = c(rep("white",5), "#B6D7A8", "#83B9E3")

plot_name = paste0("/Users/bhavnasharma/Downloads/color_plot", substring(general_filename,9,nchar(general_filename)-3), "pdf")
pdf(file=plot_name, width=18, height=5)
par(mfrow=c(1,4), las=2, cex.main=2.33, cex.axis = 2.15, cex.lab=1.95, mar=c(10.3,6.6,6.1,0.9)) #, srt=45)
p1 = boxplot(unlist(cbind(test_mse_df[,1:7]))
        ~sort(rep(1:7, simN)), 
        ylim=c(-10000,5000000),
        col=color,
        names=c("Separate X1","Separate X2", "Separate X3", "Early fusion", "Late fusion", "Coop", "Adap Coop"), 
        xlab="",ylab="", xaxt = "n")#, main="Test MSE")
tick = seq_along(p1$names)
axis(1, at = tick, labels = F)
text(tick-0.6, par("usr")[3]-0.2*(par("usr")[4]-par("usr")[3])-9, p1$names, srt = 45, xpd = T, cex=2.0)
title(main="Test MSE", line=1.8, cex.lab=2.75)

boxplot(unlist(cbind(support_df[,1:7]))~
          sort(rep(1:7, simN)),
        #ylim=c(0,200),
        col=color,
        names=c("Separate X1","Separate X2", "Separate X3", "Early fusion", "Late fusion", "Coop", "Adap Coop"), 
        xlab="", ylab="", xaxt = "n")
tick = seq_along(p1$names)
axis(1, at = tick, labels = F)
text(tick-0.6, par("usr")[3]-0.05*(par("usr")[4]-par("usr")[3])-9, p1$names, srt = 45, xpd = T, cex=2.0)
title(main="Number of Features Selected", line=1.8, cex.lab=2.75)

counts = sapply(alphalist, function(x) sum(alpha_df$alpha_by_cv_no_pf == x))
counts_ada = sapply(alphalist, function(x) sum(alpha_df$alpha_by_cv == x))
barplot(rbind(counts,counts_ada),
        beside=TRUE,
        main="",
        names=c(as.character(round(alphalist,1))),
        ylim=c(0,10), cex.lab=2.0, 
        density=c(66,66),
        angle=c(36,36),
        col=c("#B6D7A8", "#83B9E3"))
legend("topleft", c("Coop","Adap Coop"), 
       cex=2.33, 
       pch=15,
       bty = "n",
       col=c("#B6D7A8", "#83B9E3"))

title(main=expression(bold(paste("Selected ", rho, " by CV (Counts)"))), line=2.2, cex.lab=2.75)
mtext(TeX(sprintf("$\\rho$")), side=1, las=1, line=6.3, cex=1.72)
dev.off()

