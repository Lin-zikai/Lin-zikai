library(data.table)

library(grid)
library(forestploter)
library(gridExtra)


#cell----
a <- fread("forest/cell_2.csv")
a$' ' <- paste(rep(" ",20),collapse = "  ")
a$Exposure <- paste0(a$Exposure,"            ")
a$nSNP <- paste0(a$nSNP,"            ")
a$`OR(95%CI)` <- paste0(a$`OR(95%CI)`,"            ")
tm <- forest_theme(base_size = 17,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   core=list(bg_params=list(fill = c("white", "#dae3f3"))))

p <-
  forest(a[,c(1:3,8,4)],
         est = a$OR,       #效应值
         lower = a$Low,     #可信区间下限
         upper = a$High,      #可信区间上限
         # sizes = dt$se,     #黑框的大小
         ci_column = 4,   #在那一列画森林图，要选空的那一列
         ref_line = 1,
         xlim = c(0, 4),
         nudge_y = 0.1,
         ticks_at = c(0.5, 1, 2, 3),
         theme = tm)





# Assume g is your final plot
# Use this to get the heights of each row to inform the choice of the height
convertHeight(p$heights, "mm", valueOnly = TRUE) 
# Change the height with some reasonable value
p$heights <- rep(unit(12, "mm"), nrow(p)) #You can assign different value for each row if you want


convertWidth(p$widths, "mm", valueOnly = TRUE)
new_col_widths <- c(5, 70, 40, 80, 120, 20, 5)
p$widths <- unit(new_col_widths, "mm")
p



#gene------

a <- fread("forest/gene.csv")
a$' ' <- paste(rep(" ",20),collapse = "  ")
a$Exposure <- paste0(a$Exposure,"            ")
a$nSNP <- paste0(a$nSNP,"            ")
a$`OR(95%CI)` <- paste0(a$`OR(95%CI)`,"            ")
tm <- forest_theme(base_size = 17,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   core=list(bg_params=list(fill = c("white", "#dae3f3"))))
a$OR <- log10(a$OR+1)
a$Low <- log10(a$Low+1)
a$High <- log10(a$High+1)


p2 <-
  forest(a[,c(1:3,8,4)],
         est = a$OR,       #效应值
         lower = a$Low,     #可信区间下限
         upper = a$High,      #可信区间上限
         # sizes = dt$se,     #黑框的大小
         ci_column = 4,   #在那一列画森林图，要选空的那一列
         ref_line = log10(2),
         x_trans = "none",
         xlim = c(0, 4),
         nudge_y = 0.1,
         ticks_at = c(log10(2), log10(11), log10(101), log10(1001)),
         theme = tm)

# Assume g is your final plot
# Use this to get the heights of each row to inform the choice of the height
convertHeight(p2$heights, "mm", valueOnly = TRUE) 
# Change the height with some reasonable value
p2$heights <- rep(unit(12, "mm"), nrow(p2)) #You can assign different value for each row if you want
p2

convertWidth(p2$widths, "mm", valueOnly = TRUE)
new_col_widths <- c(5, 70, 40, 80, 120, 20, 5)
p2$widths <- unit(new_col_widths, "mm")
p2

#lipid--------

a <- fread("forest/xuezhi.csv")
a$' ' <- paste(rep(" ",20),collapse = "  ")
a$Exposure <- paste0(a$Exposure,"            ")
a$nSNP <- paste0(a$nSNP,"            ")
a$`OR(95%CI)` <- paste0(a$`OR(95%CI)`,"            ")
tm <- forest_theme(base_size = 17,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   core=list(bg_params=list(fill = c("white", "#dae3f3"))))

p1 <-
  forest(a[,c(1:3,8,4)],
         est = a$OR,       #效应值
         lower = a$Low,     #可信区间下限
         upper = a$High,      #可信区间上限
         # sizes = dt$se,     #黑框的大小
         ci_column = 4,   #在那一列画森林图，要选空的那一列
         ref_line = 1,
         xlim = c(0, 10),
         nudge_y = 0.1,
         ticks_at = c(0.5, 1, 5, 10),
         theme = tm)
# Assume g is your final plot
# Use this to get the heights of each row to inform the choice of the height
convertHeight(p1$heights, "mm", valueOnly = TRUE) 
# Change the height with some reasonable value
p1$heights <- rep(unit(12, "mm"), nrow(p1)) #You can assign different value for each row if you want
p1

convertWidth(p1$widths, "mm", valueOnly = TRUE)
new_col_widths <- c(5, 70, 40, 80, 120, 20, 5)
p1$widths <- unit(new_col_widths, "mm")
p1

p
p1
p2
