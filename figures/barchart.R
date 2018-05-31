
df = read.table("gem_tStatMeans", header = FALSE, sep = "\t");

df = df[order(df[,3], decreasing=T),]

rbPal = colorRampPalette(c('red','blue'))

df$col = rbPal(10)[as.numeric(cut(df[,3],breaks = 10))]

barplot(df[,3], col = df$col, names.arg=df[,1], cex.names=1, las=2, ylim=c(-1.5,1.5), ylab="Average t-statistic")

