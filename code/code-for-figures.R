# lineup for airline
xx <- seq(0, 360, by=1)
yy <- rep(0.5, length=length(xx))
refdf <- data.frame(xx=xx, yy=yy)

ggplot(flights_sample, aes(x = (WindDirDegrees-1 + X)%%360, fill = factor(nf, levels = c(0:8)))) +
geom_bar(binwidth = 10, position="fill") +
scale_x_continuous(limits = c(0, 360), breaks=seq(0, 360, by=45), labels=c("N", "NE","E","SE","S","SW","W","NW","N")) +
coord_polar(start = 0) + geom_line(aes(x=xx, y=yy), size = .5, alpha=0.5, colour = "white", inherit.aes=FALSE, data=refdf) + opts(legend.position="none", plot.margin=unit(c(0,0,0,0), "cm")) +
seq7rev + labs(x = " ", y = " ") 
ggsave(file="polar_line.pdf", width=4, height=4, dpi=75)


ggplot(flights_sample, aes(x = (WindDirDegrees-1 + X)%%360, fill = factor(nf, levels = c(0:8)))) +
geom_bar(binwidth = 10, position="fill") +
scale_x_continuous(limits = c(0, 360), breaks=seq(0, 360, by=45), labels=c("N", "NE","E","SE","S","SW","W","NW","N")) +
coord_polar(start = 0) + opts(legend.position="none", plot.margin=unit(c(0,0,0,0), "cm")) +
seq7rev + labs(x = " ", y = " ") 
ggsave(file="polar_noline.pdf", width=4, height=4, dpi=75)


ggplot(flights_sample, aes(x = (WindDirDegrees-1 + X)%%360, fill = factor(nf, levels = c(0:8)))) +
geom_bar(binwidth = 10, position="fill", alpha=0.8) +
scale_x_continuous(limits = c(0, 360), breaks=seq(0, 360, by=45), labels=c("N", "NE","E","SE","S","SW","W","NW","N")) + geom_hline(yintercept=0.5, size = .5, alpha=0.5, colour = "white") + opts(legend.position="none", plot.margin=unit(c(0,0,0,0), "cm")) +
seq7rev + labs(x = " ", y = " ") 

ggsave(file="euclid_line.pdf", width=4, height=4, dpi=75)


ggplot(flights_sample, aes(x = (WindDirDegrees-1 + X)%%360, fill = factor(nf, levels = c(0:8)))) +
geom_bar(binwidth = 10, position="fill", alpha=0.8) +
scale_x_continuous(limits = c(0, 360), breaks=seq(0, 360, by=45), labels=c("N", "NE","E","SE","S","SW","W","NW","N")) + opts(legend.position="none", plot.margin=unit(c(0,0,0,0), "cm")) +
seq7rev + labs(x = " ", y = " ") 

ggsave(file="euclid_noline.pdf", width=4, height=4, dpi=75)






cors <- replicate(1000,
{
	x = rexp(100)
	y = rexp(100)
	cor(x,y)
}
)

qplot(geom="histogram", cors)

x <- rexp(100)
y <- rexp(100)
cor(x,y)
qplot(x,y)
ggsave("rexp.pdf", height=3, width=3)

x <- factor(rbinom(100,1,prob=0.1))
y <- rexp(100, rate=3)
t.test(y~x)
#y <- rbeta(100, shape1=0.25, shape2=0.25)
qplot(x,y, geom="boxplot")
ggsave("two-exp.pdf", height=3, width=3)
write.csv(data.frame(x=x, y=y), file="two-exp.csv", row.names=F)
pvals <- replicate(1000,
{
x <- factor(rbinom(100,1,prob=0.1))
y <- rexp(100, rate=3)
t.test(log(y)~x)$p.value
}
)
qplot(pvals, geom="histogram", binwidth=0.05)


x <- c(rnorm(50, mean=3), rnorm(50, mean=-3))
y <- c(rnorm(50, mean=3), rnorm(50, mean=-3))[sample(100,100, FALSE)]
qplot(x,y)
ggsave("rnorm.pdf", height=3, width=3)


x <- rep(1:7, c(1,2,3,4,3,2,1))





####################
# turk 5 pictures
# conc data:
conc <- read.csv("/Users/heike/Dropbox/Lineups-nf/turk5/data/turk5_1_15_5_12_16.csv")

sample <- subset(conc, .sample==16)
t.test(values~group, data=sample)

lineup.ops <- opts(legend.position="none", axis.ticks = theme_blank(), #axis.text.y = theme_blank(), 
axis.text.x = theme_blank(), plot.margin=unit(c(0,0,0,0), "cm"))

ggplot(aes(x=values,   fill=factor(group)), data=sample) + geom_histogram(binwidth=25, alpha=0.5)+ xlab("")  + ylab("") + lineup.ops
ggsave("hist-conc.pdf")

alpha=0.5
ggplot(aes(x=factor(group), y=values, fill=factor(group), colour=factor(group)), data=sample)   + geom_boxplot(alpha=alpha) + coord_flip() + lineup.ops + xlab("")  + ylab("") 
ggsave("boxplot-conc.pdf")


ggplot(aes(x=values,  colour=factor(group), fill=factor(group)), data=sample) + geom_density(alpha=alpha)+ lineup.ops + xlab("")  + ylab("") 
ggsave("density-conc.pdf")

ggplot(aes(x=values, y=factor(group), colour=factor(group)), data=sample)  + geom_jitter(position=position_jitter(height=0.1, width=0), size=4, alpha=alpha)+ lineup.ops + xlab("")  + ylab("") 
ggsave("dotplot-conc.pdf")

conc.res <- subturk[grep("_15_15_1_", as.character(subturk$pic_name)),]

stats <- ddply(conc.res, .(n1,n2,d, rep), summarize,
	correct=sum(response),
	n=length(param_value),
	difficulty=mean(difficulty),
	pval=mean(p_value)
)
qplot(interaction(n1,n2,d),correct/n, data=stats) + coord_flip()

conc.res <- subset(subturk, n1==15 & n2==15 & d==1)
ddply(conc.res, .(n1,n2,d,test_param), summarize,
	correct=sum(response),
	n=length(param_value),
	difficulty=mean(difficulty),
	pval=mean(p_value)
)


xtabs(~ pic_name + responsedata=conc.res)