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
