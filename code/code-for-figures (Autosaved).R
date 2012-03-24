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
