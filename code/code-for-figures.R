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


x <- c(rnorm(50, mean=-3), rnorm(50, mean=3))

y <- c(rnorm(50, mean=3), rnorm(50, mean=-3))[sample(100,100, FALSE)]
qplot(x,y)
