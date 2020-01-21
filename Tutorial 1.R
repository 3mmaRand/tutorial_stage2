library(ggplot2)
#data input
response <- read.table("response.txt", skip = 6, header = TRUE)
str(response)
ggplot(data = response,
       aes(x = GSH, y = sens, col = genotype)) +
  geom_point() +
  xlim(0, 7) +
  ylim(0, 40) +
  geom_smooth(method = "lm", se =FALSE, fullrange = TRUE)

mod <- lm(data = response, sens ~ GSH * genotype)
summary(mod)

anova(mod)

mod_2  <- update(mod, .~. -GSH:genotype)

summary(mod_2)
