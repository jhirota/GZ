library(glm)

# Not completed
# Poisson regression ------

DATAfinal$q <- DATAfinal$cumGZ * DATAfinal$pop

psr <- glm(nofcases ~ cumGZ + mobility + nofcases_lm,
           family = poisson(link = "log"), #link = "identity"?
           data = mondata100)

summary(psr)

exp(coef(psr))

r1 <- lm(scaled_cases14 ~ lnGZ, 
         weights = lmobility,
         data = DATAfinal)
summary(r1)

r2 <- lm(scaled_cases14 ~ lnGZ,
         data = DATAfinal)

plot(DATAfinal$lnGZ, DATAfinal$scaled_cases14)
abline(r1, lty = 3)
abline(a = psr$coef["lmobility"], b = psr$coef["lnGZ"])
abline(r2, lty = 3)


glmnb <- MASS::glm.nb(scaled_cases14 ~ lnGZ*lmobility,
                      data = DATAfinal)

summary(glmnb)


poisr1 <- glm(scaled_cases14 ~ lnGZ*lmobility + factor(city) + factor(date), 
              data = DATAfinal,
              family = poisson)

summary(poisr1)

p <- predict(psr, cumGZ = NA)
plot(p, fun = plogis)

var(predict(psr))
fitted(psr)


hist(x = DATAfinal$nofcases14,
     breaks = seq(from = 0, to = 20, by = 0.01))


m <- mean(cleanmondata$nofcases)
v <- var(cleanmondata$nofcases) 
v / m # over dispersion?

lm <- lm(nofcases ~ lmobility, data = cleanmondata)
summary(lm)

