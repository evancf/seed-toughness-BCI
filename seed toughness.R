# Packages ---------------------------------------------------------------------

library("lme4")


# Load data --------------------------------------------------------------------

seed.dat <- read.csv("seed.dat.csv", header = T)


# Analysis ---------------------------------------------------------------------

# Density vs mass

density.mod0 <- lmer(density.gcm3 ~ 1 + (1|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
density.mod1 <- lmer(density.gcm3 ~ log.mass + (1|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
AIC(density.mod0,density.mod1)
summary(density.mod0)
summary(density.mod1)
anova(density.mod0,density.mod1)

density.coefs <- coef(density.mod1)$sp.type.combo
colnames(density.coefs) <- c("density.int","density.slope")


# Toughness vs mass
tough.mod0 <- lmer(log.toughness ~ 1 + (1|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
tough.mod1 <- lmer(log.toughness ~ log.mass + (1|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
AIC(tough.mod0,tough.mod1)
summary(tough.mod0)
summary(tough.mod1)
anova(tough.mod0,tough.mod1)

tough.coefs <- coef(tough.mod1)$sp.type.combo
colnames(tough.coefs) <- c("tough.int","tough.slope")


# Peak force vs mass
peak.mod0 <- lmer(log.peak.force ~ 1 + (1|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
peak.mod1 <- lmer(log.peak.force ~ log.mass + (1|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
AIC(peak.mod0,peak.mod1)
summary(peak.mod0)
summary(peak.mod1)

peak.coefs <- coef(peak.mod1)$sp.type.combo
colnames(peak.coefs) <- c("peak.int","peak.slope")


# Toughness vs mass and density
td.mod0 <- lmer(log.toughness ~ 1 + (1|sp.type.combo) + (0+density.gcm3|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
td.mod1 <- lmer(log.toughness ~ density.gcm3 + (1|sp.type.combo) + (0+density.gcm3|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
td.mod2 <- lmer(log.toughness ~ log.mass + (1|sp.type.combo) + (0+density.gcm3|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
td.mod3 <- lmer(log.toughness ~ density.gcm3 + log.mass + (1|sp.type.combo) + (0+density.gcm3|sp.type.combo) + (0+log.mass|sp.type.combo), data=seed.dat)
AIC(td.mod0,td.mod1,td.mod2,td.mod3)
summary(td.mod0)
summary(td.mod1)
summary(td.mod2)
summary(td.mod3)