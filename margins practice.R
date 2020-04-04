mtcars
write.csv(mtcars, "mtcars.csv")

library(margins)

# 1: SAME

x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
(m <- margins(x))

summary(m)

plot(m)

# 2: SAME

x <- lm(mpg ~ cyl + hp * wt, data = mtcars)

margins(x, at = list(cyl = mean(mtcars$cyl, na.rm = T)))


# FINAL SUB MODEL

# Stata: xtmixed mpg wt i.carb || cyl:

# R: t4 <- lmer(mpg ~ wt + as.factor(carb) + (1 | cyl), data = mtcars, REML = FALSE)

# FINAL MODEL:



# STATA: margins, at((mean) _all H_citytract_multi_i=(.23 .54))




################################## regressions in R v. stata ###############################3


## model 1: exactly the same

# stata : reg mpg cyl hp wt

t1 <- lm(mpg ~ cyl + hp + wt, data = mtcars)

summary(t1)


## model 2: as.factor is the same thing as doing i. in stata!!!!

# stata: reg mpg i.cyl hp wt

t2 <- lm(mpg ~ as.factor(cyl) + hp + wt, data = mtcars)

summary(t2)


## model 3: THIS IS THE SAME

# stata: xtmixed mpg wt || geo_id2:

t3 <- lmer(mpg ~ wt + (1 | cyl), data = mtcars, REML = FALSE)

summary(t3)

## model 3: THIS IS THE SAME

# stata: xtmixed mpg wt || cyl:

t3 <- lmer(mpg ~ wt + (1 | cyl), data = mtcars, REML = FALSE)

summary(t3)

## model 4: THIS IS THE SAME

# stata xtmixed mpg wt i.carb || cyl:

t4 <- lmer(mpg ~ wt + as.factor(carb) + (1 | cyl), data = mtcars, REML = FALSE)

summary(t4)


# FINAL MODEL:

# xtmixed biggestsplit H_citytract_multi_i diversityinterp pctasianpopinterp
# pctblkpopinterp pctlatinopopinterp medincinterp pctrentersinterp
# pctcollegegradinterp biracial nonpartisan primary logpop i. year south midwest
# west if winner==1||geo_id2:

m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + 
             pctasianpopinterp + pctblkpopinterp + pctlatinopopinterp + 
             medincinterp + pctrentersinterp +  pctcollegegradinterp + 
             biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), 
              data = rp_1, REML=FALSE)


  
