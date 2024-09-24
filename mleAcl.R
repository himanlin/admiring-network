

aclw2

library(lme4)
library(lmerTest)

intercept.only.model <- lmer(acl_indegree.y ~ 1+gender.y+grade_pr.y+class_cadre.y + (1 | wave), data = aclw2, REML = TRUE)
#df$intercept.only.preds <- predict(intercept.only.model)
summary(intercept.only.model)


fit1 <- lmer(wt_page.y ~  ccl_pagerank.y+gender.y+class_cadre.y +
               par_edu.y+kind3.to + talent1.to +teac.to+
               gender.x+gender.x*gender.y+class_cadre.x+kind3.from +
               talent1.from+hgrade+wave+
               (1|school),
            data = aclw2c, REML = TRUE)
summary(fit1)


fit3 <- lmer(wt_page.y ~  familiar.y++gender.y+class_cadre.y +
               par_edu.y+kind3.to + talent1.to +teac.to+
               gender.x+gender.x*gender.y+class_cadre.x+kind3.from +
               talent1.from+hgrade+wave+
               (1|school),
             data = aclw2c, REML = TRUE)
summary(fit3)


fit4 <- lmer(wt_page.y ~  favorability.y+gender.y+class_cadre.y +
               par_edu.y+kind3.to + talent1.to +teac.to+
               gender.x+gender.x*gender.y+class_cadre.x+kind3.from +
               talent1.from+hgrade+wave+
               (1|school),
             data = aclw2c, REML = TRUE)
summary(fit4)


fit2 <- lmer(strength.y ~   ccl_indegree.y+gender.y+grade_pr.y+class_cadre.y +
               par_edu.y+kind1.to + talent1.to +
               gender.x+gender.x*gender.y+grade_pr.x+class_cadre.x+ kind1.from + talent1.from+wave+
               (1|school),
             data = aclw2, REML = TRUE)
summary(fit2)
summary(fit4)

fit5 <- lmer(strength.y ~  familiar.y+gender.y+grade_pr.y+class_cadre.y +
               par_edu.y+kind1.to + talent1.to +gender.x+gender.x*gender.y+grade_pr.x+class_cadre.x+
               kind1.from + talent1.from+
               (1|school)+ (1 | wave),
             data = aclw2, REML = TRUE)
summary(fit5)

fit6 <- lmer(strength.y ~  favorability.y+gender.y+grade_pr.y+class_cadre.y +
               par_edu.y+kind3.to + talent1.to +gender.x+gender.x*gender.y+grade_pr.x+class_cadre.x+
               kind.from + talent1.from+
               (1|school)+ (1 | wave),
             data = aclw2, REML = TRUE)
summary(fit6)




library(modelsummary)
modelsummary(c(fit1,fit2,fit3,fit4,fit5,fit6),"markdown")
library(texreg)
texreg(c(fit1,fit3,fit4,fit2,fit5,fit6))
