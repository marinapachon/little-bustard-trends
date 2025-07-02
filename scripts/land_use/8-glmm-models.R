library(lme4)
library(readxl)

tabla_glm_test<- read_excel("SIGPAC/outputs/all/glmm.xlsx")

glmm <- glmer(Abundancia ~ regadio + (1 | CUADRICULA), data = tabla_glm_test, family = poisson)

summary(glmm)

performance::r2(glmm)
