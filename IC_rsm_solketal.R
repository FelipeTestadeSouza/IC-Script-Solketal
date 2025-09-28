#Pacotes:
library(readxl)
library(rsm)

#Dados:
data <- read_excel("Data.xlsx",2)
data

#Response Surface Method:
#------ Verificação da adequação do modelo (Dados completos):
#Y = A0 + A1x1 + A2x2 + e -> Modelo linear
mod_l <- rsm(Conversao ~ FO(x1, x2), data = data)
summary(mod_l)

#Y = A0 + A1x1 + A2x2 + A12x1x2 + A11x1^2 + A22x2^2 + e -> Modelo Quadratico - Original/Codificada
mod_quad_cod <- rsm(Conversao ~ SO(x1, x2), data = data)
summary(mod_quad_cod)

#Análise de resíduo:
#lm -> Outra função para a análise
fit <- lm(Conversao ~ x1+x2+(x1*x2)+I(x1^2)+I(x2^2), data = data)
summary(fit)

#Residuo estudentizado
res <- rstudent(fit)

#Homogeneidade de variâncias
lmtest::bptest(fit)

#QQplot:
car::qqPlot(res,
            xlab = "Quantis Normalizados",
            ylab = "Resíduos")

#------ Análise do modelo final:
#Modelo final (dados sem outliers):
#Y = A0 + A1x1 + A2x2 + A11x1^2 + A22x2^2 + e -> Modelo Quadratico - Sem interação
mod_quad_si <- rsm(Conversao ~ SO(x1, x2) - TWI(x1,x2), data = data)
summary(mod_quad_si)

fit_final <- lm(Conversao ~ x1+x2+I(x1^2)+I(x2^2), data = data)
summary(fit_final)

#Contour plot - 2D:
par(mfrow=c(1,1))
contour.lm(mod_quad_cod, ~ x1+x2,
           image = TRUE,
           bounds = list(x1=c(-1.5,1.5),x2=c(-1.5,1.5)),
           xlabs = c("Razão Molar (x1) - Codificado", "Catalisador (x2) - Codificado"))

#Perspective plot - 3D:
persp.lm(mod_quad_cod, ~x1+x2,
         contours = "col", 
         col = heat.colors(100),
         bounds = list(x1=c(-1.5,1.5),x2=c(-1.5,1.5)),
         zlab = "Conversão (decimal)",
         xlabs = c("Catalisador (%) - codificado", "Razão Molar (acetona:glicerol) - codificado"))
