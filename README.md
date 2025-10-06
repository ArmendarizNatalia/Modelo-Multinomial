# Modelo-Multinomial
# ==========================================================
# Multinomial logit con mi base (solo: sexo, zona, grupo_edad, outcome, n)
# outcome {no_votaron, si_votaron, votos_nulos}
# ==========================================================

# (1) Librerías
# ----------------------------------------------------------
# install.packages("nnet")
# install.packages("marginaleffects")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("readxl")
# install.packages("ggplot2")

library(nnet)
library(marginaleffects)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

# (2) Cargar datos 
datos <- read_excel("gto_2024_multinom_full.xlsx")

# (3) Validaciones rápidas
# ----------------------------------------------------------
glimpse(datos)
stopifnot(all(c("sexo","zona","grupo_edad","outcome","n") %in% names(datos)))
stopifnot(is.numeric(datos$n))

# Asegurar tipos factor y referencia de la DV
datos <- datos |>
  mutate(
    outcome = factor(outcome, levels = c("no_votaron","si_votaron","votos_nulos")),
    sexo    = factor(sexo),           # "H","M"
    zona    = factor(zona),           # "Rural","Urbana"
    grupo_edad = factor(grupo_edad,   # "18-19","20-24",...,"100+"
                        ordered = TRUE,
                        levels = c("18-19","20-24","25-29","30-34","35-39",
                                   "40-44","45-49","50-54","55-59","60-64",
                                   "65-69","70-74","75-79","80-84","85-89",
                                   "90-94","95-99","100+"))
  )

# (4) Modelo logit multinomial (como tu ejemplo, con referencia "no_votaron")
# ----------------------------------------------------------
mod_gto <- multinom(outcome ~ grupo_edad + sexo + zona,
                    data = datos,
                    weights = n,
                    trace = FALSE)

summary(mod_gto)

# (5) Coeficientes como Odds Ratios (igual que en tu código)
# ----------------------------------------------------------
exp(coef(mod_gto))

# (6) Average Marginal Effects (AME), versión factores
# ----------------------------------------------------------
# Para factores, avg_slopes usa la primera categoría como referencia
# (grupo_edad: "18-19"; sexo: la primera en el nivel alfabético; zona: "Rural" si es el primer nivel)
ame_ge   <- avg_slopes(mod_gto, variables = "grupo_edad", type = "probs")
ame_sexo <- avg_slopes(mod_gto, variables = "sexo",       type = "probs")
ame_zona <- avg_slopes(mod_gto, variables = "zona",       type = "probs")

ame_ge
ame_sexo
ame_zona

# (7) Predicciones de probabilidades para combinaciones observadas
# ----------------------------------------------------------
# Usamos las combinaciones presentes en la base (sin crear nada nuevo)
newdata <- datos |>
  distinct(grupo_edad, sexo, zona) |>
  arrange(grupo_edad, sexo, zona)

pred <- predictions(mod_gto, newdata = newdata, type = "probs")
head(pred)


