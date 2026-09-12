#EDA de train.csv de competencia de kaggle
#empiezo importando dataset
library(tidyverse)
library(Amelia)

train <- read.csv("~/GitHub/metodos-proyecto/train.csv")
head(train,n=5)
dim(train)
colnames(train)
str(train)
df <- train

# Ojo: Occupation (y quiza otras categoricas) usan "" en vez de NA,
# por eso is.na() no las contaba. Las normalizo antes de contar NAs.
df <- df |>
    mutate(across(where(is.character), ~ na_if(., "")))

numeric_vars <- c("Age", "Annual.Income", "Health.Score", "Number.of.Dependents",
                   "Previous.Claims", "Vehicle.Age", "Credit.Score", "Insurance.Duration")

categorical_vars <- c("Gender", "Marital.Status", "Education.Level", "Occupation",
                       "Location", "Policy.Type", "Customer.Feedback",
                       "Smoking.Status", "Exercise.Frequency", "Property.Type")

# conteo de NA por columna (ahora si contempla los "")
df |>
    summarise(across(everything(), ~ sum(is.na(.)), .names = "NA_{.col}"))

missmap(df)

#  Resumen univariado de predictores (antes de cruzarlos con el target) 
summary(df[, numeric_vars])

for (v in categorical_vars) {
    cat("\n---", v, "---\n")
    print(table(df[[v]], useNA = "ifany"))
}

#  EDA variable objetivo (Premium.Amount) 
ggplot(df, aes(x = Premium.Amount)) +
    geom_histogram(bins = 50) +
    labs(title = "Distribucion de Premium.Amount")

# la competencia evalua con RMSLE, por eso miramos tambien la escala log
ggplot(df, aes(x = log1p(Premium.Amount))) +
    geom_histogram(bins = 50) +
    labs(title = "Distribucion de log(1 + Premium.Amount)")

#  EDA numericas vs target 
for (v in numeric_vars) {
    p <- ggplot(df, aes(x = .data[[v]], y = Premium.Amount)) +
        geom_point(alpha = 0.05) +
        labs(title = paste("Premium.Amount vs", v))
    print(p)
}

# matriz de correlacion entre TODAS las numericas (no solo contra el target),
# para revisar multicolinealidad antes de interpretar
cor_matrix <- df |>
    select(all_of(numeric_vars), Premium.Amount) |>
    cor(use = "pairwise.complete.obs")
print(cor_matrix)

#  EDA categoricas vs target 
for (v in categorical_vars) {
    p <- ggplot(df, aes(x = .data[[v]], y = Premium.Amount)) +
        geom_boxplot() +
        labs(title = paste("Premium.Amount por", v)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
}

#  Policy.Start.Date: la dejo como fecha y extraigo componentes 
df <- df |>
    mutate(
        Policy.Start.Date = as.POSIXct(Policy.Start.Date),
        Policy.Start.Year = year(Policy.Start.Date),
        Policy.Start.Month = month(Policy.Start.Date),
        Policy.Start.Wday = wday(Policy.Start.Date, label = TRUE)
    )

#  Imputacion de ausentes 
# numericas: promedio de la columna
# categoricas: moda (el promedio no aplica a texto)
#
# NOTA sobre Previous.Claims (~30% NA) y Credit.Score (~11% NA):
# son las columnas con mayor proporcion de ausentes. Imputar con el promedio
# aqui aplana artificialmente su varianza y puede debilitar su correlacion
# real con Premium.Amount, sesgando la interpretacion ("esta variable no
# importa" podria ser un efecto de la imputacion y no del dato real).
# Tratar sus resultados de EDA con cautela; si el modelo final las usa,
# considerar una imputacion mas robusta (KNN, por grupo, o un indicador
# binario de "fue imputado").
moda <- function(x) {
    ux <- na.omit(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
}

df <- df |>
    mutate(across(all_of(numeric_vars), ~ replace_na(., mean(., na.rm = TRUE)))) |>
    mutate(across(all_of(categorical_vars), ~ replace_na(., moda(.))))

# verificacion post-imputacion: debieron quedar todas en 0
df |>
    summarise(across(everything(), ~ sum(is.na(.)), .names = "NA_{.col}"))
