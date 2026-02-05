# Étude de l'obésité - Importation et remaniement des données
# Auteur : ___
# Date : ____
###############################################################################

# Packages utiles
SciViews::R(lang = "fr")

# Importation des données brutes

## Création des dossier `data/` et `cache/`
fs::dir_create("data/cache")

## Importation du dictionnaire des données
biometry_metadata <- read$csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vS09O5K562SrsVbi7-Ap4ASnQuS7P10AMVuQ1kGrISfFhWMMM_WlCY1-EErhGqdpXIKAjmY7W3mZgo1/pub?gid=0&single=true&output=csv",
  cache_file = "data/cache/biometry_metadata_raw.csv",
  force = FALSE
)

## Importation du tableau de données
biometry <- read$csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vT2UjU4ScS1cm0Khk7xuTdiJNMYrrx4TugMSosQAjqkIuvFEDyugYmJ5CU3RCrAfyUgh68v6B3IihWI/pub?gid=0&single=true&output=csv",
  cache_file = "data/cache/biometry_raw.csv",
  force = FALSE
)

# Exploration des données

skimr::skim(biometry)

# Remplacement de chaine de caractères vide par un NA --------------------------

biometry <- mutate_(biometry, ~ across(where(is.character), na_if, ""))

# propostion d'une version alternative en R de base 
#biometry[biometry == ""] <- NA

# Remplacement du séparateur décimal : , et . ----------------------------------

cols <- c("masse_std_ref", "masse_std", "masse", "taille", "tour_poignet", 
          "tour_taille", "tour_hanche", "acti_sport", "marche", "fast_food", 
          "alcool",  "sommeil","diabete", "eau")


# Application de la fonction replace_sep() sur l'ensemble des colonnes ci-dessus

replace_sep <- function(x){
  as.numeric(gsub(",", ".", x))
}

biometry <- mutate_(biometry, ~across(cols, replace_sep))
# propostion d'une version alternative en R de base 
# biometry[cols] <- lapply(biometry[cols], replace_sep)

rm(cols, replace_sep)

# Modification des types de variables des données et correction ---------------- 
## Variables temporelles
biometry$date_mesure[biometry$date_mesure == "2025-12"] <- "2025-12-01"
biometry <- mutate_(biometry,
                    date_mesure = ~ lubridate::ymd(date_mesure),
                    date_naissance = ~ lubridate::ymd(date_naissance))

# Variables facteurs 
## genre
unique(biometry$genre)
biometry$genre[biometry$genre == "M"] <- "H"
biometry$genre[biometry$genre == "f"] <- "F"
biometry$genre <- factor(biometry$genre, levels = c("H", "F"))

## diabete
biometry$diabete <- factor(biometry$diabete, 
  levels = c(0:2), ordered = TRUE)

## sedentarite
unique(biometry$sedentarite)
biometry$sedentarite[biometry$sedentarite == "elevé"] <- "élevé"

biometry$sedentarite <- factor(biometry$sedentarite,
  levels = c("faible", "moyen", "élevé"), ordered = TRUE)

## stress
unique(biometry$stress)
biometry$stress[biometry$stress == "Élevé"] <- "élevé"

biometry$stress <- factor(biometry$stress,
    levels = c("faible", "moyen", "élevé"), ordered = TRUE)

## finance
unique(biometry$finance)
biometry$finance[biometry$finance == "Bonne"] <- "bonne"
biometry$finance[biometry$finance == "élevé"] <- "bonne"

biometry$finance <- factor(biometry$finance,
    levels = c("mauvaise", "acceptable", "bonne"), ordered = TRUE)

# Calcul de nouvelles variables ------------------------------------------------

biometry <- mutate_(biometry, 
    # Calcul de l'age des individu
    age = ~ as.numeric(difftime(date_mesure, date_naissance, units = "days")/365.25), 
    # Calcul de la masse corrigée
    masse_corr = ~ masse * (masse_std_ref/masse_std)) 

# Filtre sur les données manquantes --------------------------------------------
biometry <- drop_na_(biometry, "masse_corr")

# Ajout des labels et des unités
# TODO


# Sauvegarde local des données importantes 
write$rds(biometry, "data/biometry.rds", compress = "xz")
write$rds(biometry_metadata, "data/biometry_metadata.rds", compress = "xz")

# Élimination des objets de l'environnement global
rm(biometry_metadata, biometry)
