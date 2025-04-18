# Executez ce code afin de jouer au jeu Takuzu

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("LucienDuigou/Takuzu-Project")

library(Takuzu)
Takuzu()