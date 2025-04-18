# Projet d'etude : programmation d'une application du jeu du takuzu 


Ce dépôt Github est un projet d'étude dans la formation de master 1 de statistique et sciences des données.
Il consiste en la création d'un package R permettant d'exécuter une application de type ShinyApp. 
Une fois réalisée, cette application devient une interface de jeu, celui du Takuzu (ou binary) de taille 8x8. 

## Comment installer le package ?

Pour installer ce package, il vous suffit d'éxecuter le Rscript "Guide_installation" dans votre environnement R. 
Attention toutefois à compiler ce code ligne par ligne. 
Lorsque vous compilerez la ligne contenant la commande "remotes", il vous faudra cocher "yes" dans la fenêtre qui s'ouvrira. Cette action peut prendre plusieurs minutes. 

Il vous suffit ensuite de compiler le reste du code pour avoir accès à votre interface de jeu. 


## Comment jouer ? 

Le Takuzu est un jeu ressemblant fortement au sudoku. Dans le jeu que l'on propose, il est composé d'une grille de 
taille 8x8. Lorsque vous débuterez la partie, la grille apparaîtra partiellement remplie de zéros et de uns.
Le but du jeu est de remplir la grille de zéros et de uns en suivant 3 règles simples : 
- Il ne peut pas y avoir plus de 2 mêmes chiffres consécutifs sur une même ligne ou une même colonne.
- Chaque ligne et chaque colonne doit posséder exactement 4 zéros et 4 uns.
- Il ne peut pas y avoir deux lignes identiques et deux colonnes identiques

En vous souhaitant un bon jeu ! 
