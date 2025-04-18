# Projet d'etude : programmation d'une application du jeu du takuzu 


Ce dépôt Github est un projet d'étude dans la formation de master 1 de statistique et sciences des données.
Il consiste en la création d'un package R permettant d'executer une application de type ShinyApp. 
Une fois executée cette application devient une interface de jeu, celui du Takuzu (ou binary) de taille 8x8. 

## Comment installer le package ?

Pour installer ce package, il vous suffit d'éxecuter le Rscript "Guide_installation" dans votre environnement R. 
Lors de cette execution, il nous est arrivé de rencontrer une erreur, en relançant l'environnement, nous avons pu 
éxécuter le et joué au jeu. 

## Comment jouer ? 

Le Takuzu est un jeu ressemblant fortement au sudoku. dans le jeu que l'on propose il est composé d'une grille de 
taille 8x8. Lorsque vous débuterez la partie, la grille apparaîtra partiellement remplie de zéros et de uns.
Le but du jeu est de remplir la grille de zéros et de uns en suivant 3 règles simples : 
- Il ne peut pas y avoir plus de 2 mêmes chiffres consécutifs sur une même ligne ou une même colonne
- Chaque ligne et chaque colonne doit posséder éxactement 4 zéros et 4 uns.
- Il ne peut pas y avoir de lignes identiques et deux colonnes identiques

En vous souhaitant un bon jeu ! 
