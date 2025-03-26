# Analyse de données ouvertes choisies librement

La science est de plus en plus *ouverte* (le terme en anglais est **Open Science**). Il s'agit ici de rendre les analyses accessibles et de documenter tout le processus qui mène aux résultats obtenus. Les publications sont aussi librement accessibles en Open Science. Cette volonté de rendre la science plus transparente va de pair avec l'*Open Data*, données ouvertes. Cela signifie que les données sont également rendues publiques et accessibles.

Lors de la publication d'articles scientifiques, il est de plus en plus courant que les revues demandent aux auteurs de rendre leurs données disponibles en Open Data. Il est même conseillé de fournir le code employé pour réaliser les analyses statistiques. On peut mettre en avant quelques avantages de ces bonnes pratiques :

-   Reproductibilité : en fournissant les données et le code, les autres scientifiques peuvent reproduire les analyses et les résultats. Cela contribue à renforcer la transparence et la confiance dans les conclusions de l'étude.

-   Vérification des résultats : les données et le code peuvent être utilisés pour vérifier les résultats de l'étude et détecter les erreurs ou les biais. Cela permet d'améliorer la qualité de la recherche scientifique et d'éviter, ou du moins, limiter fortement les erreurs.

-   Réutilisation des données : les données peuvent être réutilisées pour d'autres analyses, ce qui permet de maximiser l'utilisation des ressources.

-   Accélération de la recherche : en partageant les données et le code, les autres scientifiques peuvent construire sur les résultats antérieurs plus rapidement et plus efficacement. Cela aide à trouver des solutions plus rapidement.

-   Meilleure collaboration : en partageant les données et le code, les scientifiques peuvent collaborer plus facilement pour résoudre des problèmes complexes.

La mise à disposition des données (et du code) doit respecter certains critères que l'on peut regrouper sous l'acronyme FAIR pour **Findable**, **Accessible**, **Interoperable**, **Reusable**. Les données qui ne se conforment pas à ces critères peuvent être partiellement ou même totalement inutilisables.

## Objectifs

Ce projet est **libre** et sera réalisé en **groupes de quatre étudiants**. Répartissez-vous le travail. Il permettra de démontrer que vous avez acquis les compétences suivantes :

-   trouver des données ouvertes qui se prêtent à une analyse multivariée de type SOM (tout le monde cherche et vous choisissez de manière collégiale le jeu de données à retenir au final)
-   critiquer les données trouvées selon le principe FAIR (chaque étudiant rédige la partie relative à un des critères)
-   réaliser une SOM sur ces données et l'interpréter (tous les étudiants doivent rédiger une partie du rapport, répartissez-vous les tâches au départ)

## Consignes

La première étape consiste en la recherche de données ouvertes. Vous suivrez les instructions du fichier `data/README` pour récupérer vos données depuis une URL de téléchargement direct avec mise en cache dans `data/cache`. Si ce n'est pas possible, il faudra le justifier dans le document `open_data.qmd` et placer les données manuellement dans le sous-dossier `data` (max 50Mo). **Avant de faire un choix définitif de votre jeu de données, assurez-vous que le contenu du tableau de données convient pour la réalisation d'une analyse multivariée de type SOM**. Cela fait partie de l'exercice que de choisir un jeu de données adéquat ici !

Voici plusieurs sites que vous pouvez utiliser pour rechercher vos données :

-   [Zenodo](https://zenodo.org/)

-   [Dryad](https://datadryad.org/)

-   [Free and open access to biodiversity data](https://www.gbif.org/)

-   [The Knowledge Network for Biocomplexity](https://knb.ecoinformatics.org/data)

-   [EDI Data Portal](https://portal.edirepository.org/nis/home.jsp)

La deuxième étape consiste à expliquer de manière détaillée en quoi les données choisies répondent (ou non) au principe FAIR. Il s'agit de formuler une critique des données sélectionnées sur base du principe FAIR dans le document `open_data.qmd`.

Enfin, pour la troisième et dernière étape; vous complétez le rapport dans `report.qmd`. Vous réalisez et interprétez une analyse multivariée de type SOM sur les données choisies. Dans le cas où vous vous rendriez compte que cela ne fonctionne pas, justifiez pourquoi et expliquez en quoi vous n'étiez pas capables, avant de faire l'analyse, de déterminer que ces données ne conviendraient pas. Dans ce cas, nous déterminerons si vos justifications tiennent la route pour établir votre note de la troisième partie du travail.

N'oubliez pas de réaliser un "Rendu" des documents en HTML à la fin pour vérifier que tout fonctionne bien. Corrigez les erreurs éventuelles rencontrées à ce stade avant de clôturer votre projet. Vérifiez également que votre dernier commit a bien été pushé sur GitHub avant la deadline.

Ce projet correspond au template <https://github.com/BioDataScience-Course/B10Ga_open_data>.
