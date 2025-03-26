# Vérifications de open_data.qmd
open_data <- parse_rmd("../../open_data.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes open_data est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("open_data.qmd"))
  # La version compilée HTML du carnet de notes open_data est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE !

  expect_true(is_rendered_current("open_data.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document open_data est-elle conservée ?", {
  expect_true(all(c("Principe FAIR", "Récupération des données",
    "Findable", "Accessible", "Interoperable", "Reusable")
    %in% (rmd_node_sections(open_data) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes open_data.qmd ne sont pas
  # toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "dataread") %in% rmd_node_label(open_data)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent dans
  # open_data.qmd
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(open_data))))
  # Un ou plusieurs labels de chunks sont dupliqués dans open_data.qmd
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété dans open_data.qmd ?", {
  expect_true(open_data[[1]]$author != "___, ___, ___, ___")
  expect_true(!grepl("___", open_data[[1]]$author))
  expect_true(grepl("^[^_]....+", open_data[[1]]$author))
  # Le nom des auteurs n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer le nom des étudiants du groupe dans l'entête YAML à la
  # place de "___, ___, ___, ___" et éliminer les caractères '_' par la même
  # occasion.

  expect_true(grepl("[a-z]", open_data[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom des auteurs
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire vos noms tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", open_data[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom des auteurs
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire vos noms tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})
