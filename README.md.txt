# Projet RShiny - Analyse DPE pour Enedis

**Client :** Enedis
**Auteur :** [Nassir - Farès - Pharell]
**Date :** 15/11/2025

Ce projet vise à analyser l’impact de la classe de Diagnostic de Performance Énergétique (DPE) sur les consommations électriques des logements du département du Rhône (69), en particulier sur la ville de Lyon.
L’application RShiny développée permet à Enedis d’explorer dynamiquement les données du parc immobilier local, dans une logique de sobriété énergétique et d’optimisation des usages. Réalisé en 2ème année de BUT SD à l'IUT Lumière Lyon 2
---

## Liens Rapides

* **Lien vers l'application déployée :** `[COLLEZ VOTRE LIEN SHINYAPPS.IO ICI]`
* **Lien vers la vidéo de démonstration :** https://youtu.be/ces1Ex9wU_E?si=eH_qIaxOOrn8zc2k

---

## 1. Contexte du Projet

Avec l'accélération du changement climatique et la hausse des prix de l'énergie, la sobriété énergétique est un enjeu majeur[cite: 17]. Ce tableau de bord interactif a été commandé par Enedis pour analyser la performance du parc immobilier lyonnais.

## 2. Fonctionnalités de l'Application

L'application Shiny (disponible dans le dossier `/app`) fournit une analyse interactive complète :

Connexion Sécurisée :** Accès protégé par mot de passe (Fonctionnalité "Expert").
Tableau de Bord :** KPIs et filtres dynamiques (Standard).
Analyses Graphiques :** 4 types de graphiques (Histogramme, Boxplot, Barres, Nuage de points).
Analyse de Régression :** Corrélation et régression linéaire entre deux variables (Intermédiaire).
Cartographie :** Carte `leaflet` interactive des logements (Standard).
Exports :** Téléchargement des données filtrées en `.csv` et des graphiques en `.png` (Intermédiaire).

## 3. Structure du Dépôt

/Projet_Enedis/ │ ├── app/ # Code source de l'application Shiny │ ├── data/ # Données nettoyées pour l'app │ ├── www/ # Fichiers CSS et images │ └── app.R # Script principal de l'app │ ├── data_preparation/ # Scripts de nettoyage des données │ └── extraction_api.R │ ├── docs/ # Documentations technique et fonctionnelle │ ├── technical_doc.md │ └── functional_doc.md │ ├── rapport/ # Rapport d'analyse statistique (RMarkdown) │ └── rapport_statistique.Rmd │ └── README.md # Ce fichier


## 4. Comment l'utiliser (Installation locale)

Pour lancer ce projet sur votre machine, suivez ces étapes :

1.  **Prérequis :** Assurez-vous d'avoir R et RStudio installés, ainsi que tous les packages listés dans la `docs/technical_doc.md`.
2.  **Préparer les données :**
    * Placez les CSV bruts de l'ADEME dans le dossier `data_preparation/`.
    * Exécutez le script `data_preparation/extraction_api.R` **une seule fois**.
3.  **Lancer l'application :**
    * Ouvrez le fichier `app/app.R` dans RStudio.
    * Cliquez sur **"Run App"**.

    * Utilisez les identifiants : `élève` / `projet`.



