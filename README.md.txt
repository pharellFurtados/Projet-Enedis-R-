# Projet RShiny - Analyse DPE pour Enedis

**Client :** Enedis
**Auteur :** [Nassir - Farès - Pharell]
**Date :** 15/11/2025

Ce projet a été réalisé dans le cadre du cours SD2. Il vise à évaluer l'impact de la classe de Diagnostic de Performance Energétique (DPE) sur les consommations électriques, en se basant sur les données des logements de Lyon.

---

## Liens Rapides

* **Lien vers l'application déployée :** `[COLLEZ VOTRE LIEN SHINYAPPS.IO ICI]`
* **Lien vers la vidéo de démonstration :** https://youtu.be/ces1Ex9wU_E?si=eH_qIaxOOrn8zc2k

---

## 1. Contexte du Projet

[cite_start]Avec l'accélération du changement climatique et la hausse des prix de l'énergie, la sobriété énergétique est un enjeu majeur[cite: 17]. [cite_start]Ce tableau de bord interactif a été commandé par Enedis pour analyser la performance du parc immobilier lyonnais[cite: 18].

## 2. Fonctionnalités de l'Application

L'application Shiny (disponible dans le dossier `/app`) fournit une analyse interactive complète :

* [cite_start]**Connexion Sécurisée :** Accès protégé par mot de passe (Fonctionnalité "Expert").
* [cite_start]**Tableau de Bord :** KPIs et filtres dynamiques (Standard).
* [cite_start]**Analyses Graphiques :** 4 types de graphiques (Histogramme, Boxplot, Barres, Nuage de points).
* [cite_start]**Analyse de Régression :** Corrélation et régression linéaire entre deux variables (Intermédiaire).
* [cite_start]**Cartographie :** Carte `leaflet` interactive des logements (Standard).
* [cite_start]**Exports :** Téléchargement des données filtrées en `.csv` et des graphiques en `.png` (Intermédiaire).

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

