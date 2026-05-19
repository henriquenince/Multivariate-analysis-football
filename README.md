# Multivariate Analyses Applied to Player Performance Evaluation in the 2024 Brazilian Football Championship (Série A)

> Undergraduate Thesis (TCC) — Department of Statistics, University of Brasília (UnB)
> **Author:** Henrique Silva Nince
> **Advisor:** Prof. Gladston Luiz da Silva
> **Committee:** Prof. Donald Matthew Pianto · Prof. Raul Yukihiro Matsushita
> **Presented:** December 8, 2025

> 📄 **Note:** This work was written in **Brazilian Portuguese**. The report and presentation slides are available in the repository in their original language.

---

## About

This project applies multivariate statistical techniques — Factor Analysis and K-means Clustering — to evaluate individual player performance in the 2024 Brazilian Série A. The goal is to demonstrate the value of data-driven approaches in Brazilian football, inspired by the Moneyball revolution and the growing use of analytics departments at clubs like Flamengo, Palmeiras, and Athletico-PR.

The analysis was conducted separately for each position (goalkeepers, centre-backs, fullbacks, defensive midfielders, attacking midfielders, wingers, and forwards), producing performance scores and cluster groupings that culminated in a statistically grounded **Team of the Season**.

---

## Objectives

- Synthesize position-specific indicators via **Factor Analysis**
- Build a **Final Performance Score** for each player
- Identify performance groups with **K-means Clustering**
- Propose a **Team of the Season**

---

## Methodology

### Data Collection & Preparation

- Match data collected from **FBref** (128 variables for outfield players, 42 for goalkeepers)
- Position enrichment via the **Transfermarkt API**
- Identification of players who appeared for multiple clubs during the season
- Missing value treatment
- Removal of composite variables, zero-variance variables, and highly correlated variables
- Standardization using `scale()` (mean 0, sd 1)

### Factor Analysis

1. Adequacy assessment: **KMO index** and normality testing (**Shapiro–Wilk**)
2. Removal of variables with KMO < 0.5
3. Number of factors: **Kaiser criterion**, scree plot, and **Parallel Analysis**
4. Estimation: **MINRES** method with oblique **oblimin** rotation
5. Removal of variables with communality < 0.5
6. Factor interpretation and score computation

### K-means Clustering

1. Number of clusters: **Elbow method**, **Silhouette**, and **NbClust**
2. K-means execution
3. Validation: Silhouette score, **Kruskal–Wallis** test, and **Dunn's post-hoc test**

### Tools

| Tool | Purpose |
|---|---|
| **RStudio (R)** | Data processing, cleaning, and statistical analyses |
| **VSCode (Python)** | Transfermarkt API access for position data collection |

---

## Results by Position

### Goalkeepers
- **Sample:** 45 players · **KMO:** 0.863 · **Factors:** 1
- Single factor: *Overall Performance* (87.3% of variance)
- Final Score inversely weighted by goals conceded per 90 min
- **2 clusters:** High Performance (17) · Lower Performance (28)

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | John | Botafogo | 46.1 |
| 2 | João Ricardo | Fortaleza | 44.9 |
| 3 | Weverton | Palmeiras | 42.8 |
| 4 | Fábio | Fluminense | 42.7 |
| 5 | Marcos Felipe | Bahia | 41.3 |

---

### Centre-Backs
- **Sample:** 116 players · **KMO:** 0.896 · **Factors:** 4
- F1: Ball Playing (44%) · F2: Build-up & Link (17.8%) · F3: Direct Offensive Action (10.7%) · F4: Duels & Containment (10.3%)
- **3 clusters:** High-Performance Ball-Playing CBs (26) · Balanced Intermediates (51) · Low Participation (39)

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | Alexander Barboza | Botafogo | 40.1 |
| 2 | Bastos | Botafogo | 36.7 |
| 3 | Murilo | Palmeiras | 35.3 |
| 4 | Alan Empereur | Cuiabá | 34.1 |
| 5 | Thiago Santos | Fluminense | 33.9 |

---

### Fullbacks
- **Sample:** 109 players · **KMO:** 0.873 · **Factors:** 3
- F1: Overall Contribution & Progression (55.9%) · F2: Direct Offensive Impact (16.6%) · F3: Crossing & Set Pieces (8.6%)
- **2 clusters:** High Performance & High Influence (47) · Low Participation (62)

**Top 5 — Right Backs**

| # | Player | Club | Score |
|---|---|---|---|
| 1 | William | Cruzeiro | 76.3 |
| 2 | João Lucas | Juventude | 74.6 |
| 3 | João Pedro | Grêmio | 74.2 |
| 4 | Ewerthon | Juventude | 73.0 |
| 5 | Paulo Henrique | Vasco | 72.1 |

**Top 5 — Left Backs**

| # | Player | Club | Score |
|---|---|---|---|
| 1 | Alexandro Bernabei | Internacional | 81.9 |
| 2 | Lucas Esquivel | Athletico PR | 80.1 |
| 3 | Luciano Juba | Bahia | 77.4 |
| 4 | Ramon | Cuiabá | 73.5 |
| 5 | Marcelo Hermes | Criciúma | 71.1 |

---

### Defensive Midfielders (Volantes)
- **Sample:** 152 players · **KMO:** 0.906 · **Factors:** 6
- F1: Build-up & Distribution (33.8%) · F2: Defensive Duels (14.8%) · F3: Direct Offensive Action (13.8%) · F4: Aggressive Carrying (8.3%) · F5: Crossing & Set Pieces (8.1%) · F6: Advanced Creation (5.7%)
- **3 clusters:** High Impact (25) · Intermediate Contribution (56) · Low Participation (71)

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | Gerson | Flamengo | 64.4 |
| 2 | Caio Alexandre | Bahia | 63.1 |
| 3 | Jean Lucas | Bahia | 60.8 |
| 4 | Gregore | Botafogo | 60.2 |
| 5 | Lucas Romero | Cruzeiro | 57.9 |

---

### Attacking Midfielders (Meias)
- **Sample:** 62 players · **KMO:** 0.778 · **Factors:** 2
- F1: Offensive Action & Direct Creation (48.1%) · F2: Build-up & Tactical Participation (31%)
- **3 clusters:** High Creative Impact (10) · Intermediate Contribution (27) · Low Influence (25)

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | Rodrigo Garro | Corinthians | 91.5 |
| 2 | Raphael Veiga | Palmeiras | 85.9 |
| 3 | Gustavo Scarpa | Atlético MG | 77.7 |
| 4 | Matheus Pereira | Cruzeiro | 76.6 |
| 5 | Franco Cristaldo | Grêmio | 75.7 |

---

### Wingers (Pontas)
- **Sample:** 136 players · **KMO:** 0.924 · **Factors:** 4
- F1: Overall Game Contribution (35%) · F2: Individual Creation (19.4%) · F3: Direct Offensive Presence (16.4%) · F4: Set Pieces & Crossing (13.6%)
- **3 clusters:** High Impact (11) · Moderate Participation (53) · Low Influence (72)

**Top 5 — Right Wingers**

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | Jhon Arias | Fluminense | 71.5 |
| 2 | Lucas Moura | São Paulo | 66.9 |
| 3 | Lucas Barbosa | Juventude | 65.7 |
| 4 | Estevão | Palmeiras | 64.5 |
| 5 | Luiz Henrique | Botafogo | 63.6 |

**Top 5 — Left Wingers**

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | Wesley | Internacional | 70.3 |
| 2 | Tomás Cuello | Athletico PR | 65.7 |
| 3 | Vitinho | RB Bragantino | 64.6 |
| 4 | Alejo Cruz | Atlético GO | 56.9 |
| 5 | Álvaro Barreal | Cruzeiro | 55.8 |

---

### Forwards (Atacantes)
- **Sample:** 89 players · **KMO:** 0.852 · **Factors:** 4
- F1: Direct Offensive Output (41%) · F2: Individual Action & Progression (18.2%) · F3: Set-Piece Actions (11.1%) · F4: Defensive Support (9.9%)
- **3 clusters:** High Output (20) · Balanced Contribution (44) · Low Impact (25)

| # | Player | Club | Final Score |
|---|---|---|---|
| 1 | Hulk | Atlético MG | 75.1 |
| 2 | Luciano | São Paulo | 70.3 |
| 3 | Yuri Alberto | Corinthians | 69.2 |
| 4 | Alerrandro | Vitória | 68.4 |
| 5 | Janderson | Vitória | 68.3 |

---

## Team of the Season

The Final Score selection showed strong alignment with the Team of the Season published by GE and ESPN's Bola de Prata award, reinforcing the model's external validity.

```
              [ Hulk ]
   [ Wesley ]          [ Arias ]
           [ Garro ]
  [ Caio Alexandre ]  [ Gerson ]
[ Bernabei ]              [ William ]
      [ Barboza ]  [ Bastos ]
              [ John ]
```

---

## Limitations

- Model does not account for individual physical attributes
- No spatial/tracking data available
- Tactical contributions may be underestimated
- Players evaluated only in their registered Transfermarkt position

---

## Key References

- Hair, J. F. et al. (2009). *Multivariate Data Analysis*. 7th ed. Pearson.
- Performance data: [FBref](https://fbref.com)
- Position data: [Transfermarkt](https://www.transfermarkt.com) API
