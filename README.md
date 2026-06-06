# CAA-Hockey-Analytics
Hockey analytics project for CAA: 
Forward and Defenceman salary regression model (adj. R² ≈ 0.68) and rank-based z-score percentile rankings across 100+ metrics, to create a statistical model to support contract negotiations for CAA's NHL clients. Built in R.

Layer 1 - Salary Regression Models
Multiple Linear regression models for forwards and defenceman that predict a player's cap hit. Built on 3 seasons of data(2023–2026). To qualify, a player must have played at least 100 NHL regular-season games within the last 3 years (40% of all possible regular-season games). 

A correlation matrix was created for both forwards and defencemen to determine the best metrics to use for the model, while also taking into consideration multicollinearity. This is intended to reflect what the market pays players for, as opposed to what the analytics might say a player should be paid for. 

Forwards: TOI/GP + Total Points/60 - adj. R² = 0.6584 | Defensemen: TOI/GP + xGF/60 - adj. R² = 0.6868

Layer 2 - Rank-based Z-Score Percentile Rankings
100+ metrics (team-level data, individual production, zone entries for & against, and defensive zone exits) were first calculated into per 60-minute rates (stat*60 / total 5v5 ice-time), then z-scored and converted to rank-based percentiles across all qualifying players. 

Rank-based means that if a player had the highest z-score for their group in "x" metric, they were assigned the 100th percentile. The assumption of normality was not a factor here; I wanted to quantify who is the best in the NHL. This was also an attempt to capture what the market (regression model) doesn't price in i.e. zone exits, zone entries, defensive suppression, and transition quality. Thus giving agents the analytical argument for their player to be paid above the regression floor.

Built With
R: dplyr, ggplot2, car (VIF), scales, ggrepel

Note: Data files and generated outputs are excluded from this repository. To access the results from my analysis, please refer to my website, as there will be a front-end interactive model where you can see the regression model for each player, along with their player cards that show all their micro-analytic percentiles.

A Note on AI-Assisted Development: 
Claude (Anthropic) was used throughout the coding process of this project. The statistical skeleton of the project, the regression model, z-scores, the decision to use a rank-based percentile system, and the use of the Euclidean Distance formula for some players was designed and validated thoroughly beforehand. The implementation of Euclidean Distance methodology and the percentile card visualizations was self-learned using external resources (YouTube) before using Claude to assist in building the ideas out in R.

Where Claude was most utilized was in the organizing and cleaning of the data. This project had 3 years of data across hundreds of NHL games, from over 600+ players, who each had 100+ metrics. Through hundreds of lines of code, I was able to complete the project much faster than I would have otherwise.

The other area where I leaned heavily on Anthropic's model was in the Z-Score card visualization. My goal was to create something similar to Baseball Savant's player cards, but I had no idea how to make that possible. Again, through YouTube videos and Claude Code, I was able to learn how to utilize new libraries, syntax, and techniques that I did not know were possible in R.

The goal in using Claude in this project was to make the process faster, more efficient, and more informed. My intention was never to prompt my way to a result that I do not understand. This felt important to address and to be transparent about. Not only as AI becomes a standard part of analytical work, but also as to how AI can be appropriately and ethically used for academic work.
