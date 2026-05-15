# CAA-Hockey-Analytics
Hockey analytics project for CAA: 
Forward and Defenceman salary regression model (adj. R² ≈ 0.68) and rank-based z-score percentile rankings across 100+ metrics, to create a statistical model to support contract negotiations for CAA's NHL clients. Built in R.

Layer 1 - Salary Regression Models
Multiple Linear regression models for forwards and defenceman that predict a player's cap hit. Built on 3 seasons of 5v5 data (2023–2026). To qualify, a player must have played at least 100 NHL regular-season games within the last 3 years (40% of all possible regular-season games). 

A correlation matrix was created for both forwards and defencemen to determine the best metrics to use for the model, while also taking into consideration multicollinearity. This is intended to reflect what the market pays players for, as opposed to what the analytics might say a player should be paid for. 

Forwards: TOI/GP + Total Points/60 - adj. R² = 0.6584 | Defensemen: TOI/GP + xGF/60 - adj. R² = 0.6868

Layer 2 - Rank-based Z-Score Percentile Rankings
100+ metrics (team-level data, individual production, zone entries for & against, and defensive zone exits) were first calculated into per 60-minute rates (stat*60 / total 5v5 ice-time), then z-scored and converted to rank-based percentiles across all qualifying players. 

Rank-based means that if a player had the highest z-score for their group in "x" metric, they were assigned the 100th percentile. The assumption of normality was not a factor here; I wanted to quantify who is the best in the NHL. This was also an attempt to capture what the market (regression model) doesn't price in i.e. zone exits, zone entries, defensive suppression, and transition quality. Thus giving agents the analytical argument for their player to be paid above the regression floor.

Built With
R: dplyr, ggplot2, car (VIF), scales, ggrepel

Note: Data files and generated outputs are excluded from this repository. To access the results from my analysis, please refer to my website, as there will be a front-end interactive model where you can see the regression model for each player, along with their player cards that show all their micro-analytic percentiles. 
