# An-Analysis-of-World-Cup-2022-using-R

# Project Overview

This project provide an overall exploratory analysis of the World Cup 2022. Based on historical data, I will try to unfold the following insights:

1. How Qatar, as the WC 2022 host country, perfomed compared to other host country in the past?
2. Countries that overachieved and underperformed in WC2022 based on historical dominance and recent form
3. Argentina path to glory compared to France's in WC2018

Tools implemented: the whole analysis will be conducted in R, particularly the **tidyverse package including forcats, ggplot2, lubridate, etc.** and some other smaller analysis packages.

# Datasets:

The following datasets is being used:

**1. World Cup Events:**
- *world_cups.csv*: Information World Cup Events since 1935 and countries in the top 4 from Maven Analytics

**2. World Cup Matches:**
- *2022_world_cup_matches.csv*: Information about world cup 2022 matches from Maven Analytics
- *world_cup_ matches.csv*: Information and **results** of all world cup matches before 2022 from Maven Analytics
- *Fifa_world_cup_matches.csv*: world cup 2022 match result and stats (**note**: this data will be used to join with *2022_world_cup_matches* to retrieve all the match results for WC 2022)

3. International Matches
- *International_Matches.csv*: Information about International Matches before WC 2022

4. World Cup Groups
- *2022_world_cup_groups.csv*: WC 2022 participants along with their group and final standing


# Directory

.my-code-block {
  background-color: #f8f8f8;
  border: 1px solid #ccc;
  border-radius: 4px;
  padding: 16px;
}

<div class ="my-code-block">
| ---------------------------------------------------------------------------------------------|        
|   -- data                                         Includes all datasets used for the project
|--- World_cup.csv|
|     --- 2022_world_matches.csv|
|     --- world_cup_matches.csv|
|     --- Fifa_world_cup_matches.csv| 
|     --- International_Matches.csv                                                            |
|     --- 2022_world_cup_groups.csv                                                            |
|   -- R_Script.r                                   R Analysis File                            |
|   -- R_Script.md                                  Markdown File for display in Github        |
|   -- README.md                                    Project Overview                           |

<div>

  
├── README.md
├── data
│   ├── input.csv
│   ├── output.csv
│   └── temp.csv
├── figures
│   ├── chart.png
│   └── plot.pdf
├── scripts
│   ├── data-cleaning.R
│   ├── data-visualization.R
│   ├── model-building.R
│   └── report-generation.R
└── my-project.Rproj



