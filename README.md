<p align="center">
  <img src="https://cdn-icons-png.flaticon.com/512/2762/2762575.png" width="100" />
</p>
<p align="center">
    <h1 align="center">Diet and Exercise Tracker Analysis</h1>
</p>
<p align="center">
    <em>Track and analyze your daily diet and exercise to understand their impact on your weight.</em>
</p>
<p align="center">
	<img src="https://img.shields.io/github/license/mashape/apistatus.svg?style=flat&color=0080ff" alt="license">
	<img src="https://img.shields.io/github/last-commit/prasxanth/diet-log?style=flat&logo=git&logoColor=white&color=808080" alt="last-commit">
	<img src="https://img.shields.io/github/languages/top/prasxanth/diet-log?style=flat&color=808080&size_weight=0&count_weight=1" alt="repo-top-language">
	<img src="https://img.shields.io/github/languages/count/prasxanth/diet-log?style=flat&color=0080ff&size_weight=0&count_weight=1" alt="repo-language-count">
</p>
<p align="center">
		<em>Developed with the software and tools below.</em>
</p>
<p align="center">
	<img src="https://img.shields.io/badge/R-276DC3.svg?style=flat&logo=R&logoColor=white" alt="R">
	<img src="https://img.shields.io/badge/RStudio-75AADB.svg?style=flat&logo=RStudio&logoColor=white" alt="RStudio">
	<img src="https://img.shields.io/badge/Plotly-00BB00.svg?style=flat&logo=Plotly&logoColor=white" alt="Plotly">
</p>
<hr>

## Introduction
This repository contains a suite of R scripts designed to analyze my daily logs of diet, exercise, and weight tracking. A sample from these logs looks like,

```csv
Datetime,Value,Activity
2/24/2024 19:37:00,Dinner; 1 cucumber,Meal
2/24/2024 18:09:00,Evening Snack; 50 gms makhana with sunflower and pumpkin seeds,Meal
2/24/2024 17:35:00,"Evening Snack; 100 gm mixed nuts (almonds, cranberries etc.)",Meal
2/24/2024 13:13:00,Lunch; 3 millet pessarattu (~ 150 gms) with peanut butter,Meal
2/24/2024 12:15:00,Afternoon Drink; 1 cup caramel macchiato,Meal
2/24/2024 11:50:00,Morning Snack; 40 gms cashews + almonds,Meal
2/24/2024 9:29:00,Breakfast; Kefir chia seed pudding 350 gms,Meal
2/24/2024 9:11:00,167.6 lbs,Weight
2/24/2024 9:01:00,40 mins; indoor cycling; (~ 275 cals),Exercise
```

The scripts process input data from CSV files, perform detailed analyses to correlate activities with weight changes, and generate insightful visualizations showing the impact of diet and exercise on weight over time.

## Table of Contents
- [Introduction](#introduction)
- [Table of Contents](#table-of-contents)
- [Installation 🛠️](#installation-️)
- [Usage 🔧](#usage-)
- [Features ✨](#features-)
- [Configuration ⚙️](#configuration-️)
- [Dependencies 📦](#dependencies-)
- [Contributors 👥](#contributors-)
- [License 📄](#license-)

## Installation 🛠️
![GitHub](https://img.shields.io/github/license/mashape/apistatus.svg)
![Maintenance](https://img.shields.io/maintenance/yes/2024.svg)

Follow these steps to set up the environment:
1. Clone the repository to your local machine.
2. Unzip the contents of `library.zip` into the repository folder. This step ensures that all required R packages are available and version-controlled.
3. Ensure R is installed on your machine.

## Usage 🔧
Each script is designed to be run from the command line. Here’s how to use each one:
- **Processing Data**: `Rscript process.R --config_file="config/process.yaml"`
- **Analyzing Data**: `Rscript analyze.R --config_file="config/analyze.yaml"`
- **Generating Plots**: `Rscript plot.R --config_file="config/plot.yaml"`

Or use [littler](https://github.com/eddelbuettel/littler) instead of `RScript`.

Ensure that the configuration files are set correctly as per your data paths and requirements before running the scripts.

## Features ✨
- **Command Line Interface**: All scripts can be run from the command line, allowing for automation and ease of use.
- **Comprehensive Logging**: Each step of the process, from data processing to plotting, is logged to help track the flow and troubleshoot issues.
- **Interactive Plots**: The final output is an interactive plot that visually represents weight changes alongside diet and exercise activities.
- **Configurable**: All operations can be customized through YAML configuration files located in the `config/` folder.

## Configuration ⚙️
Configuration files in the YAML format are used to specify input details, processing parameters, and output preferences. These files are located in the `config/` directory. Users can modify these files to suit their specific needs for data paths, processing logic, and visualization details.

## Dependencies 📦
Dependencies are managed through the included `library.zip` file, which contains all the necessary R packages. This method ensures that the environment is consistent and that the package versions are controlled. Unzip this file in the project root directory before running the scripts.

## Contributors 👥
If you would like to contribute to this project, please fork the repository and submit a pull request.

## License 📄
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

