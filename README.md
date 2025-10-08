# COVID-19 Vaccine Acceptance Prediction

This project analyzes psychological and demographic predictors of COVID-19 vaccine acceptance using survey data collected in 2022.

## 📊 Project Overview
The goal of this project was to explore which psychological and sociodemographic variables best predict the willingness to get vaccinated against COVID-19.

- Dataset: N = 580 respondents (Germany)
- Data collection: SoSci Survey (online questionnaire)
- Methods: multiple Regression
- Performance: R² = .77
- Tools: R 

## 🧠 Research Focus
- Trust in science and perceived risk were the strongest predictors.
- Political orientation showed a moderate effect.
- The final model was interpretable using SHAP values.

## 📁 Repository Structure

covid19-vaccine-acceptance/
│
├── data/
│ ├── covid_raw.csv # Dataset with missing values
│ ├── covid_clean.csv # Cleaned dataset
│
├── code/
│ ├── covid_analysis.R # Main analysis script
│
├── documentation/
│ ├── variable_codebook.pdf # Variable definitions
│ ├── questionnaire.pdf # Full survey (SoSci)
│ ├── report.pdf # Summary report



