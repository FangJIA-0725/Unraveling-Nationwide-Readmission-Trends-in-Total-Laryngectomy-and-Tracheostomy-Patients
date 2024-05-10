# Unraveling-Nationwide-Readmission-Trends-in-Total-Laryngectomy-and-Tracheostomy-Patients
## Repository Overview
This repository contains the code and analysis for the thesis project titled "Unraveling Nationwide Readmission Trends in Total Laryngectomy and Tracheostomy Patients" conducted by Fang Jia, Ng Tsz Wai, and Yuyang Sun at Boston University under the supervision of Professor Masanao Yajima. The project utilizes the Nationwide Readmissions Database (NRD) to develop predictive models that help identify patients at high risk of readmission within 30 days post-discharge.

## Repository Structure
Data_Cleaning.ipynb: Jupyter notebook containing all preprocessing and cleaning steps applied to the dataset. Prepared by Ng Tsz Wai.

Causal_ML_Models.ipynb: Jupyter notebook detailing the causal machine learning analysis. Responsible author: Ng Tsz Wai.

Predictive_Modeling.ipynb: Contains the development and validation of predictive models using Random Forest and XGBoost algorithms. Developed by Fang Jia.

Survival_Analysis.ipynb: Notebook for survival analysis, including Kaplan-Meier and Cox Proportional Hazards models. Managed by Yuyang Sun.

Figures/: Directory containing all figures generated during the analysis, organized by the model type. Each figure is linked to its respective section in the notebooks.

README.md: Overview and guide for navigating the repository.

## Collaborators and Contributions
Fang Jia: Focused on predictive modeling and significantly contributed to the methodology and result interpretation sections.

Yuyang Sun: Focused on survival analysis parts and significantly contributed to the methodology and result interpretation sections.

Ng Tsz Wai: Led the data cleaning and specialized in causal machine learning models, providing insights into factors influencing readmissions.

## Usage
Each notebook is designed to be self-contained and includes detailed comments explaining the steps of the analysis. Users are encouraged to review the Data_Cleaning.ipynb notebook first to understand the dataset's structure and preprocessing steps.

## Installation and Requirements
To run the notebooks, ensure you have Python 3.x installed along with Jupyter Notebook or JupyterLab. The necessary Python libraries include pandas, numpy, matplotlib, seaborn, scikit-learn, xgboost, and lifelines. These can be installed via pip:

pip install pandas numpy matplotlib seaborn scikit-learn xgboost lifelines

## License
This project is licensed under the terms of the MIT license.

## Contact
For questions or feedback related to this project, please reach out to any of the contributors via their university email addresses.

Fang Jia: jiafang@bu.edu

Yuyang Sun: tms28k@bu.edu

Ng Tsz Wai: ngtszwai@bu.edu
