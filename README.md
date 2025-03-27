# STEM PD Exit Survey Analysis

This project contains R code used to analyze participant responses to a professional development (PD) event hosted by the Maine Mathematics and Science Alliance (MMSA). The analysis focuses on understanding participant experiences, perceived quality of the event, and likelihood of future engagement.

##  Project Highlights

-  Automated generation of Likert-style histograms for survey responses
-  Dynamic labeling and wrapping for clean plot output
-  Multiple regression models exploring predictors of satisfaction
-  Clean project structure with ignored private data and reproducible analysis scripts

## Project Structure
stem-pd-exit-survey/ 
├── scripts/ # Main analysis script(s) 
├── data/ # (Ignored) Folder for raw data 
├── images/ # (Ignored) Folder for auto-generated plots 
├── .gitignore # Prevents tracking of sensitive files 
├── README.md # You're reading it!


## Data & Privacy

**Note**: This repository does not include raw survey data to protect participant privacy. Data files are referenced in the analysis scripts but are not publicly available. If you're reviewing this for demonstration or portfolio purposes, a synthetic example dataset can be created upon request.

## Requirements

This project was written in R using the following core packages:
- `ggplot2`
- `dplyr`
- `tidyr`
- `stringr`
- `modelsummary`
- `gt`

To run the analysis, simply clone the repository, ensure the dependencies are installed, and place a data file with the appropriate structure in the `data/` directory.

## Reproducibility

All plots and models are generated via parameterized loops or clearly defined code blocks. The structure allows for rerunning with future data by simply updating the CSV files (if available) and rerunning the script.

## Author

**Devan C. Arnold**  
Research Assistant, Maine Mathematics and Science Alliance

Project built with advisory support from *Sage* — an AI collaborator helping bring clarity and structure to complex ideas.

---

## License

This project is shared for demonstration and educational purposes. Please contact the author if you are interested in reusing or extending this work.
