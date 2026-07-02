# Transformations scripts

# MetaboDirect for CoreMS

This repository has an adapted version of [MetaboDirect](https://github.com/Coayala/MetaboDirect). It has been modified to accept two types of CoreMS inputs: **unprocessed single-sample files** and **processed merged files**. 

The pipeline allows you to estimate chemical transformations and generate node/edge tables for network analysis.

---

## Workflows

Depending on your starting data, you can choose one of two workflows:

### Workflow 1: Unprocessed Single Files
Use this workflow if you have individual, unprocessed CoreMS files for each sample.
* **Notebook:** `corems_transformations_unprocessed_files.ipynb`
* **Description:** Takes unprocessed CoreMS files (one file per sample) as input. It utilizes MetaboDirect's functions to calculate chemical transformations and generate node and edge tables for network visualization.

### Workflow 2: Processed Merged File
Use this workflow if you have a single, merged CoreMS file containing all samples (only peaks where formulas have been assigned and unknowns were removed).
1. **Format Input:** Run the R script `1_Create_MetaboDirect_input.Rmd` to format your merged CoreMS file (e.g., the output from preprocessing/merging scripts) into a MetaboDirect-compatible format.
2. **Run Pipeline:** Use the Jupyter notebook `corems_transformations_processed.ipynb` to process the formatted data.

---

## Downstream Analysis (R Scripts)

Once you have generated the output tables using either of the workflows above, you can perform downstream analysis using the following R Markdown scripts:

* **`2_Network_index.Rmd`**: Calculates network indices.
* **`3_Transformations_analysis_total_tranformations.Rmd`**: Performs statistical and exploratory analysis on the total identified transformations.

Both scripts are compatible with the outputs of **both** Workflow 1 and Workflow 2.