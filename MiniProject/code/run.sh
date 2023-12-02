#!/bin/bash

# Stop script on error
set -e


# Run the R scripts
echo "Running data preprocessing..."
Rscript data_pre.R

echo "Running data analysis..."
Rscript dataanalysis.R

echo "Generating plots..."
Rscript plot1.R

# Compile the LaTeX document to PDF (adjust path to your LaTeX installation if necessary)
echo "Compiling LaTeX document to PDF..."
pdflatex myreport
bibtex myreport
pdflatex myreport
pdflatex myreport

echo "All processes completed successfully!"
