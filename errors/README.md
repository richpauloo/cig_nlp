# README

The scripts and files herein will reproduce errors encountered by `readPDFXML()` and `getSectionText()` to aid the development of their respective packages.  

# Getting Started  

1. Clone this folder  
2. Open and run `01_reproduce_errors.R` in `readPDFXML_errors` and `getSectionText_errors`, to reproduce errors associated with each function.  
3. Within `01_reproduce_errors.R`, you will need to change local path variables to point to various folders.  

# Notes

* `00_log_errors.R` can be ignored. It's just a script to locate errors, copy the offending files, and so forth.  

* The PDFs and XML files that generate the errors are included in `readPDFXML_errors` and `getSectionText_errors` under the `pdf` and `xml` directories respectively.  
