## About 

The Computational Infrastructure for Geodynamics is a community of software users and user-developers who model physical processes in the Earth and planetary interiors.  

This application contains papers from the CIG Publication database from 2010-2018, and accompanies publication Hwang et al. (2019).  

The analysis behind this paper, and the source code for this app are available at [this Github repository](https://github.com/richpauloo/cig_nlp).  


## How to Interpret Plots

Each plot under the `Explore` tab represents a different CIG software.  

Each line along the y axis is a different paper that mentions that software. Each dot along each line is the estimated location of that software's mention in the document.   


## How to Use this Application

The plot is interactive.  

* Select different software by clicking on the buttons in the left-hand toolbar.   

* Hover over points to view the paper filename, and the context in which the software was mentioned in that paper.  

* Zoom in on a region of the plot by left-clicking and dragging a rectangle over an area of interest.  

* Click on the purple and black dots in the upper-left plot to toggle black adn purple points on and off.   

* The buttons in the upper-right plot may be used to export a view to PNG, pan, zoom in, zoom out, and return to the original zoom level.  


## Method  

In order to understand how software are mentioned in CIG publications, we employed methods from natural language processing (also called text mining) implemented in R ( R Crore Team, 2018) and the `pdftools` R package (Ooms, 2017). This allowed us to programmatically parse the entire CIG publication database through 2018 (n = 713), and build a data pipeline that is transparent, reproducible, and extensible to new papers. This data pipeline consisted of three main steps: (1) parsing PDFs and extracting software mentions and the context in which they appear, (2) iteratively refining a whitelist of software names and variants, and (3) examining the position of each software mention and co-mentioned software.

The first task in most natural language processing pipelines is called tokenization (Webster, 1992). Tokenization simply divides a document into to set of elements (tokens) that can be processed individually. We sought to understand the context in which CIG software are mentioned, thus for each PDF, we extracted the raw text, then tokenized sentences by splitting at period (“.”) punctuation marks.  

Next, we refined a whitelist of software names. We started with a whitelist of CIG software names and located sentences within each document that mentioned a software name on our whitelist. In order to capture the context in which the software was mentioned, we extracted these sentences, and the two preceding and following sentences, which were exported for further analysis. Many CIG software have name aliases (e.g. -  specfem2d, specfem3d_globe, specfem3d_cartesian, and Specfem3-D Globe are all variants of SPECFEM), thus we identified papers that did not return a software mention, manually searched those papers for name variants, and added those variants to our whitelist. This process was iteratively repeated until the whitelist contained all name variants (Table of name variants in appendix), and all papers that mentioned a CIG software were identified by the data pipeline. 

With a working data pipeline, we determined the position of each software mention in each document by counting the number of characters at which the software mention begins. We experimented with using sentence tokens as a unit of location within a document, but found that tokens in the “References” section tended to be much shorter than tokens elsewhere, which skewed the results. Moreover, nearly all papers in the database are formatted with two columns, and text parsing occurs across these columns, thus there is error associated with the estimated location of each software mention. We conservatively quantify the maximum error of each software mention’s location as half of the average number of characters per page, per document. Thus for longer documents with more characters, we observe a smaller margin of error. Lastly, for each PDF, we compute whether multiple software mentions occur in the same text to understand the linkages between different communities of software users.


## Contact  

For questions about the CIG: Lorraine Hwang [ljhwang at ucdavis dot edu]  

For questions about this app: Rich Pauloo [rpauloo at ucdavis dot edu]  


## References  

1. R  Core  Team,  R:  A  Language  and  Environment  for  Statistical  Computing,  R  Foundation  for  Statistical Computing, Vienna, Austria, URL [`https://www.R-project.org`](https://www.R-project.org), 2018  

2. Jeroen Ooms (2017). pdftools: Text Extraction, Rendering and Converting of PDF Documents. R package version 1.4. [`https://CRAN.R-project.org/package=pdftools`](https://CRAN.R-project.org/package=pdftools)  

3. Webster, J. J., & Kit, C. (1992). Tokenization as the initial phase in NLP. In *COLING 1992 Volume 4: The 15th International Conference on Computational Linguistics* (Vol. 4).
